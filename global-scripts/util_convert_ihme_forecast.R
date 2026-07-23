# util_convert_ihme_forecast.R — Convert one IHME GBD cause-specific rate
# forecast + the matching population forecast into the pipeline's mortality
# RDS format.
#
# Inputs:
#   data/gbd-forecasts/{cause_file}.csv  — rates per (Location, Age Group,
#       Sex, Year) with 500 draws
#   data/gbd-forecasts/population.csv     — population per (Location, Age
#       Group, Sex, Year) with 500 draws
#   from-samuel/IHME_GBD_..._HIERARCHIES_*.XLSX — Location Name -> loc_id
#       (sheet "All Location Hierarchies"). Cached as RDS on first run.
#
# Output:
#   data/mortality/{LOCATION_ID}_mortality_ihme_{acause}.rds  — counts at
#       (year_id, age_group_id, sex_id, acause) granularity. The downstream
#       pipeline doesn't currently carry a mortality-draw dimension, so this
#       MVP saves the per-row mean of (rate_draw_i * pop_draw_i) across draws.
#       Per-draw mortality propagation is a follow-on.
#
# Usage:
#   Rscript util_convert_ihme_forecast.R \
#     --location_id=125 \
#     --cause_file=data/gbd-forecasts/ischemic_heart_disease.csv \
#     --acause=cvd_ihd \
#     --pop_file=data/gbd-forecasts/population.csv

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

defaults <- list(
  CAUSE_FILE = file.path(DATA_DIR, "gbd-forecasts", "ischemic_heart_disease.csv"),
  ACAUSE     = "cvd_ihd",
  POP_FILE   = file.path(DATA_DIR, "gbd-forecasts", "population.csv"),
  HIERARCHY_FILE = file.path(PROJECT_ROOT, "from-samuel",
                             "IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX")
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

# =============================================================================
# Lookup tables
# =============================================================================

# IHME age-group text -> pipeline age_group_id. We collapse "<1 year" + "1 to 4"
# into id=0 (matches our "0-4") and 80..95+ into id=80 (matches our ">80").
build_age_map <- function() {
  data.table(
    age_text = c("<1 year",   "1 to 4",
                 "5 to 9",    "10 to 14",  "15 to 19",  "20 to 24",
                 "25 to 29",  "30 to 34",  "35 to 39",  "40 to 44",
                 "45 to 49",  "50 to 54",  "55 to 59",  "60 to 64",
                 "65 to 69",  "70 to 74",  "75 to 79",
                 "80 to 84",  "85 to 89",  "90 to 94",  "95 plus"),
    age_group_id = c(0L, 0L,
                     5L, 10L, 15L, 20L,
                     25L, 30L, 35L, 40L,
                     45L, 50L, 55L, 60L,
                     65L, 70L, 75L,
                     80L, 80L, 80L, 80L)
  )
}

# GBD loc_id <-> ISO3 (+ a clean country name) from the GBD2023 shapefile
# attribute table (national rows). Same source the life-table crosswalk uses
# (util_download_un_lifetables.R) so the two stay consistent. The dbf's loc_name
# is well-encoded UTF-8, unlike the hierarchy .xlsx, which is double-encoded for
# at least Türkiye (loc_id 155); callers use name_clean for name-fallback
# matching so that corruption can't drop a location. Returns a
# (loc_id, iso3, name_clean) data.table, or NULL if the .dbf is absent.
build_gbd_iso3 <- function(shp_path = file.path(PROJECT_ROOT, "data", "shapefiles",
                                                "GBD2023_mapping_final.shp")) {
  dbf <- sub("\\.shp$", ".dbf", shp_path)
  if (!file.exists(dbf)) {
    warning("GBD shapefile .dbf not found (", dbf, "); ISO3 matching disabled, ",
            "falling back to country-name matching only.")
    return(NULL)
  }
  if (!requireNamespace("foreign", quietly = TRUE)) {
    warning("Package 'foreign' not available; ISO3 matching disabled, ",
            "falling back to country-name matching only.")
    return(NULL)
  }
  d <- as.data.table(foreign::read.dbf(dbf, as.is = TRUE))
  unique(d[level == 3 & !is.na(ihme_lc_id) & ihme_lc_id != "",
           .(loc_id = as.integer(loc_id), iso3 = toupper(ihme_lc_id),
             name_clean = as.character(loc_name))])
}

# Country-level location map: loc_id, country name, and ISO3. The ISO3 column
# lets downstream converters match forecast files on a stable code rather than a
# country-name string (which drifts across GBD vintages, e.g. Turkey ->
# Türkiye, and breaks on encoding differences). Cached; rebuilt when the
# hierarchy .xlsx or the .dbf is newer than the cache, or when an older cache
# without the iso3 column is found.
build_loc_map <- function(hierarchy_xlsx) {
  cache <- file.path(INTERMEDIATE_DIR, "ihme_loc_map.rds")
  iso3  <- build_gbd_iso3()
  dbf_mtime <- {
    dbf <- file.path(PROJECT_ROOT, "data", "shapefiles", "GBD2023_mapping_final.dbf")
    if (file.exists(dbf)) file.info(dbf)$mtime else as.POSIXct(0, origin = "1970-01-01")
  }
  if (file.exists(cache)) {
    cached <- readRDS(cache)
    fresh <- file.info(cache)$mtime > file.info(hierarchy_xlsx)$mtime &&
             file.info(cache)$mtime > dbf_mtime
    if (fresh && all(c("iso3", "name_clean") %in% names(cached))) return(cached)
  }
  log_msg("Building location-name/ISO3 -> loc_id map from ", hierarchy_xlsx)
  h <- as.data.table(read_excel(hierarchy_xlsx, sheet = "All Location Hierarchies"))
  setnames(h, c("Location ID", "Location Name", "Level"),
              c("loc_id",      "location",      "level"))
  # Sovereign-nation level is 3 in IHME's standard hierarchy; we keep level 3
  # only since the forecast files are at the country level.
  h <- unique(h[level == 3, .(location, loc_id)])
  if (!is.null(iso3)) {
    h <- merge(h, iso3, by = "loc_id", all.x = TRUE)
    n_no_iso3 <- h[is.na(iso3), .N]
    if (n_no_iso3 > 0) {
      log_msg("  note: ", n_no_iso3, " of ", nrow(h),
              " countries have no ISO3 in the .dbf (name-match only for those)")
    }
  } else {
    h[, `:=`(iso3 = NA_character_, name_clean = NA_character_)]
  }
  # Prefer the dbf's clean name for name-fallback matching; fall back to the
  # hierarchy name where the dbf has no row.
  h[is.na(name_clean), name_clean := location]
  setcolorder(h, c("loc_id", "location", "name_clean", "iso3"))
  saveRDS(h, cache)
  log_msg("Cached IHME loc map (", nrow(h), " countries, ",
          h[!is.na(iso3), .N], " with ISO3) -> ", cache)
  h
}

sex_map <- function(s) fifelse(s == "Male", 1L, fifelse(s == "Female", 2L, NA_integer_))

# =============================================================================
# Streamed read: filter to (location, valid-age, single-sex) at fread time so
# we don't materialise the full 5-7 GB CSV in memory. Keeps age_text so the
# caller can match rate rows to pop rows at the SOURCE bin before collapsing
# duplicates that share a target age_group_id.
# =============================================================================
read_ihme_csv <- function(path, target_location, age_map) {
  log_msg("Reading ", basename(path))
  raw <- fread(path)
  setnames(raw, c("Location", "Age Group", "Sex", "Year"),
                c("location", "age_text", "sex_text", "year_id"))
  raw <- raw[location == target_location
             & sex_text %in% c("Male", "Female")
             & age_text %in% age_map$age_text]
  raw[, location := NULL]
  setkey(raw, year_id, age_text, sex_text)
  raw
}

# =============================================================================
# Main
# =============================================================================
convert_one <- function() {
  age_map <- build_age_map()
  loc_map <- build_loc_map(HIERARCHY_FILE)

  loc_row <- loc_map[loc_id == LOCATION_ID]
  if (nrow(loc_row) != 1) {
    stop("No unique level-3 IHME location for loc_id=", LOCATION_ID,
         " (found ", nrow(loc_row), " rows)")
  }
  target_location <- loc_row$location
  log_msg("Target location: ", target_location, " (loc_id=", LOCATION_ID, ")")

  rate <- read_ihme_csv(CAUSE_FILE, target_location, age_map)
  pop  <- read_ihme_csv(POP_FILE,   target_location, age_map)
  log_msg("rate rows: ", nrow(rate), " | pop rows: ", nrow(pop))

  # IHME's population file extends one year earlier than the cause forecasts
  # (rates: 2022-2050, pop: 2021-2050). Intersect on the rate years.
  common_years <- intersect(rate$year_id, pop$year_id)
  if (length(common_years) < length(unique(rate$year_id))) {
    stop("Rate has years not in pop: ",
         paste(setdiff(rate$year_id, pop$year_id), collapse = ","))
  }
  rate <- rate[year_id %in% common_years]
  pop  <- pop[year_id  %in% common_years]
  setkey(rate, year_id, age_text, sex_text)
  setkey(pop,  year_id, age_text, sex_text)
  log_msg("After year-intersect: rate ", nrow(rate), " | pop ", nrow(pop),
          " over ", length(common_years), " years")

  draw_cols <- grep("^draw_\\d+$", names(rate), value = TRUE)
  if (!identical(draw_cols, grep("^draw_\\d+$", names(pop), value = TRUE))) {
    stop("Rate and pop have different draw-column sets")
  }
  log_msg("Draws on both sides: ", length(draw_cols))

  # Sanity check that the keyed rows align (same year/age/sex grain on both).
  key_cols <- c("year_id", "age_text", "sex_text")
  if (!identical(rate[, ..key_cols], pop[, ..key_cols])) {
    stop("Rate and pop rows do not align on (year, age_text, sex_text). ",
         "Suggests one file is missing rows the other has.")
  }

  # Element-wise per-draw counts: count_draw_i = rate_draw_i * pop_draw_i.
  # Emit long-form (year, age, sex, draw, deaths). Two sink files:
  #   {LOC}_mortality_ihme_{acause}.rds         — mean across draws (back-
  #       compat for code paths that don't yet handle mortality draws)
  #   {LOC}_mortality_ihme_{acause}_draws.rds    — per-draw long form
  # The pipeline's 04 picks whichever the user points it at via
  # --mortality_file=.
  counts_mat <- as.matrix(rate[, ..draw_cols]) * as.matrix(pop[, ..draw_cols])
  out <- rate[, ..key_cols]
  out[, age_group_id := age_map$age_group_id[match(age_text, age_map$age_text)]]
  out[, sex_id := sex_map(sex_text)]
  out[, age_text := NULL]
  out[, sex_text := NULL]

  # --- per-draw long form ---
  draws_long <- cbind(out, as.data.table(counts_mat))
  draws_long <- melt(draws_long,
                     id.vars = c("year_id", "age_group_id", "sex_id"),
                     variable.name = "draw_text", value.name = "deaths",
                     variable.factor = FALSE)
  draws_long[, draw := as.integer(sub("^draw_", "", draw_text))]
  draws_long[, draw_text := NULL]
  # Collapse source bins that share a target age_group_id (sum within draw).
  draws_long <- draws_long[, .(deaths = sum(deaths, na.rm = TRUE)),
                           by = .(year_id, age_group_id, sex_id, draw)]
  draws_long[, acause := ACAUSE]
  draws_long[, location_id := LOCATION_ID]

  draws_path <- file.path(MORTALITY_DIR,
                          sprintf("%d_mortality_ihme_%s_draws.rds",
                                  LOCATION_ID, ACAUSE))
  saveRDS(draws_long, draws_path)
  log_msg("Saved per-draw long form (", nrow(draws_long), " rows = ",
          uniqueN(draws_long$year_id), " yrs x ",
          uniqueN(draws_long$age_group_id), " ages x 2 sexes x ",
          uniqueN(draws_long$draw), " draws) -> ", draws_path)

  # --- mean across draws (back-compat) ---
  mort_mean <- draws_long[, .(deaths = mean(deaths, na.rm = TRUE)),
                          by = .(year_id, age_group_id, sex_id)]
  mort_mean[, acause := ACAUSE]
  mort_mean[, location_id := LOCATION_ID]

  mean_path <- file.path(MORTALITY_DIR,
                         sprintf("%d_mortality_ihme_%s.rds",
                                 LOCATION_ID, ACAUSE))
  saveRDS(mort_mean, mean_path)
  log_msg("Saved mean form (", nrow(mort_mean), " rows) -> ", mean_path)

  invisible(list(mean = mort_mean, draws = draws_long))
}

if (sys.nframe() == 0L) {
  convert_one()
}
