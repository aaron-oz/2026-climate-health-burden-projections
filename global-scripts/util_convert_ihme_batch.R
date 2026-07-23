# util_convert_ihme_batch.R — Convert every (cause, location) pairing from
# IHME's downloaded forecast CSVs into pipeline-format mortality RDS files,
# in one serial run.
#
# Caspar's workflow:
#   1. Download the 17 cause-specific CSVs and the population CSV from
#      IHME's GBD Results Tool into ANY directory he likes.
#   2. Run this script with --source_dir=<that path>. It reads each cause
#      CSV ~once, iterates all ~205 IHME locations internally, and writes
#      per-(location, cause) RDS files to
#      data/mortality/{LOC}_mortality_ihme_{acause}{,_draws}.rds.
#
# File-name matching: config.R::IHME_CAUSE_FILES has expected filenames per
# cause, but those are used as SUBSTRING PATTERNS — so an IHME-UI-generated
# name like "IHME-GBD_2021_DATA-ischemic_heart_disease-1.csv" still matches
# the "ischemic_heart_disease" pattern for cvd_ihd. The discovered file map
# is printed up front so any mismatches are visible before the run starts.
#
# Naive looping (calling util_convert_ihme_forecast.R 17 × 205 = 3,485
# times) would re-read the 6 GB CSV ~205 times per cause. This batch script
# reads each cause CSV once, then loops locations in-memory.
#
# Skips any (cause, location) where the output file already exists — safe to
# re-run after a partial failure.
#
# Usage:
#   Rscript global-scripts/util_convert_ihme_batch.R \
#     --source_dir=/path/to/where/csvs/were/downloaded
#   Rscript global-scripts/util_convert_ihme_batch.R \
#     --source_dir=... --causes=cvd_ihd,cvd_stroke
#   Rscript global-scripts/util_convert_ihme_batch.R \
#     --source_dir=... --locations=125,135,102
#   Rscript global-scripts/util_convert_ihme_batch.R \
#     --source_dir=... --force=TRUE   # ignore existing outputs

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
source(file.path(SCRIPTS_DIR, "util_convert_ihme_forecast.R"))
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

defaults <- list(
  SOURCE_DIR = file.path(DATA_DIR, "gbd-forecasts"),
  CAUSES     = paste(GBD_CAUSES, collapse = ","),
  LOCATIONS  = "",   # blank means "all locations from the IHME hierarchy"
  FORCE      = FALSE
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

# =============================================================================
# Location-key resolution
#
# The forecast CSVs are matched to GBD loc_ids on the most stable key the file
# offers, in priority order:
#   1. a numeric location_id column        -> used directly
#   2. an ISO3 / ihme_lc_id column         -> crosswalked to loc_id via loc_map
#   3. the country-name string (fallback)  -> normalized match + alias table
# ISO3/id matching is preferred because country-name strings drift across GBD
# vintages (e.g. Turkey -> Türkiye in GBD2023) and break on text-encoding
# differences; a name-only match silently drops such locations.
# =============================================================================

# Normalize a country name for tolerant matching: transliterate accents to
# ASCII, lowercase, collapse non-alphanumerics to single spaces, trim. Makes
# "Türkiye", a mis-encoded "TÃ¼rkiye", and "turkiye" collapse to one key. Does
# NOT bridge genuine renames (Turkey vs Türkiye); that's NAME_ALIASES + ISO3.
norm_name <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Drop qualifier suffixes that GBD/UN-style long names carry but the GBD
# hierarchy's short names do not, e.g.
#   "Taiwan (Province of China)" -> "Taiwan"
#   "Virgin Islands, U.S."       -> "Virgin Islands"
# Applied as a second matching pass only; exact matches always win.
strip_qualifiers <- function(x) {
  x <- as.character(x)
  x <- gsub("\\s*\\([^)]*\\)", "", x)   # parenthetical qualifier
  x <- sub(",.*$", "", x)               # trailing comma-qualifier
  trimws(x)
}

# Known GBD-hierarchy <-> forecast-file country-name divergences, keyed and
# valued in normalized form. Only consulted on the name-fallback path, and only
# for divergences that qualifier-stripping cannot resolve. Extend as new
# mismatches surface; the durable fix is an ISO3/id column in the source.
NAME_ALIASES <- c(
  "turkey"         = "turkiye",   # GBD2023 hierarchy: "Türkiye"; older forecasts: "Turkey"
  "chinese taipei" = "taiwan"     # occasional alternate rendering of loc_id 8
)

# Add a canonical `loc_id` column to a source frame `dt` (already column-renamed
# so its country-name column is `location`). Returns the strategy used, for
# logging. Sets loc_id = NA on rows that don't resolve.
add_source_loc_id <- function(dt, loc_map) {
  cn <- names(dt)
  pick <- function(cands) {
    hit <- cn[tolower(cn) %in% tolower(cands)]
    if (length(hit)) hit[1] else NA_character_
  }
  id_col  <- pick(c("location_id", "location id"))
  iso_col <- pick(c("iso3", "ihme_lc_id", "iso_code", "iso3_code"))

  if (!is.na(id_col)) {
    dt[, loc_id := suppressWarnings(as.integer(get(id_col)))]
    return(sprintf("numeric '%s' column", id_col))
  }
  if (!is.na(iso_col) && any(!is.na(loc_map$iso3))) {
    xw <- loc_map[!is.na(iso3)]
    dt[, loc_id := xw$loc_id[match(toupper(as.character(get(iso_col))), xw$iso3)]]
    return(sprintf("ISO3 '%s' column via dbf crosswalk", iso_col))
  }
  # Key on the clean dbf name where available (the hierarchy .xlsx is
  # double-encoded for at least Türkiye); alias the source side for known
  # cross-vintage renames. Matching runs in passes, exact first:
  #   1. normalized source name (+ alias) vs normalized hierarchy name
  #   2. qualifier-stripped source name vs normalized hierarchy name
  #      ("Taiwan (Province of China)" -> "Taiwan")
  #   3. normalized source name vs qualifier-stripped hierarchy name
  #      ("Virgin Islands" -> "Virgin Islands, U.S.")
  name_col <- if ("name_clean" %in% names(loc_map)) "name_clean" else "location"
  nm <- loc_map[, .(loc_id, key = norm_name(get(name_col)))]

  # Stripped hierarchy keys for pass 3. Keys that collide with an exact key, or
  # that map to more than one loc_id once stripped, are dropped so stripping can
  # never silently mis-assign a location.
  nm_strip <- unique(loc_map[, .(loc_id, key = norm_name(strip_qualifiers(get(name_col))))])
  nm_strip <- nm_strip[!key %in% nm$key]
  key_n    <- nm_strip[, .(n = uniqueN(loc_id)), by = key]
  nm_strip <- unique(nm_strip[key %in% key_n[n == 1L]$key], by = "key")

  src_raw <- norm_name(dt$location)
  aliased <- NAME_ALIASES[src_raw]
  src_key <- ifelse(!is.na(aliased), aliased, src_raw)

  out <- nm$loc_id[match(src_key, nm$key)]                       # pass 1
  if (anyNA(out)) {                                              # pass 2
    src_strip <- norm_name(strip_qualifiers(dt$location))
    need <- is.na(out)
    out[need] <- nm$loc_id[match(src_strip[need], nm$key)]
  }
  if (anyNA(out) && nrow(nm_strip) > 0) {                        # pass 3
    need <- is.na(out)
    out[need] <- nm_strip$loc_id[match(src_raw[need], nm_strip$key)]
  }
  dt[, loc_id := out]
  "normalized country name (+ alias/qualifier passes)"
}

# =============================================================================
# Convert all (location) pairings for ONE cause file
# =============================================================================
convert_one_cause_all_locations <- function(acause, cause_file_path, pop_file_path,
                                            location_ids, age_map, loc_map,
                                            force = FALSE) {
  t0 <- Sys.time()
  log_msg(sprintf("[%s] reading %s + %s",
                  acause, basename(cause_file_path), basename(pop_file_path)))

  # Read both CSVs once. ~5-15 s each at NVMe speeds.
  rate_full <- fread(cause_file_path)
  pop_full  <- fread(pop_file_path)

  setnames(rate_full, c("Location", "Age Group", "Sex", "Year"),
                     c("location", "age_text", "sex_text", "year_id"))
  setnames(pop_full,  c("Location", "Age Group", "Sex", "Year"),
                     c("location", "age_text", "sex_text", "year_id"))

  # Keep only the granular 5-year bins + valid sexes.
  rate_full <- rate_full[sex_text %in% c("Male", "Female") & age_text %in% age_map$age_text]
  pop_full  <- pop_full[ sex_text %in% c("Male", "Female") & age_text %in% age_map$age_text]

  draw_cols <- grep("^draw_\\d+$", names(rate_full), value = TRUE)
  if (!identical(draw_cols, grep("^draw_\\d+$", names(pop_full), value = TRUE))) {
    stop("Rate and pop have different draw-column sets for ", acause)
  }

  # Match IHME's annual ranges (pop has one extra year, 2021, vs rate's 2022+)
  common_years <- intersect(rate_full$year_id, pop_full$year_id)
  rate_full <- rate_full[year_id %in% common_years]
  pop_full  <- pop_full [year_id %in% common_years]

  # Resolve GBD loc_id on both frames (ISO3/id preferred, name as fallback) so
  # the per-location loop matches on loc_id rather than a fragile name string.
  strategy <- add_source_loc_id(rate_full, loc_map)
  add_source_loc_id(pop_full, loc_map)
  setkey(rate_full, loc_id)
  setkey(pop_full,  loc_id)
  log_msg(sprintf("[%s] loaded; %d locations available, matched via %s",
                  acause, uniqueN(rate_full$loc_id), strategy))

  n_done <- 0L; n_skip <- 0L; n_miss <- 0L
  missing_ids <- integer(0)
  for (this_loc in location_ids) {
    out_path <- file.path(MORTALITY_DIR,
                          sprintf("%d_mortality_ihme_%s_draws.rds", this_loc, acause))
    if (!force && file.exists(out_path)) { n_skip <- n_skip + 1L; next }

    loc_row <- loc_map[loc_id == this_loc]
    if (nrow(loc_row) != 1) { n_miss <- n_miss + 1L; missing_ids <- c(missing_ids, this_loc); next }

    rate <- rate_full[.(this_loc), nomatch = 0L]
    pop  <- pop_full[ .(this_loc), nomatch = 0L]
    if (nrow(rate) == 0 || nrow(pop) == 0) {
      loc_name <- if (!is.null(loc_row$name_clean) && !is.na(loc_row$name_clean))
                    loc_row$name_clean else loc_row$location
      warning(sprintf(
        "[%s] loc_id=%d (%s / %s): no rows in source forecast after %s matching -- skipped",
        acause, this_loc, loc_name,
        if (is.na(loc_row$iso3)) "no-ISO3" else loc_row$iso3, strategy))
      n_miss <- n_miss + 1L; missing_ids <- c(missing_ids, this_loc); next
    }

    setkey(rate, year_id, age_text, sex_text)
    setkey(pop,  year_id, age_text, sex_text)
    if (!identical(rate[, .(year_id, age_text, sex_text)],
                   pop [, .(year_id, age_text, sex_text)])) {
      warning(sprintf("[%s loc=%d] rate/pop key mismatch -- skipping", acause, this_loc))
      n_miss <- n_miss + 1L; next
    }

    # Element-wise per-draw counts
    counts_mat <- as.matrix(rate[, ..draw_cols]) * as.matrix(pop[, ..draw_cols])
    out_long <- cbind(rate[, .(year_id, age_text, sex_text)],
                      as.data.table(counts_mat))
    out_long <- melt(out_long,
                     id.vars = c("year_id", "age_text", "sex_text"),
                     variable.name = "draw_text", value.name = "deaths",
                     variable.factor = FALSE)
    out_long[, draw := as.integer(sub("^draw_", "", draw_text))]
    out_long[, draw_text := NULL]
    out_long[, age_group_id := age_map$age_group_id[match(age_text, age_map$age_text)]]
    out_long[, sex_id := sex_map(sex_text)]
    out_long[, c("age_text", "sex_text") := NULL]

    # Collapse source bins that share a target age_group_id
    draws_long <- out_long[, .(deaths = sum(deaths, na.rm = TRUE)),
                           by = .(year_id, age_group_id, sex_id, draw)]
    draws_long[, acause := acause]
    draws_long[, location_id := this_loc]
    saveRDS(draws_long, out_path)

    # Mean form (back-compat for code paths that don't carry mortality draws)
    mort_mean <- draws_long[, .(deaths = mean(deaths, na.rm = TRUE)),
                            by = .(year_id, age_group_id, sex_id)]
    mort_mean[, acause := acause]
    mort_mean[, location_id := this_loc]
    saveRDS(mort_mean,
            file.path(MORTALITY_DIR,
                      sprintf("%d_mortality_ihme_%s.rds", this_loc, acause)))
    n_done <- n_done + 1L
  }

  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  log_msg(sprintf(
    "[%s] done: %d converted, %d skipped (cached), %d missing -- %.1fs",
    acause, n_done, n_skip, n_miss, elapsed))
  if (length(missing_ids) > 0) {
    log_msg(sprintf("[%s] MISSING loc_ids (no source match): %s",
                    acause, paste(sort(missing_ids), collapse = ",")))
  }
  invisible(list(done = n_done, skipped = n_skip, missing = n_miss,
                 missing_ids = sort(missing_ids)))
}

# =============================================================================
# Main
# =============================================================================
batch_convert <- function() {
  causes <- strsplit(as.character(CAUSES), ",", fixed = TRUE)[[1]]
  unknown_causes <- setdiff(causes, names(IHME_CAUSE_FILES))
  if (length(unknown_causes) > 0) {
    stop("Unknown causes (no entry in IHME_CAUSE_FILES): ",
         paste(unknown_causes, collapse = ", "))
  }

  source_dir <- as.character(SOURCE_DIR)
  if (!dir.exists(source_dir)) {
    stop("source_dir does not exist: ", source_dir)
  }
  log_msg("Scanning source dir: ", source_dir)
  csvs_in_dir <- list.files(source_dir, pattern = "\\.csv$",
                            full.names = TRUE, ignore.case = TRUE)
  if (length(csvs_in_dir) == 0) {
    stop("No CSV files found in source_dir: ", source_dir)
  }

  # Cause-file discovery: for each cause's expected filename, try (a) exact
  # match in source_dir, then (b) substring match against the basename of
  # files in source_dir. The basename of the expected filename (without
  # extension) is the pattern. The first single match wins; multi-matches
  # warn and pick the first.
  discover_file <- function(expected_filename) {
    exact <- file.path(source_dir, expected_filename)
    if (file.exists(exact)) return(exact)
    pattern <- tools::file_path_sans_ext(basename(expected_filename))
    matches <- csvs_in_dir[grepl(pattern, basename(csvs_in_dir),
                                  fixed = TRUE, ignore.case = TRUE)]
    if (length(matches) == 0) return(NA_character_)
    if (length(matches) >  1) {
      warning(sprintf("Multiple files match '%s' (using first): %s",
                      pattern, paste(basename(matches), collapse = ", ")))
    }
    matches[1]
  }

  # Build cause -> file map and print before any heavy work.
  cause_file_map <- vapply(causes,
    function(c) discover_file(IHME_CAUSE_FILES[[c]]),
    character(1))
  pop_file_path <- discover_file("population.csv")

  cat("\n--- File discovery ---\n")
  cat(sprintf("%-18s | %-50s | %s\n", "cause", "expected pattern", "matched file"))
  cat(strrep("-", 95), "\n", sep = "")
  for (c in causes) {
    cat(sprintf("%-18s | %-50s | %s\n",
                c, IHME_CAUSE_FILES[[c]],
                if (is.na(cause_file_map[c])) "*** NOT FOUND ***"
                else basename(cause_file_map[c])))
  }
  cat(sprintf("%-18s | %-50s | %s\n",
              "(population)", "population.csv",
              if (is.na(pop_file_path)) "*** NOT FOUND ***"
              else basename(pop_file_path)))
  cat("\n")
  if (is.na(pop_file_path)) {
    stop("Population file not found in ", source_dir,
         ". Expected a CSV whose name contains 'population'.")
  }

  age_map <- build_age_map()
  loc_map <- build_loc_map(file.path(PROJECT_ROOT, "from-samuel",
                                     "IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX"))

  if (nzchar(as.character(LOCATIONS))) {
    location_ids <- as.integer(strsplit(as.character(LOCATIONS),
                                        ",", fixed = TRUE)[[1]])
  } else {
    location_ids <- loc_map$loc_id
  }
  log_msg(sprintf("Batch convert: %d causes x %d locations = %d (cause,loc) pairs",
                  length(causes), length(location_ids),
                  length(causes) * length(location_ids)))

  summary_rows <- list()
  for (i in seq_along(causes)) {
    cause <- causes[i]
    cause_file_path <- cause_file_map[cause]
    if (is.na(cause_file_path)) {
      log_msg(sprintf("[%s] CAUSE FILE NOT FOUND (pattern '%s') -- skipping",
                      cause, IHME_CAUSE_FILES[[cause]]))
      summary_rows[[i]] <- data.table(acause = cause, done = 0L, skipped = 0L,
                                      missing = NA, file_missing = TRUE)
      next
    }
    r <- convert_one_cause_all_locations(cause, cause_file_path, pop_file_path,
                                         location_ids, age_map, loc_map,
                                         force = isTRUE(FORCE))
    summary_rows[[i]] <- data.table(acause = cause,
                                    done = r$done, skipped = r$skipped,
                                    missing = r$missing, file_missing = FALSE,
                                    missing_ids = paste(r$missing_ids, collapse = " "))
  }

  summary <- rbindlist(summary_rows, fill = TRUE)
  manifest_path <- file.path(OUTPUT_DIR, "ihme_batch_manifest.csv")
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  fwrite(summary, manifest_path)
  log_msg("Manifest: ", manifest_path)
  print(summary)
  invisible(summary)
}

if (sys.nframe() == 0L) batch_convert()
