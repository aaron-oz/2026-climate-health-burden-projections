# util_download_un_lifetables.R — Download life tables from UN Population Data Portal API
#
# Downloads life expectancy E(x) by single age, sex, year from the
# UN World Population Prospects 2024 via the Data Portal API.
#
# Requires a Bearer token. Get one at:
#   https://population.un.org/dataportalapi/index.html
#
# Usage:
#   export UN_API_TOKEN="your_token_here"
#   Rscript util_download_un_lifetables.R
#
# Output: data/lifetables/{gbd_location_id}_lifetable.csv

library(data.table)

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT", unset = dirname(normalizePath(SCRIPTS_DIR)))
LIFETABLE_DIR <- file.path(PROJECT_ROOT, "data", "lifetables")
dir.create(LIFETABLE_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Bearer token ---
TOKEN <- Sys.getenv("UN_API_TOKEN")
if (TOKEN == "") stop("Set UN_API_TOKEN environment variable first.")

# --- Settings ---
BASE_URL <- "https://population.un.org/dataportalapi/api/v1"
INDICATOR_ID <- 76  # Life expectancy E(x) - complete (single-year ages)

# --- Location crosswalk (UN M49 <-> GBD loc_id) -----------------------------
# Built from authoritative sources rather than a hand-maintained table (the old
# hardcoded map had e.g. loc_id 148 = Chile, but GBD2023 loc_id 148 = Morocco;
# Chile is 98). Two pieces, joined on ISO3:
#   GBD loc_id <-> ISO3 : from the GBD2023 shapefile .dbf (ihme_lc_id, level 3)
#   ISO3 <-> UN M49 id  : from the UN Data Portal /locations endpoint
#
# Prefer the augmented shapefile when it exists, matching config.R's
# DEFAULT_SHAPEFILE. The original GBD2023 file carries 198 level-3 rows; the
# burden universe is 204. The 6 it omits (Maldives 14, Marshall Islands 24,
# Monaco 367, Nauru 369, Tokelau 413, Tuvalu 416) have no geometry in the GBD
# mapping file, so building the crosswalk from it silently dropped them: their
# life tables were never requested, and every run of those locations died in
# 07_compute_ylls.R. util_augment_shapefile.R adds those 6 rows with their
# ihme_lc_id, which is all the crosswalk needs.
SHAPEFILE <- Sys.getenv("GBD_SHAPEFILE", local({
  shp_dir <- file.path(PROJECT_ROOT, "data", "shapefiles")
  aug <- file.path(shp_dir, "GBD2023_mapping_final_augmented.shp")
  if (file.exists(aug)) aug else file.path(shp_dir, "GBD2023_mapping_final.shp")
}))

# Optional subset: --locations=125,135 (GBD loc_ids). Empty = all national.
.args <- commandArgs(trailingOnly = TRUE)
.loc_arg <- sub("^--locations=", "",
                grep("^--locations=", .args, value = TRUE))
LOCATION_SUBSET <- if (length(.loc_arg) && nzchar(.loc_arg)) {
  as.integer(strsplit(.loc_arg, ",", fixed = TRUE)[[1]])
} else integer(0)

# GBD loc_id <-> ISO3 from the GBD2023 shapefile attribute table (national rows).
build_gbd_iso3 <- function(shp_path) {
  dbf <- sub("\\.shp$", ".dbf", shp_path)
  if (!file.exists(dbf)) stop("Shapefile .dbf not found: ", dbf,
                              " (set GBD_SHAPEFILE or unzip the GBD2023 shape).")
  d <- as.data.table(foreign::read.dbf(dbf, as.is = TRUE))
  nat <- d[level == 3 & !is.na(ihme_lc_id) & ihme_lc_id != "",
           .(loc_id = as.integer(loc_id), iso3 = toupper(ihme_lc_id))]
  unique(nat)
}

# ISO3 <-> UN M49 id from the UN Data Portal /locations endpoint (same auth and
# pipe-delimited CSV format as the /data endpoint).
fetch_un_locations <- function(token) {
  library(httr)
  url <- paste0(BASE_URL, "/locations?pagingInHeader=false&format=csv")
  resp <- GET(url, add_headers(Authorization = paste("Bearer", token)))
  if (status_code(resp) != 200)
    stop("UN /locations returned status ", status_code(resp))
  dt <- fread(text = content(resp, "text", encoding = "UTF-8"), sep = "|", skip = 1)
  setnames(dt, tolower(names(dt)))
  id_col  <- intersect(c("id", "locid", "location_id", "locationid"), names(dt))[1]
  iso_col <- intersect(c("iso3", "iso3_code", "iso3code"), names(dt))[1]
  if (is.na(id_col) || is.na(iso_col))
    stop("Could not find id/iso3 columns in UN /locations response. Columns: ",
         paste(names(dt), collapse = ", "))
  unique(dt[!is.na(get(iso_col)) & get(iso_col) != "",
            .(un_id = as.integer(get(id_col)), iso3 = toupper(get(iso_col)))])
}

build_crosswalk <- function(shp_path, token) {
  gbd <- build_gbd_iso3(shp_path)
  un  <- fetch_un_locations(token)
  xw  <- merge(gbd, un, by = "iso3")          # iso3, loc_id, un_id
  unmatched <- setdiff(gbd$iso3, un$iso3)
  if (length(unmatched) > 0)
    cat("  NOTE:", length(unmatched), "GBD national ISO3 codes had no UN match:",
        paste(head(unmatched, 20), collapse = ","), "\n")
  xw[order(loc_id)]
}

# Age group mapping: take ex at the integer start of each 5-year bin
# (0,5,...,80); the burden keys age_group_id on these integer age-starts.
AGE_STARTS <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)

# --- Download function ---
# The UN Data Portal rate-limits: when throttled it returns HTTP 200 with an
# empty body (header only), so an empty result is retryable, not "no data".
# Retry with exponential backoff; treat both non-200 and empty-200 as transient.
download_lifetable <- function(loc_un, token, max_tries = 6) {
  url <- paste0(BASE_URL, "/data/indicators/", INDICATOR_ID,
                "/locations/", loc_un,
                "/start/1990/end/2100",
                "?pagingInHeader=false&format=csv")
  library(httr)
  for (try in seq_len(max_tries)) {
    resp <- GET(url, add_headers(Authorization = paste("Bearer", token)))
    if (status_code(resp) == 200) {
      raw_text <- content(resp, "text", encoding = "UTF-8")
      dt <- tryCatch(fread(text = raw_text, sep = "|", skip = 1),
                     error = function(e) NULL)
      if (!is.null(dt) && nrow(dt) > 0) return(dt)
    }
    if (try < max_tries) {
      wait <- 2 * try   # 2,4,6,8,10s
      cat(sprintf("    transient (status %s, empty=%s); retry %d/%d after %ds\n",
                  status_code(resp),
                  status_code(resp) == 200, try, max_tries, wait))
      Sys.sleep(wait)
    }
  }
  warning("Gave up on UN location ", loc_un, " after ", max_tries, " tries")
  NULL
}

# --- Main ---
cat("=== Downloading UN WPP Life Tables ===\n")

cat("Building UN<->GBD crosswalk from", basename(SHAPEFILE), "+ UN /locations...\n")
xwalk <- build_crosswalk(SHAPEFILE, TOKEN)
if (length(LOCATION_SUBSET) > 0) {
  xwalk <- xwalk[loc_id %in% LOCATION_SUBSET]
  missing <- setdiff(LOCATION_SUBSET, xwalk$loc_id)
  if (length(missing) > 0)
    cat("  WARNING: requested loc_ids with no crosswalk entry:",
        paste(missing, collapse = ","), "\n")
}
cat("Crosswalk covers", nrow(xwalk), "location(s).\n")

# --force re-downloads even if the output exists (default: skip existing so a
# re-run only fills gaps left by rate-limiting).
FORCE <- any(grepl("^--force(=TRUE)?$", .args))

for (i in seq_len(nrow(xwalk))) {
  loc_un  <- xwalk$un_id[i]
  gbd_id  <- xwalk$loc_id[i]
  out_file <- file.path(LIFETABLE_DIR, paste0(gbd_id, "_lifetable.csv"))
  if (!FORCE && file.exists(out_file)) {
    cat("\nGBD", gbd_id, "(", xwalk$iso3[i], ") already present, skipping.\n")
    next
  }
  cat("\nDownloading: GBD", gbd_id, "(ISO3", xwalk$iso3[i],
      "/ UN ID", loc_un, ")...\n")

  raw <- download_lifetable(loc_un, TOKEN)
  Sys.sleep(1)   # gentle throttle between locations to avoid the rate limit

  if (is.null(raw) || nrow(raw) == 0) {
    cat("  No data returned, skipping\n")
    next
  }
  cat("  Raw rows:", nrow(raw), "\n")

  # Standardize column names to lowercase
  setnames(raw, tolower(names(raw)))

  # Filter: Median variant, Male + Female only
  raw <- raw[tolower(variant) == "median"]
  raw <- raw[tolower(sex) %in% c("male", "female")]
  # Integer GBD sex_id (1=male, 2=female) -- matches the burden; "M"/"F" would
  # not merge in 07_compute_ylls.R and would silently yield all-NA YLLs.
  raw[, sex_id := fifelse(tolower(sex) == "male", 1L, 2L)]

  # Extract year and age
  raw[, year_id := as.integer(timelabel)]
  raw[, age := as.integer(agestart)]
  raw[, ex := as.numeric(value)]

  # Take ex at the start of each 5-year age group. age_group_id is the integer
  # age-start (0,5,...,80) -- the format the burden carries and 07 merges on.
  out <- raw[age %in% AGE_STARTS]
  out[, age_group_id := as.integer(age)]

  # gbd_id comes from the crosswalk (set at the top of the loop).
  out <- out[, .(year_id, age_group_id, sex_id, ex, location_id = gbd_id)]

  # Save (out_file defined at loop top for the skip-existing check)
  fwrite(out, out_file)
  cat("  Saved:", nrow(out), "rows →", basename(out_file), "\n")
  cat("  Years:", paste(range(out$year_id), collapse = "–"), "\n")
  cat("  Age groups:", length(unique(out$age_group_id)), "\n")
}

cat("\n=== Download complete ===\n")
