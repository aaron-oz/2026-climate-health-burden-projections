# util_fetch_un_lifetables_bulk.R: build life tables from the UN WPP 2024 bulk
# CSVs, with no API token.
#
# Sibling to util_download_un_lifetables.R, which pulls the same numbers one
# location at a time from the UN Data Portal API. Prefer this script. It needs
# no credentials, makes two HTTP requests instead of ~204, and is unaffected by
# the API's rate limiting. It exists because the Data Portal was returning HTTP
# 502 service-wide on 2026-08-05 (its own index page included) while these
# static files stayed up, which left the API path unusable for a full day.
#
# Verified equivalent to the API path on 2026-08-05: tables built here
# reproduced all 198 then-existing API-derived tables in data/lifetables/
# exactly, zero difference in ex across 198 x 3,774 = 747,252 values, with
# identical row counts and key sets.
#
# Usage:
#   Rscript global-scripts/util_fetch_un_lifetables_bulk.R
#   Rscript global-scripts/util_fetch_un_lifetables_bulk.R --locations=14,413
#   Rscript global-scripts/util_fetch_un_lifetables_bulk.R --force
#
# Flags:
#   --locations=14,413   only these GBD loc_ids (default: every national one)
#   --force              overwrite life tables that already exist
#   --refetch            re-download the bulk CSVs instead of using the cache
#
# Output: data/lifetables/{gbd_location_id}_lifetable.csv, columns
#   year_id, age_group_id, sex_id, ex, location_id
# which is what 07_compute_ylls.R merges on.

library(data.table)

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
PROJECT_ROOT  <- Sys.getenv("PROJECT_ROOT", unset = dirname(normalizePath(SCRIPTS_DIR)))
LIFETABLE_DIR <- file.path(PROJECT_ROOT, "data", "lifetables")
# Where the ~290 MB of bulk CSVs are kept between runs. Override it to put them
# outside a synced or backed-up tree; the default sits under data/ to match the
# rest of the project layout.
CACHE_DIR     <- Sys.getenv("UN_WPP_BULK_DIR",
                            unset = file.path(PROJECT_ROOT, "data", "un-wpp-bulk"))
dir.create(LIFETABLE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE_DIR,     showWarnings = FALSE, recursive = TRUE)

# --- Settings ---------------------------------------------------------------

# Two files, because WPP splits estimates from projections. Both are needed for
# the pipeline's 1990-2100 span. Sizes are as observed on 2026-08-05 and are
# only used for a post-download sanity check, not an equality test, since UN
# may reissue them.
BASE_URL <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES"
BULK_FILES <- c("WPP2024_Life_Table_Abridged_Medium_1950-2023.csv.gz",
                "WPP2024_Life_Table_Abridged_Medium_2024-2100.csv.gz")
MIN_BYTES <- 50e6   # each file was ~145 MB; anything far below is a bad download

# The burden keys age_group_id on these integer age-starts, and 07 merges on it.
AGE_STARTS <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
YEAR_MIN <- 1990
YEAR_MAX <- 2100

# Prefer the augmented shapefile, matching config.R's DEFAULT_SHAPEFILE. The
# original GBD2023 file has only 198 level-3 rows against a 204-location burden
# universe: it carries no geometry for Maldives (14), Marshall Islands (24),
# Monaco (367), Nauru (369), Tokelau (413) or Tuvalu (416). Building a location
# list from it silently drops those six, which is exactly how they came to have
# no life table and to die in 07_compute_ylls.R. util_augment_shapefile.R adds
# them back with their ihme_lc_id, which is all that is needed here.
SHAPEFILE <- Sys.getenv("GBD_SHAPEFILE", local({
  shp_dir <- file.path(PROJECT_ROOT, "data", "shapefiles")
  aug <- file.path(shp_dir, "GBD2023_mapping_final_augmented.shp")
  if (file.exists(aug)) aug else file.path(shp_dir, "GBD2023_mapping_final.shp")
}))

.args <- commandArgs(trailingOnly = TRUE)
.loc_arg <- sub("^--locations=", "", grep("^--locations=", .args, value = TRUE))
LOCATION_SUBSET <- if (length(.loc_arg) && nzchar(.loc_arg)) {
  as.integer(strsplit(.loc_arg, ",", fixed = TRUE)[[1]])
} else integer(0)
# --force overwrites existing life tables. It deliberately does NOT re-download
# the ~290 MB of bulk CSVs: use --refetch for that, on its own or with --force.
FORCE   <- any(grepl("^--force(=TRUE)?$",   .args))
REFETCH <- any(grepl("^--refetch(=TRUE)?$", .args))

# --- GBD loc_id <-> ISO3 ----------------------------------------------------
# Unlike the API path, no UN /locations lookup is needed: the bulk files carry
# ISO3_code directly, so ISO3 alone joins them to GBD.
build_gbd_iso3 <- function(shp_path) {
  dbf <- sub("\\.shp$", ".dbf", shp_path)
  if (!file.exists(dbf)) stop("Shapefile .dbf not found: ", dbf,
                              " (set GBD_SHAPEFILE, or run util_augment_shapefile.R).")
  d <- as.data.table(foreign::read.dbf(dbf, as.is = TRUE))
  nat <- d[level == 3 & !is.na(ihme_lc_id) & ihme_lc_id != "",
           .(loc_id = as.integer(loc_id), iso3 = toupper(ihme_lc_id))]
  unique(nat)
}

# --- Download ---------------------------------------------------------------
# Cached under data/un-wpp-bulk/. These are static release files, so a cached
# copy is reused unless --force. Together they are roughly 290 MB.
fetch_bulk <- function(fname) {
  dest <- file.path(CACHE_DIR, fname)
  if (!REFETCH && file.exists(dest) && file.size(dest) >= MIN_BYTES) {
    cat("  cached:", fname, sprintf("(%.0f MB)\n", file.size(dest) / 1e6))
    return(dest)
  }
  url <- paste0(BASE_URL, "/", fname)
  cat("  downloading:", fname, "...\n")
  ok <- tryCatch(download.file(url, dest, mode = "wb", quiet = TRUE) == 0,
                 error = function(e) FALSE)
  if (!ok || !file.exists(dest))
    stop("Download failed: ", url)
  if (file.size(dest) < MIN_BYTES)
    stop("Downloaded file is implausibly small (", file.size(dest), " bytes): ",
         dest, ". The UN may have moved or reissued it; check ", BASE_URL)
  cat("  got:", sprintf("%.0f MB\n", file.size(dest) / 1e6))
  dest
}

# --- Main -------------------------------------------------------------------
cat("=== UN WPP 2024 life tables, bulk CSV path (no token needed) ===\n")

cat("Crosswalk from", basename(SHAPEFILE), "...\n")
xwalk <- build_gbd_iso3(SHAPEFILE)
if (length(LOCATION_SUBSET) > 0) {
  xwalk <- xwalk[loc_id %in% LOCATION_SUBSET]
  missing <- setdiff(LOCATION_SUBSET, xwalk$loc_id)
  if (length(missing) > 0)
    stop("Requested loc_ids absent from ", basename(SHAPEFILE), ": ",
         paste(missing, collapse = ","),
         ". If these are the micro-nations, run util_augment_shapefile.R first.")
}
cat("  covers", nrow(xwalk), "location(s).\n")

cat("Bulk files:\n")
paths <- vapply(BULK_FILES, fetch_bulk, character(1))

# One pass per file, keeping only the wanted ISO3 codes. fread on the gz reads
# roughly 5.6M rows per file; restricting to the columns actually used keeps
# this to a few hundred MB rather than several GB.
KEEP <- c("ISO3_code", "Variant", "SexID", "Time", "AgeGrpStart", "AgeGrpSpan", "ex")

# Decompress through a pipe rather than letting fread open the .gz directly:
# direct gz reading needs R.utils, which is not a project dependency and is
# absent from the production R library. gzip is present everywhere this runs.
read_bulk <- function(p) {
  if (nzchar(Sys.which("gzip"))) {
    fread(cmd = paste("gzip -dc", shQuote(p)), select = KEEP, showProgress = FALSE)
  } else if (requireNamespace("R.utils", quietly = TRUE)) {
    fread(p, select = KEEP, showProgress = FALSE)
  } else {
    stop("Need either gzip on PATH or the R.utils package to read ", basename(p))
  }
}

cat("Reading (this takes a minute or two)...\n")
dat <- rbindlist(lapply(paths, function(p) {
  d <- read_bulk(p)
  d[ISO3_code %in% xwalk$iso3]
}))
cat("  kept", format(nrow(dat), big.mark = ","), "rows for the requested locations.\n")

# Filters, each of which silently corrupts the output if skipped:
#   Variant   these files should be Medium only, but do not assume it.
#   SexID     the files also carry 3 = Total. 1 = Male / 2 = Female already
#             match the GBD sex_id convention, so no remapping is needed.
#   AgeGrpSpan  in an abridged table age 0 appears both as the span-1 infant
#             row and inside a wider band. Keeping both doubles the age-0 rows.
dat <- dat[Variant == "Medium" & SexID %in% c(1L, 2L)]
dat <- dat[AgeGrpStart %in% AGE_STARTS]
dat <- dat[!(AgeGrpStart == 0L & AgeGrpSpan != 1L)]
dat <- dat[Time >= YEAR_MIN & Time <= YEAR_MAX]

dat <- merge(dat, xwalk, by.x = "ISO3_code", by.y = "iso3")
out <- unique(dat[, .(year_id = as.integer(Time),
                      age_group_id = as.integer(AgeGrpStart),
                      sex_id = as.integer(SexID),
                      ex = as.numeric(ex),
                      location_id = loc_id)])
setorder(out, location_id, year_id, age_group_id, sex_id)

# One ex per (location, year, age, sex), or something upstream changed.
dup <- out[, .N, by = .(location_id, year_id, age_group_id, sex_id)][N > 1L]
if (nrow(dup) > 0)
  stop("Conflicting ex values for ", nrow(dup), " key(s), e.g. location ",
       dup$location_id[1], " year ", dup$year_id[1], ". Check the WPP schema.")

EXPECT_ROWS <- (YEAR_MAX - YEAR_MIN + 1L) * length(AGE_STARTS) * 2L   # 3,774

absent <- setdiff(xwalk$loc_id, unique(out$location_id))
if (length(absent) > 0)
  cat("\nWARNING: no WPP rows for loc_id(s):", paste(absent, collapse = ","),
      "\n  (ISO3:", paste(xwalk[loc_id %in% absent]$iso3, collapse = ","), ")\n")

cat("\nWriting to", LIFETABLE_DIR, "\n")
n_written <- 0L; n_skipped <- 0L
for (loc in sort(unique(out$location_id))) {
  out_file <- file.path(LIFETABLE_DIR, paste0(loc, "_lifetable.csv"))
  if (!FORCE && file.exists(out_file)) {
    n_skipped <- n_skipped + 1L
    next
  }
  d <- out[location_id == loc]
  if (nrow(d) != EXPECT_ROWS)
    cat(sprintf("  WARNING loc %d: %d rows, expected %d\n", loc, nrow(d), EXPECT_ROWS))
  if (anyNA(d$ex) || any(d$ex <= 0))
    cat(sprintf("  WARNING loc %d: NA or non-positive ex\n", loc))
  fwrite(d, out_file)
  n_written <- n_written + 1L
}

cat("\n=== Done ===\n")
cat("  written:", n_written, "\n")
cat("  skipped (already present; --force to overwrite):", n_skipped, "\n")
cat("  life tables now in", basename(LIFETABLE_DIR), ":",
    length(list.files(LIFETABLE_DIR, pattern = "^[0-9]+_lifetable\\.csv$")), "\n")
