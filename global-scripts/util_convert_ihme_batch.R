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

source("config.R")
source("util_convert_ihme_forecast.R")
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

  log_msg(sprintf("[%s] loaded; %d locations available, iterating",
                  acause, length(unique(rate_full$location))))

  n_done <- 0L; n_skip <- 0L; n_miss <- 0L
  for (this_loc in location_ids) {
    out_path <- file.path(MORTALITY_DIR,
                          sprintf("%d_mortality_ihme_%s_draws.rds", this_loc, acause))
    if (!force && file.exists(out_path)) { n_skip <- n_skip + 1L; next }

    loc_row <- loc_map[loc_id == this_loc]
    if (nrow(loc_row) != 1) { n_miss <- n_miss + 1L; next }
    target_location <- loc_row$location

    rate <- rate_full[location == target_location]
    pop  <- pop_full[ location == target_location]
    if (nrow(rate) == 0 || nrow(pop) == 0) { n_miss <- n_miss + 1L; next }

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
  invisible(list(done = n_done, skipped = n_skip, missing = n_miss))
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
                                    missing = r$missing, file_missing = FALSE)
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
