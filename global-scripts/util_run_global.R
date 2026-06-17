# util_run_global.R — Single-location end-to-end runner for the projection
# workflow. Composes the CCKP-conversion + burden-pipeline calls for one
# location and all its (model × scenario × year) combos, with the right
# mortality-file list assembled from the per-cause IHME RDS files.
#
# Designed to be called in parallel for many locations -- e.g. via GNU
# parallel on Caspar's machine:
#
#   parallel -j 125 \
#     'Rscript global-scripts/util_run_global.R --location_id={}' \
#     ::: $(Rscript -e 'cat(readRDS("output/intermediate/ihme_loc_map.rds")$loc_id)')
#
# One process per location → ~205 processes spread across 125 logical
# cores. Each process handles all (model × scenario × year) combos for its
# location, leaning on the cause+draw chunking inside 05 to fit memory.
#
# Idempotent on both inner runners' levels (skip if output exists). Safe to
# rerun after a partial failure.
#
# Usage (single location):
#   Rscript global-scripts/util_run_global.R \
#     --location_id=125 \
#     --scenarios=ssp245 \
#     --years=2022-2050
#
# Defaults: scenarios = all four SSPs, years = 2022-2050, all models from
# config.R::MODELS_ALL. Override per-flag.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(
  SCENARIOS    = "ssp126,ssp245,ssp370,ssp585",
  YEARS        = "2022-2050",
  MODELS       = paste(MODELS_ALL, collapse = ","),
  USE_DRAWS_RUN= TRUE,
  N_DRAWS_RUN  = 500,
  CAUSES       = paste(GBD_CAUSES, collapse = ","),
  SHAPEFILE    = file.path(SHAPEFILE_DIR, "GBD2023_mapping_final.shp"),
  FORCE        = FALSE   # --force=TRUE forwarded to both inner runners (recompute)
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

run_one_location <- function() {
  loc <- LOCATION_ID
  causes <- strsplit(as.character(CAUSES), ",", fixed = TRUE)[[1]]

  # Compose the per-location mortality-file list (one RDS per cause). Filter
  # to files that actually exist on disk -- missing causes get logged but
  # don't block the run (the pipeline handles a partial cause set).
  mort_paths <- file.path(MORTALITY_DIR,
                          sprintf("%d_mortality_ihme_%s_draws.rds", loc, causes))
  existing   <- mort_paths[file.exists(mort_paths)]
  missing    <- mort_paths[!file.exists(mort_paths)]
  if (length(missing) > 0) {
    log_msg("WARN missing mortality files for loc=", loc, ":\n  ",
            paste(basename(missing), collapse = "\n  "))
  }
  if (length(existing) == 0) {
    stop("No IHME mortality files exist for loc=", loc,
         ". Run util_convert_ihme_batch.R first.")
  }
  mort_file_arg <- paste(existing, collapse = ",")

  log_msg(sprintf("=== loc=%d : %d models x %d scenarios x %d-yr range ===",
                  loc,
                  length(strsplit(as.character(MODELS),    ",", fixed=TRUE)[[1]]),
                  length(strsplit(as.character(SCENARIOS), ",", fixed=TRUE)[[1]]),
                  length(strsplit(as.character(YEARS),     ",", fixed=TRUE)[[1]])))

  # === Step 1: pull + convert CCKP NetCDFs ===
  t0 <- Sys.time()
  rc <- system2("Rscript", c(file.path(SCRIPTS_DIR, "util_run_cckp_pipeline.R"),
                             paste0("--location_id=", loc),
                             paste0("--models=",      MODELS),
                             paste0("--scenarios=",   SCENARIOS),
                             paste0("--years=",       YEARS),
                             paste0("--shapefile=",   SHAPEFILE),
                             paste0("--subnational=", SUBNATIONAL),
                             paste0("--cckp_local_root=", CCKP_LOCAL_ROOT),
                             paste0("--cckp_pop_local_root=", CCKP_POP_LOCAL_ROOT),
                             paste0("--force=", FORCE)))
  log_msg(sprintf("util_run_cckp_pipeline.R exit=%d in %.1fs",
                  rc, as.numeric(Sys.time() - t0, units = "secs")))
  if (rc != 0) stop("util_run_cckp_pipeline.R failed for loc=", loc)

  # === Step 2: run the burden pipeline for each combo ===
  t0 <- Sys.time()
  rc <- system2("Rscript", c(file.path(SCRIPTS_DIR, "util_run_cckp_burden.R"),
                             paste0("--location_id=",     loc),
                             paste0("--models=",          MODELS),
                             paste0("--scenarios=",       SCENARIOS),
                             paste0("--years=",           YEARS),
                             paste0("--use_draws_run=",
                                    if (isTRUE(USE_DRAWS_RUN)) "TRUE" else "FALSE"),
                             paste0("--n_draws_run=",     N_DRAWS_RUN),
                             paste0("--force=",           FORCE),
                             paste0("--mortality_file=",  mort_file_arg)))
  log_msg(sprintf("util_run_cckp_burden.R exit=%d in %.1fs",
                  rc, as.numeric(Sys.time() - t0, units = "secs")))
  if (rc != 0) stop("util_run_cckp_burden.R failed for loc=", loc)

  invisible(NULL)
}

if (sys.nframe() == 0L) run_one_location()
