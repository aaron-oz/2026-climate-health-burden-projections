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
  # The 4 production scenarios, in run-priority order: reference SSP2-RCP4.5
  # first (needed as the Workflow-B denominator for every target), then targets
  # SSP3-RCP7.0, SSP5-RCP8.5, SSP1-RCP2.6. The low target is SSP1-RCP2.6
  # (ssp126), not SSP1-RCP1.9 -- CCKP's daily product has no ssp119 for any
  # model (only an annual ensemble), so 1.9 isn't runnable here. (A few models
  # lack ssp126/ssp370; see MODELS_ALL in config.R -- those combos skip.)
  SCENARIOS    = "ssp245,ssp370,ssp585,ssp126",
  YEARS        = "2022-2050",
  MODELS       = paste(MODELS_ALL, collapse = ","),
  USE_DRAWS_RUN= TRUE,
  N_DRAWS_RUN  = 500,
  CAUSES       = paste(GBD_CAUSES, collapse = ","),
  SHAPEFILE    = DEFAULT_SHAPEFILE,
  FORCE        = FALSE,  # --force=TRUE forwarded to both inner runners (recompute)
  # --force_burden=TRUE forces ONLY the burden phase; the temperature-convert
  # phase keeps its skip-if-exists behavior. This is the flag for a
  # burden-only rerun (e.g. the 2026-08 uncertainty-draws fix): conversions
  # are reusable and re-converting them wastes most of the wall-clock.
  FORCE_BURDEN = FALSE,
  # Workers within this location. 0 (the default) means "size it automatically
  # from how big the location is"; any positive value is used as given.
  N_CORES      = 0L
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

# Workers to give this location, from the size of its bounding box on the CCKP
# 0.25-degree grid.
#
# The bbox is the right scale to key on even though the conversion now keeps
# only the in-country cells, because the array read out of the NetCDF is still
# bbox-sized (bbox cells x days x 8 bytes) and that read is the floor on both
# time and memory per combo. It is a proxy, not a measurement: a location with
# a lot of ocean in its box, like Fiji, is cheaper than its bbox suggests. Being
# generous there is harmless, since every location has the same several-thousand
# combo grid to get through.
#
# The distribution is heavily skewed. The median level-3 location covers about
# 570 cells; the US covers 306,000 and Russia 239,000. With these thresholds 24
# of 204 locations get more than one worker, adding 38 processes in total on top
# of one per location.
#
# MAX_WORKERS_PER_LOCATION caps this. Lower it if the machine runs short of
# memory: peak is roughly 3 GB per US combo and 4 GB per Russia combo after the
# in-country selection, against 20 GB and 14 GB before it.
#
# Returns 1 (unchanged behaviour) when the shapefile cannot be read, so a
# geometry problem degrades to the old serial path rather than stopping the run.
auto_n_cores <- function(loc, shapefile) {
  cap <- suppressWarnings(as.integer(Sys.getenv("MAX_WORKERS_PER_LOCATION", "4")))
  if (is.na(cap) || cap < 1L) cap <- 1L
  tryCatch({
    suppressPackageStartupMessages(library(sf))
    sf::sf_use_s2(FALSE)
    shp <- sf::st_read(shapefile, quiet = TRUE)
    g <- shp[!is.na(shp$loc_id) & shp$loc_id == loc, ]
    if (nrow(g) == 0) return(1L)
    b <- sf::st_bbox(g)
    cells <- (as.numeric(b["xmax"] - b["xmin"]) / 0.25 + 3) *
             (as.numeric(b["ymax"] - b["ymin"]) / 0.25 + 3)
    n <- if      (cells >= 100000) 4L
         else if (cells >=  20000) 3L
         else if (cells >=   5000) 2L
         else                      1L
    min(n, cap)
  }, error = function(e) {
    log_msg("auto_n_cores: could not size loc=", loc, " (", conditionMessage(e),
            "); using 1 worker")
    1L
  })
}

run_one_location <- function() {
  loc <- LOCATION_ID
  causes <- strsplit(as.character(CAUSES), ",", fixed = TRUE)[[1]]

  # Workers are given to the convert phase only, by default.
  #
  # The two phases have opposite cost profiles. Convert scales with the size of
  # the location: the US bbox is 300,000 grid cells against a median of 570, so
  # a few locations dominate the tail and splitting exactly those shortens the
  # run. Burden costs roughly the same per combo everywhere, because it is one
  # run_location.R invocation over that combo's data, so extra workers there do
  # not fix an imbalance; they just raise throughput, which is what the outer
  # -j already controls, and they multiply the phase's memory.
  #
  # Burden is also the memory-hungry phase: a single combo peaked at 3.7 GB
  # measured on Colombia in summary mode with one cause, so production draws
  # and 17 causes will be at least that. At the default -j that is already the
  # dominant memory consumer, which is why burden gets one worker unless
  # someone deliberately raises BURDEN_WORKERS.
  n_convert <- as.integer(N_CORES)
  if (is.na(n_convert) || n_convert < 1L) n_convert <- auto_n_cores(loc, SHAPEFILE)
  n_burden <- suppressWarnings(as.integer(Sys.getenv("BURDEN_WORKERS", "1")))
  if (is.na(n_burden) || n_burden < 1L) n_burden <- 1L
  log_msg(sprintf("loc=%d convert_workers=%d burden_workers=%d",
                  loc, n_convert, n_burden))

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
                             paste0("--require_local=", CCKP_REQUIRE_LOCAL),
                             paste0("--n_cores=", n_convert),
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
                             paste0("--force=",
                                    if (isTRUE(FORCE) || isTRUE(FORCE_BURDEN))
                                      "TRUE" else "FALSE"),
                             paste0("--n_cores=",         n_burden),
                             paste0("--mortality_file=",  mort_file_arg),
                             # uncertainty-fix flags (config defaults or CLI
                             # overrides) forwarded so a production rerun can
                             # enable the derived-TMREL / exposure-noise modes
                             paste0("--tmrel_mode=",        TMREL_MODE),
                             paste0("--tmrel_round_whole=",
                                    if (isTRUE(TMREL_ROUND_WHOLE)) "TRUE" else "FALSE"),
                             paste0("--temp_noise_mode=",   TEMP_NOISE_MODE),
                             paste0("--temp_sd_file=",      TEMP_SD_FILE),
                             paste0("--temp_sd_var=",       TEMP_SD_VAR)))
  log_msg(sprintf("util_run_cckp_burden.R exit=%d in %.1fs",
                  rc, as.numeric(Sys.time() - t0, units = "secs")))
  if (rc != 0) stop("util_run_cckp_burden.R failed for loc=", loc)

  invisible(NULL)
}

if (sys.nframe() == 0L) run_one_location()
