# util_run_cckp_burden.R — Run the burden pipeline (01-08) against the
# per-(model, scenario, year) temperature RDS files produced upstream by
# util_run_cckp_pipeline.R.
#
# Sources its per-year temp from
#   data/temperature/cckp/{LOCATION_ID}/{model}-{scenario}/daily_temp_{year}.rds
# Writes per-year outputs to
#   output/results/cckp/{LOCATION_ID}/{model}-{scenario}/{burden,pafs,ylls,sevs}_{year}.rds
#
# Each combo spawns Rscript run_location.R with --temp_file=<override>
# --year_start=<year> --year_end=<year>. This is intentionally per-combo
# rather than batched-in-process: the ERF cache makes the per-process
# overhead small (~3 s/combo from process startup + cache copy) and
# spawning isolates failures cleanly. In-process batching would save
# roughly 3 minutes total at 100 cores — not worth the state-management
# complexity.
#
# Usage:
#   Rscript util_run_cckp_burden.R \
#     --location_id=125 \
#     --models=access-cm2-r1i1p1f1,bcc-csm2-mr-r1i1p1f1 \
#     --scenarios=ssp245 \
#     --years=2010-2015 \
#     --use_draws=TRUE \
#     --n_draws=500
#
# Idempotent: skips combos where burden_{year}.rds already exists.

source("config.R")
suppressPackageStartupMessages(library(data.table))

defaults <- list(
  MODELS       = "access-cm2-r1i1p1f1",
  SCENARIOS    = "ssp245",
  YEARS        = "2010",
  CCKP_TEMP_ROOT  = file.path(TEMP_DIR, "cckp"),
  OUTPUT_ROOT_CCKP = file.path(RESULTS_DIR, "cckp"),
  BURDEN_MANIFEST = file.path(OUTPUT_DIR, "cckp_burden_manifest.csv"),
  USE_DRAWS_RUN   = TRUE,   # forwarded as --use_draws to run_location.R
  N_DRAWS_RUN     = N_DRAWS,# forwarded as --n_draws
  MORTALITY_FILE  = NULL    # forwarded as --mortality_file; comma-separated
                            # list of per-cause IHME-derived RDS files
                            # supported (04 rbinds them). When NULL, the
                            # pipeline falls back to the canonical
                            # data/mortality/{LOC}_mortality.rds (Samuel /
                            # GBD-historical workflow).
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) {
    assign(k, defaults[[k]], envir = globalenv())
  }
}

parse_years <- function(s) {
  s <- as.character(s)
  parts <- strsplit(s, ",", fixed = TRUE)[[1]]
  out <- integer(0)
  for (p in parts) {
    if (grepl("-", p, fixed = TRUE)) {
      ab <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]])
      out <- c(out, seq(ab[1], ab[2]))
    } else {
      out <- c(out, as.integer(p))
    }
  }
  unique(sort(out))
}

# =============================================================================
# Run one (location, model, scenario, year) combo
# =============================================================================
run_one_combo <- function(loc, model, scen, year) {
  out_dir   <- file.path(OUTPUT_ROOT_CCKP, loc, paste0(model, "-", scen))
  out_burden <- file.path(out_dir, sprintf("burden_%d.rds", year))
  if (file.exists(out_burden)) {
    return(list(status = "skip", elapsed = 0, msg = ""))
  }

  cckp_temp <- file.path(CCKP_TEMP_ROOT, loc, paste0(model, "-", scen),
                         sprintf("daily_temp_%d.rds", year))
  if (!file.exists(cckp_temp)) {
    return(list(status = "missing-temp", elapsed = 0,
                msg = paste("not found:", cckp_temp)))
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Clear stale per-location outputs at RESULTS_DIR root so a partial pipeline
  # failure can't be confused with a successful run (we'd otherwise copy the
  # previous combo's burden_{loc}.rds into the new combo's tree).
  for (prefix in c("burden", "pafs", "ylls", "sevs")) {
    stale <- file.path(RESULTS_DIR, sprintf("%s_%d.rds", prefix, loc))
    if (file.exists(stale)) file.remove(stale)
  }

  t0 <- Sys.time()
  args <- c("run_location.R",
            paste0("--location_id=", loc),
            paste0("--year_start=", year),
            paste0("--year_end=",   year),
            paste0("--use_draws=",  if (isTRUE(USE_DRAWS_RUN)) "TRUE" else "FALSE"),
            paste0("--n_draws=",    N_DRAWS_RUN),
            paste0("--temp_file=",  cckp_temp),
            "--run_diagnostics=FALSE")
  if (!is.null(MORTALITY_FILE) && nzchar(as.character(MORTALITY_FILE))) {
    args <- c(args, paste0("--mortality_file=", as.character(MORTALITY_FILE)))
  }

  exit_code <- system2("Rscript", args,
                       stdout = file.path(out_dir, sprintf("run_%d.log", year)),
                       stderr = file.path(out_dir, sprintf("run_%d.log", year)))
  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  if (exit_code != 0) {
    return(list(status = "fail", elapsed = elapsed,
                msg = paste("Rscript exit", exit_code)))
  }

  # Move pipeline outputs (named *_{loc}.rds at RESULTS_DIR root) into the
  # per-combo tree, renamed to *_{year}.rds. Existing in-place outputs are
  # overwritten silently — they represent the previous combo's run.
  ok <- TRUE
  for (prefix in c("burden", "pafs", "ylls", "sevs")) {
    src <- file.path(RESULTS_DIR, sprintf("%s_%d.rds", prefix, loc))
    dst <- file.path(out_dir, sprintf("%s_%d.rds", prefix, year))
    if (file.exists(src)) {
      success <- file.copy(src, dst, overwrite = TRUE)
      if (!success) { ok <- FALSE; break }
    } else if (prefix == "burden") {
      # burden is the one required output; missing == pipeline failure
      ok <- FALSE; break
    }
  }
  if (!ok) {
    return(list(status = "fail", elapsed = elapsed,
                msg = "post-run output move failed"))
  }
  list(status = "ok", elapsed = elapsed, msg = "")
}

# =============================================================================
# Main loop
# =============================================================================
run_cckp_burden_grid <- function() {
  models    <- strsplit(as.character(MODELS),    ",", fixed = TRUE)[[1]]
  scenarios <- strsplit(as.character(SCENARIOS), ",", fixed = TRUE)[[1]]
  years     <- parse_years(YEARS)

  grid <- CJ(model = models, scenario = scenarios, year = years, sorted = FALSE)
  log_msg("CCKP burden grid: ", nrow(grid), " combos (",
          length(models), " models x ", length(scenarios),
          " scenarios x ", length(years), " years) for location ", LOCATION_ID)

  results <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    g <- grid[i]
    log_msg(sprintf("[%d/%d] %s/%s/%d", i, nrow(grid), g$model, g$scenario, g$year))
    r <- run_one_combo(LOCATION_ID, g$model, g$scenario, g$year)
    log_msg(sprintf("  -> %s (%.1fs) %s", r$status, r$elapsed, r$msg))
    results[[i]] <- data.table(grid[i],
                               location_id = LOCATION_ID,
                               status = r$status,
                               elapsed_s = round(r$elapsed, 2),
                               message = r$msg)
  }

  manifest <- rbindlist(results)
  manifest[, run_ts := format(Sys.time(), "%Y-%m-%dT%H:%M:%S")]
  fwrite(manifest, BURDEN_MANIFEST, append = file.exists(BURDEN_MANIFEST))
  log_msg("Done: ",
          sum(manifest$status == "ok"),           " ok | ",
          sum(manifest$status == "skip"),         " skip | ",
          sum(manifest$status == "fail"),         " fail | ",
          sum(manifest$status == "missing-temp"), " missing-temp -> ",
          BURDEN_MANIFEST)
  invisible(manifest)
}

if (sys.nframe() == 0L) {
  run_cckp_burden_grid()
}
