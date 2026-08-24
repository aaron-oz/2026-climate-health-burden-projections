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
#     --use_draws_run=TRUE \
#     --n_draws_run=500
#
# Idempotent: skips combos where burden_{year}.rds already exists.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
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
  FORCE           = FALSE,  # --force=TRUE recomputes even if burden_{year}.rds exists
  N_CORES         = 1L,     # workers over this location's grid; see the note in
                            # util_run_cckp_pipeline.R. Each worker spawns a
                            # run_location.R subprocess, so budget ~2x processes.
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
  if (!isTRUE(FORCE) && file.exists(out_burden)) {
    return(list(status = "skip", elapsed = 0, msg = ""))
  }

  cckp_temp <- file.path(CCKP_TEMP_ROOT, loc, paste0(model, "-", scen),
                         sprintf("daily_temp_%d.rds", year))
  if (!file.exists(cckp_temp)) {
    return(list(status = "missing-temp", elapsed = 0,
                msg = paste("not found:", cckp_temp)))
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Private scratch for this combo.
  #
  # run_location.R's steps hand data to each other through files, not memory:
  # 03 writes INTERMEDIATE_DIR/temperature.rds and 05 reads it back, and 05-08
  # write their results as RESULTS_DIR/{burden,pafs,ylls,sevs}_{loc}.rds. Both
  # of those paths were scoped per LOCATION, which is enough only while a
  # location runs its combos one at a time. Two concurrent combos of the same
  # location would otherwise interleave on them, and the damage would be silent
  # rather than loud: the post-run move below would pick up whichever combo's
  # burden_{loc}.rds happened to be on disk and stamp it with THIS combo's
  # model and scenario, writing a result that is wrong and labelled as correct.
  #
  # PIPELINE_RUN_TAG (see config.R) moves both directories under a per-combo
  # subdirectory in the child process, so each combo gets its own scratch.
  run_tag <- combo_id(model, scen, year)
  staging <- file.path(RESULTS_ROOT, "_staging", as.character(loc), run_tag)
  scratch <- file.path(OUTPUT_DIR, "intermediate", as.character(loc), run_tag)
  unlink(c(staging, scratch), recursive = TRUE)   # no leftovers from a crash
  dir.create(staging, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(c(staging, scratch), recursive = TRUE), add = TRUE)

  t0 <- Sys.time()
  args <- c(file.path(SCRIPTS_DIR, "run_location.R"),
            paste0("--location_id=", loc),
            paste0("--year_start=", year),
            paste0("--year_end=",   year),
            paste0("--use_draws=",  if (isTRUE(USE_DRAWS_RUN)) "TRUE" else "FALSE"),
            paste0("--n_draws=",    N_DRAWS_RUN),
            paste0("--temp_file=",  cckp_temp),
            # forward the uncertainty-fix flags so a child combo always runs
            # with the same TMREL / exposure-noise configuration as this
            # invocation (config defaults or CLI overrides alike)
            paste0("--tmrel_mode=",        TMREL_MODE),
            paste0("--tmrel_round_whole=", if (isTRUE(TMREL_ROUND_WHOLE)) "TRUE" else "FALSE"),
            paste0("--temp_noise_mode=",   TEMP_NOISE_MODE),
            paste0("--temp_sd_file=",      TEMP_SD_FILE),
            paste0("--temp_sd_var=",       TEMP_SD_VAR),
            "--run_diagnostics=FALSE")
  if (!is.null(MORTALITY_FILE) && nzchar(as.character(MORTALITY_FILE))) {
    args <- c(args, paste0("--mortality_file=", as.character(MORTALITY_FILE)))
  }

  exit_code <- system2("Rscript", args,
                       env = paste0("PIPELINE_RUN_TAG=", run_tag),
                       stdout = file.path(out_dir, sprintf("run_%d.log", year)),
                       stderr = file.path(out_dir, sprintf("run_%d.log", year)))
  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  if (exit_code != 0) {
    return(list(status = "fail", elapsed = elapsed,
                msg = paste("Rscript exit", exit_code)))
  }

  # Move pipeline outputs (named *_{loc}.rds in this combo's staging dir) into
  # the per-combo tree, renamed to *_{year}.rds, stamping model + scenario
  # columns so each RDS is self-describing (the scenario/model were previously
  # encoded only in the directory path).
  ok <- TRUE
  for (prefix in c("burden", "pafs", "ylls", "sevs")) {
    src <- file.path(staging, sprintf("%s_%d.rds", prefix, loc))
    dst <- file.path(out_dir, sprintf("%s_%d.rds", prefix, year))
    if (file.exists(src)) {
      success <- tryCatch({
        d <- readRDS(src)
        d$model <- model
        d$scenario <- scen
        save_rds_atomic(d, dst)
        TRUE
      }, error = function(e) FALSE)
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

  # A (re)run is starting: drop any stale terminal sentinel from a prior run so
  # it can't be misread as this run's state.
  clear_done_sentinel(LOCATION_ID)
  clear_combo_statuses(LOCATION_ID, "burden")

  # Each combo spawns its own run_location.R subprocess with a private scratch
  # (see run_one_combo), so combos of one location can now run concurrently.
  # See the N_CORES note in util_run_cckp_pipeline.R: memory is the constraint,
  # and mc.preschedule = FALSE keeps one worker death from taking a chunk with
  # it. Note each worker here spawns a subprocess, so the real process count is
  # about twice n_cores.
  n_cores <- max(1L, as.integer(N_CORES))
  log_msg("Burden workers: ", n_cores)

  run_idx <- function(i) {
    g <- grid[i]
    log_msg(sprintf("[%d/%d] %s/%s/%d", i, nrow(grid), g$model, g$scenario, g$year))
    r <- run_one_combo(LOCATION_ID, g$model, g$scenario, g$year)
    log_msg(sprintf("  -> %s (%.1fs) %s", r$status, r$elapsed, r$msg))
    write_combo_status(LOCATION_ID, "burden", g$model, g$scenario, g$year,
                       status = r$status, elapsed_s = r$elapsed, message = r$msg)
    refresh_run_progress(LOCATION_ID, "burden", total = nrow(grid),
                         last = sprintf("%s/%s/%d", g$model, g$scenario, g$year))
    data.table(grid[i],
               location_id = LOCATION_ID,
               status = r$status,
               elapsed_s = round(r$elapsed, 2),
               message = r$msg)
  }

  results <- if (n_cores > 1L) {
    parallel::mclapply(seq_len(nrow(grid)), run_idx,
                       mc.cores = n_cores, mc.preschedule = FALSE)
  } else {
    lapply(seq_len(nrow(grid)), run_idx)
  }

  bad <- vapply(results, function(r) inherits(r, "try-error") || !is.data.frame(r),
                logical(1))
  if (any(bad)) {
    log_msg("WARN ", sum(bad), " combo(s) died in a worker (likely OOM); ",
            "recording as fail. Reduce --n_cores if this repeats.")
    for (i in which(bad)) {
      g <- grid[i]
      msg <- paste("worker died:", trimws(paste(as.character(results[[i]]),
                                                collapse = " ")))
      results[[i]] <- data.table(grid[i], location_id = LOCATION_ID,
                                 status = "fail", elapsed_s = NA_real_,
                                 message = msg)
      write_combo_status(LOCATION_ID, "burden", g$model, g$scenario, g$year,
                         status = "fail", message = msg)
    }
  }
  if (length(results) != nrow(grid)) {
    stop("Burden grid loop exited early: recorded ", length(results), " of ",
         nrow(grid), " combos. This is a bug, not a data problem.")
  }

  manifest <- rbindlist(results)
  manifest[, run_ts := format(Sys.time(), "%Y-%m-%dT%H:%M:%S")]
  fwrite(manifest, BURDEN_MANIFEST, append = file.exists(BURDEN_MANIFEST))
  n_ok   <- sum(manifest$status == "ok")
  n_skip <- sum(manifest$status == "skip")
  n_fail <- sum(manifest$status == "fail")
  n_miss <- sum(manifest$status == "missing-temp")
  log_msg("Done: ", n_ok, " ok | ", n_skip, " skip | ", n_fail, " fail | ",
          n_miss, " missing-temp -> ", BURDEN_MANIFEST)

  # Terminal sentinel, derived from the per-combo status files rather than from
  # this process's in-memory tally, so it stays correct however the grid was
  # divided among workers.
  #
  # missing-temp no longer blocks DONE on its own. A model x scenario that CCKP
  # genuinely does not publish (gfdl-cm4 has no ssp370, and there are others)
  # produces missing-temp on every rerun, so treating it as incomplete marked
  # those locations _INCOMPLETE forever and gave no way to distinguish them from
  # locations that really did need another pass. Failures still block DONE.
  fin <- finalize_run_sentinel(LOCATION_ID, "burden", total = nrow(grid))
  log_msg("Sentinel: ", if (fin$complete) "_DONE" else "_INCOMPLETE",
          " | ", fin$summary)
  invisible(manifest)
}

if (sys.nframe() == 0L) {
  run_cckp_burden_grid()
}
