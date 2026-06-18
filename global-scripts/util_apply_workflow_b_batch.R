# util_apply_workflow_b_batch.R — Apply the Workflow B ratio (reference /
# target scenario) to every (location, model, year, target_scenario) combo
# produced by util_run_global.R, combining the per-scenario burden outputs
# with IHME's mortality counts.
#
# Inputs (all produced earlier by util_run_global.R + util_convert_ihme_batch.R):
#   output/results/cckp/{LOC}/{model}-ssp245/burden_{year}.rds   (reference)
#   output/results/cckp/{LOC}/{model}-{target}/burden_{year}.rds (target)
#   data/mortality/{LOC}_mortality_ihme_{cause}_draws.rds        (per cause)
#
# Outputs (one file per combo):
#   output/results/workflow_b/{LOC}/{model}-{target}/wb_{year}.rds
#
# Schema (from util_workflow_b_ratio.R):
#   year, subloc_id, age_group_id, sex_id, acause, [draw], ihme_deaths,
#   scale_factor, paf_heat, paf_cold, m_scaled,
#   deaths_heat_attrib, deaths_cold_attrib, deaths_nonopt_attrib, location_id
#   (draw is present in draws mode, giving per-draw uncertainty)
#
# Calls util_workflow_b_ratio.R::apply_ratio() in-process (no Rscript
# spawning) so per-combo overhead is just the I/O + ratio math. Idempotent
# (skip combos whose output exists).
#
# Usage:
#   Rscript global-scripts/util_apply_workflow_b_batch.R
#   Rscript global-scripts/util_apply_workflow_b_batch.R \
#     --ref_scenario=ssp245 \
#     --target_scenarios=ssp126,ssp370,ssp585 \
#     --years=2022-2050 \
#     --models=access-cm2-r1i1p1f1,canesm5-r1i1p1f1
#
# Per-location parallelism: call this script in parallel for many
# locations the same way as util_run_global.R, e.g.:
#   parallel -j 125 \
#     'Rscript global-scripts/util_apply_workflow_b_batch.R --location_id={}' \
#     ::: $(Rscript -e 'cat(readRDS("output/intermediate/ihme_loc_map.rds")$loc_id)')

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
source(file.path(SCRIPTS_DIR, "util_workflow_b_ratio.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(
  REF_SCENARIO       = "ssp245",
  TARGET_SCENARIOS   = "ssp245,ssp370,ssp585,ssp126",  # priority order; low target is SSP1-RCP2.6 (ssp126) -- CCKP daily has no ssp119
  YEARS              = "2022-2050",
  MODELS             = paste(MODELS_ALL, collapse = ","),
  CAUSES             = paste(GBD_CAUSES, collapse = ","),
  BURDEN_ROOT        = file.path(RESULTS_DIR, "cckp"),
  WORKFLOW_B_ROOT    = file.path(RESULTS_DIR, "workflow_b"),
  WORKFLOW_B_MANIFEST = file.path(OUTPUT_DIR, "workflow_b_batch_manifest.csv"),
  FORCE = FALSE   # --force=TRUE recomputes even if wb_{year}.rds exists
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

parse_years <- function(s) {
  s <- as.character(s); parts <- strsplit(s, ",", fixed = TRUE)[[1]]
  out <- integer(0)
  for (p in parts) {
    if (grepl("-", p, fixed = TRUE)) {
      ab <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]])
      out <- c(out, seq(ab[1], ab[2]))
    } else out <- c(out, as.integer(p))
  }
  unique(sort(out))
}

apply_workflow_b_grid <- function() {
  loc       <- LOCATION_ID
  models    <- strsplit(as.character(MODELS),           ",", fixed = TRUE)[[1]]
  targets   <- strsplit(as.character(TARGET_SCENARIOS), ",", fixed = TRUE)[[1]]
  years     <- parse_years(YEARS)
  causes    <- strsplit(as.character(CAUSES),           ",", fixed = TRUE)[[1]]
  ref_scen  <- as.character(REF_SCENARIO)

  # IHME mortality files for this location, one per cause that exists.
  mort_paths <- file.path(MORTALITY_DIR,
                          sprintf("%d_mortality_ihme_%s_draws.rds", loc, causes))
  mort_existing <- mort_paths[file.exists(mort_paths)]
  if (length(mort_existing) == 0) {
    stop("No IHME mortality files for loc=", loc,
         ". Run util_convert_ihme_batch.R first.")
  }
  ihme_arg <- paste(mort_existing, collapse = ",")

  grid <- CJ(model = models, target = targets, year = years, sorted = FALSE)
  log_msg(sprintf("Workflow B grid: loc=%d, %d combos (%d models x %d targets x %d years)",
                  loc, nrow(grid), length(models), length(targets), length(years)))

  results <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    g <- grid[i]
    ref_path <- file.path(BURDEN_ROOT, loc, paste0(g$model, "-", ref_scen),
                          sprintf("burden_%d.rds", g$year))
    tgt_path <- file.path(BURDEN_ROOT, loc, paste0(g$model, "-", g$target),
                          sprintf("burden_%d.rds", g$year))
    out_dir  <- file.path(WORKFLOW_B_ROOT, loc, paste0(g$model, "-", g$target))
    out_path <- file.path(out_dir, sprintf("wb_%d.rds", g$year))

    if (!isTRUE(FORCE) && file.exists(out_path)) {
      results[[i]] <- data.table(grid[i], location_id = loc,
                                 status = "skip", elapsed_s = 0, message = "")
      next
    }
    if (!file.exists(ref_path)) {
      log_msg(sprintf("[%d/%d] missing-ref: %s", i, nrow(grid), ref_path))
      results[[i]] <- data.table(grid[i], location_id = loc, status = "missing-ref",
                                 elapsed_s = 0, message = ref_path)
      next
    }
    if (!file.exists(tgt_path)) {
      log_msg(sprintf("[%d/%d] missing-target: %s", i, nrow(grid), tgt_path))
      results[[i]] <- data.table(grid[i], location_id = loc, status = "missing-target",
                                 elapsed_s = 0, message = tgt_path)
      next
    }

    t0 <- Sys.time()
    ok <- tryCatch({
      apply_ratio(ref_burden     = ref_path,
                  target_burden  = tgt_path,
                  ihme_mortality = ihme_arg,
                  output         = out_path,
                  location_id    = loc,
                  verbose        = FALSE)
      TRUE
    }, error = function(e) { log_msg("ERROR: ", conditionMessage(e)); FALSE })
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")

    log_msg(sprintf("[%d/%d] %s/%s/%d -> %s (%.2fs)",
                    i, nrow(grid), g$model, g$target, g$year,
                    if (ok) "ok" else "fail", elapsed))
    results[[i]] <- data.table(grid[i], location_id = loc,
                               status = if (ok) "ok" else "fail",
                               elapsed_s = round(elapsed, 2), message = "")
  }

  manifest <- rbindlist(results, fill = TRUE)
  manifest[, run_ts := format(Sys.time(), "%Y-%m-%dT%H:%M:%S")]
  fwrite(manifest, WORKFLOW_B_MANIFEST, append = file.exists(WORKFLOW_B_MANIFEST))
  log_msg(sprintf("Done: %d ok, %d skip, %d fail, %d missing-ref, %d missing-target -> %s",
                  sum(manifest$status == "ok"),
                  sum(manifest$status == "skip"),
                  sum(manifest$status == "fail"),
                  sum(manifest$status == "missing-ref"),
                  sum(manifest$status == "missing-target"),
                  WORKFLOW_B_MANIFEST))
  invisible(manifest)
}

if (sys.nframe() == 0L) apply_workflow_b_grid()
