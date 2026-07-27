# util_backfill_scenario_columns.R — Add `model` + `scenario` columns to
# per-combo output RDS files that were written BEFORE those columns were stamped
# (see util_run_cckp_burden.R / util_workflow_b_ratio.R). The scenario/model are
# recoverable from the combo directory name (`<model>-<scenario>`), so this
# backfills them in place rather than recomputing the pipeline.
#
# Idempotent: files that already carry both columns are skipped, so it is safe
# to run repeatedly (e.g. after a partial production run) to keep the whole
# output set self-describing.
#
# Covers both output trees:
#   output/results/cckp/<loc>/<model>-<scenario>/{burden,pafs,ylls,sevs}_<year>.rds
#   output/results/workflow_b/<loc>/<model>-<scenario>/wb_<year>.rds
#
# Usage:
#   Rscript global-scripts/util_backfill_scenario_columns.R            # backfill
#   Rscript global-scripts/util_backfill_scenario_columns.R --dry_run=TRUE
#   Rscript global-scripts/util_backfill_scenario_columns.R --results_root=/path

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(
  RESULTS_ROOT = RESULTS_DIR,   # parent of cckp/ and workflow_b/
  DRY_RUN      = FALSE
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

# Split a combo directory name "<model>-<scenario>" into its parts. Model names
# contain hyphens (e.g. access-cm2-r1i1p1f1) and the scenario is the final
# token (ssp245 / ssp370 / ...), so split on the LAST hyphen.
parse_combo_dir <- function(dir_name) {
  scenario <- sub("^.*-", "", dir_name)
  model    <- sub("-[^-]*$", "", dir_name)
  list(model = model, scenario = scenario)
}

backfill_one <- function(path, dry_run) {
  ms <- parse_combo_dir(basename(dirname(path)))
  if (!grepl("^ssp[0-9]+$", ms$scenario)) {
    return("bad-path")   # not a <model>-<scenario> combo dir; leave alone
  }
  d <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(d)) return("read-error")
  if (all(c("model", "scenario") %in% names(d))) return("already")
  if (!dry_run) {
    d$model    <- ms$model
    d$scenario <- ms$scenario
    ok <- tryCatch({ saveRDS(d, path); TRUE }, error = function(e) FALSE)
    if (!ok) return("write-error")
  }
  "stamped"
}

backfill <- function() {
  patterns <- c(
    file.path(RESULTS_ROOT, "cckp", "*", "*", "burden_*.rds"),
    file.path(RESULTS_ROOT, "cckp", "*", "*", "pafs_*.rds"),
    file.path(RESULTS_ROOT, "cckp", "*", "*", "ylls_*.rds"),
    file.path(RESULTS_ROOT, "cckp", "*", "*", "sevs_*.rds"),
    file.path(RESULTS_ROOT, "workflow_b", "*", "*", "wb_*.rds"))
  files <- unlist(lapply(patterns, Sys.glob), use.names = FALSE)
  log_msg(sprintf("Backfill: %d candidate RDS files under %s%s",
                  length(files), RESULTS_ROOT,
                  if (isTRUE(DRY_RUN)) "  [DRY RUN]" else ""))

  tally <- c(stamped = 0L, already = 0L, `read-error` = 0L,
             `write-error` = 0L, `bad-path` = 0L)
  for (i in seq_along(files)) {
    res <- backfill_one(files[i], isTRUE(DRY_RUN))
    tally[res] <- tally[res] + 1L
    if (i %% 5000 == 0) log_msg(sprintf("  ... %d/%d", i, length(files)))
  }
  log_msg(sprintf(
    "Backfill done: %d stamped, %d already-had-columns, %d read-error, %d write-error, %d bad-path",
    tally[["stamped"]], tally[["already"]], tally[["read-error"]],
    tally[["write-error"]], tally[["bad-path"]]))
  invisible(tally)
}

if (sys.nframe() == 0L) backfill()
