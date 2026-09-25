# util_run_provenance.R: does each summarized output file come from the run we
# think it does? Sourced by util_summarize_run.R; no side effects on source.
#
# Why this exists: in the 2026-09 ssp245 rerun four combos were SIGKILLed
# ("Rscript exit 137") and never retried. Their output files from the August
# run were still on disk, so the summarizer read them and every QA check
# passed: the arithmetic of an old file is as clean as that of a new one. The
# burden manifest (output/cckp_burden_manifest.csv, one appended row per combo
# attempt) knew; nothing read it. See
# docs/reviews/ssp245-round2-review-2026-09-24.org.
#
# The checks, per summarized (location, model, scenario, year):
#   stale     the latest real attempt (status other than "skip") is not "ok",
#             so the file on disk predates a failed attempt
#   not_since with --since given: no "ok" attempt at or after that time, so the
#             file was not produced by the run being reviewed (e.g. a location
#             whose process died before reaching the combo writes no row at all)
#   no_record no manifest row at all (reported, not failed: output made by hand
#             or before the manifest existed)
#
# run_ts is the writing machine's local time as "%Y-%m-%dT%H:%M:%S"; ISO text
# sorts in time order, so it is compared as text, and --since may be a date
# ("2026-09-16") or a full timestamp.

# Latest attempt per combo, and latest ok, from the append-only manifest.
manifest_latest <- function(path, scenarios = NULL) {
  m <- fread(path, colClasses = list(character = c("model", "scenario", "status",
                                                    "message", "run_ts")))
  if (!is.null(scenarios)) m <- m[scenario %in% scenarios]
  m[, row := .I]   # file order breaks ties between rows with the same run_ts
  real <- m[status != "skip"][order(run_ts, row)]
  last <- real[, .SD[.N], by = .(location_id, model, scenario, year),
               .SDcols = c("status", "message", "run_ts")]
  ok <- m[status == "ok"][order(run_ts, row)][
    , .(last_ok_ts = run_ts[.N]), by = .(location_id, model, scenario, year)]
  merge(last, ok, by = c("location_id", "model", "scenario", "year"), all = TRUE)
}

# combos: data.table with location_id, model, scenario, year (what was summarized).
provenance_check <- function(combos, latest, since = "") {
  k <- c("location_id", "model", "scenario", "year")
  x <- merge(unique(combos[, ..k]), latest, by = k, all.x = TRUE)
  x[, issue := NA_character_]
  x[is.na(status) & is.na(last_ok_ts), issue := "no_record"]
  x[!is.na(status) & status != "ok", issue := "stale"]
  if (nzchar(since))
    x[is.na(issue) & (is.na(last_ok_ts) | last_ok_ts < since), issue := "not_since"]
  x[]
}

# "2022,2023,2024,2030" -> "2022-2024,2030"
compress_years <- function(y) {
  y <- sort(unique(as.integer(y)))
  grp <- cumsum(c(1L, diff(y) != 1L))
  paste(vapply(split(y, grp), function(v)
    if (length(v) == 1L) as.character(v) else paste0(v[1], "-", v[length(v)]),
    character(1)), collapse = ",")
}

# Shell script that repairs what the checks found, in execution order:
# rerun the combos (serially: the failures we have seen were memory kills
# under a wide parallel run), drop the affected locations from the summary
# cache, re-summarize. rerun: combos to recompute (force_burden); missing:
# combos with no output at all (a plain rerun computes only what is absent).
fix_script <- function(rerun, missing, cache_dir, summarize_cmd) {
  if (nrow(rerun) + nrow(missing) == 0) return(character(0))
  cmd <- function(d, force) {
    if (nrow(d) == 0) return(character(0))
    g <- d[, .(years = compress_years(year)), by = .(location_id, scenario, model)]
    setorder(g, location_id, scenario, model)
    sprintf("Rscript global-scripts/util_run_global.R --location_id=%d --scenarios=%s --models=%s --years=%s%s",
            g$location_id, g$scenario, g$model, g$years,
            if (force) " --force_burden=TRUE" else "")
  }
  locs <- sort(unique(c(rerun$location_id, missing$location_id)))
  c("#!/usr/bin/env bash",
    "# Written by util_summarize_run.R. Run from the repo root, after reading",
    "# the Provenance section of qa_report.txt. One combo at a time on purpose.",
    "set -e",
    "",
    if (nrow(rerun)) c("# 1. Recompute combos whose file on disk is not from this run", cmd(rerun, TRUE), "") else character(0),
    if (nrow(missing)) c("# 1b. Compute combos with no output (skip-if-exists, so no force)", cmd(missing, FALSE), "") else character(0),
    "# 2. Drop the affected locations from the summary cache, then re-summarize",
    sprintf("rm -f %s", paste(file.path(cache_dir, paste0(locs, ".rds")), collapse = " ")),
    summarize_cmd)
}
