# util_run_status.R — One-command view of a whole production run.
#
# Reads the per-combo status files written by util_run_cckp_pipeline.R (convert)
# and util_run_cckp_burden.R (burden) and prints one line per location plus a
# fleet summary. Nothing here writes: it is safe to run at any time, as often as
# you like, while the run is going.
#
# Usage:
#   Rscript global-scripts/util_run_status.R              # summary + per-location table
#   Rscript global-scripts/util_run_status.R --failures   # also list failing combos
#   Rscript global-scripts/util_run_status.R --loc=62     # one location, verbose
#
# Column meanings:
#   phase    convert = building daily temperature RDS from CCKP NetCDFs
#            burden  = running the burden pipeline over those
#   ok/skip  computed this run / already on disk from an earlier run
#   fail     needs attention; rerun fills these in
#   gap      model x scenario CCKP does not publish. Expected, not an error,
#            and rerunning will never fill it.
#   state    DONE / INCOMPLETE / (running)

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(FAILURES = FALSE, LOC = NULL)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

root <- cckp_marker_root()
if (!dir.exists(root)) {
  cat("No run markers yet at ", root, "\n",
      "(Nothing has started, or this is the wrong project root.)\n", sep = "")
  quit(status = 0)
}

locs <- basename(list.dirs(root, recursive = FALSE))
locs <- suppressWarnings(locs[!is.na(as.integer(locs))])
if (!is.null(LOC)) locs <- as.character(LOC)
if (length(locs) == 0) {
  cat("No locations under ", root, "\n", sep = "")
  quit(status = 0)
}

read_kv <- function(path) {
  if (!file.exists(path)) return(NULL)
  kv <- strsplit(readLines(path, warn = FALSE), "\t", fixed = TRUE)
  kv <- kv[lengths(kv) >= 2]
  setNames(vapply(kv, `[`, "", 2), vapply(kv, `[`, "", 1))
}

rows <- rbindlist(lapply(locs, function(loc) {
  d <- file.path(root, loc)
  state <- if (file.exists(file.path(d, "_DONE"))) "DONE"
           else if (file.exists(file.path(d, "_INCOMPLETE"))) "INCOMPLETE"
           else "(running)"
  prog <- read_kv(file.path(d, "_progress.tsv"))
  out <- lapply(c("convert", "burden"), function(ph) {
    st <- read_combo_statuses(loc, ph)
    if (nrow(st) == 0) return(NULL)
    n <- function(s) sum(st$status == s, na.rm = TRUE)
    data.table(loc = as.integer(loc), phase = ph, state = state,
               done = nrow(st),
               total = suppressWarnings(as.integer(
                 if (!is.null(prog) && identical(unname(prog["phase"]), ph))
                   prog["combos_total"] else NA)),
               ok = n("ok"), skip = n("skip"), fail = n("fail"),
               gap = n("missing-on-s3") + n("missing-temp"),
               median_s = round(median(st$elapsed_s[st$status == "ok"], na.rm = TRUE), 1),
               updated = if (!is.null(prog)) unname(prog["updated"]) else "")
  })
  rbindlist(out[!vapply(out, is.null, logical(1))], fill = TRUE)
}), fill = TRUE)

if (nrow(rows) == 0) {
  cat("Locations exist under ", root, " but none have per-combo status yet.\n",
      "If a run is in flight it should appear within a combo or two.\n", sep = "")
  quit(status = 0)
}

rows[, pct := ifelse(is.na(total) | total == 0, NA_real_,
                     round(100 * done / total, 1))]
setorder(rows, phase, loc)

cat("\n=== Run status (", root, ") ===\n\n", sep = "")
print(rows[, .(loc, phase, done, total, pct, ok, skip, fail, gap,
               median_s, state)], nrows = 500)

burden <- rows[phase == "burden"]
cat("\n--- Summary ---\n")
cat(sprintf("locations with any status : %d\n", uniqueN(rows$loc)))
cat(sprintf("locations DONE            : %d\n", uniqueN(rows[state == "DONE"]$loc)))
cat(sprintf("locations INCOMPLETE      : %d\n", uniqueN(rows[state == "INCOMPLETE"]$loc)))
cat(sprintf("locations still running   : %d\n", uniqueN(rows[state == "(running)"]$loc)))
cat(sprintf("combos failed (all locs)  : %d\n", sum(rows$fail, na.rm = TRUE)))
cat(sprintf("combos in CCKP gaps       : %d  (expected; rerunning will not fill these)\n",
            sum(rows$gap, na.rm = TRUE)))

if (sum(rows$fail, na.rm = TRUE) > 0) {
  cat("\nSome combos FAILED. Rerun the same command that launched the run:\n",
      "failures recompute, everything already finished is skipped.\n", sep = "")
  if (!isTRUE(FAILURES)) cat("Re-run this with --failures to list them.\n")
}

if (isTRUE(FAILURES)) {
  cat("\n--- Failing combos ---\n")
  any_fail <- FALSE
  for (loc in unique(rows$loc)) {
    for (ph in c("convert", "burden")) {
      st <- read_combo_statuses(loc, ph)
      f <- st[status == "fail"]
      if (nrow(f) == 0) next
      any_fail <- TRUE
      cat(sprintf("\nloc %d / %s (%d failing)\n", loc, ph, nrow(f)))
      print(head(f[, .(model, scenario, year, message)], 20))
      if (nrow(f) > 20) cat(sprintf("  ... and %d more\n", nrow(f) - 20))
    }
  }
  if (!any_fail) cat("(none)\n")
}
cat("\n")
