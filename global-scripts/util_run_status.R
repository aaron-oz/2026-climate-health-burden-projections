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
#   Rscript global-scripts/util_run_status.R --exact      # recount from every status file
#
# Speed: each location's tallies come from its two small _progress_<phase>.tsv
# files, which the run itself rewrites after every combo, so a location still
# running can be one combo behind (finished locations are exact). That is a couple
# of file reads per location instead of one per combo: a full 200-location grid
# holds a few hundred thousand per-combo status files, and reading them all is
# what made this take minutes on a busy machine. --loc and --failures still read
# per-combo files, but only for the locations being drilled into. Pass --exact
# to recount everything from the per-combo files (slow, and only needed if a
# progress file is suspected of being stale).
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

defaults <- list(FAILURES = FALSE, LOC = NULL, EXACT = FALSE)
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

# The cheap path: one location-phase row straight from the progress file the
# run maintains. Returns NULL if that file is absent or predates the tallies
# being written into it, so the caller falls back to counting combos.
TALLY_KEYS <- c("ok", "skip", "fail", "missing-on-s3", "missing-temp")

row_from_progress <- function(loc, ph, state, d) {
  # Per-phase progress file, so convert keeps its own total after burden
  # starts overwriting the combined one.
  prog <- read_kv(file.path(d, paste0("_progress_", ph, ".tsv")))
  if (is.null(prog) || !identical(unname(prog["phase"]), ph)) return(NULL)
  if (!all(TALLY_KEYS %in% names(prog))) return(NULL)
  n <- function(k) suppressWarnings(as.integer(prog[k]))
  data.table(loc = as.integer(loc), phase = ph, state = state,
             done = n("combos_done"), total = n("combos_total"),
             ok = n("ok"), skip = n("skip"), fail = n("fail"),
             gap = n("missing-on-s3") + n("missing-temp"),
             median_s = suppressWarnings(as.numeric(prog["median_ok_s"])),
             updated = unname(prog["updated"]))
}

# The exact path: count every per-combo status file for this location-phase.
row_from_statuses <- function(loc, ph, state, d) {
  st <- read_combo_statuses(loc, ph)
  if (nrow(st) == 0) return(NULL)
  prog <- read_kv(file.path(d, paste0("_progress_", ph, ".tsv")))
  if (is.null(prog)) prog <- read_kv(file.path(d, "_progress.tsv"))
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
}

# One location asked for by name is worth counting exactly: it is a single
# directory, so the cost is small and the drill-down is where an exact answer
# matters most.
exact <- isTRUE(EXACT) || !is.null(LOC)

rows <- rbindlist(lapply(locs, function(loc) {
  d <- file.path(root, loc)
  state <- if (file.exists(file.path(d, "_DONE"))) "DONE"
           else if (file.exists(file.path(d, "_INCOMPLETE"))) "INCOMPLETE"
           else "(running)"
  out <- lapply(c("convert", "burden"), function(ph) {
    r <- if (exact) NULL else row_from_progress(loc, ph, state, d)
    if (is.null(r)) r <- row_from_statuses(loc, ph, state, d)
    r
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

# A progress file written before per-combo timings were recorded in it has
# every count but no median. Say so, rather than leaving a blank column that
# looks like a missing measurement.
if (!exact && anyNA(rows$median_s)) {
  cat("\n(median_s is blank where a location's progress file predates this",
      "version of the run scripts.\n Counts are unaffected;",
      "--exact recomputes timings from the per-combo files.)\n")
}

# System memory. The run's steady-state footprint is roughly the number of
# concurrent locations times the per-combo burden peak (3.7 to 4.7 GB measured),
# which on a large fan-out is the binding constraint. Surfacing it here means
# the one number worth watching is in the same place as progress.
mem <- tryCatch({
  mi <- readLines("/proc/meminfo", warn = FALSE)
  gv <- function(k) {
    v <- grep(paste0("^", k, ":"), mi, value = TRUE)
    if (!length(v)) return(NA_real_)
    as.numeric(gsub("[^0-9]", "", v)) / 1024 / 1024   # kB -> GiB
  }
  list(total = gv("MemTotal"), avail = gv("MemAvailable"),
       swap_total = gv("SwapTotal"), swap_free = gv("SwapFree"))
}, error = function(e) NULL)

burden <- rows[phase == "burden"]
cat("\n--- Summary ---\n")
if (!is.null(mem) && !is.na(mem$total)) {
  used <- mem$total - mem$avail
  swap_used <- if (is.na(mem$swap_total)) 0 else mem$swap_total - mem$swap_free
  cat(sprintf("memory                    : %.0f of %.0f GB used, %.0f GB available%s\n",
              used, mem$total, mem$avail,
              if (swap_used > 1) sprintf("  [SWAPPING: %.0f GB swap in use]", swap_used) else ""))
  if (mem$avail < 0.08 * mem$total)
    cat("  WARNING: under 8% of memory free. Lower JOBS in run_env.sh and relaunch.\n")
}
cat(sprintf("locations with any status : %d\n", uniqueN(rows$loc)))
cat(sprintf("locations DONE            : %d\n", uniqueN(rows[state == "DONE"]$loc)))
cat(sprintf("locations INCOMPLETE      : %d\n", uniqueN(rows[state == "INCOMPLETE"]$loc)))
cat(sprintf("locations still running   : %d\n", uniqueN(rows[state == "(running)"]$loc)))
cat(sprintf("combos failed (all locs)  : %d\n", sum(rows$fail, na.rm = TRUE)))
# Count gaps once, in convert. A model and scenario absent from the mirror
# makes the burden phase report missing-temp for the same combo, so summing
# both phases would report each gap twice.
cat(sprintf("combos in CCKP gaps       : %d  (expected; rerunning will not fill these)\n",
            sum(rows[phase == "convert"]$gap, na.rm = TRUE)))

if (sum(rows$fail, na.rm = TRUE) > 0) {
  cat("\nSome combos FAILED. Rerun the same command that launched the run:\n",
      "failures recompute, everything already finished is skipped.\n", sep = "")
  if (!isTRUE(FAILURES)) cat("Re-run this with --failures to list them.\n")
}

if (isTRUE(FAILURES)) {
  cat("\n--- Failing combos ---\n")
  any_fail <- FALSE
  # Only the locations whose tally shows a failure: re-reading every location's
  # status files to find a handful of failures is what this listing used to do,
  # and on a full grid that is hundreds of thousands of files for nothing.
  fail_locs <- sort(unique(rows[!is.na(fail) & fail > 0]$loc))
  all_fail <- list()
  for (loc in fail_locs) {
    for (ph in c("convert", "burden")) {
      st <- read_combo_statuses(loc, ph)
      f <- st[status == "fail"]
      if (nrow(f) == 0) next
      any_fail <- TRUE
      all_fail[[length(all_fail) + 1L]] <- data.table(loc = loc, phase = ph,
                                                      message = f$message)
      cat(sprintf("\nloc %d / %s (%d failing)\n", loc, ph, nrow(f)))
      print(head(f[, .(model, scenario, year, message)], 20))
      if (nrow(f) > 20) cat(sprintf("  ... and %d more\n", nrow(f) - 20))
    }
  }
  if (!any_fail) cat("(none)\n")
  # Failures repeat: one broken input file shows up once per year of the grid.
  # Grouping by message turns a long listing into the handful of distinct causes
  # actually worth acting on.
  if (length(all_fail) > 0) {
    fa <- rbindlist(all_fail)
    by_msg <- fa[, .(combos = .N, locations = uniqueN(loc)),
                 by = .(phase, message)][order(-combos)]
    cat("\n--- Distinct failure causes ---\n")
    print(by_msg, nrows = 50)
  }
}
cat("\n")
