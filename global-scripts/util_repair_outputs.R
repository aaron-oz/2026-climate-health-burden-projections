# util_repair_outputs.R — Delete output files that were left half-written, so
# the next run recomputes them instead of skipping them forever.
#
#   Rscript global-scripts/util_repair_outputs.R              # check recent files
#   Rscript global-scripts/util_repair_outputs.R --all        # check every output
#   Rscript global-scripts/util_repair_outputs.R --recent=50  # check fewer
#   Rscript global-scripts/util_repair_outputs.R --dry_run    # report, delete nothing
#
# Why this exists: a combo's output file doubles as its resume marker. Both
# runners skip a combo when the file exists, without opening it. A run killed
# mid-write therefore leaves a truncated file that looks finished and is never
# recomputed; the damage only shows up later as an unreadable input.
#
# Writes are atomic now (save_rds_atomic in config.R), so files written from
# that version on cannot be truncated. This covers files left behind by earlier
# versions, which wrote in place.
#
# Scope: only the most recently modified RECENT files, because only a file being
# written at the moment of the kill can be damaged, and the number of concurrent
# writers bounds how many that is. Checking more is just slower: the integrity
# test decompresses each file, which costs about a quarter second on a large one,
# so an earlier default of 2000 meant roughly eight minutes of silence before the
# run started. Use --all when you want certainty rather than speed.
#
# Only files whose contents cannot be decompressed are removed. A readable file
# is never touched, so this cannot discard good work.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(
  ALL     = FALSE,   # check every output rather than the recent ones
  RECENT  = 300L,    # how many most-recently-modified files to check
  DRY_RUN = FALSE,
  QUIET   = FALSE    # suppress the "nothing to do" chatter
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

say <- function(...) if (!isTRUE(QUIET)) cat(...)

# saveRDS compresses with gzip by default, so a decompression test detects a
# truncated file. It costs a fraction of a full readRDS (0.01s against 0.19s on
# a 1.8 MB file) but is still proportional to file size, which is why the number
# of files checked is bounded. Falls back to readRDS where gzip is unavailable.
have_gzip <- nzchar(Sys.which("gzip"))
is_readable <- function(path) {
  if (have_gzip) {
    suppressWarnings(system2("gzip", c("-t", shQuote(path)),
                             stdout = FALSE, stderr = FALSE)) == 0
  } else {
    isTRUE(tryCatch({ readRDS(path); TRUE }, error = function(e) FALSE,
                    warning = function(w) FALSE))
  }
}

targets <- c(
  Sys.glob(file.path(TEMP_DIR, "cckp", "*", "*", "daily_temp_*.rds")),
  Sys.glob(file.path(RESULTS_ROOT, "cckp", "*", "*", "*_[0-9][0-9][0-9][0-9].rds"))
)
targets <- targets[file.exists(targets)]

if (length(targets) == 0) {
  say("No combo outputs found yet; nothing to check.\n")
  quit(status = 0)
}

info  <- file.info(targets)
ord   <- order(info$mtime, decreasing = TRUE)
check <- if (isTRUE(ALL)) targets[ord] else utils::head(targets[ord], as.integer(RECENT))

# Announce the size of the job before starting it, with an estimate, so a long
# check never looks like a hang. Note the work shows up in `top` as gzip rather
# than R, since each test is a separate gzip process.
est <- sum(utils::head(info$size[ord], length(check))) / 1e6 / 100  # ~100 MB/s
say(sprintf("Checking %s of %s output file(s) for truncation%s.\n",
            format(length(check), big.mark = ","),
            format(length(targets), big.mark = ","),
            if (isTRUE(ALL)) " (--all)" else ""))
if (est > 5) say(sprintf("  Roughly %.0f seconds; progress every 50 files.\n", est))

bad <- character(0)
for (i in seq_along(check)) {
  f <- check[i]
  if (!is_readable(f) || file.info(f)$size == 0) bad <- c(bad, f)
  if (i %% 50 == 0)
    say(sprintf("  %d/%d checked, %d bad so far\n", i, length(check), length(bad)))
}

if (length(bad) == 0) {
  say("All checked files are intact. Nothing to repair.\n")
  quit(status = 0)
}

cat(sprintf("\n%d file(s) are truncated or unreadable:\n", length(bad)))
for (f in utils::head(bad, 20)) cat("  ", sub(paste0("^", PROJECT_ROOT, "/"), "", f), "\n", sep = "")
if (length(bad) > 20) cat(sprintf("  ... and %d more\n", length(bad) - 20))

if (isTRUE(DRY_RUN)) {
  cat("\n--dry_run: nothing deleted. Re-run without it to remove them.\n")
  quit(status = 0)
}

removed <- file.remove(bad)
cat(sprintf("\nRemoved %d file(s). The next run will recompute them.\n", sum(removed)))
if (any(!removed)) {
  cat(sprintf("WARNING: %d could not be removed (permissions?).\n", sum(!removed)))
  quit(status = 1)
}
quit(status = 0)
