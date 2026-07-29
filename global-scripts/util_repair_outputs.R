# util_repair_outputs.R — Delete output files that were left half-written, so
# the next run recomputes them instead of skipping them forever.
#
#   Rscript global-scripts/util_repair_outputs.R            # check recent files
#   Rscript global-scripts/util_repair_outputs.R --all      # check everything
#   Rscript global-scripts/util_repair_outputs.R --dry_run  # report, delete nothing
#
# Why this exists: a combo's output file doubles as its resume marker. Both
# runners skip a combo when the file exists, without opening it. A run killed
# mid-write therefore leaves a truncated file that looks finished and is never
# recomputed; the damage only shows up later as an unreadable input.
#
# Writes are atomic now (see save_rds_atomic in config.R), so files written from
# this version on cannot be truncated. This exists for files left behind by the
# previous version, which wrote in place. Running it is harmless either way.
#
# Only files whose contents cannot be decompressed are removed. A readable file
# is never touched, so this cannot discard good work.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(
  ALL     = FALSE,   # check every output, not just recently modified ones
  RECENT  = 2000L,   # how many most-recently-modified files to check by default
  DRY_RUN = FALSE
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

# saveRDS compresses with gzip by default, so a decompression test detects a
# truncated file and costs a fraction of a full readRDS (0.01s vs 0.19s on a
# 1.8 MB file). Fall back to readRDS where gzip is unavailable.
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
  cat("No combo outputs found yet; nothing to check.\n")
  quit(status = 0)
}

info <- file.info(targets)
ord  <- order(info$mtime, decreasing = TRUE)
check <- if (isTRUE(ALL)) targets[ord] else utils::head(targets[ord], as.integer(RECENT))

cat(sprintf("Checking %s of %s combo output file(s)%s...\n",
            format(length(check), big.mark = ","),
            format(length(targets), big.mark = ","),
            if (isTRUE(ALL)) "" else " (most recently modified; --all for every file)"))

bad <- check[!vapply(check, is_readable, logical(1))]

# A zero-byte file is unreadable but gzip may not always flag it; catch it too.
zero <- check[file.info(check)$size == 0]
bad <- unique(c(bad, zero))

if (length(bad) == 0) {
  cat("All checked files are intact. Nothing to repair.\n")
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
