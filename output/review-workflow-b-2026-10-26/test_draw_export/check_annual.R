# The annual export plus periods formed in the pooling script must give the
# same pooled.csv as exporting those periods directly.
# Usage: Rscript check_annual.R <fixture root>
suppressMessages(library(data.table))
R <- commandArgs(TRUE)[1]
ok <- function(lbl, cond) { cat(sprintf("  %-66s %s\n", lbl, if (isTRUE(cond)) "PASS" else "FAIL")); invisible(cond) }
a <- rbindlist(lapply(list.files(file.path(R, "output/summary_annual/draws"), full.names = TRUE), readRDS))
ok("annual draw files hold single-year periods 2022, 2023, 2030", setequal(unique(a$period), c("2022", "2023", "2030")) && all(a$n_years == 1))
for (ms in c("paired", "common")) {
  x <- fread(file.path(R, paste0("output/pool_", ms), "pooled.csv"))
  y <- fread(file.path(R, paste0("output/pool_annual_", ms), "pooled.csv"))
  num <- names(x)[vapply(x, is.numeric, logical(1))]
  same_keys <- identical(x[, .(unit_type, unit, target, period, measure)], y[, .(unit_type, unit, target, period, measure)])
  # relative to the size of the burden itself: a target-minus-reference difference
  # can be small, so 6-digit rounding of the inputs shows up there relative to
  # the difference, though not relative to the burden
  scale <- pmax(abs(x$target_mean), abs(x$ref_mean), 1e-9)
  dev <- max(vapply(num, function(cn) max(abs(x[[cn]] - y[[cn]]) / pmax(abs(x[[cn]]), scale)), numeric(1)))
  ok(sprintf("%s: annual route reproduces period route (worst rel. dev %.1e)", ms, dev), same_keys && dev < 1e-5)
}
