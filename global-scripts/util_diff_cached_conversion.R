# util_diff_cached_conversion.R — Compare cached vs freshly-converted Colombia files
#
# Sanity check that re-running util_convert_samuel_colombia.R produces identical
# output to the prior cached run. Any divergence indicates either:
#   - the source data changed
#   - the converter code changed
#   - non-deterministic behavior crept in (which would be a bug)

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))

library(data.table)

CACHE_DIR <- file.path(DATA_DIR, ".cache_pre_rerun_2026-05-04")

pairs <- list(
  list(name = "temperature",
       cached = file.path(CACHE_DIR, "125_daily_temp.rds"),
       new    = file.path(TEMP_DIR, "125_daily_temp.rds")),
  list(name = "mortality",
       cached = file.path(CACHE_DIR, "125_mortality.rds"),
       new    = file.path(MORTALITY_DIR, "125_mortality.rds")),
  list(name = "lifetable",
       cached = file.path(CACHE_DIR, "125_lifetable.rds"),
       new    = file.path(LIFETABLE_DIR, "125_lifetable.rds")),
  list(name = "divipola",
       cached = file.path(CACHE_DIR, "divipola.rds"),
       new    = file.path(DATA_DIR, "colombia", "divipola.rds"))
)

all_pass <- TRUE

for (p in pairs) {
  cat("\n=== ", p$name, " ===\n", sep = "")
  a <- readRDS(p$cached); setDT(a)
  b <- readRDS(p$new);    setDT(b)

  cat("  cached: ", nrow(a), " rows × ", ncol(a), " cols\n", sep = "")
  cat("  new:    ", nrow(b), " rows × ", ncol(b), " cols\n", sep = "")

  if (!identical(sort(names(a)), sort(names(b)))) {
    cat("  FAIL: column names differ\n")
    cat("    cached only: ", paste(setdiff(names(a), names(b)), collapse = ", "), "\n")
    cat("    new only:    ", paste(setdiff(names(b), names(a)), collapse = ", "), "\n")
    all_pass <- FALSE
    next
  }

  setcolorder(b, names(a))
  setkeyv(a, names(a))
  setkeyv(b, names(b))

  cmp <- all.equal(a, b)
  if (isTRUE(cmp)) {
    cat("  PASS: identical\n")
  } else {
    cat("  FAIL:\n")
    for (line in cmp) cat("    ", line, "\n", sep = "")

    # Try to localize the diff: numeric diffs by row sums
    num_cols <- intersect(names(a)[sapply(a, is.numeric)],
                          names(b)[sapply(b, is.numeric)])
    if (length(num_cols) > 0) {
      cat("  Numeric column totals (cached vs new):\n")
      for (col in num_cols) {
        sa <- sum(a[[col]], na.rm = TRUE)
        sb <- sum(b[[col]], na.rm = TRUE)
        cat(sprintf("    %-15s  %20.6f  %20.6f  diff=%g\n",
                    col, sa, sb, sb - sa))
      }
    }
    all_pass <- FALSE
  }
}

cat("\n=========================================\n")
if (all_pass) {
  cat("OVERALL: PASS — all four files match cached versions exactly.\n")
} else {
  cat("OVERALL: FAIL — see diffs above.\n")
  quit(status = 1, save = "no")
}
