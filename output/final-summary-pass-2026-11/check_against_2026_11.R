# check_against_2026_11.R: after the production run, compare the final tables'
# totals with the 2026-11 pooled results (review/multimodel-2026-11-wbg/tables),
# which were computed from the same runs, models and draws by a different route
# (the summarizer's per-draw export, util_summarize_workflow_b.R, then
# util_pool_workflow_b_draws.R). Expectation, written before any production
# run: every mean, interval bound and difference agrees to a relative 1e-4 or
# better (both round to 6 significant digits; the compact copy stores 4-byte
# floats), for every location, group, decade, scenario and measure. YLLs are
# compared too: both use the same national life tables.
# Usage: Rscript check_against_2026_11.R <final tables dir> <2026-11 tables dir>
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); TB <- a[1]; OLD <- a[2]
old <- rbind(fread(file.path(OLD, "country_totals.csv"))[, unit := as.character(location_id)],
             fread(file.path(OLD, "world_and_income_groups_totals.csv")), fill = TRUE)[slice == "all"]
fs <- list.files(file.path(TB, "main"), "\\.csv\\.gz$", full.names = TRUE)
new <- rbindlist(lapply(fs, function(f) fread(cmd = paste("zcat", shQuote(f)))[cause == "all" & age == "all" & sex == "both"]), fill = TRUE)
cols <- c("mean", "lower95", "upper95", "lower80", "upper80", "lower66", "upper66", "diff_mean", "diff_lower95", "diff_upper95")
m <- merge(old[, c("unit", "period", "scenario", "measure", cols), with = FALSE],
           new[, c("unit", "period", "scenario", "measure", cols), with = FALSE],
           by = c("unit", "period", "scenario", "measure"), suffixes = c(".old", ".new"))
cat("matched rows:", nrow(m), "of", nrow(old), "old rows\n")
res <- rbindlist(lapply(cols, function(cl) {
  o <- m[[paste0(cl, ".old")]]; n <- m[[paste0(cl, ".new")]]; ok <- !is.na(o) & !is.na(n)
  rel <- abs(n[ok] - o[ok]) / pmax(abs(o[ok]), 1)
  data.table(column = cl, compared = sum(ok), max_rel = max(rel), n_over_1e4 = sum(rel > 1e-4))
}))
print(res)
worst <- m[, .(unit, period, scenario, measure, mean.old, mean.new, rel = abs(mean.new - mean.old) / pmax(abs(mean.old), 1))][order(-rel)][1:10]
print(worst)
cat(if (all(res$n_over_1e4 == 0)) "PASS: agrees with the 2026-11 pooled results\n" else "CHECK: differences above 1e-4, see above\n")
