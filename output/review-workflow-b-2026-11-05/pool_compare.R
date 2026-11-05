# pool_compare.R: compares util_pool_workflow_b_draws.R output with the independent
# numbers from draw_export_check.R (totals, all locations and World), and pools a
# small subset of the breakdown files with both (cause and age).
# Usage: Rscript pool_compare.R <review_out_dir> <summary_dir>; run from the repo root.
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); OUT <- a[1]; SUM <- a[2]
options(width = 170)
pl <- readRDS(file.path(OUT, "loc_pooled_all.rds"))
P <- fread(file.path(OUT, "pool_totals_common", "pooled.csv"))
cat("pooled.csv columns:", paste(names(P)[1:30], collapse = " "), "...\n")
cat("rows", nrow(P), "units", uniqueN(P$unit), "n_models", paste(unique(P$n_models), collapse = ","), "\n")
print(P[, .N, by = .(unit_type)])
loc <- P[unit_type == "location"][, location_id := as.integer(unit)]
j <- merge(loc, pl[slice == "all"], by.x = c("location_id", "period", "target", "measure"), by.y = c("location_id", "period", "target", "measure"))
cat("location rows matched:", nrow(j), "of", nrow(loc), "\n")
rel <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-6))
for (pr in list(c("target_mean", "t_mean"), c("target_lower95", "t_l95"), c("target_upper95", "t_u95"), c("target_lower80", "t_l80"), c("target_upper80", "t_u80"),
                c("target_lower66", "t_l66"), c("target_upper66", "t_u66"), c("ref_mean", "r_mean"), c("ref_lower95", "r_l95"), c("ref_upper95", "r_u95"), c("diff_mean", "d_mean"), c("diff_lower95", "d_l95"), c("diff_upper95", "d_u95"), c("target_model_min", "t_mod_min"), c("target_model_max", "t_mod_max")))
  cat(sprintf("  %-18s vs independent: worst relative difference %.2e\n", pr[1], rel(j[[pr[1]]], j[[pr[2]]])))
w <- fread(file.path(OUT, "world_common24_decades.csv"))
pw <- P[unit == "World" & unit_type == "group"]
jw <- merge(pw, w, by.x = c("target", "period", "measure"), by.y = c("scenario", "period", "measure"))
cat("World rows matched", nrow(jw), "of", nrow(pw), "; worst rel: mean", signif(rel(jw$target_mean, jw$mean), 3), " l95", signif(rel(jw$target_lower95, jw$l95), 3), " u95", signif(rel(jw$target_upper95, jw$u95), 3),
    " l80", signif(rel(jw$target_lower80, jw$l80), 3), " u66", signif(rel(jw$target_upper66, jw$u66), 3), "\n")
jr <- merge(pw[target == "ssp585"], w[scenario == "ssp245"], by = c("period", "measure"))
cat("World reference columns vs ssp245 row: worst rel mean", signif(rel(jr$ref_mean, jr$mean), 3), " u95", signif(rel(jr$ref_upper95, jr$u95), 3), "\n")
ms <- fread(file.path(OUT, "pool_totals_common", "model_sets.csv")); print(ms[, .N, by = .(target, note)])
Pp <- fread(file.path(OUT, "pool_totals_paired", "pooled.csv"))
print(Pp[unit == "World" & period == "2041-2050" & measure == "deaths_heat", .(target, n_models, target_mean, ref_mean, ratio)])
ref_diff <- Pp[unit == "World" & measure == "deaths_heat", .(lo = min(ref_mean), hi = max(ref_mean)), by = period]
cat("paired set: the reference (ssp245) World heat differs by target, as the status doc warned:\n"); print(ref_diff)

# Breakdown smoke test on 3 locations
sub <- c(6L, 114L, 349L)
for (kind in c("draws_cause", "draws_age")) {
  d <- file.path(OUT, paste0("smoke_", kind)); dir.create(d, showWarnings = FALSE)
  for (l in sub) { f <- file.path(d, paste0(l, ".rds")); if (!file.exists(f)) file.symlink(file.path(SUM, kind, paste0(l, ".rds")), f) }
  o <- file.path(OUT, paste0("smoke_", kind, "_out"))
  st <- system(paste("Rscript global-scripts/util_pool_workflow_b_draws.R", shQuote(paste0("--draws_dir=", d)), "--model_set=common --period_batch=2", shQuote(paste0("--out=", o))), ignore.stdout = TRUE)
  cat(kind, "pooling script exit:", st, "\n")
  S <- fread(file.path(o, "pooled.csv"))
  cat(kind, "rows", nrow(S), "; slices", uniqueN(S$slice), "; periods", paste(unique(S$period), collapse = " "), "\n")
  X <- rbindlist(lapply(sub, function(l) setDT(readRDS(file.path(SUM, kind, paste0(l, ".rds"))))[period == "2046-2050"]))
  X <- X[model %in% unique(fread(file.path(o, "model_sets.csv"))$model)]
  X[, slice := if ("acause" %in% names(X)) acause else paste0(age_group, "_sex", sex_id)]
  G <- X[, .(v = sum(deaths_heat) / 5), by = .(scenario, slice, model, draw)]
  ind <- G[scenario == "ssp370", .(m = mean(v), l = quantile(v, .025, names = FALSE), u = quantile(v, .975, names = FALSE)), by = slice]
  jj <- merge(S[unit == "World" & target == "ssp370" & period == "2046-2050" & measure == "deaths_heat"], ind, by = "slice")
  cat(kind, ": group sum of 3 locations vs by-hand, slices", nrow(jj), "; worst rel mean", signif(rel(jj$target_mean, jj$m), 3), " l95", signif(rel(jj$target_lower95, jj$l), 3), " u95", signif(rel(jj$target_upper95, jj$u), 3), "\n")
}
cat("\nDecade periods from the breakdown blocks through --periods: expected to refuse\n")
st <- system(paste("Rscript global-scripts/util_pool_workflow_b_draws.R", shQuote(paste0("--draws_dir=", file.path(OUT, "smoke_draws_age"))), "--model_set=common --periods=2041-2050", shQuote(paste0("--out=", file.path(OUT, "smoke_x")))), ignore.stdout = TRUE, ignore.stderr = TRUE)
cat("exit status:", st, "\n")
