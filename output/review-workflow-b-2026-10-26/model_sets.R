# How much does the choice of model set move the 2041-2050 global heat
# numbers? Global heat deaths = mean over models of the sum over locations
# and years 2041-2050 (draw means). Usage: Rscript model_sets.R <scenario bundles dir> <workflow_b dir>
suppressMessages(library(data.table))
a <- commandArgs(TRUE); SC <- a[1]; WB <- a[2]
u <- rbindlist(lapply(c("ssp126", "ssp245", "ssp370", "ssp585"), function(s)
  fread(file.path(SC, s, s, "national_by_year.csv"), select = c("location_id", "model", "scenario", "year", "deaths_heat_mean"))[year >= 2041]))
w <- fread(file.path(WB, "national_by_year.csv"), select = c("location_id", "model", "scenario", "year", "deaths_heat_attrib_mean"))[year >= 2041 & scenario != "ssp245"]
pm_u <- u[, .(h = sum(deaths_heat_mean)), by = .(scenario, model)]
pm_w <- w[, .(h = sum(deaths_heat_attrib_mean)), by = .(scenario, model)]
sets <- split(pm_u$model, pm_u$scenario)
common <- Reduce(intersect, sets)
cat("unscaled models per scenario:", paste(names(sets), lengths(sets), collapse = ", "), "\n")
cat("models in all four scenarios:", length(common), "\n")
cat("models not in all four:\n"); print(sapply(sets, function(s) paste(sub("-r.*", "", setdiff(s, common)), collapse = ",")))
h245 <- pm_u[scenario == "ssp245"]
f <- function(pm, s, m) pm[scenario == s & model %in% m, mean(h)]
out <- rbindlist(lapply(c("ssp126", "ssp370", "ssp585"), function(s) {
  all_u <- sets[[s]]; scaled <- pm_w[scenario == s, model]; pair <- intersect(all_u, h245$model)
  data.table(scenario = s,
    n_all = length(all_u), heat_all_unscaled = f(pm_u, s, all_u),
    n_scaled = length(scaled), heat_scaled = f(pm_w, s, scaled),
    n_common = length(common), heat_common_scaled = f(pm_w, s, common),
    ratio_all_vs_all245 = f(pm_u, s, all_u) / f(pm_u, "ssp245", h245$model),
    ratio_paired_scaled = f(pm_w, s, scaled) / f(pm_u, "ssp245", scaled),
    ratio_common_scaled = f(pm_w, s, common) / f(pm_u, "ssp245", common))
}))
print(out, digits = 4)
cat("\nssp245 heat, all 27 models:", round(f(pm_u, "ssp245", h245$model)), " common set:", round(f(pm_u, "ssp245", common)), "\n")
cat("\nper-model heat 2041-2050, ssp585 / ssp245 (unscaled), and the two ssp585 models with no ssp245 run (level only)\n")
x <- dcast(pm_u[scenario %in% c("ssp245", "ssp585")], model ~ scenario, value.var = "h")
x[, ratio := ssp585 / ssp245]; x[, model := sub("-r.*", "", model)]
print(x[order(-ssp585)], digits = 4)
