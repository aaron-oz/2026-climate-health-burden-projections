# Are the ssp126 and ssp370 departures from ssp245 bigger than the spread
# between climate models? Per model (the 24 run for all four scenarios),
# global heat deaths per decade, target / ssp245 (scaled, Workflow B). Tests
# across models: sign test (how many of 24 models go each way) and a t-test
# on log ratios. Models are treated as independent, which overstates the
# evidence somewhat (several share code: two EC-Earth, two MPI, two NorESM,
# two INM, two CNRM, two MIROC, two HadGEM/UKESM).
# Usage: Rscript scenario_noise.R <workflow_b dir>
suppressMessages(library(data.table))
n <- fread(file.path(commandArgs(TRUE)[1], "national_by_year.csv"),
           select = c("location_id", "model", "scenario", "year", "deaths_heat_attrib_mean", "deaths_cold_attrib_mean",
                      "ref_deaths_heat_mean", "ref_deaths_cold_mean"))[scenario != "ssp245"]
common <- Reduce(intersect, split(n$model, n$scenario))
n <- n[model %in% common]
n[, decade := fcase(year <= 2030, "2022-2030", year <= 2040, "2031-2040", default = "2041-2050")]
test <- function(d) {
  r <- log(d$h / d$hr); rc <- log(d$c / d$cr)
  list(models = nrow(d), heat_ratio_mean = exp(mean(r)), models_higher = sum(r > 0),
       sign_p = binom.test(sum(r > 0), length(r))$p.value, t_p = t.test(r)$p.value,
       cold_ratio_mean = exp(mean(rc)), cold_models_higher = sum(rc > 0), cold_sign_p = binom.test(sum(rc > 0), length(rc))$p.value)
}
g <- n[, .(h = sum(deaths_heat_attrib_mean), hr = sum(ref_deaths_heat_mean),
           c = sum(deaths_cold_attrib_mean), cr = sum(ref_deaths_cold_mean)), by = .(scenario, model, decade)]
cat("== World, target / ssp245 per model,", length(common), "models\n")
print(g[, test(.SD), by = .(scenario, decade)][order(scenario, decade)], digits = 3)
eu <- c(Austria = 75, Germany = 81, Czechia = 47, Netherlands = 89)
ge <- n[scenario == "ssp370" & location_id %in% eu, .(h = sum(deaths_heat_attrib_mean), hr = sum(ref_deaths_heat_mean),
         c = sum(deaths_cold_attrib_mean), cr = sum(ref_deaths_cold_mean)), by = .(location_id, model, decade)]
cat("\n== ssp370 / ssp245, Western/Central Europe, per model\n")
x <- ge[, test(.SD), by = .(location_id, decade)][order(location_id, decade)]
x[, name := names(eu)[match(location_id, eu)]]
print(x, digits = 3)
