# Headline global numbers on the paired model sets (models with both the
# target and ssp245), scaled (Workflow B), from draw means only: ensemble
# mean of per-model global totals and the range of per-model values. No
# pooled models x draws intervals (those need draw-level output).
# Usage: Rscript paired_headline.R <workflow_b dir>
suppressMessages(library(data.table))
n <- fread(file.path(commandArgs(TRUE)[1], "national_by_year.csv"),
           select = c("location_id", "model", "scenario", "year", "deaths_heat_attrib_mean", "deaths_cold_attrib_mean",
                      "deaths_nonopt_attrib_mean", "ref_deaths_heat_mean", "ref_deaths_cold_mean", "ref_deaths_nonopt_mean"))[scenario != "ssp245"]
n[, decade := fcase(year <= 2030, "2022-2030", year <= 2040, "2031-2040", default = "2041-2050")]
g <- n[, .(h = sum(deaths_heat_attrib_mean), c = sum(deaths_cold_attrib_mean), t = sum(deaths_nonopt_attrib_mean),
           hr = sum(ref_deaths_heat_mean), cr = sum(ref_deaths_cold_mean), tr = sum(ref_deaths_nonopt_mean),
           ny = uniqueN(year)), by = .(scenario, model, decade)]
# annual averages (millions of deaths per year), and per-model ratios
out <- g[, .(models = .N,
             heat_per_yr_M = mean(h / ny) / 1e6, heat_min = min(h / ny) / 1e6, heat_max = max(h / ny) / 1e6,
             heat_ratio = mean(h) / mean(hr), heat_ratio_min = min(h / hr), heat_ratio_max = max(h / hr),
             cold_ratio = mean(c) / mean(cr), cold_ratio_min = min(c / cr), cold_ratio_max = max(c / cr),
             nonopt_ratio = mean(t) / mean(tr), nonopt_ratio_min = min(t / tr), nonopt_ratio_max = max(t / tr)),
         by = .(scenario, decade)][order(scenario, decade)]
print(out, digits = 3)
cat("\nssp245 on each paired set, heat deaths per year (millions), mean over models\n")
print(g[, .(ssp245_heat_per_yr_M = mean(hr / ny) / 1e6, ssp245_cold_per_yr_M = mean(cr / ny) / 1e6), by = .(scenario, decade)][order(scenario, decade)], digits = 3)
