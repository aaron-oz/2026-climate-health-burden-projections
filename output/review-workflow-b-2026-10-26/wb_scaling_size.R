# Size of the Workflow B scaling per location: scaled / unscaled national
# non-optimal deaths (draw means), per (location, model, year), summarized by
# target scenario. Usage: Rscript wb_scaling_size.R <workflow_b dir>
suppressMessages(library(data.table))
n <- fread(file.path(commandArgs(TRUE)[1], "national_by_year.csv"),
           select = c("location_id", "model", "scenario", "year", "deaths_nonopt_attrib_mean",
                      "pipeline_deaths_nonopt_mean", "m_scaled_mean", "ihme_deaths_mean"))
n <- n[scenario != "ssp245"]
n[, `:=`(r = deaths_nonopt_attrib_mean / pipeline_deaths_nonopt_mean, mchg = m_scaled_mean / ihme_deaths_mean - 1)]
cat("== per (location, model, year) ratio scaled / unscaled non-optimal deaths\n")
print(n[, .(q01 = quantile(r, .01), median = median(r), q99 = quantile(r, .99), min = min(r), max = max(r),
            share_within_5pct = mean(abs(r - 1) <= 0.05), max_abs_mortality_change = max(abs(mchg))), by = scenario], digits = 4)
cat("\n== locations whose median ratio (over models and years) is furthest from 1\n")
l <- n[, .(median = median(r), min = min(r), max = max(r), deaths = mean(pipeline_deaths_nonopt_mean)), by = .(scenario, location_id)]
print(l[order(-abs(median - 1))][, head(.SD, 4), by = scenario], digits = 4)
cat("\n== most extreme single combos\n")
print(n[order(-abs(r - 1))][, head(.SD, 3), by = scenario][, .(scenario, location_id, model, year, r, pipeline_deaths_nonopt_mean)], digits = 4)
