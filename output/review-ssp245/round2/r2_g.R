suppressPackageStartupMessages(library(data.table))
SP <- "output/review-ssp245/round2/work"
m <- readRDS(file.path(SP, "m.rds"))
options(width = 220)
e <- m[location_id %in% c(114, 129, 133, 69, 131), .(mean = mean(deaths_nonopt_mean_2), median = mean(deaths_nonopt_median), heat_mean = mean(deaths_heat_mean_2), heat_med = mean(deaths_heat_median), cold_mean = mean(deaths_cold_mean_2), cold_med = mean(deaths_cold_median), lo = mean(deaths_nonopt_lower_2), hi = mean(deaths_nonopt_upper_2), old = mean(deaths_nonopt_mean_1), nflag = sum(abs(deaths_nonopt_mean_2 - deaths_nonopt_median)/abs(deaths_nonopt_median) > .1)), by = .(location_id, year)]
print(e[year %in% c(2022, 2026, 2030, 2035, 2040, 2045, 2050)], digits = 4)
# Temporal smoothness: per (loc, model) year-on-year log change of deaths_nonopt_mean; compare old vs new
m <- m[order(location_id, model, year)]
m[, `:=`(d2 = c(NA, diff(log(pmax(deaths_nonopt_median, 1e-9)))), d2m = c(NA, diff(log(pmax(deaths_nonopt_mean_2, 1e-9)))), d1 = c(NA, diff(log(pmax(deaths_nonopt_mean_1, 1e-9))))), by = .(location_id, model)]
big <- m[deaths_nonopt_median > 100]
cat("\nyear-on-year |log change| of median, locs with median>100: quantiles\n"); print(quantile(abs(big$d2), c(.5, .9, .99, .999, 1), na.rm = TRUE))
cat("same for new mean:\n"); print(quantile(abs(big$d2m), c(.5, .9, .99, .999, 1), na.rm = TRUE))
cat("same for old mean (>100):\n"); print(quantile(abs(m[deaths_nonopt_mean_1 > 100]$d1), c(.5, .9, .99, .999, 1), na.rm = TRUE))
print(big[order(-abs(d2m))][1:10, .(location_id, model, year, deaths_nonopt_mean_2, deaths_nonopt_median, d2m, d2)])
# ensemble per-location trend 2022->2050, new vs old
tr <- m[, .(n22 = mean(deaths_nonopt_mean_2[year == 2022]), n50 = mean(deaths_nonopt_mean_2[year == 2050]), o22 = mean(deaths_nonopt_mean_1[year == 2022]), o50 = mean(deaths_nonopt_mean_1[year == 2050])), by = location_id]
tr[, `:=`(g_new = n50 / n22, g_old = o50 / o22)]
cat("\n2050/2022 growth, new vs old, locations with old 2022 > 100: corr ", tr[o22 > 100, cor(g_new, g_old)], "\n")
print(summary(tr[o22 > 100, g_new / g_old]))
# YLL per death new vs old 2022
m22 <- m[year == 2022]
cat("YLL/death global 2022 old:", m22[, sum(yll_nonopt_mean_1) / sum(deaths_nonopt_mean_1)], " new:", m22[, sum(yll_nonopt_mean_2) / sum(deaths_nonopt_mean_2)], "\n")
