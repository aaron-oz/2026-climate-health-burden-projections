suppressPackageStartupMessages(library(data.table))
SP <- "output/review-ssp245/round2/work"
REV <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"  # untracked analysis CSVs from the round-1 review live in the main checkout
meta <- fread(file.path(REV, "location_meta.csv"))[, .(location_id, location_name)]
options(width = 220)
m <- readRDS(file.path(SP, "m.rds"))
m[, mgap := deaths_mean_2 / deaths_median - 1]
cat("mortality (17-cause) mean/median - 1, all combos:\n"); print(quantile(m$mgap, c(0, .5, .9, .99, .999, 1)))
s <- m[, .(mgap_max = max(mgap), mgap_med = median(mgap), yr_max = year[which.max(mgap)]), by = location_id][order(-mgap_max)]
print(merge(s[1:15], meta)[order(-mgap_max)])
v <- m[location_id %in% c(133, 114, 129), .(mgap = mean(mgap), bgap = mean(deaths_nonopt_mean_2 / deaths_nonopt_median - 1), cgap = mean(deaths_cold_mean_2/deaths_cold_median - 1), hgap = mean(deaths_heat_mean_2/deaths_heat_median-1), mort_hi_over_med = mean(deaths_upper_2 / deaths_median)), by = .(location_id, year)]
print(dcast(melt(v[year %in% c(2022:2027, 2030, 2040, 2050)], id.vars = c("location_id", "year")), location_id + variable ~ year), digits = 3)
# trend outliers new vs old growth
tr <- m[, .(n22 = mean(deaths_nonopt_mean_2[year == 2022]), n50 = mean(deaths_nonopt_mean_2[year == 2050]), o22 = mean(deaths_nonopt_mean_1[year == 2022]), o50 = mean(deaths_nonopt_mean_1[year == 2050])), by = location_id]
tr[, rr := (n50 / n22) / (o50 / o22)]
print(merge(tr[o22 > 100][order(abs(log(rr)), decreasing = TRUE)][1:10], meta), digits = 4)
