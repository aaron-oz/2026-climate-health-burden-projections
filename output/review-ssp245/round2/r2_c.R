suppressPackageStartupMessages(library(data.table))
dir.create("output/review-ssp245/round2/work", showWarnings = FALSE, recursive = TRUE)
R2 <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/review_bundle"
R1 <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245"
REV <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"  # untracked analysis CSVs from the round-1 review live in the main checkout
meta <- fread(file.path(REV, "location_meta.csv"))[, .(location_id, location_name)]
n2 <- fread(file.path(R2, "summary/national_by_year.csv"))
n1 <- fread(file.path(R1, "national_by_year.csv"))
k <- c("location_id", "model", "year")
m <- merge(n1, n2, by = k, suffixes = c("_1", "_2"))
cat("merged combos:", nrow(m), "\n")
cat("all-cause deaths identical:", m[, max(abs(deaths_mean_1 - deaths_mean_2))], "\n")
# identical burden = stale
m[, same := abs(deaths_nonopt_mean_1 - deaths_nonopt_mean_2) < 1e-6]
cat("combos with deaths_nonopt_mean identical to round 1:", sum(m$same), "\n")
print(merge(m[same == TRUE, .(location_id, model, year, deaths_nonopt_mean_2)], meta, by = "location_id"))
m[, r := deaths_nonopt_mean_2 / deaths_nonopt_mean_1]
cat("\nratio new/old deaths_nonopt_mean, all combos:\n"); print(quantile(m$r, c(0, .01, .05, .25, .5, .75, .95, .99, 1), na.rm = TRUE))
cat("share of combos where burden went up:", mean(m$deaths_nonopt_mean_2 > m$deaths_nonopt_mean_1), "\n")
# per location, median ratio over models & years
pl <- m[, .(r_med = median(r), r_min = min(r), r_max = max(r), old22 = mean(deaths_nonopt_mean_1[year == 2022]), new22 = mean(deaths_nonopt_mean_2[year == 2022])), by = location_id]
pl <- merge(pl, meta, by = "location_id")
cat("\nper-location median ratio quantiles:\n"); print(quantile(pl$r_med, c(0, .05, .25, .5, .75, .95, 1)))
cat("locations with median ratio < 1:", sum(pl$r_med < 1), "\n")
print(pl[order(r_med)][1:12])
print(pl[order(-r_med)][1:12])
# global totals by year, ensemble mean of per-model global sums
g <- m[, .(old = sum(deaths_nonopt_mean_1), new = sum(deaths_nonopt_mean_2), oldh = sum(deaths_heat_mean_1), newh = sum(deaths_heat_mean_2), oldc = sum(deaths_cold_mean_1), newc = sum(deaths_cold_mean_2), yold = sum(yll_nonopt_mean_1), ynew = sum(yll_nonopt_mean_2)), by = .(model, year)][, lapply(.SD, mean), by = year, .SDcols = -"model"]
g[, `:=`(ratio = new / old, ratio_heat = newh / oldh, ratio_cold = newc / oldc, ratio_yll = ynew / yold)]
cat("\nGlobal (ensemble mean over 27 models):\n"); print(g[year %in% c(2022, 2030, 2040, 2050)], digits = 6)
# lower bound negative counts
cat("\ncombos with deaths_nonopt_lower < 0: old", sum(m$deaths_nonopt_lower_1 < 0), " new", sum(m$deaths_nonopt_lower_2 < 0), "\n")
print(merge(m[deaths_nonopt_lower_2 < 0, .N, by = location_id], meta)[order(-N)])
# relative width
m[, w1 := (deaths_nonopt_upper_1 - deaths_nonopt_lower_1) / abs(deaths_nonopt_mean_1)]
m[, w2 := (deaths_nonopt_upper_2 - deaths_nonopt_lower_2) / abs(deaths_nonopt_mean_2)]
cat("\nrelative width quantiles old / new:\n"); print(rbind(old = quantile(m$w1, c(.05,.25,.5,.75,.95,.99)), new = quantile(m$w2, c(.05,.25,.5,.75,.95,.99))))
saveRDS(m, "output/review-ssp245/round2/work/m.rds")
fwrite(pl, "output/review-ssp245/round2/work/per_loc_ratio.csv")
