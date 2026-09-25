suppressPackageStartupMessages(library(data.table))
SP <- "output/review-ssp245/round2/work"
REV <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"  # untracked analysis CSVs from the round-1 review live in the main checkout
meta <- fread(file.path(REV, "location_meta.csv"))[, .(location_id, location_name)]
m <- readRDS(file.path(SP, "m.rds"))
m[, `:=`(gap_tot = (deaths_nonopt_mean_2 - deaths_nonopt_median) / abs(deaths_nonopt_median),
         gap_heat = (deaths_heat_mean_2 - deaths_heat_median) / abs(deaths_heat_median),
         gap_cold = (deaths_cold_mean_2 - deaths_cold_median) / abs(deaths_cold_median),
         heat_ul = deaths_heat_upper_2 / deaths_heat_median)]
s <- m[, .(combos_flag = sum(abs(gap_tot) > 0.10, na.rm=TRUE), gap_tot = median(gap_tot, na.rm=TRUE), gap_heat = median(gap_heat, na.rm=TRUE), gap_cold = median(gap_cold, na.rm=TRUE),
           heat_ul = median(heat_ul, na.rm=TRUE), mean22 = mean(deaths_nonopt_mean_2[year == 2022]), med22 = mean(deaths_nonopt_median[year == 2022]),
           heat22 = mean(deaths_heat_mean_2[year == 2022]), cold22 = mean(deaths_cold_mean_2[year == 2022]),
           lo22 = mean(deaths_nonopt_lower_2[year==2022]), hi22 = mean(deaths_nonopt_upper_2[year==2022])), by = location_id]
cat("locations with NaN heat gap:", s[is.na(gap_heat), paste(location_id, collapse=",")], "\n"); s <- merge(s, meta, by = "location_id")
cat("heat mean/median gap distribution (median over combos per location):\n"); print(quantile(s$gap_heat, na.rm=TRUE, c(0,.05,.25,.5,.75,.9,.95,.99,1)))
cat("cold gap distribution:\n"); print(quantile(s$gap_cold, na.rm=TRUE, c(0,.05,.25,.5,.75,.95,1)))
options(width = 250)
print(s[combos_flag > 0 | gap_heat > 0.1][order(-gap_heat)], digits = 3)
fwrite(s, file.path(SP, "shock_screen.csv"))
