# ssp245 review, part 3: age-composition check on the rising cold share,
# latitude patterns, sub-grid group comparison, named outliers.
suppressPackageStartupMessages(library(data.table))

REV <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245"
OUT <- "/var/home/aoz/code/2026-climate-health-burden-projections/output/review-ssp245"
meta <- fread(file.path(OUT, "location_meta.csv"))
ens  <- fread(file.path(OUT, "ensemble_by_location_year.csv"))
tr   <- fread(file.path(OUT, "trend_2020s_vs_2040s.csv"))
sp   <- fread(file.path(OUT, "location_spread_2050.csv"))
yr   <- fread(file.path(OUT, "yll_per_death_2022.csv"))

cat("== global ensemble totals 2022 ==\n")
print(ens[year == 2022, .(heat = sum(heat), cold = sum(cold), nonopt = sum(nonopt),
                          yll = sum(yll_nonopt), deaths_17cause = sum(deaths_total))],
      digits = 4)

# ---- 1. Age-composition check on the cold share ----
# Crude cold share rose 2022 -> 2050 in most locations; test whether that
# survives age-sex standardization (fixed 2022 cell weights).
as <- fread(file.path(REV, "by_age_sex.csv"))
as <- as[year %in% c(2022, 2050)]
# (stale-combo exclusion removed 2026-08-17 after Caspar's recompute)
cell <- as[, .(deaths = mean(deaths), cold = mean(deaths_cold), heat = mean(deaths_heat)),
           by = .(location_id, year, age_group_id, sex_id)]
w <- cell[year == 2022, .(age_group_id, sex_id, w = deaths / sum(deaths)),
          by = location_id]
cell[, `:=`(cs = fifelse(deaths > 0, cold / deaths, NA_real_),
            hs = fifelse(deaths > 0, heat / deaths, NA_real_))]
std <- merge(cell, unique(w), by = c("location_id", "age_group_id", "sex_id"))
stdloc <- std[!is.na(cs), .(cold_std = sum(w * cs) / sum(w),
                            heat_std = sum(w * hs) / sum(w)),
              by = .(location_id, year)]
dstd <- dcast(stdloc, location_id ~ year, value.var = c("cold_std", "heat_std"))
dstd[, `:=`(d_cold_std = cold_std_2050 - cold_std_2022,
            d_heat_std = heat_std_2050 - heat_std_2022)]
cat("\n== age-sex-STANDARDIZED share changes, 2022 -> 2050 (fixed 2022 weights) ==\n")
cat("cold share up:", dstd[d_cold_std > 0, .N], " down:", dstd[d_cold_std < 0, .N], "\n")
cat("heat share up:", dstd[d_heat_std > 0, .N], " down:", dstd[d_heat_std < 0, .N], "\n")
fwrite(dstd, file.path(OUT, "standardized_share_change.csv"))

# Compare with crude (2022 vs 2050 single years, same years as std for fairness)
cr <- dcast(ens[year %in% c(2022, 2050),
                .(location_id, year, cs = cold / deaths_total, hs = heat / deaths_total)],
            location_id ~ year, value.var = c("cs", "hs"))
cr[, `:=`(d_cs = cs_2050 - cs_2022, d_hs = hs_2050 - hs_2022)]
cat("crude (same single years) cold share up:", cr[d_cs > 0, .N],
    " down:", cr[d_cs < 0, .N], "\n")
both <- merge(cr[, .(location_id, d_cs)], dstd[, .(location_id, d_cold_std)], by = "location_id")
cat("locations where crude cold share rises but standardized falls:",
    both[d_cs > 0 & d_cold_std < 0, .N], "\n")

# ---- 2. Latitude and the net trend ----
tr2 <- merge(tr, meta[, .(location_id, location_name, lat, cells = cells_off125)],
             by = "location_id")
tr2[, band := cut(abs(lat), c(0, 23.5, 45, 90),
                  labels = c("tropical", "subtropic/mid", "high-lat"))]
cat("\n== net (heat+cold) SHARE change by latitude band ==\n")
print(tr2[, .(n = .N, share_up = sum(d_heat_share + d_cold_share > 0),
              median_d_heat_share = median(d_heat_share),
              median_d_cold_share = median(d_cold_share)), by = band])
cat("corr(|lat|, d_cold_share):", round(cor(abs(tr2$lat), tr2$d_cold_share), 3), "\n")
cat("corr(|lat|, d_heat_share):", round(cor(abs(tr2$lat), tr2$d_heat_share), 3), "\n")

# ---- 3. Sub-grid locations vs the rest ----
sp2 <- merge(sp, meta[, .(location_id, location_name, cells = cells_off125)], by = "location_id")
sp2[, grp := fifelse(cells <= 1, "subgrid(<=1 cell)", "resolved")]
cat("\n== 2050 across-model CV by group ==\n")
print(sp2[, .(n = .N, cv_median = round(median(cv, na.rm=TRUE), 3),
              cv_q90 = round(quantile(cv, .9, na.rm=TRUE), 3)), by = grp])
e22 <- merge(ens[year == 2022, .(location_id, heat, cold, deaths_total)],
             meta[, .(location_id, cells = cells_off125)], by = "location_id")
e22[, `:=`(grp = fifelse(cells <= 1, "subgrid(<=1 cell)", "resolved"),
           nonopt_share = (heat + cold) / deaths_total,
           heat_frac = heat / (heat + cold))]
cat("\n== 2022 burden shares by group ==\n")
print(e22[, .(n = .N, nonopt_share_median = round(median(nonopt_share), 3),
              heat_frac_median = round(median(heat_frac), 3)), by = grp])

# ---- 4. Named outliers ----
cat("\n== YLL/death extremes, named ==\n")
print(merge(yr[order(ratio)][c(1:5, (.N-4):.N)],
            meta[, .(location_id, location_name)], by = "location_id")[order(ratio)])
cat("\n== widest 2050 model spread (top 10 CV), named ==\n")
print(head(sp2[order(-cv), .(location_name, cells, cv = round(cv, 2),
                             nonopt = round(nonopt), nonopt_min = round(nonopt_min),
                             nonopt_max = round(nonopt_max))], 10))
