# Section-3 review computations over the ssp245 summary tables.
# Reads national_by_year.csv; writes compact CSVs to output/review-ssp245/.
suppressPackageStartupMessages(library(data.table))

REV <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245"
OUT <- "/var/home/aoz/code/2026-climate-health-burden-projections/output/review-ssp245"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

nat <- fread(file.path(REV, "national_by_year.csv"))

# 2026-08-17: Caspar recomputed the three defective combos (125 & 172
# access-cm2 2022 single-cause; 20 kace 2029 missing YLLs), verified fixed,
# so no exclusions are needed any more.

# ---- Ensemble per (location, year): mean over models; model spread ----
ens <- nat[, .(
  n_models      = .N,
  deaths_total  = mean(deaths_mean),
  heat          = mean(deaths_heat_mean),
  cold          = mean(deaths_cold_mean),
  nonopt        = mean(deaths_nonopt_mean),
  nonopt_sd     = sd(deaths_nonopt_mean),
  nonopt_min    = min(deaths_nonopt_mean),
  nonopt_max    = max(deaths_nonopt_mean),
  heat_lo_draw  = mean(deaths_heat_lower),   # avg of per-model 95% draw CIs
  heat_hi_draw  = mean(deaths_heat_upper),
  cold_lo_draw  = mean(deaths_cold_lower),
  cold_hi_draw  = mean(deaths_cold_upper),
  nonopt_lo_draw= mean(deaths_nonopt_lower),
  nonopt_hi_draw= mean(deaths_nonopt_upper),
  yll_nonopt    = mean(yll_nonopt_mean, na.rm = TRUE),
  yll_heat      = mean(yll_heat_mean,  na.rm = TRUE),
  yll_cold      = mean(yll_cold_mean,  na.rm = TRUE)
), by = .(location_id, year)]
fwrite(ens, file.path(OUT, "ensemble_by_location_year.csv"))

# ---- Global totals, 2022 and 2050 ----
glob <- ens[year %in% c(2022, 2030, 2040, 2050),
            .(heat = sum(heat), cold = sum(cold), nonopt = sum(nonopt),
              yll_nonopt = sum(yll_nonopt), deaths_total = sum(deaths_total)),
            by = year]
cat("\n== global ensemble totals (deaths, all 204 locations) ==\n")
print(glob, digits = 4)

# ---- Anchor: 9 Burkart countries at 2022 ----
bm <- fread(file.path("data/benchmarks/burkart2021_table1.csv"))
bm19 <- dcast(bm[year == 2019],
              location_id + location_name ~ risk,
              value.var = c("deaths", "deaths_lower", "deaths_upper", "paf_pct"))
anchor <- merge(bm19,
                ens[year == 2022,
                    .(location_id, ours_heat = heat, ours_cold = cold,
                      ours_nonopt = nonopt, ours_nonopt_min = nonopt_min,
                      ours_nonopt_max = nonopt_max,
                      ours_heat_lo = heat_lo_draw, ours_heat_hi = heat_hi_draw,
                      ours_nonopt_lo = nonopt_lo_draw, ours_nonopt_hi = nonopt_hi_draw,
                      deaths_total_17cause = deaths_total, n_models)],
                by = "location_id")
anchor[, `:=`(ratio_nonopt = ours_nonopt / deaths_nonopt,
              ratio_heat   = ours_heat / deaths_high,
              ratio_cold   = ours_cold / deaths_low,
              heat_share_ours    = ours_heat / (ours_heat + ours_cold),
              heat_share_burkart = deaths_high / (deaths_high + deaths_low))]
fwrite(anchor, file.path(OUT, "anchor_vs_burkart2022.csv"))
cat("\n== anchor: ours(2022 ensemble) vs Burkart Table 1 (2019) ==\n")
print(anchor[order(-deaths_nonopt),
             .(location_name, burkart_nonopt = deaths_nonopt,
               ours_nonopt = round(ours_nonopt),
               ratio = round(ratio_nonopt, 2),
               burkart_heat = deaths_high, ours_heat = round(ours_heat),
               burkart_cold = deaths_low,  ours_cold = round(ours_cold),
               hs_burkart = round(heat_share_burkart, 3),
               hs_ours = round(heat_share_ours, 3), n_models)])

# Rank ordering across the nine countries
cat("\nSpearman rank corr (nonopt deaths, ours vs Burkart):",
    round(cor(anchor$ours_nonopt, anchor$deaths_nonopt, method = "spearman"), 3), "\n")

# ---- Trend: ensemble means 2022-2026 vs 2046-2050 ----
tr <- merge(
  ens[year %in% 2022:2026, .(heat0 = mean(heat), cold0 = mean(cold),
                             nonopt0 = mean(nonopt), tot0 = mean(deaths_total)),
      by = location_id],
  ens[year %in% 2046:2050, .(heat1 = mean(heat), cold1 = mean(cold),
                             nonopt1 = mean(nonopt), tot1 = mean(deaths_total)),
      by = location_id],
  by = "location_id")
tr[, `:=`(d_heat = heat1 - heat0, d_cold = cold1 - cold0,
          d_net = (heat1 + cold1) - (heat0 + cold0),
          # PAF-like shares against the 17-cause denominator, to strip the
          # mortality-forecast trend out of the climate signal
          d_heat_share = heat1 / tot1 - heat0 / tot0,
          d_cold_share = cold1 / tot1 - cold0 / tot0)]
fwrite(tr, file.path(OUT, "trend_2020s_vs_2040s.csv"))
cat("\n== trend direction counts (204 locations) ==\n")
cat("heat deaths up:", sum(tr$d_heat > 0), " down:", sum(tr$d_heat < 0), "\n")
cat("cold deaths up:", sum(tr$d_cold > 0), " down:", sum(tr$d_cold < 0), "\n")
cat("heat SHARE up:", sum(tr$d_heat_share > 0), " down:", sum(tr$d_heat_share < 0), "\n")
cat("cold SHARE up:", sum(tr$d_cold_share > 0), " down:", sum(tr$d_cold_share < 0), "\n")
cat("net share up:", sum(tr$d_heat_share + tr$d_cold_share > 0),
    " down:", sum(tr$d_heat_share + tr$d_cold_share < 0), "\n")

# ---- Model spread & outliers ----
# Robust per-(loc, year) z of each model against the ensemble median.
nat[, `:=`(med = median(deaths_nonopt_mean), madv = mad(deaths_nonopt_mean)),
    by = .(location_id, year)]
nat[, z := (deaths_nonopt_mean - med) / pmax(madv, 1e-9)]
modstat <- nat[, .(mean_z = mean(z), mean_abs_z = mean(abs(z)),
                   frac_extreme = mean(abs(z) > 5)), by = model][order(-mean_abs_z)]
fwrite(modstat, file.path(OUT, "model_outlier_stats.csv"))
cat("\n== model outlier stats (top 8 by mean |robust z|) ==\n")
print(head(modstat, 8), digits = 3)

# Location-level spread: CV across models of the 2050 nonopt deaths
spread50 <- ens[year == 2050, .(location_id, cv = nonopt_sd / abs(nonopt),
                                nonopt, nonopt_min, nonopt_max)]
fwrite(spread50, file.path(OUT, "location_spread_2050.csv"))
cat("\n== location CV of 2050 nonopt deaths: quantiles ==\n")
print(round(quantile(spread50$cv, c(.05, .25, .5, .75, .95), na.rm = TRUE), 3))

# ---- YLL per attributable death, 2022 ensemble ----
yr <- ens[year == 2022 & abs(nonopt) > 1,
          .(location_id, ratio = yll_nonopt / nonopt, nonopt)]
cat("\n== YLL per attributable nonopt death, 2022 (locations with |nonopt|>1) ==\n")
print(round(quantile(yr$ratio, c(.01, .05, .25, .5, .75, .95, .99), na.rm = TRUE), 2))
fwrite(yr, file.path(OUT, "yll_per_death_2022.csv"))
cat("\nextremes:\n")
print(yr[order(ratio)][c(1:5, (.N-4):.N)])
