# ssp245 round-2 follow-up review (2026-09-28), see docs/reviews/ssp245-round2-followup-review-2026-09-28.org. Run from the main checkout (needs data/erf symlink).
suppressPackageStartupMessages(library(data.table)); options(width=220)
H <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/followup-review-bundle/honduras_cache/data/tmrel/derived_cache"
S <- rbindlist(lapply(2022:2050, function(y) { x <- readRDS(file.path(H, sprintf("129_%d_N500.rds", y)))
  cod <- as.data.table(x$cod); tm <- as.data.table(x$tmrel)
  sh <- cod[, .(share=sum(deaths[acause=="inj_disaster"])/sum(deaths)), by=draw]
  fl <- tm[, .(nfl=sum(tmrel==66L)), by=draw]
  data.table(year=y, n_shock=sum(sh$share>.3), n_floor_draws=sum(fl$nfl>0), floored_zone_draws=sum(fl$nfl)) }))
nb <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/followup-review-bundle/review_bundle/summary/national_by_year.csv")[location_id==129]
g <- nb[, .(heat_mean=mean(deaths_heat_mean), heat_median=mean(deaths_heat_median), cold_gap=mean(deaths_cold_mean/deaths_cold_median-1),
   nonopt_mean=mean(deaths_nonopt_mean), nonopt_median=mean(deaths_nonopt_median), heat_gap=mean(deaths_heat_mean/deaths_heat_median-1),
   heat_mean_minus_median=mean(deaths_heat_mean-deaths_heat_median)), by=year]
m <- merge(S, g, by="year")
print(m[, lapply(.SD, function(v) if (is.double(v)) round(v,3) else v)])
cat("\ncor(floored zone-draws, heat mean-median) across 29 years:", round(cor(m$floored_zone_draws, m$heat_mean_minus_median),3),
    "\ncor(n_shock, heat mean-median):", round(cor(m$n_shock, m$heat_mean_minus_median),3),
    "\ncor(n_shock, heat gap ratio):", round(cor(m$n_shock, m$heat_gap),3), "\n")
print(summary(lm(heat_mean_minus_median ~ floored_zone_draws, m))$coef)
