# ssp245 round-2 follow-up review (2026-09-28), see docs/reviews/ssp245-round2-followup-review-2026-09-28.org. Run from the main checkout (needs data/erf symlink).
suppressPackageStartupMessages(library(data.table)); options(width=220)
H <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/followup-review-bundle/honduras_cache/data/tmrel/derived_cache"
PC <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-pilot-2026-09/caspar_derived_cache"
files <- c(Honduras=file.path(H,"129_2022_N500.rds"), Nicaragua=file.path(PC,"131_2022_N500.rds"), Haiti=file.path(PC,"114_2022_N500.rds"))
cat("== Per-draw disaster share vs TMREL, 2022 ==\n")
for (nm in names(files)) {
  x <- readRDS(files[nm]); cod <- as.data.table(x$cod); tm <- as.data.table(x$tmrel)
  sh <- cod[, .(dis=sum(deaths[acause=="inj_disaster"]), tot=sum(deaths)), by=draw][, share:=dis/tot]
  t <- tm[, .(tmean=mean(tmrel)/10, nfloor=sum(tmrel==66L), nbelow12=sum(tmrel<120L)), by=draw]
  m <- merge(sh, t, by="draw"); m[, grp := cut(share, c(-Inf,.01,.1,.3,.5,Inf), labels=c("<1%","1-10%","10-30%","30-50%",">50%"))]
  cat("\n--", nm, " disaster deaths median", round(median(m$dis)), " max", round(max(m$dis)), " 17-cause median", round(median(m$tot)), "\n")
  print(m[, .(n_draws=.N, dis_deaths_min=round(min(dis)), dis_deaths_max=round(max(dis)), share_max=round(max(share),3), mean_tmrel_C=round(mean(tmean),2), min_drawmean_tmrel_C=round(min(tmean),2), zones_floored_mean=round(mean(nfloor),1), zones_below12C_mean=round(mean(nbelow12),1)), keyby=grp])
  if (nm=="Honduras") { hi <- m[share>.3, sort(draw)]; cat("Honduras draws with share>30%:", hi, "\n") }
}
# Are the same draws high-share every year?
cat("\n== Honduras: persistence of high-share draws across years ==\n")
S <- rbindlist(lapply(2022:2050, function(y) { cod <- as.data.table(readRDS(file.path(H, sprintf("129_%d_N500.rds", y)))$cod)
  cod[, .(share=sum(deaths[acause=="inj_disaster"])/sum(deaths)), by=draw][, year:=y] }))
cnt <- S[share>.1, .N, by=draw][order(-N)]
cat("distinct draws with share>10% in any year:", nrow(cnt), "; in how many of 29 years each (table):\n"); print(table(cnt$N))
