# ssp245 round-2 follow-up review (2026-09-28), see docs/reviews/ssp245-round2-followup-review-2026-09-28.org. Run from the main checkout (needs data/erf symlink).
suppressPackageStartupMessages(library(data.table)); options(width=200)
H <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/followup-review-bundle/honduras_cache/data/tmrel/derived_cache"
x <- readRDS(file.path(H,"129_2022_N500.rds")); str(x, max.level=1); print(lapply(x, function(e) if (is.data.frame(e)) head(e,3) else e))
res <- rbindlist(lapply(2022:2050, function(y) {
  x <- readRDS(file.path(H, sprintf("129_%d_N500.rds", y))); cod <- as.data.table(x$cod); tm <- as.data.table(x$tmrel)
  sh <- cod[, .(dis = sum(deaths[acause=="inj_disaster"]), tot = sum(deaths)), by=draw][, share := dis/tot]
  t <- tm[, .(tmin = min(tmrel), tmean = mean(tmrel), nfloor = sum(tmrel==66L), nz=.N), by=draw]
  m <- merge(sh, t, by="draw")
  data.table(year=y, dis_median=median(m$dis), dis_mean=mean(m$dis), dis_max=max(m$dis), share_med=median(m$share), share_max=max(m$share),
    n_share_gt10=sum(m$share>.10), n_share_gt30=sum(m$share>.30), n_any_floor=sum(m$nfloor>0), n_majority_floor=sum(m$nfloor > m$nz/2),
    tmrel_mean_C=mean(m$tmean)/10, n_tmean_below15=sum(m$tmean<150), cor_share_tmean=cor(m$share, m$tmean))
}))
print(res)
