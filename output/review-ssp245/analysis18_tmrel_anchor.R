# External anchor for the derived per-draw TMRELs: compare their draw means
# (and spreads) against IHME's RELEASED TMREL summaries/draws per
# (location, zone). Same argmin definition, so the two should agree up to
# known differences: weight source and year (our IHME-forecast 2022 cause
# deaths vs their GBD 2019 CoD at year 2020), their subnational aggregation,
# and draw-set size (500 ERF draws here vs their 100 released draws).
# Weights here are the production by_cause 2022 point shares (as in the
# replica), applied per zone over the zone's ERF causes.
suppressPackageStartupMessages(library(data.table))
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
OUT <- "output/review-ssp245"
SEARCH <- c(66L, 346L)  # tmrelCalculator search range, t10 units

bc <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/by_cause.csv")
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022]
meta <- fread(file.path(OUT, "location_meta.csv"))

E <- setDT(readRDS("data/erf/cache/erf_curves_draws_N500.rds"))
if (min(E$rr) < 0) E[, rr := exp(rr)]
E <- E[daily_temp >= SEARCH[1] & daily_temp <= SEARCH[2]]
Z <- list()
for (z in sort(unique(E$zone))) {
  ez <- dcast(E[zone == z], acause + daily_temp ~ draw, value.var = "rr")
  causes <- sort(unique(ez$acause)); tvals <- sort(unique(ez$daily_temp))
  dcols <- setdiff(names(ez), c("acause", "daily_temp"))
  blocks <- lapply(causes, function(c_) {
    b <- as.matrix(ez[acause == c_][match(tvals, daily_temp), ..dcols])
    b[is.na(b)] <- 1
    b
  })
  Z[[as.character(z)]] <- list(causes = causes, tvals = tvals, blocks = blocks)
}
rm(E); gc()

res <- list()
for (id in meta$location_id) {
  f <- sprintf("data/tmrel/tmrel_%d.csv", id)
  fs <- sprintf("data/tmrel/tmrel_%d_summaries.csv", id)
  if (!file.exists(f) || !file.exists(fs)) next
  rel_d <- fread(f)[year_id == 2020]
  rel_s <- fread(fs)[year_id == 2020]
  wts <- bc[location_id == id, .(acause, deaths)]
  if (nrow(wts) == 0) next
  deaths_v <- setNames(wts$deaths, wts$acause)
  for (z in names(Z)) {
    zi <- Z[[z]]
    wv <- deaths_v[zi$causes]; wv[is.na(wv)] <- 0
    if (sum(wv) == 0) next
    wvn <- wv / sum(wv)
    W <- matrix(0, length(zi$tvals), ncol(zi$blocks[[1]]))
    for (ci in seq_along(zi$causes)) W <- W + wvn[ci] * zi$blocks[[ci]]
    td <- zi$tvals[apply(W, 2, which.min)] / 10
    zrow_s <- rel_s[meanTempCat == as.integer(z)]
    zrow_d <- rel_d[meanTempCat == as.integer(z)]
    if (nrow(zrow_s) != 1 || nrow(zrow_d) != 1) next
    reld <- as.numeric(zrow_d[, paste0("tmrel_", 0:99), with = FALSE])
    res[[length(res) + 1]] <- data.table(
      location_id = id, zone = as.integer(z),
      derived_mean = mean(td), derived_sd = sd(td),
      released_mean = zrow_s$tmrelMean, released_sd = sd(reld))
  }
}
R <- rbindlist(res)
R[, diff := derived_mean - released_mean]
fwrite(R, file.path(OUT, "analysis18_tmrel_anchor.csv"))

cat("rows (location x zone):", nrow(R), "| locations:", uniqueN(R$location_id), "\n")
cat(sprintf("derived - released mean [C]: mean %+.2f | median %+.2f | mean|.| %.2f | p90|.| %.2f | max|.| %.2f\n",
            mean(R$diff), median(R$diff), mean(abs(R$diff)),
            quantile(abs(R$diff), 0.9), max(abs(R$diff))))
cat("correlation of means:", round(cor(R$derived_mean, R$released_mean), 3), "\n")
cat(sprintf("draw spread [C]: derived median sd %.2f | released median sd %.2f\n",
            median(R$derived_sd), median(R$released_sd)))
cat("\nworst 8 (loc, zone) by |diff|:\n")
print(R[order(-abs(diff))][1:8])
