# Draw-pairing test. Hypothesis: production's recycled ERF-draw x TMREL-draw
# pairing (02_load_tmrel.R) breaks IHME's per-draw argmin property
# (tmrelCalculator.R line 126: TMREL draw d is the minimum of RR draw d's
# death-weighted curve), letting rescaled RR dip below 1 and dragging the
# draw-mean PAF down, worst in flat-minimum (cool-side) countries.
# Test: draw-mode replica burden under (A) recycled pairing vs (B) per-draw
# argmin TMRELs, vs the summary-curve value, production, and GBD 2019.
suppressPackageStartupMessages({library(ncdf4); library(data.table)})
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
C <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "output/review-ssp245"
TARGETS <- data.table(loc = c(125L, 11L),
                      name = c("Colombia(ctrl)", "Indonesia"))

nc <- nc_open(file.path(C, "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245/timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
v <- names(nc$var)[which.max(sapply(nc$var, function(x) prod(x$size)))]
arr <- ncvar_get(nc, v); nc_close(nc)
nc <- nc_open(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245/climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc"))
pp <- ncvar_get(nc, names(nc$var)[1]); pp[is.na(pp)] <- 0; nc_close(nc)
grid_dt <- readRDS(file.path(OUT, "pixel_loc_map.rds"))
E <- setDT(readRDS("data/erf/cache/erf_curves_draws_N500.rds"))  # long: zone, daily_temp, acause, draw, rr
draw_ids <- sort(unique(E$draw))
cat("draws:", length(draw_ids), "ids", draw_ids[1], "to", tail(draw_ids, 1),
    "| rr range:", round(min(E$rr), 3), "-", round(max(E$rr), 3), "\n")
if (min(E$rr) < 0) { cat("rr is log-scale; exponentiating\n"); E[, rr := exp(rr)] }
bc <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/by_cause.csv")
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022 & location_id %in% TARGETS$loc]
nat <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/national_by_year.csv")
nat <- nat[model == "access-cm2-r1i1p1f1" & year == 2022 & location_id %in% TARGETS$loc]
M <- fread(file.path(OUT, "revision_all204.csv"))

f_signed <- function(rel) ifelse(rel >= 1, (rel - 1) / rel, rel - 1)

for (k in seq_len(nrow(TARGETS))) {
  id <- TARGETS$loc[k]
  px <- grid_dt[loc_id == id]; px[, pop := pp[cbind(ilon, ilat)]]; px <- px[pop > 0]
  d <- t(apply(px[, cbind(ilon, ilat)], 1, function(ij) arr[ij[1], ij[2], ]))
  zone <- pmin(pmax(round(rowMeans(d)), 6), 28)
  nd <- ncol(d)
  long <- data.table(pix = rep(seq_len(nrow(px)), nd),
                     t10 = as.integer(round(10 * as.vector(d))), w = rep(px$pop, nd))
  long[, zone := zone[pix]]; long[, w := w / sum(w)]
  zl <- E[, .(tmin = min(daily_temp), tmax = max(daily_temp)), by = zone]
  long <- merge(long, zl, by = "zone")
  long[, t10c := pmin(pmax(t10, tmin), tmax)]
  agg <- long[, .(pr = sum(w)), by = .(zone, t10c)]
  tmr <- fread(sprintf("data/tmrel/tmrel_%d.csv", id))[year_id == 2020]
  wts <- bc[location_id == id, .(acause, deaths)]
  deaths_v <- setNames(wts$deaths, wts$acause)

  burden_A <- 0; burden_B <- 0
  for (z in sort(unique(agg$zone))) {
    ez <- dcast(E[zone == z], acause + daily_temp ~ draw, value.var = "rr")
    setorder(ez, acause, daily_temp)
    causes <- sort(unique(ez$acause)); tvals <- sort(unique(ez$daily_temp))
    dcols <- as.character(draw_ids)
    D <- as.matrix(ez[, ..dcols])                       # (cause x temp) rows, 500 cols
    prz <- agg[zone == z]
    ti <- match(prz$t10c, tvals)
    wv <- deaths_v[causes]; wv[is.na(wv)] <- 0; wvn <- wv / sum(wv)
    # robust per-cause blocks on a complete (cause x temp) grid
    blocks <- lapply(causes, function(c_) {
      sub <- ez[acause == c_]
      as.matrix(sub[match(tvals, daily_temp), ..dcols])
    })
    # per-draw death-weighted mean curve (temps x draws), argmin -> TMREL_B
    Wm <- matrix(0, length(tvals), length(dcols))
    for (ci in seq_along(causes)) { B <- blocks[[ci]]; B[is.na(B)] <- 1
      Wm <- Wm + wvn[ci] * B }
    tB <- tvals[apply(Wm, 2, which.min)]
    # recycled pairing: ERF draw j (1..500) with TMREL draw (j-1) %% 100
    tmz <- as.numeric(tmr[meanTempCat == z, paste0("tmrel_", 0:99), with = FALSE])
    tA <- as.integer(round(10 * tmz[(draw_ids %% 100) + 1]))
    tA <- pmin(pmax(tA, min(tvals)), max(tvals))
    for (ci in seq_along(causes)) {
      block <- blocks[[ci]]
      refA <- block[cbind(match(tA, tvals), seq_along(dcols))]
      refB <- block[cbind(match(tB, tvals), seq_along(dcols))]
      sub <- block[ti, , drop = FALSE]
      pafA <- colSums(prz$pr * f_signed(sweep(sub, 2, refA, "/")), na.rm = TRUE)
      pafB <- colSums(prz$pr * f_signed(sweep(sub, 2, refB, "/")), na.rm = TRUE)
      burden_A <- burden_A + mean(pafA) * wv[ci]
      burden_B <- burden_B + mean(pafB) * wv[ci]
    }
  }
  prod_v <- nat[location_id == id, deaths_nonopt_mean]
  pap <- M[location_id == id]
  cat(sprintf("%-15s production %6.0f | replica-draws A(recycled) %6.0f | B(per-draw TMREL) %6.0f | GBD2019 %5.0f [%5.0f, %5.0f]\n",
      TARGETS$name[k], prod_v, burden_A, burden_B, pap$val_nonopt, pap$lower_nonopt, pap$upper_nonopt))
}