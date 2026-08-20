# Diagnose replica-emulation failures (China 0.37x, Philippines 0.49x of
# production). Compare replica-A per-cause PAFs against production's implied
# per-cause PAFs (deaths_heat_c / deaths_c from by_cause.csv), and report
# exposure structure (pop share and PAF by zone), to localize whether the
# replica diverges uniformly (exposure/pop weighting) or by cause/zone.
suppressPackageStartupMessages({library(ncdf4); library(data.table)})
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
C <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "output/review-ssp245"
TARGETS <- c(6L, 16L)   # China, Philippines

nc <- nc_open(file.path(C, "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245/timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
v <- names(nc$var)[which.max(sapply(nc$var, function(x) prod(x$size)))]
arr <- ncvar_get(nc, v); nc_close(nc)
nc <- nc_open(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245/climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc"))
pp <- ncvar_get(nc, names(nc$var)[1]); pp[is.na(pp)] <- 0; nc_close(nc)
grid_dt <- readRDS(file.path(OUT, "pixel_loc_map.rds"))
E <- setDT(readRDS("data/erf/cache/erf_curves_draws_N500.rds"))
draw_ids <- sort(unique(E$draw)); ndr <- length(draw_ids)
if (min(E$rr) < 0) E[, rr := exp(rr)]
bc <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/by_cause.csv")
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022 & location_id %in% TARGETS]
f_signed <- function(rel) ifelse(rel >= 1, (rel - 1) / rel, rel - 1)

for (id in TARGETS) {
  px <- grid_dt[loc_id == id]; px[, pop := pp[cbind(ilon, ilat)]]; px <- px[pop > 0]
  d <- t(apply(px[, cbind(ilon, ilat)], 1, function(ij) arr[ij[1], ij[2], ]))
  zone <- pmin(pmax(round(rowMeans(d)), 6), 28)
  cat(sprintf("\n==== loc %d: %d populated pixels ====\n", id, nrow(px)))
  zs <- data.table(zone = zone, pop = px$pop)[, .(pop_share = sum(pop)), by = zone][order(zone)]
  zs[, pop_share := round(pop_share / sum(pop_share), 3)]
  cat("pop share by zone:\n"); print(zs[pop_share > 0.005])
  nd <- ncol(d)
  long <- data.table(pix = rep(seq_len(nrow(px)), nd),
                     t10 = as.integer(round(10 * as.vector(d))), w = rep(px$pop, nd))
  long[, zone := zone[pix]]; long[, w := w / sum(w)]
  tmr <- fread(sprintf("data/tmrel/tmrel_%d.csv", id))[year_id == 2020]
  wts <- bc[location_id == id]
  # replica-A per-cause PAFs (recycled pairing), accumulated by cause and zone
  paf_c <- setNames(numeric(nrow(wts)), wts$acause)
  paf_ch <- setNames(numeric(nrow(wts)), wts$acause)   # heat side
  zone_contrib <- list()
  for (z in sort(unique(long$zone))) {
    ez <- dcast(E[zone == z], acause + daily_temp ~ draw, value.var = "rr")
    setorder(ez, acause, daily_temp)
    causes <- sort(unique(ez$acause)); tvals <- sort(unique(ez$daily_temp))
    dcols <- as.character(draw_ids)
    blocks <- lapply(causes, function(c_) {
      sub <- ez[acause == c_]
      as.matrix(sub[match(tvals, daily_temp), ..dcols]) })
    lz <- long[zone == z]
    lz[, t10c := pmin(pmax(t10, min(tvals)), max(tvals))]
    prz <- lz[, .(pr = sum(w)), by = t10c]
    ti <- match(prz$t10c, tvals)
    tmz <- as.numeric(tmr[meanTempCat == z, paste0("tmrel_", 0:99), with = FALSE])
    tA <- as.integer(round(10 * tmz[(draw_ids %% 100) + 1]))
    tA <- pmin(pmax(tA, min(tvals)), max(tvals))
    iA <- match(tA, tvals)
    ztot <- 0
    for (ci in seq_along(causes)) {
      block <- blocks[[ci]]
      refA <- block[cbind(iA, seq_len(ndr))]
      sub <- block[ti, , drop = FALSE]
      rel <- sweep(sub, 2, refA, "/")
      heat_mask <- prz$t10c > tA[col(rel)]     # per draw heat classification
      contrib <- prz$pr * f_signed(rel)
      pafz <- mean(colSums(contrib, na.rm = TRUE))
      pafz_h <- mean(colSums(contrib * (matrix(prz$t10c, nrow(contrib), ndr) >
                     matrix(tA, nrow(contrib), ndr, byrow = TRUE)), na.rm = TRUE))
      cs <- causes[ci]
      if (cs %in% names(paf_c)) { paf_c[cs] <- paf_c[cs] + pafz
        paf_ch[cs] <- paf_ch[cs] + pafz_h
        ztot <- ztot + pafz * wts[acause == cs, deaths] }
    }
    zone_contrib[[as.character(z)]] <- ztot
  }
  cmp <- wts[, .(acause, deaths = round(deaths),
                 prod_paf = round(deaths_nonopt / deaths, 4),
                 prod_paf_heat = round(deaths_heat / deaths, 4))]
  cmp[, `:=`(repl_paf = round(paf_c[acause], 4), repl_paf_heat = round(paf_ch[acause], 4))]
  cmp[, ratio := round(repl_paf / prod_paf, 2)]
  print(cmp[order(-abs(deaths * prod_paf))][1:10])
  zc <- data.table(zone = as.integer(names(zone_contrib)),
                   burden = round(unlist(zone_contrib)))
  cat("replica attributable deaths by zone:\n"); print(zc[order(zone)])
  cat("replica total:", round(sum(unlist(zone_contrib))),
      "| production:", round(sum(wts$deaths_nonopt)), "\n")
}