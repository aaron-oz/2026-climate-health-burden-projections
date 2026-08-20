# Local test of re-referencing the CCKP (NEX-GDDP, GMFD-referenced) daily
# temps to ERA5, for countries with local ERA5 2022. Three exposure variants
# through the replica burden: raw CCKP; mean-shifted to ERA5; full empirical
# quantile mapping to ERA5 (per pixel, 2022-on-2022). Compare against the
# paper-era (ERA5-exposure) burden values.
suppressPackageStartupMessages({library(ncdf4); library(data.table); library(sf)})
sf_use_s2(FALSE)

C   <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"
REV <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245"
TARGETS <- data.table(loc = c(185L, 16L, 126L, 125L),
                      name = c("Rwanda", "Philippines", "Costa Rica", "Colombia"))

read_tas <- function(path) {
  nc <- nc_open(path); on.exit(nc_close(nc))
  v <- names(nc$var)[which.max(sapply(nc$var, function(x) prod(x$size)))]
  arr <- ncvar_get(nc, v)
  dn <- sapply(nc$var[[v]]$dim, function(d) d$name)
  ilon <- grep("lon", dn); ilat <- grep("lat", dn)
  itime <- setdiff(seq_along(dn), c(ilon, ilat))
  if (!identical(c(ilon, ilat, itime), seq_along(dn))) arr <- aperm(arr, c(ilon, ilat, itime))
  lon <- nc$var[[v]]$dim[[ilon]]$vals; lat <- nc$var[[v]]$dim[[ilat]]$vals
  if (any(lon > 180)) lon <- ifelse(lon > 180, lon - 360, lon)
  if (max(arr, na.rm = TRUE) > 200) arr <- arr - 273.15
  list(arr = arr, lon = lon, lat = lat)
}
grid_dt <- readRDS(file.path(OUT, "pixel_loc_map.rds"))
f1 <- read_tas(file.path(C, "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245",
  "timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
nc <- nc_open(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245",
  "climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc"))
pp <- ncvar_get(nc, names(nc$var)[1]); pp[is.na(pp)] <- 0; nc_close(nc)

erf <- setDT(readRDS("data/erf/cache/erf_curves_summary.rds"))[, .(zone, daily_temp, acause, rr_mean)]
zlim <- erf[, .(tmin = min(daily_temp), tmax = max(daily_temp)), by = zone]
bc <- fread(file.path(REV, "by_cause.csv"))
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022 & location_id %in% TARGETS$loc]
M <- fread(file.path(OUT, "revision_all204.csv"))

burden_from_daily <- function(id, daily, px) {
  zone <- pmin(pmax(round(rowMeans(daily)), 6), 28)
  tm <- fread(sprintf("data/tmrel/tmrel_%d_summaries.csv", id))[year_id == 2020]
  tmrel10 <- setNames(round(tm$tmrelMean * 10), tm$meanTempCat)
  nd <- ncol(daily)
  long <- data.table(pix = rep(seq_len(nrow(px)), nd),
                     t10 = as.integer(round(10 * as.vector(daily))),
                     w = rep(px$pop, nd))
  long[, zone := zone[pix]]
  long <- merge(long, zlim, by = "zone")
  long[, t10c := pmin(pmax(t10, tmin), tmax)]
  long[, w := w / sum(w)]
  long[, tmrel := tmrel10[as.character(zone)]]
  agg <- long[, .(pr = sum(w)), by = .(zone, t10c, t10, tmrel)]
  cs <- erf[agg, on = c(zone = "zone", daily_temp = "t10c"), allow.cartesian = TRUE]
  ref <- erf[unique(agg[, .(zone, tmrel)]), on = c(zone = "zone", daily_temp = "tmrel")]
  cs <- merge(cs, ref[, .(zone, acause, rr_ref = rr_mean)], by = c("zone", "acause"))
  cs <- cs[t10 != tmrel]
  cs[, rel := rr_mean / rr_ref]
  paf <- cs[, .(paf = sum(fifelse(rel >= 1, pr * (rel - 1) / rel, pr * (rel - 1)))),
            by = acause]
  merge(paf, bc[location_id == id, .(acause, deaths)], by = "acause")[, sum(paf * deaths)]
}

for (k in seq_len(nrow(TARGETS))) {
  id <- TARGETS$loc[k]; nm <- TARGETS$name[k]
  px <- grid_dt[loc_id == id]
  px[, pop := pp[cbind(ilon, ilat)]]; px <- px[pop > 0]
  daily <- t(apply(px[, cbind(ilon, ilat)], 1, function(ij) f1$arr[ij[1], ij[2], ]))
  e <- read_tas(sprintf("/var/home/aoz/data/era5_raw/%d_2022_t2m.nc", id))
  nh <- dim(e$arr)[3]; ndays <- min(nh %/% 24, ncol(daily))
  de <- array(e$arr[, , 1:(ndays * 24)], c(dim(e$arr)[1], dim(e$arr)[2], 24, ndays))
  de <- apply(de, c(1, 2, 4), mean)
  # nearest ERA5 pixel for each CCKP pixel
  ei <- sapply(f1$lon[px$ilon], function(x) which.min(abs(e$lon - x)))
  ej <- sapply(f1$lat[px$ilat], function(x) which.min(abs(e$lat - x)))
  era_daily <- t(sapply(seq_len(nrow(px)), function(r) de[ei[r], ej[r], ]))
  dailyc <- daily[, seq_len(ndays), drop = FALSE]
  # variants
  ms <- dailyc - rowMeans(dailyc) + rowMeans(era_daily)
  qm <- t(sapply(seq_len(nrow(px)), function(r)
    sort(era_daily[r, ])[rank(dailyc[r, ], ties.method = "first")]))
  b_raw <- burden_from_daily(id, dailyc, px)
  b_ms  <- burden_from_daily(id, ms, px)
  b_qm  <- burden_from_daily(id, qm, px)
  tgt <- M[location_id == id]
  cat(sprintf("%-12s raw %6.0f | mean-shift %6.0f | full QM->ERA5 %6.0f | paper %5.0f [%5.0f, %5.0f]\n",
      nm, b_raw, b_ms, b_qm, tgt$val_nonopt, tgt$lower_nonopt, tgt$upper_nonopt))
}
