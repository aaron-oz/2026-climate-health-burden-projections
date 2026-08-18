# Follow-ups to the revision measurement. The residual ours-side gap vs the
# paper era concentrates in tropical countries, pointing at damped daily
# temperature variance in CMIP6. Three local tests:
#   1. Within-pixel daily SD, ERA5 vs CMIP6, for the ERA5-2022 countries;
#      correlate the SD bias with the ours/paper burden ratio.
#   2. Haiti uncertainty: does the paper-era value sit inside our production
#      draw CI? Plus a 100-draw TMREL-only band from the replica.
#   3. Variance-inflation sensitivity: scale daily anomalies by k around the
#      pixel mean and see what k would reproduce the paper-era burden
#      (Haiti and Indonesia).
suppressPackageStartupMessages({library(ncdf4); library(data.table); library(sf)})
sf_use_s2(FALSE)

C   <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "/var/home/aoz/code/2026-climate-health-burden-projections/output/review-ssp245"
REV <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245"
meta <- fread(file.path(OUT, "location_meta.csv"))
M    <- fread(file.path(OUT, "revision_all204.csv"))

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
shp <- st_read("data/shapefiles/GBD2023_mapping_final_augmented.shp", quiet = TRUE)
grid_dt <- readRDS(file.path(OUT, "pixel_loc_map.rds"))
f1 <- read_tas(file.path(C, "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245",
  "timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
tas_c <- read_tas(file.path(C, "cmip6-daily-x0.25/tas/canesm5-r1i1p1f1-ssp245",
  "timeseries-tas-daily-mean_cmip6-daily-x0.25_canesm5-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
nc <- nc_open(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245",
  "climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc"))
pp <- ncvar_get(nc, names(nc$var)[1]); pp[is.na(pp)] <- 0; nc_close(nc)

# ---- 1. daily SD comparison ----
era_ids <- c(125, 126, 130, 161, 16, 163, 185, 196, 86)
res <- list()
for (id in era_ids) {
  ids_dt <- grid_dt[loc_id == id]
  ids_dt[, pop := pp[cbind(ilon, ilat)]]; ids_dt <- ids_dt[pop > 0]
  e <- tryCatch(read_tas(sprintf("/var/home/aoz/data/era5_raw/%d_2022_t2m.nc", id)),
                error = function(err) {cat("loc", id, "ERA5 read failed:", conditionMessage(err), "\n"); NULL})
  if (is.null(e)) next
  nh <- dim(e$arr)[3]; ndays <- nh %/% 24
  daily_e <- array(e$arr[, , 1:(ndays * 24)], c(dim(e$arr)[1], dim(e$arr)[2], 24, ndays))
  daily_e <- apply(daily_e, c(1, 2, 4), mean)
  gd <- CJ(i = seq_along(e$lon), j = seq_along(e$lat))
  pts <- st_as_sf(data.frame(lon = e$lon[gd$i], lat = e$lat[gd$j]),
                  coords = c("lon", "lat"), crs = st_crs(shp))
  inside <- lengths(st_intersects(pts, shp[shp$loc_id == id, ])) > 0
  gd <- gd[inside]
  if (nrow(gd) == 0) {cat("loc", id, ": no ERA5 pixels inside polygon (lon range ",
      paste(round(range(e$lon), 1), collapse=" to "), ")\n"); next}
  gd[, `:=`(plon = pmin(pmax(findInterval(e$lon[i], f1$lon + 0.125), 1), length(f1$lon)),
            plat = pmin(pmax(findInterval(e$lat[j], f1$lat + 0.125), 1), length(f1$lat)))]
  gd[, pop := pp[cbind(plon, plat)]]
  gd <- gd[pop > 0]
  if (nrow(gd) == 0) {cat("loc", id, ": no populated ERA5 pixels after pop match\n"); next}
  sd_e <- apply(gd[, cbind(i, j)], 1, function(ij) sd(daily_e[ij[1], ij[2], ]))
  sd_a <- apply(ids_dt[, cbind(ilon, ilat)], 1, function(ij) sd(f1$arr[ij[1], ij[2], ]))
  sd_c <- apply(ids_dt[, cbind(ilon, ilat)], 1, function(ij) sd(tas_c$arr[ij[1], ij[2], ]))
  res[[length(res) + 1]] <- data.table(location_id = id,
    era5_sd = weighted.mean(sd_e, gd$pop),
    acc_sd = weighted.mean(sd_a, ids_dt$pop), can_sd = weighted.mean(sd_c, ids_dt$pop))
}
V <- merge(rbindlist(res), meta[, .(location_id, location_name)], by = "location_id")
V <- merge(V, M[, .(location_id, ours_vs_paper)], by = "location_id")
V[, `:=`(acc_ratio = acc_sd / era5_sd, can_ratio = can_sd / era5_sd)]
fwrite(V, file.path(OUT, "daily_sd_era5_vs_cmip6.csv"))
cat("\n== pop-weighted within-pixel daily SD (deg C), 2022 ==\n")
print(V[order(acc_ratio), .(location_name, era5_sd = round(era5_sd, 2),
      acc_sd = round(acc_sd, 2), can_sd = round(can_sd, 2),
      acc_ratio = round(acc_ratio, 2), can_ratio = round(can_ratio, 2),
      ours_vs_paper = round(ours_vs_paper, 2))])
cat("corr(SD ratio, ours/paper ratio):",
    round(cor(V$acc_ratio, V$ours_vs_paper, method = "spearman"), 3), "(access-cm2)\n")

# ---- 2. Haiti uncertainty ----
ens <- fread(file.path(OUT, "ensemble_by_location_year.csv"))[year == 2022 & location_id == 114]
h <- M[location_id == 114]
cat("\n== Haiti ==\n")
cat("paper-era 2019:", round(h$val_nonopt), "[", round(h$lower_nonopt), ",",
    round(h$upper_nonopt), "]  | ours 2022 ensemble:", round(ens$nonopt),
    " draw CI [", round(ens$nonopt_lo_draw), ",", round(ens$nonopt_hi_draw), "]\n")

# ---- 3. variance-inflation sensitivity (replica machinery) ----
erf <- setDT(readRDS("data/erf/cache/erf_curves_summary.rds"))[, .(zone, daily_temp, acause, rr_mean)]
zlim <- erf[, .(tmin = min(daily_temp), tmax = max(daily_temp)), by = zone]
bc <- fread(file.path(REV, "by_cause.csv"))
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022 & location_id %in% c(114L, 11L)]

replica_burden <- function(id, k) {
  px <- grid_dt[loc_id == id]
  px[, pop := pp[cbind(ilon, ilat)]]; px <- px[pop > 0]
  daily <- t(apply(px[, cbind(ilon, ilat)], 1, function(ij) f1$arr[ij[1], ij[2], ]))
  pixmean <- rowMeans(daily)
  daily <- pixmean + k * (daily - pixmean)
  zone <- pmin(pmax(round(pixmean), 6), 28)
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
  d <- bc[location_id == id, .(acause, deaths)]
  merge(paf, d, by = "acause")[, sum(paf * deaths)]
}
cat("\n== variance-inflation sensitivity: nonopt burden vs anomaly scale k ==\n")
for (tgt in list(c(114L, "Haiti"), c(11L, "Indonesia"))) {
  id <- as.integer(tgt[1])
  b <- sapply(c(1, 1.25, 1.5, 1.75, 2), function(k) replica_burden(id, k))
  cat(tgt[2], ": k=1:", round(b[1]), " k=1.25:", round(b[2]), " k=1.5:", round(b[3]),
      " k=1.75:", round(b[4]), " k=2:", round(b[5]),
      " | paper-era target:", round(M[location_id == id, val_nonopt]), "\n")
}
