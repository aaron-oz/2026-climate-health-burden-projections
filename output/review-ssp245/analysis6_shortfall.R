# Separating the drivers of the hot-belt shortfall vs GBD 2022:
#   A. GBD-round revision, measured directly on the 9 Burkart countries
#      (GBD-2023-round 2019 values vs the 2021 paper's 2019 values).
#   B. Within-zone truncation exposure: share of person-days above the
#      pixel's zone max daily-mean temperature (zone 28 max = 32.1 C),
#      global, from CCKP daily tas (access-cm2 + canesm5, 2022 + 2050).
#   C. CMIP6-vs-ERA5 temperature bias for the 9 countries with local ERA5
#      2022 (India and Bangladesh are the shortfall-relevant ones).
suppressPackageStartupMessages({library(ncdf4); library(data.table); library(sf)})
sf_use_s2(FALSE)

C   <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "/var/home/aoz/code/2026-climate-health-burden-projections/output/review-ssp245"
meta <- fread(file.path(OUT, "location_meta.csv"))

# ---------- A. GBD-round revision, 9 Burkart countries ----------
bm <- fread("data/benchmarks/burkart2021_table1.csv")[year == 2019]
g  <- fread("data/gbd-comparison/gbd_A_anchor_2019_2022.csv")
g  <- g[year == 2019 & metric_name == "Number" & measure_name == "Deaths"]
g[, risk := fcase(rei_name == "High temperature", "high",
                  rei_name == "Low temperature", "low",
                  rei_name == "Non-optimal temperature", "nonopt")]
rev <- merge(bm[, .(location_id, location_name, risk, paper = deaths)],
             g[, .(location_id, risk, round23 = val)],
             by = c("location_id", "risk"))
rev[, revision := round23 / paper]
cat("== A. GBD-round 2019 / Burkart-paper 2019 (revision factor) ==\n")
print(dcast(rev, location_name ~ risk, value.var = "revision")[
        , .(location_name, nonopt = round(nonopt, 2), high = round(high, 2),
            low = round(low, 2))])
fwrite(rev, file.path(OUT, "gbd_round_revision_9countries.csv"))

# ---------- shared machinery ----------
tl <- readRDS("output/intermediate/125/temp_limits.rds")
zmax <- tl$max_temp / 10; names(zmax) <- tl$zone   # deg C by zone

read_tas <- function(path) {
  nc <- nc_open(path); on.exit(nc_close(nc))
  # the data variable is the largest one (CCKP names it
  # "timeseries-tas-daily-mean", ERA5 names it "t2m")
  v <- names(nc$var)[which.max(sapply(nc$var, function(x) prod(x$size)))]
  arr <- ncvar_get(nc, v)
  dn <- sapply(nc$var[[v]]$dim, function(d) d$name)
  ilon <- grep("lon", dn); ilat <- grep("lat", dn)
  itime <- setdiff(seq_along(dn), c(ilon, ilat))
  if (!identical(c(ilon, ilat, itime), seq_along(dn))) arr <- aperm(arr, c(ilon, ilat, itime))
  lon <- nc$var[[v]]$dim[[ilon]]$vals; lat <- nc$var[[v]]$dim[[ilat]]$vals
  if (any(lon > 180)) lon <- ifelse(lon > 180, lon - 360, lon)   # 0-360 -> -180-180
  if (max(arr, na.rm = TRUE) > 200) arr <- arr - 273.15   # Kelvin -> C
  list(arr = arr, lon = lon, lat = lat)
}
pop_grid <- function(path) { nc <- nc_open(path); on.exit(nc_close(nc))
  p <- ncvar_get(nc, names(nc$var)[1]); p[is.na(p)] <- 0; p }

# pixel -> location map on the CCKP grid (cache to RDS for reuse)
map_path <- file.path(OUT, "pixel_loc_map.rds")
shp <- st_read("data/shapefiles/GBD2023_mapping_final_augmented.shp", quiet = TRUE)
f1 <- read_tas(file.path(C, "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245",
  "timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
if (file.exists(map_path)) { grid_dt <- readRDS(map_path) } else {
  grid_dt <- CJ(ilon = seq_along(f1$lon), ilat = seq_along(f1$lat))
  pts <- st_as_sf(data.frame(lon = f1$lon[grid_dt$ilon], lat = f1$lat[grid_dt$ilat]),
                  coords = c("lon", "lat"), crs = st_crs(shp))
  hits <- st_intersects(shp, pts)
  grid_dt[, loc_id := NA_integer_]
  for (i in seq_along(hits)) if (length(hits[[i]])) grid_dt$loc_id[hits[[i]]] <- shp$loc_id[i]
  saveRDS(grid_dt, map_path)
}

# truncation share for one global daily array + pop grid
trunc_stats <- function(arr, pp, grid_dt) {
  am <- apply(arr, c(1, 2), mean)
  zone <- pmin(pmax(round(am), 6), 28)
  thr <- matrix(zmax[as.character(zone)], nrow = nrow(zone))
  nd <- dim(arr)[3]
  over_days <- matrix(0L, nrow = dim(arr)[1], ncol = dim(arr)[2])
  exceed_sum <- matrix(0, nrow = dim(arr)[1], ncol = dim(arr)[2])
  for (d in seq_len(nd)) {
    e <- arr[, , d] - thr
    hit <- !is.na(e) & e > 0
    over_days <- over_days + hit
    exceed_sum <- exceed_sum + ifelse(hit, e, 0)
  }
  dt <- copy(grid_dt)
  dt[, `:=`(pop = pp[cbind(ilon, ilat)], over = over_days[cbind(ilon, ilat)],
            exc = exceed_sum[cbind(ilon, ilat)], nd = nd)]
  dt[!is.na(loc_id) & pop > 0]
}

runs <- CJ(model = c("access-cm2-r1i1p1f1", "canesm5-r1i1p1f1"), year = c(2022L, 2050L))
loc_res <- list()
for (r in seq_len(nrow(runs))) {
  tas <- if (r == 1) f1 else read_tas(file.path(C, "cmip6-daily-x0.25/tas",
    paste0(runs$model[r], "-ssp245"),
    sprintf("timeseries-tas-daily-mean_cmip6-daily-x0.25_%s-ssp245_timeseries_mean_%d.nc",
            runs$model[r], runs$year[r])))
  pp <- pop_grid(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245",
    sprintf("climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_%s.nc",
            fifelse(runs$year[r] == 2022L, "2020-2039", "2040-2059"))))
  s <- trunc_stats(tas$arr, pp, grid_dt)
  loc_res[[r]] <- s[, .(model = runs$model[r], year = runs$year[r],
      trunc_share = sum(pop * over / nd) / sum(pop),
      mean_exceed = sum(exc) / max(sum(over), 1)), by = loc_id]
  rm(tas); gc()
}
L <- rbindlist(loc_res)
cat("\n== B. person-day share above the zone max (truncated exposure) ==\n")
Lp <- merge(L, meta[, .(loc_id = location_id, location_name)], by = "loc_id")
fwrite(Lp, file.path(OUT, "truncation_by_location.csv"))
gc2 <- fread(file.path(OUT, "gbd2022_comparison.csv"))
top <- merge(Lp[model == "access-cm2-r1i1p1f1" & year == 2022],
             gc2[, .(loc_id = location_id, gbd_ratio = ratio, gbd_nonopt = val_d_nonopt)],
             by = "loc_id")
cat("access-cm2 2022, top 15 truncation shares:\n")
print(top[order(-trunc_share)][1:15, .(location_name, trunc_pct = round(100 * trunc_share, 1),
      mean_exceed_C = round(mean_exceed, 2), gbd_ratio = round(gbd_ratio, 2))])
cat("\ncorr(truncation share, ours/GBD ratio), GBD nonopt > 100:",
    top[gbd_nonopt > 100, round(cor(trunc_share, gbd_ratio, method = "spearman"), 3)], "\n")
cat("2050 growth (canesm5), top 5:\n")
print(Lp[model == "canesm5-r1i1p1f1" & year == 2050][order(-trunc_share)][1:5,
      .(location_name, trunc_pct = round(100 * trunc_share, 1))])

# ---------- C. ERA5 vs CMIP6, countries with local ERA5 2022 ----------
era_ids <- c(125, 126, 130, 161, 16, 163, 185, 196, 86)
res_c <- list()
pp22 <- pop_grid(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245",
  "climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc"))
cmip_daily_loc <- function(tas, ids_dt) {
  # per-pixel daily matrix for one country's pixels
  px <- ids_dt[, cbind(ilon, ilat)]
  t(apply(px, 1, function(ij) tas$arr[ij[1], ij[2], ]))
}
metrics <- function(daily, pop) {  # daily: pixels x days
  pm <- rowMeans(daily, na.rm = TRUE)
  p95 <- apply(daily, 1, quantile, 0.95, na.rm = TRUE)
  over <- rowMeans(daily > 32.1, na.rm = TRUE)
  c(mean = weighted.mean(pm, pop), p95 = weighted.mean(p95, pop),
    over32 = weighted.mean(over, pop))
}
tas_a <- f1
tas_c <- read_tas(file.path(C, "cmip6-daily-x0.25/tas/canesm5-r1i1p1f1-ssp245",
  "timeseries-tas-daily-mean_cmip6-daily-x0.25_canesm5-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
for (id in era_ids) {
  ids_dt <- grid_dt[loc_id == id]
  ids_dt[, pop := pp22[cbind(ilon, ilat)]]
  ids_dt <- ids_dt[pop > 0]
  if (nrow(ids_dt) == 0) next
  e <- read_tas(sprintf("/var/home/aoz/data/era5_raw/%d_2022_t2m.nc", id))
  nh <- dim(e$arr)[3]; ndays <- nh %/% 24
  dim3 <- dim(e$arr)
  daily_e <- array(e$arr[, , 1:(ndays * 24)], c(dim3[1], dim3[2], 24, ndays))
  daily_e <- apply(daily_e, c(1, 2, 4), mean)
  # country mask + nearest pop on the ERA5 grid
  gd <- CJ(i = seq_along(e$lon), j = seq_along(e$lat))
  pts <- st_as_sf(data.frame(lon = e$lon[gd$i], lat = e$lat[gd$j]),
                  coords = c("lon", "lat"), crs = st_crs(shp))
  inside <- lengths(st_intersects(pts, shp[shp$loc_id == id, ])) > 0
  gd <- gd[inside]
  # nearest CCKP pop cell
  gd[, `:=`(plon = findInterval(e$lon[i], f1$lon + 0.125) , plat = findInterval(e$lat[j], f1$lat + 0.125))]
  gd[, `:=`(plon = pmin(pmax(plon, 1), length(f1$lon)), plat = pmin(pmax(plat, 1), length(f1$lat)))]
  gd[, pop := pp22[cbind(plon, plat)]]
  gd <- gd[pop > 0]
  if (nrow(gd) == 0) next
  de <- t(apply(gd[, cbind(i, j)], 1, function(ij) daily_e[ij[1], ij[2], ]))
  me <- metrics(de, gd$pop)
  ma <- metrics(cmip_daily_loc(tas_a, ids_dt), ids_dt$pop)
  mc <- metrics(cmip_daily_loc(tas_c, ids_dt), ids_dt$pop)
  res_c[[length(res_c) + 1]] <- data.table(loc_id = id,
    era5_mean = me["mean"], era5_p95 = me["p95"], era5_over32 = me["over32"],
    acc_mean = ma["mean"], acc_p95 = ma["p95"], acc_over32 = ma["over32"],
    can_mean = mc["mean"], can_p95 = mc["p95"], can_over32 = mc["over32"])
}
CC <- merge(rbindlist(res_c), meta[, .(loc_id = location_id, location_name)], by = "loc_id")
fwrite(CC, file.path(OUT, "era5_vs_cmip6_2022.csv"))
cat("\n== C. ERA5 2022 vs CMIP6 2022, pop-weighted (deg C; over32 = share of days > 32.1 C) ==\n")
print(CC[, .(location_name,
   era5_mean = round(era5_mean, 1), acc_bias = round(acc_mean - era5_mean, 2),
   can_bias = round(can_mean - era5_mean, 2),
   era5_p95 = round(era5_p95, 1), acc_p95_bias = round(acc_p95 - era5_p95, 2),
   can_p95_bias = round(can_p95 - era5_p95, 2),
   era5_ov32 = round(100 * era5_over32, 1), acc_ov32 = round(100 * acc_over32, 1),
   can_ov32 = round(100 * can_over32, 1))])

# ---------- D. burden ratio = mortality-input ratio x PAF ratio ----------
f <- fread("data/gbd-comparison/gbd_F_denominator_2019_2022.csv")
f <- f[year == 2022 & metric_name == "Number", .(gbd_d17 = sum(val)), by = location_id]
ens22 <- fread(file.path(OUT, "ensemble_by_location_year.csv"))[year == 2022]
gc2b <- fread(file.path(OUT, "gbd2022_comparison.csv"))
D <- merge(merge(f, ens22[, .(location_id, ours_d17 = deaths_total, ours_nonopt = nonopt)],
                 by = "location_id"),
           gc2b[, .(location_id, location_name, burden_ratio = ratio,
                    gbd_nonopt = val_d_nonopt)], by = "location_id")
D[, `:=`(mort_ratio = ours_d17 / gbd_d17,
         paf_ratio = (ours_nonopt / ours_d17) / (gbd_nonopt / gbd_d17))]
fwrite(D, file.path(OUT, "shortfall_decomposition.csv"))
cat("\n== D. global 17-cause deaths ratio:", round(sum(D$ours_d17)/sum(D$gbd_d17), 3), "\n")
