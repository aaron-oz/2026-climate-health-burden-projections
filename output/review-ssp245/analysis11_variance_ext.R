# Extended daily-SD comparison, CCKP (NEX-GDDP) vs ERA5, for every country
# with an ERA5 2022 extract in ~/data/era5_raw (including the batch fetched
# 2026-08-18 to cover the tropical low cluster). Includes the na.rm fix that
# restores Mexico and Italy.
suppressPackageStartupMessages({library(ncdf4); library(data.table); library(sf)})
sf_use_s2(FALSE)

C   <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
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

files <- list.files("/var/home/aoz/data/era5_raw", pattern = "_2022_t2m\\.nc$")
ids <- as.integer(sub("_.*", "", files))
res <- list()
for (id in ids) {
  ids_dt <- grid_dt[loc_id == id]
  ids_dt[, pop := pp[cbind(ilon, ilat)]]; ids_dt <- ids_dt[pop > 0]
  if (nrow(ids_dt) == 0) next
  e <- tryCatch(read_tas(sprintf("/var/home/aoz/data/era5_raw/%d_2022_t2m.nc", id)),
                error = function(err) NULL)
  if (is.null(e) || dim(e$arr)[3] < 8760) {cat("loc", id, "skipped (incomplete file)\n"); next}
  nh <- dim(e$arr)[3]; ndays <- nh %/% 24
  de <- array(e$arr[, , 1:(ndays * 24)], c(dim(e$arr)[1], dim(e$arr)[2], 24, ndays))
  de <- apply(de, c(1, 2, 4), mean)
  gd <- CJ(i = seq_along(e$lon), j = seq_along(e$lat))
  pts <- st_as_sf(data.frame(lon = e$lon[gd$i], lat = e$lat[gd$j]),
                  coords = c("lon", "lat"), crs = st_crs(shp))
  inside <- lengths(st_intersects(pts, shp[shp$loc_id == id, ])) > 0
  gd <- gd[inside]
  if (nrow(gd) == 0) {cat("loc", id, ": no ERA5 pixels inside polygon\n"); next}
  gd[, `:=`(plon = pmin(pmax(findInterval(e$lon[i], f1$lon + 0.125), 1), length(f1$lon)),
            plat = pmin(pmax(findInterval(e$lat[j], f1$lat + 0.125), 1), length(f1$lat)))]
  gd[, pop := pp[cbind(plon, plat)]]
  gd <- gd[pop > 0]
  if (nrow(gd) == 0) next
  sd_e <- apply(gd[, cbind(i, j)], 1, function(ij) sd(de[ij[1], ij[2], ], na.rm = TRUE))
  sd_a <- apply(ids_dt[, cbind(ilon, ilat)], 1, function(ij) sd(f1$arr[ij[1], ij[2], ], na.rm = TRUE))
  sd_c <- apply(ids_dt[, cbind(ilon, ilat)], 1, function(ij) sd(tas_c$arr[ij[1], ij[2], ], na.rm = TRUE))
  keep_a <- !is.na(sd_a); keep_c <- !is.na(sd_c)
  res[[length(res) + 1]] <- data.table(location_id = id,
    era5_sd = weighted.mean(sd_e, gd$pop, na.rm = TRUE),
    acc_sd = weighted.mean(sd_a[keep_a], ids_dt$pop[keep_a]),
    can_sd = weighted.mean(sd_c[keep_c], ids_dt$pop[keep_c]))
  cat("done", id, "\n")
}
V <- merge(rbindlist(res), meta[, .(location_id, location_name)], by = "location_id")
V <- merge(V, M[, .(location_id, ours_vs_paper, in_paper_ui)], by = "location_id")
V[, `:=`(acc_ratio = acc_sd / era5_sd, can_ratio = can_sd / era5_sd)]
fwrite(V, file.path(OUT, "daily_sd_era5_vs_cmip6_ext.csv"))
cat("\n== pop-weighted within-pixel daily SD (deg C), 2022, all ERA5-covered countries ==\n")
print(V[order(acc_ratio), .(location_name, era5_sd = round(era5_sd, 2),
      acc_sd = round(acc_sd, 2), can_sd = round(can_sd, 2),
      acc_ratio = round(acc_ratio, 2), can_ratio = round(can_ratio, 2),
      vs_gbd2019 = round(ours_vs_paper, 2), in_ui = in_paper_ui)])
low <- V[in_paper_ui == FALSE & ours_vs_paper < 1]
cat("\nSpearman corr(SD ratio, burden ratio) across all:",
    round(cor(V$acc_ratio, V$ours_vs_paper, method = "spearman", use = "complete.obs"), 3), "\n")
cat("low-cluster members covered:", nrow(low), "| their SD ratios (access-cm2):",
    paste(low[order(acc_ratio), paste0(location_name, " ", round(acc_ratio, 2))], collapse = "; "), "\n")