# Zone-clamp quantification (agenda section 4): the pipeline assigns each
# pixel a temperature zone = round(annual mean daily temp) clamped to [6, 28]
# (config.R, 03_load_temperature.R). This script measures, from the global
# CCKP daily tas NetCDFs, what share of population lives in pixels whose
# rounded annual mean falls OUTSIDE [6, 28], i.e. where the clamp binds,
# for 2022 and 2050 under ssp245, for the models available locally.
# The second clamp aspect (daily temps truncated to the zone's modelled
# range) is NOT quantified here.
suppressPackageStartupMessages({library(ncdf4); library(data.table); library(sf)})
sf_use_s2(FALSE)

C   <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "/var/home/aoz/code/2026-climate-health-burden-projections/output/review-ssp245"

annual_mean <- function(path) {
  nc <- nc_open(path)
  on.exit(nc_close(nc))
  v <- names(nc$var)[1]
  arr <- ncvar_get(nc, v)                     # lon x lat x day
  lon <- ncvar_get(nc, grep("lon", names(nc$dim), value = TRUE)[1])
  lat <- ncvar_get(nc, grep("lat", names(nc$dim), value = TRUE)[1])
  m <- apply(arr, c(1, 2), mean)
  list(lon = lon, lat = lat, mean = m)
}

pop_grid <- function(path) {
  nc <- nc_open(path)
  on.exit(nc_close(nc))
  v <- names(nc$var)[1]
  p <- ncvar_get(nc, v)
  p[is.na(p)] <- 0
  p
}

runs <- data.table(
  model = rep(c("access-cm2-r1i1p1f1", "canesm5-r1i1p1f1"), each = 2),
  year  = rep(c(2022L, 2050L), 2))
runs[, tas := file.path(C, "cmip6-daily-x0.25/tas",
        paste0(model, "-ssp245"),
        sprintf("timeseries-tas-daily-mean_cmip6-daily-x0.25_%s-ssp245_timeseries_mean_%d.nc",
                model, year))]
runs[, pop := file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245",
        sprintf("climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_%s.nc",
                fifelse(year == 2022L, "2020-2039", "2040-2059")))]
stopifnot(file.exists(runs$tas), file.exists(runs$pop))

# Pixel -> location assignment, computed once (grid is identical across files).
first <- annual_mean(runs$tas[1])
shp <- st_read("data/shapefiles/GBD2023_mapping_final_augmented.shp", quiet = TRUE)
grid_dt <- CJ(ilon = seq_along(first$lon), ilat = seq_along(first$lat))
pts <- st_as_sf(data.frame(lon = first$lon[grid_dt$ilon], lat = first$lat[grid_dt$ilat]),
                coords = c("lon", "lat"), crs = st_crs(shp))
hits <- st_intersects(shp, pts)
grid_dt[, loc_id := NA_integer_]
for (i in seq_along(hits)) if (length(hits[[i]])) grid_dt$loc_id[hits[[i]]] <- shp$loc_id[i]
cat("pixels assigned to a location:", grid_dt[!is.na(loc_id), .N], "\n")

res_glob <- list(); res_loc <- list()
for (r in seq_len(nrow(runs))) {
  am <- if (r == 1) first else annual_mean(runs$tas[r])
  pp <- pop_grid(runs$pop[r])
  dt <- copy(grid_dt)
  dt[, `:=`(tmean = am$mean[cbind(ilon, ilat)], pop = pp[cbind(ilon, ilat)])]
  dt <- dt[!is.na(loc_id) & !is.na(tmean) & pop > 0]
  dt[, zone_raw := round(tmean)]
  dt[, clamped_hot := zone_raw > 28][, clamped_cold := zone_raw < 6]
  res_glob[[r]] <- data.table(model = runs$model[r], year = runs$year[r],
    pop_total = sum(dt$pop),
    hot_share  = sum(dt$pop * dt$clamped_hot)  / sum(dt$pop),
    cold_share = sum(dt$pop * dt$clamped_cold) / sum(dt$pop))
  res_loc[[r]] <- dt[, .(model = runs$model[r], year = runs$year[r],
    pop = sum(pop), hot_share = sum(pop * clamped_hot) / sum(pop)), by = loc_id]
}
G <- rbindlist(res_glob); L <- rbindlist(res_loc)
cat("\n== population share in clamp-binding zones (ssp245) ==\n")
print(G[, .(model, year, hot_pct = round(100 * hot_share, 2),
            cold_pct = round(100 * cold_share, 2))])

meta <- fread(file.path(OUT, "location_meta.csv"))
Lw <- dcast(L, loc_id ~ model + year, value.var = "hot_share")
setnames(Lw, c("loc_id", "acc22", "acc50", "can22", "can50"))
Lw <- merge(Lw, meta[, .(loc_id = location_id, location_name)], by = "loc_id")
gc <- fread(file.path(OUT, "gbd2022_comparison.csv"))
Lw <- merge(Lw, gc[, .(loc_id = location_id, gbd_ratio = ratio, gbd_nonopt = val_d_nonopt)],
            by = "loc_id", all.x = TRUE)
fwrite(Lw, file.path(OUT, "clamp_by_location.csv"))

cat("\n== locations with largest 2022 hot-clamp share (mean of the 2 models) ==\n")
Lw[, hot22 := (acc22 + can22) / 2][, hot50 := (acc50 + can50) / 2]
print(Lw[order(-hot22)][1:15, .(location_name, hot22 = round(hot22, 2),
      hot50 = round(hot50, 2), gbd_ratio = round(gbd_ratio, 2), gbd_nonopt = round(gbd_nonopt))])
cat("\ncorr(hot22 clamp share, ours/GBD ratio) over locations with GBD nonopt > 100:",
    with(Lw[gbd_nonopt > 100], round(cor(hot22, gbd_ratio, method = "spearman"), 3)), "\n")
cat("locations with hot22 > 0.5:", Lw[hot22 > 0.5, .N],
    " | hot50 > 0.5:", Lw[hot50 > 0.5, .N], "\n")
