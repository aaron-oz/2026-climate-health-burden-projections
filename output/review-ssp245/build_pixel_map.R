# Builds output/review-ssp245/pixel_loc_map.rds: CCKP 0.25-degree grid cell
# -> national location_id, used by every replica analysis script.
#
# CRITICAL: the GBD shapefile contains 1,042 polygons: 204 national plus 838
# subnational. Assignment MUST be restricted to the 204 national polygons;
# an unfiltered loop lets subnational polygons overwrite national ids and
# strips countries like China to a handful of border-sliver pixels (bug
# found and fixed 2026-08-20; see docs/reviews/ssp245-review-findings.org).
suppressPackageStartupMessages({library(sf); library(ncdf4); library(data.table)})
sf_use_s2(FALSE)
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
OUT <- "output/review-ssp245"

meta <- fread(file.path(OUT, "location_meta.csv"))   # the 204 national ids
shp <- st_read("data/shapefiles/GBD2023_mapping_final_augmented.shp", quiet = TRUE)
shp <- shp[shp$loc_id %in% meta$location_id, ]
stopifnot(nrow(shp) == 204)

nc <- nc_open(file.path("/var/home/aoz/data/wb-temp-attr-projections/cckp-test",
  "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245",
  "timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
dn <- names(nc$dim)
lon <- ncvar_get(nc, grep("lon", dn, value = TRUE)[1])
lat <- ncvar_get(nc, grep("lat", dn, value = TRUE)[1])
nc_close(nc)
if (any(lon > 180)) lon <- ifelse(lon > 180, lon - 360, lon)

grid_dt <- CJ(ilon = seq_along(lon), ilat = seq_along(lat))
pts <- st_as_sf(data.frame(lon = lon[grid_dt$ilon], lat = lat[grid_dt$ilat]),
                coords = c("lon", "lat"), crs = st_crs(shp))
hits <- st_intersects(shp, pts)
grid_dt[, loc_id := NA_integer_]
for (i in seq_along(hits)) if (length(hits[[i]])) grid_dt$loc_id[hits[[i]]] <- shp$loc_id[i]
saveRDS(grid_dt, file.path(OUT, "pixel_loc_map.rds"))
n <- grid_dt[!is.na(loc_id), .N, by = loc_id]
cat("assigned", sum(n$N), "pixels; sanity: China", n[loc_id == 6, N],
    "(expect ~15,224), US", n[loc_id == 102, N], "(expect ~17,546)\n")