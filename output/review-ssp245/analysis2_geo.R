# Geography helpers for the ssp245 review: location names, centroid latitude,
# and 0.25-degree grid-centre counts per polygon (sub-grid location check).
suppressPackageStartupMessages({library(sf); library(data.table)})
sf_use_s2(FALSE)

OUT <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"
shp <- st_read(file.path("data/shapefiles/GBD2023_mapping_final_augmented.shp"), quiet = TRUE)
shp <- shp[shp$level == 3 | is.na(shp$level), ]  # keep nations if level col exists
if (!"loc_id" %in% names(shp)) stop("no loc_id col")

cent <- suppressWarnings(st_coordinates(st_centroid(st_geometry(shp))))
meta <- data.table(location_id = shp$loc_id, location_name = shp$loc_name,
                   lat = cent[, 2], lon = cent[, 1])

# Grid-centre counts under the two plausible 0.25-degree alignments.
count_cells <- function(offset) {
  lons <- seq(-180 + offset, 180 - offset, by = 0.25)
  lats <- seq(-90 + offset, 90 - offset, by = 0.25)
  pts <- st_as_sf(expand.grid(lon = lons, lat = lats),
                  coords = c("lon", "lat"), crs = st_crs(shp))
  hits <- st_intersects(shp, pts)
  lengths(hits)
}
meta[, cells_off0    := count_cells(0)]
meta[, cells_off125  := count_cells(0.125)]
cat("alignment offset 0.00: empty =", sum(meta$cells_off0 == 0),
    " <=1 =", sum(meta$cells_off0 <= 1), "\n")
cat("alignment offset 0.125: empty =", sum(meta$cells_off125 == 0),
    " <=1 =", sum(meta$cells_off125 <= 1), "\n")

fwrite(meta, file.path(OUT, "location_meta.csv"))
cat("wrote", nrow(meta), "rows to location_meta.csv\n")
