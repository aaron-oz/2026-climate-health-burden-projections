# util_augment_shapefile.R — Build GBD2023_mapping_final_augmented.shp by adding
# the 6 micro-nations that the GBD mapping shapefile omits entirely (they exist
# in the GBD mortality hierarchy but have no geometry): Maldives (14), Marshall
# Islands (24), Monaco (367), Nauru (369), Tokelau (413), Tuvalu (416).
#
# Their geometries were extracted once from Natural Earth 10m (public domain;
# Tokelau from the map-units layer) and committed as
# global-scripts/micronation_shapes.geojson. This script merges them into the
# GBD shapefile's schema and writes the augmented copy alongside the original.
#
# The augmented file is a STRICT SUPERSET: identical for the ~198 existing
# locations, plus these 6 rows. config.R::DEFAULT_SHAPEFILE auto-prefers it when
# present, so once built the pipeline picks it up with no flag. These 6 are still
# sub-grid at 0.25 deg, so they resolve via the nearest-pixel fallback in
# util_convert_cckp_temperature.R -- the polygon just gives that fallback a
# location to snap from.
#
# Idempotent: run it once per machine (the augmented .shp lives under the
# gitignored data/ tree). Re-running rebuilds from the original + geojson.
#
# Usage:
#   Rscript global-scripts/util_augment_shapefile.R

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages({ library(sf); library(data.table) })

orig_shp  <- file.path(SHAPEFILE_DIR, "GBD2023_mapping_final.shp")
aug_shp   <- file.path(SHAPEFILE_DIR, "GBD2023_mapping_final_augmented.shp")
micro_geo <- file.path(SCRIPTS_DIR, "micronation_shapes.geojson")

if (!file.exists(orig_shp))  stop("Original GBD shapefile not found: ", orig_shp)
if (!file.exists(micro_geo)) stop("Micronation geometries not found: ", micro_geo)

gbd   <- st_read(orig_shp,  quiet = TRUE)
micro <- st_read(micro_geo, quiet = TRUE)
micro <- st_transform(micro, st_crs(gbd))

# Drop any micro loc_ids already present in the GBD file (idempotent / no dupes).
micro <- micro[!micro$loc_id %in% gbd$loc_id, ]
if (nrow(micro) == 0) {
  log_msg("All micronation loc_ids already in the shapefile; nothing to add.")
} else {
  log_msg("Adding ", nrow(micro), " micro-nations: ",
          paste(sort(micro$loc_id), collapse = ", "))
}

# Build the new rows with EXACTLY the GBD attribute schema. Start from a 0-row
# copy of the GBD attribute table (preserves every column's type), expand to N
# rows of NA, then fill the fields we know.
geom_col <- attr(gbd, "sf_column")
proto    <- st_drop_geometry(gbd)[0, , drop = FALSE]
n        <- nrow(micro)
newdf    <- proto[rep(NA_integer_, n), , drop = FALSE]
rownames(newdf) <- NULL

coerce_like <- function(x, template) {
  if (is.integer(template)) as.integer(x)
  else if (is.numeric(template)) as.numeric(x)
  else as.character(x)
}
newdf$loc_id     <- coerce_like(micro$loc_id,     gbd$loc_id)
newdf$level      <- coerce_like(micro$level,      gbd$level)
if ("parent_id" %in% names(newdf)) newdf$parent_id <- coerce_like(micro$parent_id, gbd$parent_id)
if ("loc_name"  %in% names(newdf)) newdf$loc_name  <- as.character(micro$loc_name)
if ("loc_nm_sh" %in% names(newdf)) newdf$loc_nm_sh <- as.character(micro$loc_name)
if ("ihme_lc_id" %in% names(newdf)) newdf$ihme_lc_id <- as.character(micro$ihme_lc_id)

newsf <- st_sf(newdf, geometry = st_geometry(micro), crs = st_crs(gbd))
newsf <- newsf[, names(gbd)]                     # exact column order incl geometry
aug   <- rbind(gbd, newsf)

st_write(aug, aug_shp, delete_dsn = TRUE, quiet = TRUE)
log_msg("Wrote augmented shapefile (", nrow(aug), " features) -> ", aug_shp)
log_msg("Level-3 count: ", sum(aug$level == 3, na.rm = TRUE),
        " (was ", sum(gbd$level == 3, na.rm = TRUE), ")")
