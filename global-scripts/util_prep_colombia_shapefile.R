# util_prep_colombia_shapefile.R — Enrich GADM Colombia admin-1 with DIVIPOLA codes
#
# GADM 4.1 has Colombia admin-1 polygons (33 departments) but its CC_1 field
# is empty. Samuel's mortality / temperature data uses DIVIPOLA 2-digit codes
# (e.g. "05" for Antioquia). This script joins the two by normalizing the
# department name (uppercase, accent-stripped, punctuation-stripped) and
# writes an enriched shapefile that the CCKP adapter can use directly.
#
# Inputs:
#   data/shapefiles/gadm-colombia/gadm41_COL_1.shp
#   data/colombia/divipola.rds  (written by util_convert_samuel_colombia.R)
#
# Output:
#   data/shapefiles/colombia_divipola_admin1.shp (+ companion files)
#   has columns: NAME_1, divipola_code (chr, zero-padded "05" etc.), geometry

source("config.R")

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
})

GADM_PATH <- file.path(SHAPEFILE_DIR, "gadm-colombia", "gadm41_COL_1.shp")
DIVIPOLA_PATH <- file.path(DATA_DIR, "colombia", "divipola.rds")
# GeoPackage (.gpkg) rather than .shp — preserves long column names (the ESRI
# shapefile driver abbreviates `divipola_code` -> `dvpl_cd`).
OUT_PATH  <- file.path(SHAPEFILE_DIR, "colombia_divipola_admin1.gpkg")

normalize_name <- function(x) {
  x <- toupper(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")  # strip accents
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

log_msg("Loading GADM admin-1: ", GADM_PATH)
gadm <- st_read(GADM_PATH, quiet = TRUE)
gadm$name_norm <- normalize_name(gadm$NAME_1)

log_msg("Loading DIVIPOLA: ", DIVIPOLA_PATH)
divipola <- as.data.table(readRDS(DIVIPOLA_PATH))
setnames(divipola,
         c("Código Departamento", "Nombre Departamento"),
         c("divipola_code_int", "name_divipola"))
divipola[, name_norm := normalize_name(name_divipola)]
divipola[, divipola_code := sprintf("%02d", divipola_code_int)]

log_msg("GADM names: ", nrow(gadm), " | DIVIPOLA names: ", nrow(divipola))

# Manual synonym map — long-form DIVIPOLA names that GADM abbreviates
synonym_map <- c(
  "SAN ANDRES Y PROVIDENCIA" =
    "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA"
)
gadm$name_norm <- ifelse(gadm$name_norm %in% names(synonym_map),
                         synonym_map[gadm$name_norm],
                         gadm$name_norm)

merged <- merge(as.data.table(gadm)[, .(NAME_1, name_norm)],
                divipola[, .(name_norm, divipola_code, name_divipola)],
                by = "name_norm", all.x = TRUE)

unmatched <- merged[is.na(divipola_code)]
if (nrow(unmatched) > 0) {
  log_msg("Unmatched GADM departments (",  nrow(unmatched), "):")
  print(unmatched[, .(NAME_1, name_norm)])
  log_msg("DIVIPOLA names not in GADM:")
  print(divipola[!name_norm %in% merged$name_norm, .(name_divipola, name_norm)])
  stop("Resolve name mismatches above before re-running")
}
log_msg("All 33 GADM departments matched to DIVIPOLA codes")

# Attach divipola_code back to the sf object via NAME_1 (unique key)
gadm$divipola_code <- merged$divipola_code[match(gadm$NAME_1, merged$NAME_1)]
out <- gadm[, c("NAME_1", "divipola_code", "geometry")]

st_write(out, OUT_PATH, delete_layer = TRUE, quiet = TRUE)
log_msg("Wrote ", OUT_PATH, " (", nrow(out), " features)")
