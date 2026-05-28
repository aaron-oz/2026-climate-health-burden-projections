# util_convert_cckp_temperature.R — Convert CCKP CMIP6 daily NetCDF + GPW pop to
# pipeline format
#
# CCKP S3 layout (https://wbg-cckp.s3.amazonaws.com/data/):
#   cmip6-daily-x0.25/{var}/{model-scenario}/timeseries-{var}-daily-mean_...nc
#       one file per (model, scenario, year); 1440 lon x 721 lat at 0.25°,
#       lat -90..90 south-to-north, lon -180..179.75, variable name matches
#       filename prefix ("timeseries-tas-daily-mean"), units degC (NOT Kelvin),
#       missing value 1e+20, calendar varies by model.
#   pop-x0.25/popcount/{pop_dataset}-{scenario}/
#       climatology-popcount-annual-mean_..._{year_start}-{year_end}.nc
#       same 0.25° grid as cmip6, units "persons" per pixel.
#
# Outputs the pipeline schema expected by 03_load_temperature.R:
#   data/temperature/{LOCATION_ID}_daily_temp.rds with columns
#   pixel_id, date, daily_temp, pop, subloc_id
#
# Pixel-id scheme: pixel_id = lat_idx * 1440L + lon_idx
#   stable across files / models / years as long as the 0.25° grid is fixed.
#
# Usage (script):
#   Rscript util_convert_cckp_temperature.R \
#     --temp_nc=data/cmip6-scratch/tas_daily_access-cm2_hist_2010.nc \
#     --pop_nc=data/cmip6-scratch/popcount_ssp245_2020-2039.nc \
#     --location_id=125 \
#     --shapefile=data/colombia/divipola_dept.shp
#
# Usage (function): see convert_cckp() below.

source("config.R")

suppressPackageStartupMessages({
  library(data.table)
  library(ncdf4)
  library(sf)
})

# Some GBD shapefile polygons have invalid loops under spherical geometry
# (e.g., Brazilian state boundaries trip s2 with duplicate-vertex errors).
# Planar geometry is fine for the bbox/intersect work we do here.
sf_use_s2(FALSE)

# =============================================================================
# Constants for the CCKP 0.25° grid
# =============================================================================
CCKP_N_LON <- 1440L
CCKP_N_LAT <- 721L
CCKP_MISSING <- 1e+20

# =============================================================================
# Calendar handling — CMIP6 models use one of:
#   "gregorian" / "standard" / "proleptic_gregorian": real calendar w/ leap years
#   "365_day" / "noleap":                              365 days, no Feb 29
#   "360_day":                                          12 x 30 days
# CCKP files declare this in time:calendar. The annual climatology file we
# probed reports "gregorian"; daily files from individual models may differ.
# Verified: TODO once path A (full daily file) lands.
# =============================================================================

read_cckp_time <- function(nc) {
  time_vals  <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  time_cal   <- ncatt_get(nc, "time", "calendar")$value
  if (is.null(time_cal) || nchar(time_cal) == 0) time_cal <- "gregorian"

  # Parse "days since YYYY-MM-DD ..." origin
  m <- regmatches(time_units,
                  regexpr("\\d{4}-\\d{2}-\\d{2}", time_units))
  if (length(m) == 0) {
    stop("Could not parse time:units = '", time_units, "'")
  }
  origin <- as.Date(m)

  # Build dates. For gregorian/standard, plain date arithmetic works; for
  # noleap / 360_day, we need to skip Feb 29 / wrap at day-of-year 360.
  cal <- tolower(time_cal)
  if (cal %in% c("gregorian", "standard", "proleptic_gregorian")) {
    dates <- origin + time_vals
  } else if (cal %in% c("365_day", "noleap")) {
    # Each day count maps to (year, doy) with year length 365
    yr_origin <- as.integer(format(origin, "%Y"))
    doy_origin <- as.integer(format(origin, "%j"))
    total_doy <- doy_origin + as.integer(time_vals)
    yr_offset <- (total_doy - 1L) %/% 365L
    doy <- ((total_doy - 1L) %% 365L) + 1L
    years <- yr_origin + yr_offset
    # Skip Feb 29: in a noleap calendar, doy 60 = Mar 1 (gregorian non-leap)
    dates <- as.Date(paste0(years, "-01-01")) + (doy - 1L)
    # Realign for leap years: in gregorian, Mar 1 is doy 61, but we want it
    # to be doy 60 in noleap. We bump dates >= Mar 1 in leap years by +1.
    is_leap <- (years %% 4 == 0 & years %% 100 != 0) | (years %% 400 == 0)
    bump <- is_leap & doy >= 60L
    dates[bump] <- dates[bump] + 1L
  } else if (cal == "360_day") {
    # 12 x 30 day calendar. Approximate by mapping doy 1..360 to actual
    # gregorian dates Jan 1 .. Dec 26 (linear scale). Good enough for
    # within-year exposure aggregation; downstream gets dates that sort
    # correctly and span Jan..Dec.
    yr_origin <- as.integer(format(origin, "%Y"))
    doy_origin <- as.integer(format(origin, "%j"))
    total_doy <- doy_origin + as.integer(time_vals)
    yr_offset <- (total_doy - 1L) %/% 360L
    doy <- ((total_doy - 1L) %% 360L) + 1L
    years <- yr_origin + yr_offset
    # 360-day to gregorian: scale doy by 365/360
    gdoy <- pmin(365L, as.integer(round((doy - 1L) * 365 / 360)) + 1L)
    dates <- as.Date(paste0(years, "-01-01")) + (gdoy - 1L)
  } else {
    stop("Unsupported calendar: '", time_cal, "'")
  }
  list(dates = dates, calendar = time_cal)
}

# =============================================================================
# Convert one (temp_nc, pop_nc, location) triple to pipeline format
# =============================================================================

convert_cckp <- function(temp_nc_path,
                         pop_nc_path,
                         location_id,
                         shapefile_path,
                         shapefile_subloc_field = NULL,
                         output_path = NULL) {

  log_msg("Opening temp NetCDF: ", basename(temp_nc_path))
  nc_t <- nc_open(temp_nc_path)
  on.exit(nc_close(nc_t), add = TRUE)

  # Find the temperature variable (matches filename prefix)
  var_names <- names(nc_t$var)
  var_t <- var_names[grepl("tas|temperature", var_names, ignore.case = TRUE)][1]
  if (is.na(var_t)) stop("No tas/temperature variable found in ", temp_nc_path)
  log_msg("Temperature variable: ", var_t)

  lat_all <- ncvar_get(nc_t, "lat")
  lon_all <- ncvar_get(nc_t, "lon")

  # --- Location bbox ---
  shp <- st_read(shapefile_path, quiet = TRUE)
  # Find this location's geometry. Heuristic: if shapefile has loc_id, match it;
  # else assume the whole file is for this location and use union.
  if ("loc_id" %in% names(shp)) {
    loc_geom <- shp[shp$loc_id == location_id, ]
    if (nrow(loc_geom) == 0) {
      # Try children at the next level (admin-1 features under this parent)
      loc_geom <- shp[!is.na(shp$parent_id) & shp$parent_id == location_id, ]
    }
    if (nrow(loc_geom) == 0) stop("No features for location_id=", location_id,
                                  " in ", shapefile_path)
  } else {
    loc_geom <- shp
  }
  bbox <- st_bbox(loc_geom)
  log_msg("Location bbox: lon[", round(bbox["xmin"], 2), ", ",
          round(bbox["xmax"], 2), "] lat[", round(bbox["ymin"], 2),
          ", ", round(bbox["ymax"], 2), "]")

  # --- Subset grid to bbox (add 1-cell buffer) ---
  buf <- 0.25
  lon_keep <- which(lon_all >= bbox["xmin"] - buf & lon_all <= bbox["xmax"] + buf)
  lat_keep <- which(lat_all >= bbox["ymin"] - buf & lat_all <= bbox["ymax"] + buf)
  if (length(lon_keep) == 0 || length(lat_keep) == 0) {
    stop("Empty subset for location ", location_id)
  }

  # ncvar_get with start/count to avoid loading the full global grid
  start <- c(min(lon_keep), min(lat_keep), 1L)
  count <- c(length(lon_keep), length(lat_keep), -1L)
  tas <- ncvar_get(nc_t, var_t, start = start, count = count,
                   collapse_degen = FALSE)
  log_msg("Subset temp dim: ", paste(dim(tas), collapse = " x "),
          " (lon x lat x time)")

  # --- Time axis ---
  tinfo <- read_cckp_time(nc_t)
  n_t_arr <- if (length(dim(tas)) >= 3) dim(tas)[3] else 1L
  if (length(tinfo$dates) != n_t_arr) {
    stop("Time-axis length (", length(tinfo$dates),
         ") does not match temp time dim (", n_t_arr, ")")
  }
  log_msg("Calendar: ", tinfo$calendar, " | dates: ",
          tinfo$dates[1], " to ", tinfo$dates[length(tinfo$dates)])

  # --- Pop NetCDF (annual or climatology snapshot) ---
  log_msg("Opening pop NetCDF: ", basename(pop_nc_path))
  nc_p <- nc_open(pop_nc_path)
  on.exit(nc_close(nc_p), add = TRUE)
  var_p <- names(nc_p$var)[grepl("pop", names(nc_p$var), ignore.case = TRUE)][1]
  if (is.na(var_p)) stop("No popcount variable found in ", pop_nc_path)
  pop_grid <- ncvar_get(nc_p, var_p, start = c(min(lon_keep), min(lat_keep), 1L),
                       count = c(length(lon_keep), length(lat_keep), 1L))
  # Pop should be 2D (lon x lat) after slicing time. Squeeze.
  if (length(dim(pop_grid)) == 3L && dim(pop_grid)[3] == 1L) {
    pop_grid <- pop_grid[, , 1]
  }

  # --- Build long pixel-day data.table ---
  # Pixel-id encoding: global lat_idx * CCKP_N_LON + lon_idx so it's stable
  # across model files / years without depending on the bbox subset.
  lon_sub <- lon_all[lon_keep]
  lat_sub <- lat_all[lat_keep]
  global_lon_idx <- lon_keep - 1L  # 0-based for arithmetic
  global_lat_idx <- lat_keep - 1L

  # Long form: one row per (lon_sub, lat_sub, date)
  n_lon <- length(lon_sub); n_lat <- length(lat_sub); n_t <- length(tinfo$dates)
  log_msg("Building long-form table: ", n_lon, " x ", n_lat, " x ", n_t,
          " = ", format(as.numeric(n_lon) * n_lat * n_t, big.mark = ","), " rows")

  dt <- data.table(
    lon_idx = rep(rep(global_lon_idx, times = n_lat), times = n_t),
    lat_idx = rep(rep(global_lat_idx, each  = n_lon), times = n_t),
    lon     = rep(rep(lon_sub,        times = n_lat), times = n_t),
    lat     = rep(rep(lat_sub,        each  = n_lon), times = n_t),
    date    = rep(tinfo$dates, each = n_lon * n_lat),
    daily_temp = as.numeric(tas)
  )
  pop_dt <- data.table(
    lon_idx = rep(global_lon_idx, times = n_lat),
    lat_idx = rep(global_lat_idx, each  = n_lon),
    pop     = as.numeric(pop_grid)
  )
  dt <- merge(dt, pop_dt, by = c("lon_idx", "lat_idx"), all.x = TRUE)

  # Drop fill values + ocean
  dt[daily_temp >= CCKP_MISSING / 2, daily_temp := NA_real_]
  dt[pop        >= CCKP_MISSING / 2, pop        := NA_real_]
  dt <- dt[!is.na(daily_temp)]
  dt[is.na(pop), pop := 0]

  # Pixel-id
  dt[, pixel_id := lat_idx * CCKP_N_LON + lon_idx]

  # --- Point-in-polygon for subloc_id ---
  # If admin-1 features exist under this location, tag each unique pixel by
  # which feature contains its centroid. Else fall back to LOCATION_ID.
  unique_pixels <- unique(dt[, .(pixel_id, lon, lat)])
  log_msg("Unique pixels in location bbox: ", nrow(unique_pixels))

  has_admin1 <- "loc_id" %in% names(shp) &&
    any(!is.na(shp$parent_id) & shp$parent_id == location_id)
  if (has_admin1) {
    admin1 <- shp[!is.na(shp$parent_id) & shp$parent_id == location_id, ]
    pts <- st_as_sf(unique_pixels, coords = c("lon", "lat"), crs = st_crs(admin1))
    hits <- st_intersects(pts, admin1)
    first_hit <- vapply(hits, function(x) if (length(x) > 0) x[1] else NA_integer_,
                       integer(1))
    unique_pixels[, subloc_id := as.character(admin1$loc_id[first_hit])]
    log_msg("Tagged ", sum(!is.na(unique_pixels$subloc_id)), " of ",
            nrow(unique_pixels), " pixels with admin-1 subloc_id")
  } else if (!is.null(shapefile_subloc_field) && shapefile_subloc_field %in% names(shp)) {
    # Country-specific admin-1 shapefile (e.g., DIVIPOLA for Colombia) — use
    # all features in the shapefile, tag by `shapefile_subloc_field`.
    pts <- st_as_sf(unique_pixels, coords = c("lon", "lat"), crs = st_crs(shp))
    hits <- st_intersects(pts, shp)
    first_hit <- vapply(hits, function(x) if (length(x) > 0) x[1] else NA_integer_,
                       integer(1))
    unique_pixels[, subloc_id := as.character(shp[[shapefile_subloc_field]][first_hit])]
    log_msg("Tagged subloc_id from field '", shapefile_subloc_field, "'; ",
            sum(!is.na(unique_pixels$subloc_id)), " of ",
            nrow(unique_pixels), " pixels matched")
  } else {
    unique_pixels[, subloc_id := as.character(location_id)]
    log_msg("No admin-1 features available; subloc_id := LOCATION_ID")
  }

  # Drop pixels with no subloc match (outside the country polygon — bbox catches
  # some ocean / neighbor-country cells).
  unique_pixels <- unique_pixels[!is.na(subloc_id)]
  dt <- merge(dt[, .(pixel_id, date, daily_temp, pop)],
              unique_pixels[, .(pixel_id, subloc_id)],
              by = "pixel_id")

  log_msg("Final pixel-day rows: ", format(nrow(dt), big.mark = ","))

  if (is.null(output_path)) {
    output_path <- file.path(TEMP_DIR, paste0(location_id, "_daily_temp.rds"))
  }
  saveRDS(dt, output_path)
  log_msg("Saved -> ", output_path)
  invisible(dt)
}

# =============================================================================
# CLI entrypoint
# =============================================================================

if (sys.nframe() == 0L) {
  # parse_args() in config.R writes overrides into globalenv()
  defaults <- list(
    TEMP_NC = NULL,
    POP_NC  = NULL,
    SHAPEFILE = file.path(SHAPEFILE_DIR, "GBD2023_mapping_final.shp"),
    SHAPEFILE_SUBLOC_FIELD = NULL,
    OUTPUT_PATH = NULL
  )
  for (k in names(defaults)) {
    if (!exists(k, envir = globalenv())) {
      assign(k, defaults[[k]], envir = globalenv())
    }
  }
  if (is.null(TEMP_NC) || is.null(POP_NC)) {
    stop("Required: --temp_nc=... --pop_nc=... [--location_id=...] ",
         "[--shapefile=...] [--shapefile_subloc_field=...] [--output_path=...]")
  }
  convert_cckp(temp_nc_path = TEMP_NC,
               pop_nc_path  = POP_NC,
               location_id  = LOCATION_ID,
               shapefile_path = SHAPEFILE,
               shapefile_subloc_field = SHAPEFILE_SUBLOC_FIELD,
               output_path = OUTPUT_PATH)
}
