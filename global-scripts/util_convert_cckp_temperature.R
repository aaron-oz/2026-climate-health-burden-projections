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

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))

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

  # Parse "days since YYYY-M-D ..." origin. Month/day may be 1 OR 2 digits --
  # CF time-units are not required to zero-pad (e.g. CMIP6 files carrying
  # "days since 1850-1-1"), so accept \d{1,2} and normalise before as.Date,
  # which is strict about padding on some platforms.
  m <- regmatches(time_units,
                  regexpr("\\d{4}-\\d{1,2}-\\d{1,2}", time_units))
  if (length(m) == 0) {
    stop("Could not parse time:units = '", time_units, "'")
  }
  ymd    <- as.integer(strsplit(m, "-", fixed = TRUE)[[1]])
  origin <- as.Date(sprintf("%04d-%02d-%02d", ymd[1], ymd[2], ymd[3]))

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
                         output_path = NULL,
                         subnational = TRUE) {

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

  # --- Pixel grid over the bbox ---
  # Pixel-id encoding: global lat_idx * CCKP_N_LON + lon_idx so it's stable
  # across model files / years without depending on the bbox subset.
  lon_sub <- lon_all[lon_keep]
  lat_sub <- lat_all[lat_keep]
  global_lon_idx <- lon_keep - 1L  # 0-based for arithmetic
  global_lat_idx <- lat_keep - 1L

  n_lon <- length(lon_sub); n_lat <- length(lat_sub); n_t <- length(tinfo$dates)

  # One row per bbox CELL (not per cell-day). This is the small table: the
  # expensive one is built later, over kept cells only.
  pix <- data.table(
    lon_i   = rep(seq_len(n_lon), times = n_lat),
    lat_i   = rep(seq_len(n_lat), each  = n_lon),
    lon_idx = rep(global_lon_idx, times = n_lat),
    lat_idx = rep(global_lat_idx, each  = n_lon),
    lon     = rep(lon_sub,        times = n_lat),
    lat     = rep(lat_sub,        each  = n_lon),
    pop     = as.numeric(pop_grid)
  )
  pix[, pixel_id := lat_idx * CCKP_N_LON + lon_idx]
  pix[pop >= CCKP_MISSING / 2, pop := NA_real_]

  pix[is.na(pop), pop := 0]

  # --- Point-in-polygon for subloc_id ---
  # Done BEFORE the pixel-day table is built, not after.
  #
  # A location's bbox is mostly not the location. The US bbox spans 304,644
  # cells of which 17,547 (5.8%) are inside the country, because the Aleutians
  # straddle the dateline and Hawaii and Point Barrow stretch the latitudes.
  # Building the table over the bbox and filtering afterwards therefore
  # materialised 111 million rows to keep 6.4 million, roughly 4.4 GB of
  # transient allocation for a 256 MB result. Tagging first and expanding only
  # the cells that survive gives the same output from a 17x smaller table.
  #
  # Narrowing the NetCDF read itself would not help: the CCKP files are chunked
  # (1, 721, 1440), one whole global field per day, so any spatial window still
  # decompresses the entire globe for every day touched. The saving here is
  # memory, not I/O.
  unique_pixels <- pix
  log_msg("Pixels in location bbox: ", nrow(unique_pixels))

  # Subnational tagging is gated on `subnational`. When FALSE (national mode),
  # both subnational branches are skipped and every pixel inside the national
  # polygon is tagged with LOCATION_ID -- the resolution that matches national
  # mortality (e.g. IHME forecasts). See config.R::SUBNATIONAL.
  has_admin1 <- subnational && "loc_id" %in% names(shp) &&
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
  } else if (subnational && !is.null(shapefile_subloc_field) &&
             shapefile_subloc_field %in% names(shp)) {
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
    # National resolution: tag every pixel that falls inside the national
    # polygon with LOCATION_ID. Still point-in-polygon (against loc_geom) so the
    # rectangular bbox's ocean / neighbour-country cells are dropped, the same
    # filtering the subnational branches apply.
    pts <- st_as_sf(unique_pixels, coords = c("lon", "lat"), crs = st_crs(loc_geom))
    hits <- st_intersects(pts, st_union(loc_geom))
    inside <- lengths(hits) > 0
    # Sub-grid geography fallback: a micro-state (Bermuda, San Marino, several
    # small islands) can contain no 0.25-deg pixel centre, so nothing falls inside
    # the polygon and the location would otherwise produce empty output. Snap to
    # the single nearest bbox pixel (by centre distance -- pure arithmetic, no
    # s2/PROJ dependency) so it still gets a nearest-cell climate series. Only
    # triggers when nothing is inside, so normal countries are unaffected.
    if (sum(inside) == 0 && nrow(unique_pixels) > 0) {
      ctr_lon <- mean(c(bbox["xmin"], bbox["xmax"]))
      ctr_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
      # Restrict candidates to cells that actually carry data. The previous
      # code derived its candidate set from the already-NA-filtered table, so
      # matching that matters for picking the same pixel. Only micro-states
      # reach this branch and their bboxes are a handful of cells, so the extra
      # pass over the array is cheap.
      has_data <- if (length(dim(tas)) >= 3L) {
        apply(tas < CCKP_MISSING / 2, c(1L, 2L),
              function(z) any(z, na.rm = TRUE))
      } else {
        tas < CCKP_MISSING / 2
      }
      d2 <- (unique_pixels$lon - ctr_lon)^2 + (unique_pixels$lat - ctr_lat)^2
      d2[!as.logical(has_data)] <- Inf
      nearest_idx <- which.min(d2)
      inside[nearest_idx] <- TRUE
      fb_pixel <- unique_pixels$pixel_id[nearest_idx]
      # Guarantee a positive pop weight for the lone pixel (its grid pop may be 0
      # over ocean); the magnitude is irrelevant since mortality supplies the
      # counts and this is the only pixel, so its within-location weight is 1.
      if (all(unique_pixels[pixel_id == fb_pixel, pop] == 0, na.rm = TRUE)) {
        unique_pixels[pixel_id == fb_pixel, pop := 1]
      }
      log_msg("  0 pixels inside national polygon (sub-grid geography, e.g. ",
              "micro-state); snapped to nearest bbox pixel ", fb_pixel,
              " as fallback")
    }
    unique_pixels[, subloc_id := NA_character_]
    unique_pixels[inside, subloc_id := as.character(location_id)]
    log_msg(if (!subnational) "subnational=FALSE: " else "No admin-1 features; ",
            "national resolution; tagged ", sum(inside), " of ",
            nrow(unique_pixels), " pixels inside the national polygon")
  }

  # Drop pixels with no subloc match (outside the country polygon — bbox catches
  # some ocean / neighbor-country cells).
  keep <- unique_pixels[!is.na(subloc_id)]
  setorder(keep, pixel_id)
  n_keep <- nrow(keep)
  log_msg("Expanding ", n_keep, " of ", nrow(unique_pixels), " bbox cells x ",
          n_t, " days = ", format(as.numeric(n_keep) * n_t, big.mark = ","),
          " rows (bbox-wide would have been ",
          format(as.numeric(n_lon) * n_lat * n_t, big.mark = ","), ")")

  if (n_keep == 0L) {
    dt <- data.table(pixel_id = integer(), date = as.Date(character()),
                     daily_temp = numeric(), pop = numeric(),
                     subloc_id = character())
  } else {
    # Pull just the kept cells out of the 3-D array by linear index, ordered
    # pixel-major then date. That reproduces the old merge()'s ordering (sorted
    # by pixel_id, dates ascending within a pixel) without ever materialising
    # the bbox-wide table. Indices are doubles so the arithmetic cannot overflow
    # integer range on a large bbox.
    slice   <- as.double(n_lon) * n_lat
    base    <- as.double(keep$lon_i) + (as.double(keep$lat_i) - 1) * n_lon
    offsets <- (seq_len(n_t) - 1) * slice
    lin     <- rep(base, each = n_t) + rep(offsets, times = n_keep)

    dt <- data.table(
      pixel_id   = rep(keep$pixel_id,  each = n_t),
      date       = rep(tinfo$dates,    times = n_keep),
      daily_temp = as.numeric(tas[lin]),
      pop        = rep(keep$pop,       each = n_t),
      subloc_id  = rep(keep$subloc_id, each = n_t))

    # Drop fill values + ocean. Population fill was already handled per cell.
    dt[daily_temp >= CCKP_MISSING / 2, daily_temp := NA_real_]
    dt <- dt[!is.na(daily_temp)]
  }
  setkeyv(dt, "pixel_id")   # merge() used to leave the result keyed this way

  log_msg("Final pixel-day rows: ", format(nrow(dt), big.mark = ","))

  if (is.null(output_path)) {
    output_path <- file.path(TEMP_DIR, paste0(location_id, "_daily_temp.rds"))
  }
  save_rds_atomic(dt, output_path)
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
    SHAPEFILE = DEFAULT_SHAPEFILE,
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
               output_path = OUTPUT_PATH,
               subnational = if (exists("SUBNATIONAL")) SUBNATIONAL else TRUE)
}
