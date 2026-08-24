# 03_load_temperature.R — Load and prepare temperature data for a location
#
# Loads daily temperature data (ERA5 or equivalent) for a location.
# Assigns temperature zones based on annual mean temperature.
# Truncates temperatures to the modeled range per zone.
#
# Expected input format: CSV or RDS with columns:
#   pixel_id, date, daily_temp (°C), pop (pixel population)
#   Optional: temp_sd (for Monte Carlo draws, used when USE_DRAWS = TRUE)
#
# Input:  TEMP_DIR/{LOCATION_ID}_daily_temp.csv (or .rds)
# Output: INTERMEDIATE_DIR/temperature.rds

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))

library(data.table)

log_msg("Loading temperature data for location", LOCATION_ID)

# --- Load temperature data ---
# If TEMP_FILE is set (via --temp_file=... or in config), use it directly.
# Otherwise look for the canonical {LOCATION_ID}_daily_temp.{rds,csv} in TEMP_DIR.
override <- if (exists("TEMP_FILE", envir = globalenv())) get("TEMP_FILE", envir = globalenv()) else NULL
if (!is.null(override) && nzchar(as.character(override))) {
  override <- as.character(override)
  if (!file.exists(override)) stop(paste("TEMP_FILE override does not exist:", override))
  log_msg("Using TEMP_FILE override: ", override)
  temp <- if (grepl("\\.csv$", override, ignore.case = TRUE)) fread(override) else readRDS(override)
  setDT(temp)
} else {
  temp_file_rds <- file.path(TEMP_DIR, paste0(LOCATION_ID, "_daily_temp.rds"))
  temp_file_csv <- file.path(TEMP_DIR, paste0(LOCATION_ID, "_daily_temp.csv"))
  if (file.exists(temp_file_rds)) {
    temp <- readRDS(temp_file_rds)
    setDT(temp)
  } else if (file.exists(temp_file_csv)) {
    temp <- fread(temp_file_csv)
  } else {
    stop(paste("No temperature file found for location", LOCATION_ID,
               "- expected", temp_file_rds, "or", temp_file_csv))
  }
}

log_msg("Loaded", nrow(temp), "pixel-day rows")

# --- Standardize columns ---
# Expect: pixel_id, date, daily_temp, pop, and optionally temp_sd, subloc_id
required_cols <- c("pixel_id", "date", "daily_temp", "pop")
missing <- setdiff(required_cols, names(temp))
if (length(missing) > 0) {
  stop(paste("Temperature file missing required columns:", paste(missing, collapse = ", ")))
}

# subloc_id is the subnational dimension (department / admin1 / state). If
# absent in the input, treat the entire country as one subnational unit so
# the rest of the pipeline still works.
if (!"subloc_id" %in% names(temp)) {
  log_msg("No subloc_id column in temperature input — treating whole location as one subloc")
  temp[, subloc_id := as.character(LOCATION_ID)]
}

temp[, date := as.Date(date)]
temp[, year := as.integer(format(date, "%Y"))]

# Filter to study period
temp <- temp[year >= YEAR_START & year <= YEAR_END]

if (COLOMBIA_VERIFICATION) {
  # Item 9: Samuel uses year-varying WorldPop (WorldPop_2010_2019_pixel.csv)
  # rather than the constant per-pixel snapshot bundled in
  # temperatura_diaria_pixel.rds. Replace pop where WorldPop has a value;
  # keep the original (San Andrés / pixels missing from WorldPop) as fallback,
  # mirroring Samuel's treatment in 11_carga_atribuible.R:308-313.
  worldpop_file <- file.path(DATA_DIR, "columbia-data-for-verifying-pipeline",
                             "colombia", "WorldPop_2010_2019_pixel.csv")
  if (file.exists(worldpop_file)) {
    worldpop <- fread(worldpop_file)
    setnames(worldpop, c("indx_rg", "sum_z", "ano"), c("pixel_id", "pop_yr", "year"))
    worldpop <- worldpop[, .(pixel_id, year, pop_yr)]
    temp <- merge(temp, worldpop, by = c("pixel_id", "year"), all.x = TRUE)
    n_replaced <- sum(!is.na(temp$pop_yr))
    n_kept     <- sum(is.na(temp$pop_yr))
    temp[!is.na(pop_yr), pop := pop_yr]
    temp[, pop_yr := NULL]
    log_msg("COLOMBIA_VERIFICATION: replaced pop with year-varying WorldPop on ",
            n_replaced, " rows; ", n_kept, " kept the constant fallback")
  } else {
    warning("COLOMBIA_VERIFICATION: WorldPop file not found; using constant pop")
  }
}

# --- Assign temperature zones ---
# Zone = rounded annual mean temperature per pixel per year
temp[, zone := as.integer(round(mean(daily_temp, na.rm = TRUE))),
     by = .(year, pixel_id)]

# Truncate zones to the modeled range (6-28°C)
temp[zone < TEMP_ZONE_MIN, zone := TEMP_ZONE_MIN]
temp[zone > TEMP_ZONE_MAX, zone := TEMP_ZONE_MAX]

# --- Convert daily temp to integer * 10 (matching ERF encoding) ---
temp[, daily_temp_10 := as.integer(round(daily_temp * 10))]

# --- Exposure uncertainty: convolve with the measured ERA5 spread ---
# Each pixel-day's population mass is spread over neighboring 0.1 C bins by a
# Gaussian kernel with the measured EDA-spread sd at that (pixel, month),
# BEFORE zone-range truncation (out-of-range mass then clamps to the grid
# edge, as a noisy draw would). This is the distribution-level equivalent of
# IHME era2melt.R's daily_temp + sd * N(0,1) exposure draws. The output is
# aggregated to (subloc, year, zone, daily_temp_10); 05 and 06 only consume
# that aggregation, so the pixel-day columns are not carried through.
if (!TEMP_NOISE_MODE %in% c("none", "era5_sd")) {
  stop("Unknown TEMP_NOISE_MODE '", TEMP_NOISE_MODE, "'")
}
if (TEMP_NOISE_MODE == "era5_sd") {
  if (COLOMBIA_VERIFICATION)
    stop("TEMP_NOISE_MODE = era5_sd is incompatible with COLOMBIA_VERIFICATION ",
         "(the daily branch needs pixel-day rows)")
  if (!file.exists(TEMP_SD_FILE))
    stop("TEMP_NOISE_MODE = era5_sd but TEMP_SD_FILE not found: ", TEMP_SD_FILE)
  suppressPackageStartupMessages(library(ncdf4))
  nc_sd <- nc_open(TEMP_SD_FILE)
  if (!TEMP_SD_VAR %in% names(nc_sd$var))
    stop("TEMP_SD_VAR '", TEMP_SD_VAR, "' not in ", TEMP_SD_FILE)
  dn <- sapply(nc_sd$var[[TEMP_SD_VAR]]$dim, function(d) d$name)
  sd_arr <- aperm(ncvar_get(nc_sd, TEMP_SD_VAR),
                  match(c("lon", "lat", "month"), dn))
  nc_close(nc_sd)
  # pixel_id = lat_idx * 1440 + lon_idx, 0-based (util_convert_cckp_temperature.R)
  CCKP_N_LON <- 1440L
  temp[, `:=`(lon_i = pixel_id %% CCKP_N_LON + 1L,
              lat_i = pixel_id %/% CCKP_N_LON + 1L,
              mon   = as.integer(format(date, "%m")))]
  temp[, s10 := as.integer(round(10 * sd_arr[cbind(lon_i, lat_i, mon)]))]
  n_na <- temp[is.na(s10), .N]
  if (n_na > 0) {
    log_msg("TEMP_NOISE era5_sd: ", n_na, " pixel-day rows have no sd value; ",
            "treated as sd = 0")
    temp[is.na(s10), s10 := 0L]
  }
  wq <- function(x, w, p) {
    o <- order(x); cw <- cumsum(w[o]) / sum(w)
    x[o][findInterval(p, cw) + 1L]
  }
  log_msg(sprintf(
    "TEMP_NOISE era5_sd (%s): pop-weighted mean sd %.2f C (p10 %.2f, p90 %.2f)",
    TEMP_SD_VAR, temp[, sum(pop * s10) / sum(pop) / 10],
    temp[, wq(s10, pop, 0.10)] / 10, temp[, wq(s10, pop, 0.90)] / 10))
  agg <- temp[, .(pop = sum(pop, na.rm = TRUE)),
              by = .(subloc_id, year, zone, s10, daily_temp_10)]
  conv <- rbindlist(lapply(split(agg, agg$s10), function(g) {
    s10 <- g$s10[1]
    if (s10 <= 0) return(g[, .(subloc_id, year, zone, daily_temp_10, pop)])
    K  <- max(10L, as.integer(ceiling(4 * s10)))
    ks <- seq(-K, K)
    kw <- dnorm(ks / 10, sd = s10 / 10); kw <- kw / sum(kw)
    g[, .(daily_temp_10 = daily_temp_10 + ks, pop = pop * kw),
      by = .(subloc_id, year, zone, orig = daily_temp_10)][
      , .(pop = sum(pop)), by = .(subloc_id, year, zone, daily_temp_10)]
  }))
  temp <- conv[, .(pop = sum(pop)), by = .(subloc_id, year, zone, daily_temp_10)]
  log_msg("TEMP_NOISE era5_sd: exposure convolved and aggregated to ",
          nrow(temp), " (subloc, year, zone, temp-bin) rows")
}

# --- Truncate daily temps to the modeled range within each zone ---
temp_limits <- readRDS(file.path(INTERMEDIATE_DIR, "temp_limits.rds"))
temp <- merge(temp, temp_limits, by = "zone", all.x = TRUE)
temp[daily_temp_10 < min_temp, daily_temp_10 := min_temp]
temp[daily_temp_10 > max_temp, daily_temp_10 := max_temp]
temp[, c("min_temp", "max_temp") := NULL]

# --- Compute population weights ---
# pr is the within-subloc population fraction: a pixel-day's pop divided by
# the total person-time in that subloc-year. Summing pr within (subloc, year)
# across all pixel-days = 1, so the PAF formula sum(pr * (RR-1)/RR) within a
# subloc-year is the standard within-population PAF.
# Also keep the country-year denominator for backwards-compatible diagnostics.
temp[, pop_total       := sum(pop, na.rm = TRUE), by = .(year)]
temp[, pop_subloc_total := sum(pop, na.rm = TRUE), by = .(year, subloc_id)]
temp[, pr := pop / pop_subloc_total]

# --- Temperature draws (if enabled and temp_sd available) ---
if (USE_DRAWS && "temp_sd" %in% names(temp)) {
  # UNMAINTAINED PATH, disabled 2026-08-20 pending the exposure-uncertainty
  # design decision: as written it drops subloc_id/date (05/06 would error on
  # the missing column), does not group pr by draw (populations would sum to
  # N_DRAWS across draws), and the rnorm is unseeded. No production input has
  # ever carried temp_sd (the CCKP converter emits none), so this has never
  # executed. Fail loudly rather than run broken.
  stop("temperature-draw path is unmaintained and disabled; input carries ",
       "temp_sd but this code needs repair before use (see 2026-08-19 audit)")
  log_msg("Generating", N_DRAWS, "temperature draws per pixel-day")

  # Create draws by adding Gaussian noise: draw_temp = daily_temp + sd * N(0,1)
  # Then collapse population by (zone, daily_temp_draw, draw)
  temp_draws <- rbindlist(lapply(0:(N_DRAWS - 1), function(d) {
    dt <- copy(temp)
    dt[, daily_temp_10 := as.integer(round((daily_temp + temp_sd * rnorm(.N)) * 10))]
    # Re-truncate after adding noise
    dt <- merge(dt, temp_limits, by = "zone", all.x = TRUE)
    dt[daily_temp_10 < min_temp, daily_temp_10 := min_temp]
    dt[daily_temp_10 > max_temp, daily_temp_10 := max_temp]
    dt[, c("min_temp", "max_temp") := NULL]
    dt[, draw := d]
    dt
  }))

  # Collapse to population by (zone, daily_temp, draw, year)
  temp_draws <- temp_draws[, .(pop = sum(pop, na.rm = TRUE)),
                           by = .(zone, daily_temp_10, draw, year)]
  temp_draws[, pr := pop / sum(pop, na.rm = TRUE), by = .(draw, year)]

  saveRDS(temp_draws, file.path(INTERMEDIATE_DIR, "temperature.rds"))
  log_msg("Temperature draws saved:", nrow(temp_draws), "rows")

} else {
  saveRDS(temp, file.path(INTERMEDIATE_DIR, "temperature.rds"))
  log_msg("Temperature data saved:", nrow(temp), "rows")
}

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS && TEMP_NOISE_MODE == "none") {
  # (skipped in noise mode: the convolved output has no pixel-day rows)
  library(ggplot2)
  log_msg("Generating temperature diagnostic plots")

  # Annual mean temperature distribution
  annual_means <- temp[, .(mean_temp = mean(daily_temp, na.rm = TRUE)), by = .(year, pixel_id)]
  p1 <- ggplot(annual_means, aes(x = mean_temp)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    facet_wrap(~year) +
    labs(x = "Annual mean temperature (°C)", y = "Pixel count",
         title = paste("Temperature zone distribution — Location", LOCATION_ID)) +
    theme_minimal()

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("temp_zones_loc", LOCATION_ID, ".png")),
         p1, width = 10, height = 8, dpi = 150)

  # Daily temperature time series (population-weighted)
  daily_pop_weighted <- temp[, .(temp_pw = weighted.mean(daily_temp, pop, na.rm = TRUE)),
                             by = date]
  p2 <- ggplot(daily_pop_weighted, aes(x = date, y = temp_pw)) +
    geom_line(color = "steelblue", linewidth = 0.3) +
    labs(x = "Date", y = "Pop-weighted daily mean temp (°C)",
         title = paste("Daily temperature — Location", LOCATION_ID)) +
    theme_minimal()

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("temp_daily_loc", LOCATION_ID, ".png")),
         p2, width = 12, height = 4, dpi = 150)

  log_msg("Temperature diagnostic plots saved")
}
