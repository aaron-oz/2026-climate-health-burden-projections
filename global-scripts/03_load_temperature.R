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

source("config.R")

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

if (RUN_DIAGNOSTICS) {
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
