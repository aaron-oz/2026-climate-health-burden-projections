# util_derive_tmrel.R — Derive annual TMRELs from cause-specific mortality + ERF curves
#
# Replicates the logic in Burkart's tmrelCalculator.R:
#   1. Load cause-specific death counts → convert to cause weights per year
#   2. Load ERF curves (exponentiated RR draws), constrain to TMREL search range
#   3. For each (zone, daily_temp, year, draw): compute death-weighted RR
#   4. TMREL = the daily_temp that minimizes the weighted RR for each (zone, year, draw)
#
# Key parameters matching Burkart:
#   - TMREL search range: 6.6°C to 34.6°C (tmrelCalculator_launch.R lines 16-17)
#   - Location-specific CoD weights (Burkart uses IHME CoDCorrect modeled estimates;
#     we use whatever mortality data is available for the location)
#   - Zones ≥ 6 only
#
# This allows computing TMRELs at annual resolution (not just decadal) and for
# projections beyond 2020 where pre-computed TMRELs are unavailable.
#
# Usage:
#   Rscript util_derive_tmrel.R                          # derive + validate for loc 125
#   Rscript util_derive_tmrel.R --location_id=125

source("config.R")

library(data.table)

# TMREL search range (from Burkart's tmrelCalculator_launch.R)
TMREL_SEARCH_MIN <- 6.6
TMREL_SEARCH_MAX <- 34.6

log_msg("Deriving TMRELs for location", LOCATION_ID)
log_msg("TMREL search range:", TMREL_SEARCH_MIN, "to", TMREL_SEARCH_MAX, "°C")

# =============================================================================
# 1. Load cause-specific mortality (annual deaths by cause)
# =============================================================================

mort_file_rds <- file.path(INTERMEDIATE_DIR, "mortality.rds")
mort_file_loc_rds <- file.path(MORTALITY_DIR, paste0(LOCATION_ID, "_mortality.rds"))
mort_file_loc_csv <- file.path(MORTALITY_DIR, paste0(LOCATION_ID, "_mortality.csv"))

if (file.exists(mort_file_rds)) {
  mort <- readRDS(mort_file_rds)
} else if (file.exists(mort_file_loc_rds)) {
  mort <- readRDS(mort_file_loc_rds)
} else if (file.exists(mort_file_loc_csv)) {
  mort <- fread(mort_file_loc_csv)
} else {
  stop("No mortality data found. Run 04_load_mortality.R first or provide the file.")
}

setDT(mort)
mort <- mort[acause %in% GBD_CAUSES]

# Aggregate to year × cause (total deaths across age/sex)
cod <- mort[, .(deaths = sum(deaths, na.rm = TRUE)), by = .(year_id, acause)]

# Convert deaths to weights (proportion of total cause-specific deaths per year)
# This matches Burkart's tmrelCalculator.R line 64:
#   cod[, paste0("weight_", 0:maxDraw) := lapply(.SD, function(x) {x/sum(x, na.rm=T)}), by=.(location_id, year_id)]
cod[, weight := deaths / sum(deaths, na.rm = TRUE), by = year_id]
log_msg("Mortality weights computed for", length(unique(cod$year_id)), "years,",
        length(unique(cod$acause)), "causes")

# =============================================================================
# 2. Load ERF curves
# =============================================================================

log_msg("Loading ERF curves from", ERF_DIR)
erf <- rbindlist(lapply(GBD_CAUSES, function(cause) {
  f <- file.path(ERF_DIR, paste0(cause, "_curve_samples.csv"))
  if (!file.exists(f)) {
    warning(paste("ERF file not found:", f))
    return(NULL)
  }
  dt <- fread(f)
  dt[, acause := cause]
  dt
}))

setnames(erf, "annual_temperature", "zone")
setnames(erf, "daily_temperature", "daily_temp")
erf[, zone := as.integer(zone)]

# Determine number of draws available
draw_cols <- grep("^draw_\\d+$", names(erf), value = TRUE)
n_draws <- length(draw_cols)
log_msg("ERF curves loaded:", nrow(erf), "rows,", n_draws, "draws")

# Constrain to TMREL search range and zones ≥ 6
# Matches Burkart's tmrelCalculator.R line 81:
#   rr <- rr[dailyTempCat>=tmrel_min & dailyTempCat<=tmrel_max & meanTempCat>=6, ]
n_before <- nrow(erf)
erf <- erf[daily_temp >= TMREL_SEARCH_MIN & daily_temp <= TMREL_SEARCH_MAX & zone >= 6]
log_msg("Constrained to TMREL search range:", n_before, "->", nrow(erf), "rows")

# Exponentiate (curves are in log-RR space)
# Matches Burkart's tmrelCalculator.R line 86:
#   rr[, paste0("rr_", 0:maxDraw) := lapply(.SD, exp), .SDcols = ...]
rr_cols <- paste0("rr_", 0:(n_draws - 1))
erf[, (rr_cols) := lapply(.SD, exp), .SDcols = draw_cols]
erf[, (draw_cols) := NULL]

erf[, daily_temp_10 := as.integer(round(daily_temp * 10))]

# =============================================================================
# 3. Compute death-weighted RR per (zone, daily_temp, year, draw)
# =============================================================================

log_msg("Computing death-weighted RR curves...")

# Merge cause weights with RR curves
# Matches Burkart's tmrelCalculator.R line 102:
#   master <- merge(cod, rr, by = "cause_id", all = TRUE, allow.cartesian = TRUE)
master <- merge(erf, cod[, .(year_id, acause, weight)],
                by = "acause", allow.cartesian = TRUE)

# For each (zone, daily_temp, year, draw): weighted_rr = sum(rr_d * weight) across causes
# Matches Burkart's tmrelCalculator.R line 114:
#   rrWt <- master[, lapply(0:maxDraw, function(x) {sum(get(paste0("rr_", x)) * get(paste0("weight_", x)))}), by=.(id)]
weighted_rr <- master[, lapply(rr_cols, function(col) sum(get(col) * weight)),
                      by = .(zone, daily_temp_10, year_id)]
setnames(weighted_rr, paste0("V", 1:n_draws), rr_cols)

log_msg("Death-weighted RR computed:", nrow(weighted_rr), "zone-temp-year combinations")

# =============================================================================
# 4. Find TMREL = daily_temp that minimizes weighted RR per (zone, year, draw)
# =============================================================================

log_msg("Finding TMREL (temperature at minimum weighted RR)...")

# For each (zone, year, draw), find the daily_temp with the lowest weighted RR
# Matches Burkart's tmrelCalculator.R line 126:
#   tmrel <- rrWt[, lapply(.SD, function(x) {sum(dailyTempCat*(x==min(x)))}), by=..., .SDcols=...]
# Note: Burkart uses sum(temp * (x==min(x))) which picks the temp at the minimum.
# We use which.min() which is equivalent when there's a unique minimum.
tmrel_draws <- weighted_rr[, lapply(rr_cols, function(col) {
  vals <- get(col)
  daily_temp_10[which.min(vals)] / 10  # convert back to °C
}), by = .(zone, year_id)]

setnames(tmrel_draws, paste0("V", 1:n_draws), paste0("tmrel_", 0:(n_draws - 1)))
tmrel_draws[, location_id := LOCATION_ID]

# Compute summaries
tmrel_cols <- paste0("tmrel_", 0:(n_draws - 1))
tmrel_draws[, tmrelMean  := rowMeans(.SD), .SDcols = tmrel_cols]
tmrel_draws[, tmrelLower := apply(.SD, 1, quantile, 0.025), .SDcols = tmrel_cols]
tmrel_draws[, tmrelUpper := apply(.SD, 1, quantile, 0.975), .SDcols = tmrel_cols]

log_msg("TMRELs derived for", length(unique(tmrel_draws$year_id)), "years,",
        length(unique(tmrel_draws$zone)), "zones")

# =============================================================================
# 5. Save derived TMRELs
# =============================================================================

derived_dir <- file.path(OUTPUT_DIR, "derived_tmrel")
dir.create(derived_dir, showWarnings = FALSE, recursive = TRUE)

# Save draws
draws_file <- file.path(derived_dir, paste0("tmrel_", LOCATION_ID, ".csv"))
fwrite(tmrel_draws[, c("location_id", "year_id", "zone", tmrel_cols), with = FALSE],
       draws_file)
log_msg("Draws saved:", draws_file)

# Save summaries
summary_dt <- tmrel_draws[, .(location_id, year_id, zone, tmrelMean, tmrelLower, tmrelUpper)]
summary_file <- file.path(derived_dir, paste0("tmrel_", LOCATION_ID, "_summaries.csv"))
fwrite(summary_dt, summary_file)
log_msg("Summaries saved:", summary_file)

# Print summary table
cat("\n=== Derived TMREL Summary (mean across draws) ===\n\n")
print_dt <- dcast(summary_dt, zone ~ year_id, value.var = "tmrelMean")
print(print_dt)

# =============================================================================
# 6. Validate against Burkart pre-computed TMRELs (if available)
# =============================================================================

burkart_file <- file.path(TMREL_DIR, paste0("tmrel_", LOCATION_ID, "_summaries.csv"))
if (file.exists(burkart_file)) {
  log_msg("\nValidating against Burkart pre-computed TMRELs...")

  burkart <- fread(burkart_file)
  setnames(burkart, "meanTempCat", "zone")

  # Find overlapping years
  common_years <- intersect(unique(summary_dt$year_id), unique(burkart$year_id))

  if (length(common_years) > 0) {
    comp <- merge(
      summary_dt[year_id %in% common_years, .(zone, year_id,
                                               derived_mean = round(tmrelMean, 2))],
      burkart[year_id %in% common_years, .(zone, year_id,
                                            burkart_mean = round(tmrelMean, 2))],
      by = c("zone", "year_id")
    )
    comp[, diff := derived_mean - burkart_mean]

    cat("\n=== Validation: Derived vs Burkart TMRELs ===\n\n")
    cat("Overlapping years:", paste(common_years, collapse = ", "), "\n")
    cat("Mean absolute difference:", round(mean(abs(comp$diff), na.rm = TRUE), 3), "°C\n")
    cat("Max absolute difference:", round(max(abs(comp$diff), na.rm = TRUE), 3), "°C\n")
    cat("Correlation:", round(cor(comp$derived_mean, comp$burkart_mean), 4), "\n\n")

    cat("Differences by zone (year", common_years[1], "):\n")
    print(comp[year_id == common_years[1]])

    # Save comparison
    comp_file <- file.path(derived_dir, paste0("validation_", LOCATION_ID, ".csv"))
    fwrite(comp, comp_file)
    log_msg("Validation comparison saved:", comp_file)
  } else {
    log_msg("No overlapping years between derived and Burkart TMRELs")
  }
} else {
  log_msg("No Burkart TMREL file found for validation")
}

log_msg("=== TMREL derivation complete ===")
