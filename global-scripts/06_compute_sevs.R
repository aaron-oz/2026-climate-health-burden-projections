# 06_compute_sevs.R — Compute Summary Exposure Values (diagnostic)
#
# SEVs are a standardized metric of exposure intensity (0 to 1) used for
# cross-risk-factor comparison and tracking exposure trends over time.
# They are NOT used in the attributable burden calculation.
#
# SEV = sum of pop-weighted (RR - 1) / (RR_max - 1), bounded [0, 1]
#
# Input:  INTERMEDIATE_DIR/erf_curves.rds, temperature.rds, tmrel.rds
# Output: RESULTS_DIR/sevs_{LOCATION_ID}.rds

source("config.R")

if (!COMPUTE_SEVS) {
  log_msg("COMPUTE_SEVS = FALSE, skipping SEV calculation")
  quit(save = "no")
}

library(data.table)

log_msg("Computing SEVs for location", LOCATION_ID)

# --- Load data ---
erf   <- readRDS(file.path(INTERMEDIATE_DIR, "erf_curves.rds"))
temp  <- readRDS(file.path(INTERMEDIATE_DIR, "temperature.rds"))
tmrel <- readRDS(file.path(INTERMEDIATE_DIR, "tmrel.rds"))

setDT(erf); setDT(temp); setDT(tmrel)

# SEVs use summary RR values even in draw mode (diagnostic metric).
if (USE_DRAWS) {
  erf <- erf[, .(rr_mean = mean(rr)), by = .(zone, daily_temp, acause)]
  tmrel_s <- tmrel[, .(tmrel_mean_10 = as.integer(round(mean(tmrel)))),
                   by = .(zone, year_id)]
} else {
  # Keep only needed columns. In verification mode also keep rr_max for the
  # Samuel-style rr_max definition below.
  if (COLOMBIA_VERIFICATION) {
    erf <- erf[, .(zone, daily_temp, acause, rr_mean, rr_max)]
  } else {
    erf <- erf[, .(zone, daily_temp, acause, rr_mean)]
  }
  tmrel_s <- tmrel[, .(zone, year_id, tmrel_mean_10)]
}

# --- Rescale RR to 1.0 at the TMREL ---
erf_yr <- merge(erf, tmrel_s, by = "zone", allow.cartesian = TRUE)

if (COLOMBIA_VERIFICATION) {
  log_msg("COLOMBIA_VERIFICATION: skipping RR rescaling for SEV calculation")
  erf_yr[, rr_rescaled := rr_mean]
} else {
  rr_ref_dt <- merge(tmrel_s, erf, by = "zone", allow.cartesian = TRUE)
  rr_ref_dt <- rr_ref_dt[daily_temp == tmrel_mean_10,
                          .(zone, year_id, acause, rr_ref = rr_mean)]
  erf_yr <- merge(erf_yr, rr_ref_dt, by = c("zone", "year_id", "acause"), all.x = TRUE)
  erf_yr[!is.na(rr_ref) & rr_ref > 0, rr_rescaled := rr_mean / rr_ref]
  erf_yr[is.na(rr_rescaled), rr_rescaled := rr_mean]
}

# --- Collapse temperature to (zone, daily_temp_10, year) before merging ---
temp_agg <- temp[, .(pop = sum(pop, na.rm = TRUE)),
                 by = .(zone, daily_temp_10, year)]

# --- Merge aggregated temperature with rescaled RR ---
sev_data <- merge(temp_agg, erf_yr,
                  by.x = c("zone", "daily_temp_10", "year"),
                  by.y = c("zone", "daily_temp", "year_id"),
                  all.x = TRUE, allow.cartesian = TRUE)
sev_data <- sev_data[!is.na(rr_rescaled)]

# --- Compute RR_max ---
if (COLOMBIA_VERIFICATION) {
  # Items 5/23: Samuel uses the max over daily_temp of the per-cell
  # 99th-percentile RR (which is in `erf$rr_max` from 01_load_erf.R), per
  # (zona, c_muerte). Reference: 11_carga_atribuible.R:270-272.
  rr_max_dt <- erf[, .(rr_max = max(rr_max, na.rm = TRUE)), by = .(zone, acause)]
  log_msg("COLOMBIA_VERIFICATION: using Samuel's max-of-99th-percentile rr_max")
  # rr_max is no longer needed on the row-level erf frame
  sev_data[, rr_max := NULL]
} else {
  # Pop-weighted 99th cumulative-fraction of rr_rescaled per (zone, cause).
  sev_data_sorted <- sev_data[order(zone, acause, rr_rescaled)]
  sev_data_sorted[, pop_cumfrac := cumsum(pop) / sum(pop),
                  by = .(zone, acause)]

  rr_max_dt <- sev_data_sorted[, {
    idx <- which.min(abs(pop_cumfrac - 0.99))
    .(rr_max = rr_rescaled[idx])
  }, by = .(zone, acause)]
}

# --- Compute SEVs ---
sev_data <- merge(sev_data, rr_max_dt, by = c("zone", "acause"), all.x = TRUE)

# Population proportion within each zone-year-cause
sev_data[, pr_zone := pop / sum(pop, na.rm = TRUE), by = .(year, zone, acause)]

sev_data[, sev_contrib := fifelse(rr_max <= 1 | rr_rescaled <= 1, 0,
                                   pr_zone * (rr_rescaled - 1) / (rr_max - 1))]

sevs <- sev_data[, .(sev = sum(sev_contrib, na.rm = TRUE)),
                 by = .(year, zone, acause)]
sevs[sev < 0, sev := 0]

if (COLOMBIA_VERIFICATION) {
  # Replicate Samuel's SEV calculation (11_carga_atribuible.R:444-469).
  # Samuel sums pixel-day contributions of pr_zona*(RR-1)/(RR_max-1) across
  # all ~365 days in the year, then caps at 1. Because pop is ~constant
  # across days, that is equivalent to multiplying our year-level SEV
  # (a true person-time fraction) by N days/year and capping at 1.
  #
  # WARNING: this departs from the GBD/Burkart SEV definition and only
  # exists to reproduce Samuel's Colombia numbers for validation. Gated on
  # COLOMBIA_VERIFICATION; config.R errors if this flag is set with any
  # LOCATION_ID other than 125.
  n_days_per_year <- temp[, .(n_days = uniqueN(date)), by = year]
  sevs <- merge(sevs, n_days_per_year, by = "year")
  sevs[, sev := pmin(sev * n_days, 1)]
  sevs[, n_days := NULL]
  log_msg("COLOMBIA_VERIFICATION: SEVs scaled by N days/year and capped at 1 ",
          "(replicates Samuel's daily-summation bug)")
} else {
  sevs[sev > 1, sev := 1]
}

# --- Aggregate across zones (population-weighted average) ---
zone_pops <- temp[, .(pop_zone = sum(pop, na.rm = TRUE)), by = .(year, zone)]
total_pops <- temp[, .(pop_total = sum(pop, na.rm = TRUE)), by = year]
zone_weights <- merge(zone_pops, total_pops, by = "year")
zone_weights[, zone_weight := pop_zone / pop_total]

sevs <- merge(sevs, zone_weights[, .(year, zone, zone_weight)],
              by = c("year", "zone"), all.x = TRUE)
sevs_agg <- sevs[, .(sev = sum(sev * zone_weight, na.rm = TRUE)),
                 by = .(year, acause)]

sevs_agg[, location_id := LOCATION_ID]
saveRDS(sevs_agg, file.path(RESULTS_DIR, paste0("sevs_", LOCATION_ID, ".rds")))
log_msg("SEVs saved to", file.path(RESULTS_DIR, paste0("sevs_", LOCATION_ID, ".rds")))

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating SEV diagnostic plots")

  p <- ggplot(sevs_agg, aes(x = year, y = sev, color = acause)) +
    geom_line() +
    labs(x = "Year", y = "SEV", color = "Cause",
         title = paste("Summary Exposure Values — Location", LOCATION_ID)) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(size = 7))

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("sevs_loc", LOCATION_ID, ".png")),
         p, width = 10, height = 6, dpi = 150)

  log_msg("SEV diagnostic plot saved")
}
