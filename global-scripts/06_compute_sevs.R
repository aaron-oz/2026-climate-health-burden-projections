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
  # Keep only needed columns
  erf <- erf[, .(zone, daily_temp, acause, rr_mean)]
  tmrel_s <- tmrel[, .(zone, year_id, tmrel_mean_10)]
}

# --- Rescale RR to 1.0 at the TMREL ---
# For each zone/year, find the RR at the TMREL temperature
rr_ref_dt <- merge(
  tmrel_s,
  erf,
  by.x = c("zone"),
  by.y = c("zone"),
  allow.cartesian = TRUE
)
rr_ref_dt <- rr_ref_dt[daily_temp == tmrel_mean_10,
                        .(zone, year_id, acause, rr_ref = rr_mean)]

# Build rescaled RR table with year dimension
erf_yr <- merge(erf, tmrel_s, by = "zone", allow.cartesian = TRUE)
erf_yr <- merge(erf_yr, rr_ref_dt, by = c("zone", "year_id", "acause"), all.x = TRUE)
erf_yr[!is.na(rr_ref) & rr_ref > 0, rr_rescaled := rr_mean / rr_ref]
erf_yr[is.na(rr_rescaled), rr_rescaled := rr_mean]

# --- Merge temperature exposure with rescaled RR ---
sev_data <- merge(temp, erf_yr,
                  by.x = c("zone", "daily_temp_10", "year"),
                  by.y = c("zone", "daily_temp", "year_id"),
                  all.x = TRUE, allow.cartesian = TRUE)
sev_data <- sev_data[!is.na(rr_rescaled)]

# --- Compute RR_max: 99th percentile of pop-weighted RR per cause/zone ---
sev_data_sorted <- sev_data[order(zone, acause, rr_rescaled)]
sev_data_sorted[, pop_cumfrac := cumsum(pop) / sum(pop),
                by = .(zone, acause)]

rr_max_dt <- sev_data_sorted[, {
  idx <- which.min(abs(pop_cumfrac - 0.99))
  .(rr_max = rr_rescaled[idx])
}, by = .(zone, acause)]

# --- Compute SEVs ---
sev_data <- merge(sev_data, rr_max_dt, by = c("zone", "acause"), all.x = TRUE)

# Population proportion within each zone-year-cause
sev_data[, pr_zone := pop / sum(pop, na.rm = TRUE), by = .(year, zone, acause)]

sev_data[, sev_contrib := fifelse(rr_max <= 1 | rr_rescaled <= 1, 0,
                                   pr_zone * (rr_rescaled - 1) / (rr_max - 1))]

sevs <- sev_data[, .(sev = sum(sev_contrib, na.rm = TRUE)),
                 by = .(year, zone, acause)]
sevs[sev < 0, sev := 0]
sevs[sev > 1, sev := 1]

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
