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

# SEVs are computed from summary RR values even in draw mode,
# since they are a reporting metric, not part of the burden calculation.
# If in draw mode, summarize ERF draws first.
if (USE_DRAWS) {
  erf_summary <- erf[, .(rr_mean = mean(rr)), by = .(zone, daily_temp, acause)]
  tmrel_summary <- tmrel[, .(tmrel_mean_10 = as.integer(round(mean(tmrel)))),
                         by = .(zone, year_id)]
} else {
  erf_summary <- copy(erf)
  tmrel_summary <- tmrel[, .(zone, year_id, tmrel_mean_10)]
}

# --- Rescale RR to 1.0 at the TMREL ---
rr_at_tmrel <- merge(
  erf_summary,
  tmrel_summary,
  by = "zone", allow.cartesian = TRUE
)
rr_ref <- rr_at_tmrel[daily_temp == tmrel_mean_10,
                       .(zone, acause, year_id, rr_ref = rr_mean)]

erf_rescaled <- merge(
  merge(erf_summary, tmrel_summary, by = "zone", allow.cartesian = TRUE),
  rr_ref,
  by = c("zone", "acause", "year_id"), all.x = TRUE
)
erf_rescaled[rr_ref > 0, rr_mean := rr_mean / rr_ref]

# --- Compute RR_max: 99th percentile of population-weighted RR per cause/zone ---
# Merge temperature exposure with rescaled RR
sev_data <- merge(temp, erf_rescaled,
                  by.x = c("zone", "daily_temp_10", "year"),
                  by.y = c("zone", "daily_temp", "year_id"),
                  all.x = TRUE)

# Compute RR_max as 99th percentile by cause and zone
sev_data <- sev_data[!is.na(rr_mean)]
sev_data[, pop_cumfrac := cumsum(pop) / sum(pop),
         by = .(zone, acause)]
rr_max <- sev_data[order(zone, acause, rr_mean)][,
  .SD[which.min(abs(pop_cumfrac - 0.99))],
  by = .(zone, acause)
][, .(zone, acause, rr_max = rr_mean)]

# --- Compute SEVs ---
sev_data <- merge(sev_data, rr_max, by = c("zone", "acause"), all.x = TRUE)

# Recalculate population proportions within each zone (per Burkart sevFix)
sev_data[, pr_zone := pop / sum(pop, na.rm = TRUE),
         by = .(year, zone, acause)]

sev_data[, sev_contrib := ifelse(rr_max <= 1 | rr_mean <= 1, 0,
                                  pr_zone * (rr_mean - 1) / (rr_max - 1))]

sevs <- sev_data[, .(sev = sum(sev_contrib, na.rm = TRUE)),
                 by = .(year, zone, acause)]
sevs[sev < 0, sev := 0]
sevs[sev > 1, sev := 1]

# Aggregate across zones (population-weighted average)
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
