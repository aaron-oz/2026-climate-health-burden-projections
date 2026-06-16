# 06_compute_sevs.R — Compute Summary Exposure Values (diagnostic)
#
# SEVs are a standardized metric of exposure intensity (0 to 1) used for
# cross-risk-factor comparison and tracking exposure trends over time.
# They are NOT used in the attributable burden calculation in production
# mode; in COLOMBIA_VERIFICATION mode they enter the burden formula via
# 05_compute_pafs.R (matching Samuel's methodology).
#
# SEV = sum of pop-weighted (RR - 1) / (RR_max - 1), bounded [0, 1]
#
# Subnational dimension: SEVs are computed per (year, subloc_id, zone,
# cause) and then aggregated across zones within a subloc using
# population-share weights, yielding per (year, subloc_id, cause) SEVs.
#
# Input:  INTERMEDIATE_DIR/erf_curves.rds, temperature.rds, tmrel.rds
# Output: RESULTS_DIR/sevs_{LOCATION_ID}.rds — (year, subloc_id, acause)

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))

# SEVs are diagnostic-only in production. 06 runs before 05/07/08, so skipping
# them must NOT quit() -- that exits the whole pipeline process (the footgun
# documented in 01_load_erf.R). Guard the body with COMPUTE_SEVS instead, so a
# FALSE just skips this step and the pipeline continues.
if (!COMPUTE_SEVS) {
  log_msg("COMPUTE_SEVS = FALSE, skipping SEV calculation")
} else {

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

# --- Aggregate temperature to (subloc, zone, daily_temp_10, year) ---
# Each (subloc, zone, daily_temp, year) row has total person-days at that
# combination. Population fractions for SEV will be computed within
# (subloc, zone, year), so the SEV is meaningful per subloc-zone-year.
temp_agg <- temp[, .(pop = sum(pop, na.rm = TRUE)),
                 by = .(subloc_id, zone, daily_temp_10, year)]

# --- Merge aggregated temperature with rescaled RR ---
sev_data <- merge(temp_agg, erf_yr,
                  by.x = c("zone", "daily_temp_10", "year"),
                  by.y = c("zone", "daily_temp", "year_id"),
                  all.x = TRUE, allow.cartesian = TRUE)
sev_data <- sev_data[!is.na(rr_rescaled)]

# --- Compute RR_max ---
if (COLOMBIA_VERIFICATION) {
  # Items 5/23: Samuel uses max over daily_temp of the per-cell 99th-
  # percentile RR (which is in `erf$rr_max`), per (zona, c_muerte).
  # Reference: 11_carga_atribuible.R:270-272. Not subloc-specific.
  rr_max_dt <- erf[, .(rr_max = max(rr_max, na.rm = TRUE)), by = .(zone, acause)]
  log_msg("COLOMBIA_VERIFICATION: using Samuel's max-of-99th-percentile rr_max")
  sev_data[, rr_max := NULL]
} else {
  # Pop-weighted 99th cumulative-fraction of rr_rescaled per (zone, cause).
  # Aggregated across subloc since RR_max for SEV is a property of the
  # zone-cause RR curve, not a subloc-specific quantity.
  sev_zone <- sev_data[, .(pop = sum(pop, na.rm = TRUE)),
                       by = .(zone, acause, daily_temp_10, rr_rescaled)]
  sev_zone_sorted <- sev_zone[order(zone, acause, rr_rescaled)]
  sev_zone_sorted[, pop_cumfrac := cumsum(pop) / sum(pop),
                  by = .(zone, acause)]

  rr_max_dt <- sev_zone_sorted[, {
    idx <- which.min(abs(pop_cumfrac - 0.99))
    .(rr_max = rr_rescaled[idx])
  }, by = .(zone, acause)]
}

# --- Compute SEVs at (year, subloc, zone, cause) granularity ---
sev_data <- merge(sev_data, rr_max_dt, by = c("zone", "acause"), all.x = TRUE)

# Population proportion within each (subloc, year, zone, cause): summing
# pr_zone over daily_temp_10 within those keys = 1.
sev_data[, pr_zone := pop / sum(pop, na.rm = TRUE),
         by = .(subloc_id, year, zone, acause)]

sev_data[, sev_contrib := fifelse(rr_max <= 1 | rr_rescaled <= 1, 0,
                                   pr_zone * (rr_rescaled - 1) / (rr_max - 1))]

sevs <- sev_data[, .(sev = sum(sev_contrib, na.rm = TRUE)),
                 by = .(year, subloc_id, zone, acause)]
sevs[sev < 0, sev := 0]

if (COLOMBIA_VERIFICATION) {
  # Replicate Samuel's daily-summation bug (11_carga_atribuible.R:444-469).
  # Multiply year-level SEV by N days/year and cap at 1, mirroring his
  # sum-across-365-days-then-cap pattern.
  n_days_per_year <- temp[, .(n_days = uniqueN(date)), by = year]
  sevs <- merge(sevs, n_days_per_year, by = "year")
  sevs[, sev := pmin(sev * n_days, 1)]
  sevs[, n_days := NULL]
  log_msg("COLOMBIA_VERIFICATION: SEVs scaled by N days/year and capped at 1 ",
          "(replicates Samuel's daily-summation bug)")
} else {
  sevs[sev > 1, sev := 1]
}

# --- Aggregate across zones within each subloc using zone-pop weights ---
# zone_weight = (pop in subloc-zone-year) / (pop in subloc-year)
zone_pops <- temp[, .(pop_zone = sum(pop, na.rm = TRUE)),
                  by = .(year, subloc_id, zone)]
subloc_pops <- temp[, .(pop_subloc = sum(pop, na.rm = TRUE)),
                    by = .(year, subloc_id)]
zone_weights <- merge(zone_pops, subloc_pops, by = c("year", "subloc_id"))
zone_weights[, zone_weight := pop_zone / pop_subloc]

sevs <- merge(sevs, zone_weights[, .(year, subloc_id, zone, zone_weight)],
              by = c("year", "subloc_id", "zone"), all.x = TRUE)
sevs_agg <- sevs[, .(sev = sum(sev * zone_weight, na.rm = TRUE)),
                 by = .(year, subloc_id, acause)]

sevs_agg[, location_id := LOCATION_ID]
saveRDS(sevs_agg, file.path(RESULTS_DIR, paste0("sevs_", LOCATION_ID, ".rds")))
log_msg("SEVs saved to ", file.path(RESULTS_DIR, paste0("sevs_", LOCATION_ID, ".rds")),
        " (", nrow(sevs_agg), " rows at year x subloc x cause)")

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating SEV diagnostic plots")

  # National-aggregated SEV for the diagnostic plot only
  pop_subloc_year <- temp[, .(pop_subloc_year = sum(pop, na.rm = TRUE)),
                          by = .(year, subloc_id)]
  pop_year <- temp[, .(pop_year = sum(pop, na.rm = TRUE)), by = year]
  sev_natl <- merge(sevs_agg, pop_subloc_year, by = c("year", "subloc_id"))
  sev_natl <- merge(sev_natl, pop_year, by = "year")
  sev_natl[, w := pop_subloc_year / pop_year]
  sev_plot <- sev_natl[, .(sev = sum(sev * w, na.rm = TRUE)),
                       by = .(year, acause)]

  p <- ggplot(sev_plot, aes(x = year, y = sev, color = acause)) +
    geom_line() +
    labs(x = "Year", y = "SEV (pop-weighted national)", color = "Cause",
         title = paste("Summary Exposure Values — Location", LOCATION_ID)) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(size = 7))

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("sevs_loc", LOCATION_ID, ".png")),
         p, width = 10, height = 6, dpi = 150)

  log_msg("SEV diagnostic plot saved")
}

}  # end if (COMPUTE_SEVS)
