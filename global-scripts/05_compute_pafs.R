# 05_compute_pafs.R — Compute PAFs and attributable burden
#
# Core calculation: merges temperature exposure with ERF curves and TMRELs,
# rescales RR to 1.0 at the TMREL, computes population-weighted PAFs for
# heat and cold, and multiplies by cause-specific deaths to get attributable
# burden.
#
# Follows Burkart et al. (2021) methodology:
#   1. Merge temperature × ERF curves × TMRELs
#   2. Rescale RR so RR = 1.0 at the TMREL
#   3. Classify days as heat (> TMREL) or cold (< TMREL)
#   4. Compute PAF = sum of pop-weighted (RR-1)/RR within heat/cold
#   5. Attributable burden = deaths × PAF (no SEV multiplier)
#
# Input:  INTERMEDIATE_DIR/erf_curves.rds, tmrel.rds, temperature.rds, mortality.rds
# Output: RESULTS_DIR/pafs_{LOCATION_ID}.rds, RESULTS_DIR/burden_{LOCATION_ID}.rds

source("config.R")

library(data.table)

log_msg("Computing PAFs for location", LOCATION_ID)

# --- Load intermediate data ---
erf   <- readRDS(file.path(INTERMEDIATE_DIR, "erf_curves.rds"))
tmrel <- readRDS(file.path(INTERMEDIATE_DIR, "tmrel.rds"))
temp  <- readRDS(file.path(INTERMEDIATE_DIR, "temperature.rds"))
mort  <- readRDS(file.path(INTERMEDIATE_DIR, "mortality.rds"))

setDT(erf); setDT(tmrel); setDT(temp); setDT(mort)

# =============================================================================
# DRAW MODE
# =============================================================================

if (USE_DRAWS) {
  log_msg("Running in DRAW mode (", N_DRAWS, " draws)")

  # --- Merge RR draws with TMREL draws ---
  rr <- merge(erf, tmrel[, .(zone, year_id, draw, tmrel)],
              by = c("zone", "draw"), all = TRUE, allow.cartesian = TRUE)

  # --- Rescale RR to 1.0 at the TMREL ---
  if (COLOMBIA_VERIFICATION) {
    log_msg("COLOMBIA_VERIFICATION: skipping RR rescaling at TMREL")
  } else {
    rr[, rr_ref := sum(rr * (daily_temp == tmrel), na.rm = TRUE),
       by = .(zone, acause, draw, year_id)]
    rr[rr_ref > 0, rr := rr / rr_ref]
    rr[, rr_ref := NULL]
    log_msg("RR curves rescaled to TMREL")
  }

  # --- Collapse temperature to (zone, daily_temp_10, draw, year) before merging ---
  temp_agg <- temp[, .(pr = sum(pr, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)),
                   by = .(zone, daily_temp_10, draw, year)]

  # --- Merge aggregated temperature with RR ---
  pafs <- merge(temp_agg, rr,
                by.x = c("zone", "daily_temp_10", "draw", "year"),
                by.y = c("zone", "daily_temp", "draw", "year_id"),
                all.x = TRUE, allow.cartesian = TRUE)

  # --- Classify heat/cold ---
  pafs[, risk := ifelse(daily_temp_10 < tmrel, "cold",
                        ifelse(daily_temp_10 > tmrel, "heat", NA_character_))]
  pafs <- pafs[!is.na(risk)]

  # --- Compute PAFs per draw ---
  # PAF = sum of pop-weighted (RR-1)/RR
  paf_results <- pafs[, .(paf = sum(ifelse(rr >= 1,
                                            pr * (rr - 1) / rr,
                                            pr * -1 * ((1/rr) - 1) / (1/rr)),
                                    na.rm = TRUE)),
                      by = .(acause, risk, draw, year)]

  # Reshape to wide for summaries
  paf_wide <- dcast(paf_results, year + acause + risk ~ paste0("draw_", draw),
                    value.var = "paf")
  draw_cols <- paste0("draw_", 0:(N_DRAWS - 1))
  paf_wide[, paf_mean  := rowMeans(.SD, na.rm = TRUE), .SDcols = draw_cols]
  paf_wide[, paf_lower := apply(.SD, 1, quantile, 0.025, na.rm = TRUE), .SDcols = draw_cols]
  paf_wide[, paf_upper := apply(.SD, 1, quantile, 0.975, na.rm = TRUE), .SDcols = draw_cols]

  # Colombia verification: floor PAFs at zero (removes protective effects)
  if (COLOMBIA_VERIFICATION) {
    paf_results[paf < 0, paf := 0]
    log_msg("COLOMBIA_VERIFICATION: PAFs floored at zero")
  }

  log_msg("Draw-level PAFs computed")

# =============================================================================
# SUMMARY MODE
# =============================================================================

} else {
  log_msg("Running in SUMMARY mode")

  # --- Merge RR summaries with TMREL summaries ---
  # Add TMREL to the RR table
  rr <- merge(erf, tmrel[, .(zone, year_id, tmrel_mean_10)],
              by = "zone", all.x = TRUE, allow.cartesian = TRUE)
  setnames(rr, "year_id", "year")

  # Rescale RR to 1.0 at the TMREL
  if (COLOMBIA_VERIFICATION) {
    log_msg("COLOMBIA_VERIFICATION: skipping RR rescaling at TMREL")
  } else {
    rr_at_tmrel <- erf[, .(zone, daily_temp, acause, rr_mean)]
    rr_at_tmrel <- merge(rr_at_tmrel,
                         tmrel[, .(zone, year_id, tmrel_mean_10)],
                         by = "zone", allow.cartesian = TRUE)
    rr_at_tmrel <- rr_at_tmrel[daily_temp == tmrel_mean_10]
    setnames(rr_at_tmrel, c("rr_mean", "year_id"), c("rr_ref", "year"))
    rr_at_tmrel <- rr_at_tmrel[, .(zone, acause, year, rr_ref)]

    rr <- merge(rr, rr_at_tmrel, by = c("zone", "acause", "year"), all.x = TRUE)
    rr[rr_ref > 0, `:=`(rr_mean  = rr_mean / rr_ref,
                         rr_lower = rr_lower / rr_ref,
                         rr_upper = rr_upper / rr_ref)]
    rr[, rr_ref := NULL]
    log_msg("RR curves rescaled to TMREL")
  }

  # --- Collapse temperature to (zone, daily_temp_10, year) before merging ---
  # Each pixel-day doesn't need its own row — we only need the aggregate
  # population proportion per (zone, temp, year) for the PAF formula.
  temp_agg <- temp[, .(pr = sum(pr, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)),
                   by = .(zone, daily_temp_10, year)]

  # --- Merge aggregated temperature with rescaled RR ---
  pafs <- merge(temp_agg, rr,
                by.x = c("zone", "daily_temp_10", "year"),
                by.y = c("zone", "daily_temp", "year"),
                all.x = TRUE, allow.cartesian = TRUE)

  # --- Classify heat/cold ---
  pafs[, risk := ifelse(daily_temp_10 < tmrel_mean_10, "cold",
                        ifelse(daily_temp_10 > tmrel_mean_10, "heat", NA_character_))]
  pafs <- pafs[!is.na(risk)]

  # --- Compute PAFs ---
  # PAF = sum of pop-weighted (RR-1)/RR across pixel-days
  paf_results <- pafs[, .(paf_mean = sum(ifelse(rr_mean >= 1,
                                                 pr * (rr_mean - 1) / rr_mean,
                                                 pr * -1 * ((1/rr_mean) - 1) / (1/rr_mean)),
                                          na.rm = TRUE)),
                      by = .(acause, risk, year)]

  # Colombia verification: floor PAFs at zero (removes protective effects)
  if (COLOMBIA_VERIFICATION) {
    paf_results[paf_mean < 0, paf_mean := 0]
    log_msg("COLOMBIA_VERIFICATION: PAFs floored at zero")
  }

  log_msg("Summary-level PAFs computed")
}

# =============================================================================
# Combine heat and cold PAFs into non-optimal
# =============================================================================

# Reshape to heat/cold columns
paf_col <- if (USE_DRAWS) "paf_mean" else "paf_mean"
paf_heat <- paf_results[risk == "heat", .(year, acause, paf_heat = get(paf_col))]
paf_cold <- paf_results[risk == "cold", .(year, acause, paf_cold = get(paf_col))]

paf_combined <- merge(paf_heat, paf_cold, by = c("year", "acause"), all = TRUE)
paf_combined[is.na(paf_heat), paf_heat := 0]
paf_combined[is.na(paf_cold), paf_cold := 0]
paf_combined[, paf_nonopt := paf_heat + paf_cold]

# Save PAFs
paf_combined[, location_id := LOCATION_ID]
saveRDS(paf_combined, file.path(RESULTS_DIR, paste0("pafs_", LOCATION_ID, ".rds")))
log_msg("PAFs saved to", file.path(RESULTS_DIR, paste0("pafs_", LOCATION_ID, ".rds")))

# =============================================================================
# Compute attributable burden = deaths × PAF
# =============================================================================

# Aggregate mortality by year and cause (across age/sex for now)
mort_agg <- mort[, .(deaths = sum(deaths, na.rm = TRUE)),
                 by = .(year_id, acause)]
setnames(mort_agg, "year_id", "year")

burden <- merge(mort_agg, paf_combined, by = c("year", "acause"), all.x = TRUE)

if (COLOMBIA_VERIFICATION) {
  # Samuel's formula: attributable = deaths × PAF × SEV
  # Load SEVs (06_compute_sevs.R must have already run)
  sev_file <- file.path(RESULTS_DIR, paste0("sevs_", LOCATION_ID, ".rds"))
  if (file.exists(sev_file)) {
    sevs <- readRDS(sev_file)
    setDT(sevs)
    burden <- merge(burden, sevs[, .(year, acause, sev)],
                    by = c("year", "acause"), all.x = TRUE)
    burden[is.na(sev), sev := 0]
    burden[, `:=`(deaths_heat   = deaths * paf_heat * sev,
                  deaths_cold   = deaths * paf_cold * sev,
                  deaths_nonopt = deaths * paf_nonopt * sev)]
    log_msg("COLOMBIA_VERIFICATION: burden = deaths * PAF * SEV")
  } else {
    warning("COLOMBIA_VERIFICATION: SEV file not found — run 06_compute_sevs.R first. Falling back to deaths * PAF.")
    burden[, `:=`(deaths_heat   = deaths * paf_heat,
                  deaths_cold   = deaths * paf_cold,
                  deaths_nonopt = deaths * paf_nonopt)]
  }
} else {
  burden[, `:=`(deaths_heat   = deaths * paf_heat,
                deaths_cold   = deaths * paf_cold,
                deaths_nonopt = deaths * paf_nonopt)]
}

burden[, location_id := LOCATION_ID]
saveRDS(burden, file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))
log_msg("Attributable burden saved to", file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating PAF/burden diagnostic plots")

  # PAFs by cause and year
  p1 <- ggplot(paf_combined, aes(x = year)) +
    geom_col(aes(y = paf_heat, fill = "Heat"), alpha = 0.7) +
    geom_col(aes(y = paf_cold, fill = "Cold"), alpha = 0.7) +
    facet_wrap(~acause, scales = "free_y") +
    scale_fill_manual(values = c("Heat" = "red", "Cold" = "blue")) +
    labs(x = "Year", y = "PAF", fill = "Effect",
         title = paste("Heat and cold PAFs — Location", LOCATION_ID)) +
    theme_minimal() +
    theme(legend.position = "bottom", strip.text = element_text(size = 7))

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("pafs_loc", LOCATION_ID, ".png")),
         p1, width = 14, height = 10, dpi = 150)

  # Attributable deaths by cause
  burden_long <- melt(burden,
                      id.vars = c("year", "acause"),
                      measure.vars = c("deaths_heat", "deaths_cold"),
                      variable.name = "effect",
                      value.name = "attrib_deaths")
  burden_long[, effect := gsub("deaths_", "", effect)]

  p2 <- ggplot(burden_long, aes(x = year, y = attrib_deaths, fill = effect)) +
    geom_col(position = "stack") +
    facet_wrap(~acause, scales = "free_y") +
    scale_fill_manual(values = c("heat" = "red", "cold" = "blue")) +
    labs(x = "Year", y = "Attributable deaths", fill = "Effect",
         title = paste("Attributable deaths — Location", LOCATION_ID)) +
    theme_minimal() +
    theme(legend.position = "bottom", strip.text = element_text(size = 7))

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("burden_loc", LOCATION_ID, ".png")),
         p2, width = 14, height = 10, dpi = 150)

  log_msg("PAF/burden diagnostic plots saved")
}
