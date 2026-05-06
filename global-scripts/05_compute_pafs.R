# 05_compute_pafs.R — Compute PAFs and attributable burden
#
# Core calculation: merges temperature exposure with ERF curves and TMRELs,
# rescales RR to 1.0 at the TMREL, computes population-weighted PAFs for
# heat and cold per (year, subnational location, cause), and multiplies by
# cause-specific deaths at (year, subloc, age, sex, cause) granularity to
# get attributable burden.
#
# Subnational dimension: pop weights `pr` from 03_load_temperature.R sum to
# 1 within (subloc, year), so PAF = sum(pr * (RR-1)/RR) within a subloc-year
# is the standard PAF for that subloc. Burden is then computed at full
# (year, subloc, age, sex, cause) granularity. National totals are obtained
# by summing over subloc, age, and sex downstream.
#
# Follows Burkart et al. (2021) methodology:
#   1. Merge temperature × ERF curves × TMRELs
#   2. Rescale RR so RR = 1.0 at the TMREL
#   3. Classify days as heat (> TMREL) or cold (< TMREL)
#   4. Compute PAF = sum of pop-weighted (RR-1)/RR within subloc x heat/cold
#   5. Attributable burden = deaths × PAF (× SEV in COLOMBIA_VERIFICATION mode)
#
# Input:  INTERMEDIATE_DIR/erf_curves.rds, tmrel.rds, temperature.rds, mortality.rds
# Output: RESULTS_DIR/pafs_{LOCATION_ID}.rds — (year, subloc_id, acause)
#         RESULTS_DIR/burden_{LOCATION_ID}.rds — (year, subloc_id, age, sex, acause)

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

  # --- Aggregate temperature to (subloc, zone, daily_temp_10, draw, year) ---
  temp_agg <- temp[, .(pr = sum(pr, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)),
                   by = .(subloc_id, zone, daily_temp_10, draw, year)]

  # --- Merge aggregated temperature with RR ---
  pafs <- merge(temp_agg, rr,
                by.x = c("zone", "daily_temp_10", "draw", "year"),
                by.y = c("zone", "daily_temp", "draw", "year_id"),
                all.x = TRUE, allow.cartesian = TRUE)

  # --- Classify heat/cold ---
  pafs[, risk := ifelse(daily_temp_10 < tmrel, "cold",
                        ifelse(daily_temp_10 > tmrel, "heat", NA_character_))]
  pafs <- pafs[!is.na(risk)]

  # --- Compute PAFs per (subloc, draw, year, cause, risk) ---
  if (COLOMBIA_VERIFICATION) {
    pafs[, paf_contrib := pmax(0, fifelse(rr >= 1,
                                          pr * (rr - 1) / rr,
                                          pr * -1 * ((1/rr) - 1) / (1/rr)))]
    paf_results <- pafs[, .(paf = sum(paf_contrib, na.rm = TRUE)),
                        by = .(acause, risk, subloc_id, draw, year)]
    log_msg("COLOMBIA_VERIFICATION: PAF contributions floored at 0 per row before summing")
  } else {
    paf_results <- pafs[, .(paf = sum(ifelse(rr >= 1,
                                              pr * (rr - 1) / rr,
                                              pr * -1 * ((1/rr) - 1) / (1/rr)),
                                      na.rm = TRUE)),
                        by = .(acause, risk, subloc_id, draw, year)]
  }

  # Reshape draws to wide and summarize per (year, subloc, cause, risk)
  paf_wide <- dcast(paf_results,
                    year + subloc_id + acause + risk ~ paste0("draw_", draw),
                    value.var = "paf")
  draw_cols <- paste0("draw_", 0:(N_DRAWS - 1))
  paf_wide[, paf_mean  := rowMeans(.SD, na.rm = TRUE), .SDcols = draw_cols]
  paf_wide[, paf_lower := apply(.SD, 1, quantile, 0.025, na.rm = TRUE), .SDcols = draw_cols]
  paf_wide[, paf_upper := apply(.SD, 1, quantile, 0.975, na.rm = TRUE), .SDcols = draw_cols]

  log_msg("Draw-level PAFs computed")

# =============================================================================
# SUMMARY MODE
# =============================================================================

} else {
  log_msg("Running in SUMMARY mode")

  # --- Merge RR summaries with TMREL summaries ---
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

  # --- Aggregate temperature to (subloc, zone, daily_temp_10, year) ---
  # `pr` from 03_load_temperature.R is normalized within (subloc, year), so
  # summing within (subloc, zone, daily_temp_10, year) gives the within-subloc
  # population fraction at each temperature bin.
  temp_agg <- temp[, .(pr = sum(pr, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)),
                   by = .(subloc_id, zone, daily_temp_10, year)]

  # --- Merge aggregated temperature with rescaled RR ---
  pafs <- merge(temp_agg, rr,
                by.x = c("zone", "daily_temp_10", "year"),
                by.y = c("zone", "daily_temp", "year"),
                all.x = TRUE, allow.cartesian = TRUE)

  # --- Classify heat/cold ---
  pafs[, risk := ifelse(daily_temp_10 < tmrel_mean_10, "cold",
                        ifelse(daily_temp_10 > tmrel_mean_10, "heat", NA_character_))]
  pafs <- pafs[!is.na(risk)]

  # --- Compute PAFs per (subloc, year, cause, risk) ---
  if (COLOMBIA_VERIFICATION) {
    pafs[, paf_contrib := pmax(0, fifelse(rr_mean >= 1,
                                          pr * (rr_mean - 1) / rr_mean,
                                          pr * -1 * ((1/rr_mean) - 1) / (1/rr_mean)))]
    paf_results <- pafs[, .(paf_mean = sum(paf_contrib, na.rm = TRUE)),
                        by = .(acause, risk, subloc_id, year)]
    log_msg("COLOMBIA_VERIFICATION: PAF contributions floored at 0 per row before summing")
  } else {
    paf_results <- pafs[, .(paf_mean = sum(ifelse(rr_mean >= 1,
                                                   pr * (rr_mean - 1) / rr_mean,
                                                   pr * -1 * ((1/rr_mean) - 1) / (1/rr_mean)),
                                            na.rm = TRUE)),
                        by = .(acause, risk, subloc_id, year)]
  }

  log_msg("Summary-level PAFs computed at (year, subloc_id, cause, risk)")
}

# =============================================================================
# Combine heat and cold PAFs at (year, subloc, cause) into non-optimal
# =============================================================================

paf_col <- "paf_mean"
paf_heat <- paf_results[risk == "heat",
                        .(year, subloc_id, acause, paf_heat = get(paf_col))]
paf_cold <- paf_results[risk == "cold",
                        .(year, subloc_id, acause, paf_cold = get(paf_col))]

paf_combined <- merge(paf_heat, paf_cold,
                      by = c("year", "subloc_id", "acause"), all = TRUE)
paf_combined[is.na(paf_heat), paf_heat := 0]
paf_combined[is.na(paf_cold), paf_cold := 0]
paf_combined[, paf_nonopt := paf_heat + paf_cold]

paf_combined[, location_id := LOCATION_ID]
saveRDS(paf_combined, file.path(RESULTS_DIR, paste0("pafs_", LOCATION_ID, ".rds")))
log_msg("PAFs saved to", file.path(RESULTS_DIR, paste0("pafs_", LOCATION_ID, ".rds")),
        " (", nrow(paf_combined), " rows at year x subloc x cause)")

# =============================================================================
# Compute attributable burden = deaths x PAF (x SEV in verification mode)
# =============================================================================
# Burden lives at (year, subloc_id, age_group_id, sex_id, acause) — the
# Cartesian product of mortality and PAF dimensions. We do NOT pre-aggregate
# mortality; that loses the subloc x deaths covariance the refactor exists
# to capture.

burden <- merge(mort[, .(year_id, subloc_id, age_group_id, sex_id, acause, deaths)],
                paf_combined[, .(year, subloc_id, acause, paf_heat, paf_cold, paf_nonopt)],
                by.x = c("year_id", "subloc_id", "acause"),
                by.y = c("year", "subloc_id", "acause"),
                all.x = TRUE)
burden[is.na(paf_heat),   paf_heat   := 0]
burden[is.na(paf_cold),   paf_cold   := 0]
burden[is.na(paf_nonopt), paf_nonopt := 0]

if (COLOMBIA_VERIFICATION) {
  # Samuel's formula: attributable = deaths * PAF * SEV, with SEV at the
  # (year, subloc, cause) level. SEV file written by 06_compute_sevs.R.
  sev_file <- file.path(RESULTS_DIR, paste0("sevs_", LOCATION_ID, ".rds"))
  if (file.exists(sev_file)) {
    sevs <- setDT(readRDS(sev_file))
    burden <- merge(burden,
                    sevs[, .(year, subloc_id, acause, sev)],
                    by.x = c("year_id", "subloc_id", "acause"),
                    by.y = c("year", "subloc_id", "acause"),
                    all.x = TRUE)
    burden[is.na(sev), sev := 0]
    burden[, `:=`(deaths_heat   = deaths * paf_heat   * sev,
                  deaths_cold   = deaths * paf_cold   * sev,
                  deaths_nonopt = deaths * paf_nonopt * sev)]
    log_msg("COLOMBIA_VERIFICATION: burden = deaths * PAF * SEV (per subloc)")
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

# Rename for backwards compatibility with downstream consumers that expect
# a `year` column on burden.
setnames(burden, "year_id", "year")
burden[, location_id := LOCATION_ID]
saveRDS(burden, file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))
log_msg("Attributable burden saved to ",
        file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")),
        " (", nrow(burden), " rows at year x subloc x age x sex x cause)")

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating PAF/burden diagnostic plots")

  # PAFs by cause and year — collapse subloc with deaths-weighted average
  # for the diagnostic plot only (full granularity is in the saved file).
  paf_for_plot <- merge(paf_combined,
                        mort[, .(deaths_subloc_year = sum(deaths, na.rm = TRUE)),
                             by = .(year_id, subloc_id, acause)],
                        by.x = c("year", "subloc_id", "acause"),
                        by.y = c("year_id", "subloc_id", "acause"),
                        all.x = TRUE)
  paf_for_plot[is.na(deaths_subloc_year), deaths_subloc_year := 0]
  paf_plot_natl <- paf_for_plot[, .(
    paf_heat = sum(paf_heat * deaths_subloc_year, na.rm = TRUE) /
               pmax(sum(deaths_subloc_year, na.rm = TRUE), 1),
    paf_cold = sum(paf_cold * deaths_subloc_year, na.rm = TRUE) /
               pmax(sum(deaths_subloc_year, na.rm = TRUE), 1)),
    by = .(year, acause)]

  p1 <- ggplot(paf_plot_natl, aes(x = year)) +
    geom_col(aes(y = paf_heat, fill = "Heat"), alpha = 0.7) +
    geom_col(aes(y = paf_cold, fill = "Cold"), alpha = 0.7) +
    facet_wrap(~acause, scales = "free_y") +
    scale_fill_manual(values = c("Heat" = "red", "Cold" = "blue")) +
    labs(x = "Year", y = "PAF (deaths-weighted national avg)", fill = "Effect",
         title = paste("Heat and cold PAFs — Location", LOCATION_ID)) +
    theme_minimal() +
    theme(legend.position = "bottom", strip.text = element_text(size = 7))

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("pafs_loc", LOCATION_ID, ".png")),
         p1, width = 14, height = 10, dpi = 150)

  # Attributable deaths by cause — sum across subloc, age, sex for the plot
  burden_summary <- burden[, .(deaths_heat = sum(deaths_heat, na.rm = TRUE),
                                deaths_cold = sum(deaths_cold, na.rm = TRUE)),
                            by = .(year, acause)]
  burden_long <- melt(burden_summary,
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
