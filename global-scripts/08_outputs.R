# 08_outputs.R — Generate summary tables and output files
#
# Aggregates results across causes / subloc / age / sex; produces summary
# CSVs for several reporting granularities. After the subnational refactor,
# burden and ylls files come in at full (year x subloc x age x sex x cause)
# granularity; this script produces:
#
#   summary_<id>.csv      — per (year, cause), national totals (back-compat)
#   summary_subloc_<id>.csv — per (year, subloc, cause)
#   total_by_year_<id>.csv — per year, aggregated across causes
#
# Input:  RESULTS_DIR/burden_{LOCATION_ID}.rds, pafs_{LOCATION_ID}.rds,
#         ylls_{LOCATION_ID}.rds, sevs_{LOCATION_ID}.rds

source("config.R")

library(data.table)

log_msg("Generating outputs for location", LOCATION_ID)

# --- Load results ---
burden <- readRDS(file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))
pafs   <- readRDS(file.path(RESULTS_DIR, paste0("pafs_", LOCATION_ID, ".rds")))
setDT(burden); setDT(pafs)

yll_file <- file.path(RESULTS_DIR, paste0("ylls_", LOCATION_ID, ".rds"))
has_ylls <- file.exists(yll_file)
if (has_ylls) {
  ylls <- readRDS(yll_file)
  setDT(ylls)
}

sev_file <- file.path(RESULTS_DIR, paste0("sevs_", LOCATION_ID, ".rds"))
has_sevs <- file.exists(sev_file)
if (has_sevs) {
  sevs <- readRDS(sev_file)
  setDT(sevs)
}

# If burden / ylls carry a draw column (per-draw output from 05), collapse
# to per-row mean for the CSV summaries below. The summaries we emit here
# are point-estimate roll-ups intended for quick inspection / sanity-checks;
# proper uncertainty bands come from the workflow_b output downstream.
collapse_draws <- function(dt, value_cols) {
  if (!"draw" %in% names(dt)) return(dt)
  group_cols <- setdiff(names(dt), c("draw", value_cols))
  dt[, lapply(.SD, function(x) mean(x, na.rm = TRUE)),
     by = group_cols, .SDcols = value_cols]
}
burden_value_cols <- intersect(names(burden),
                               c("deaths", "deaths_heat", "deaths_cold",
                                 "deaths_nonopt", "paf_heat", "paf_cold",
                                 "paf_nonopt", "sev"))
burden <- collapse_draws(burden, burden_value_cols)
if (has_ylls) {
  yll_value_cols <- intersect(names(ylls),
                              c("yll_heat", "yll_cold", "yll_nonopt",
                                "deaths", "deaths_heat", "deaths_cold",
                                "deaths_nonopt", "ex"))
  ylls <- collapse_draws(ylls, yll_value_cols)
}

# =============================================================================
# Per-subloc summary (year x subloc x cause)
# =============================================================================

summary_subloc <- burden[, .(deaths       = sum(deaths,       na.rm = TRUE),
                             deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
                             deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
                             deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                         by = .(year, subloc_id, acause)]

if (has_ylls) {
  yll_sub <- ylls[, .(yll_heat   = sum(yll_heat,   na.rm = TRUE),
                      yll_cold   = sum(yll_cold,   na.rm = TRUE),
                      yll_nonopt = sum(yll_nonopt, na.rm = TRUE)),
                  by = .(year_id, subloc_id, acause)]
  setnames(yll_sub, "year_id", "year")
  summary_subloc <- merge(summary_subloc, yll_sub,
                          by = c("year", "subloc_id", "acause"), all.x = TRUE)
}

if (has_sevs) {
  summary_subloc <- merge(summary_subloc,
                          sevs[, .(year, subloc_id, acause, sev)],
                          by = c("year", "subloc_id", "acause"), all.x = TRUE)
}

summary_subloc[, location_id := LOCATION_ID]
fwrite(summary_subloc,
       file.path(RESULTS_DIR, paste0("summary_subloc_", LOCATION_ID, ".csv")))
log_msg("Per-subloc summary saved (", nrow(summary_subloc), " rows)")

# =============================================================================
# National summary (year x cause) — collapse subloc, age, sex
# =============================================================================

summary_national <- burden[, .(deaths       = sum(deaths,       na.rm = TRUE),
                                deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
                                deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
                                deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                            by = .(year, acause)]

if (has_ylls) {
  yll_natl <- ylls[, .(yll_heat   = sum(yll_heat,   na.rm = TRUE),
                        yll_cold   = sum(yll_cold,   na.rm = TRUE),
                        yll_nonopt = sum(yll_nonopt, na.rm = TRUE)),
                    by = .(year_id, acause)]
  setnames(yll_natl, "year_id", "year")
  summary_national <- merge(summary_national, yll_natl,
                            by = c("year", "acause"), all.x = TRUE)
}

if (has_sevs) {
  # Pop-weighted national SEV per (year, cause). Approximate using burden's
  # deaths column as a population proxy (works because the same mortality
  # data flows through to burden).
  weights <- burden[, .(weight = sum(deaths, na.rm = TRUE)),
                    by = .(year, subloc_id, acause)]
  sev_natl <- merge(sevs, weights, by = c("year", "subloc_id", "acause"))
  sev_natl <- sev_natl[, .(sev = sum(sev * weight, na.rm = TRUE) /
                                   pmax(sum(weight, na.rm = TRUE), 1)),
                       by = .(year, acause)]
  summary_national <- merge(summary_national, sev_natl,
                            by = c("year", "acause"), all.x = TRUE)
}

summary_national[, location_id := LOCATION_ID]
fwrite(summary_national,
       file.path(RESULTS_DIR, paste0("summary_", LOCATION_ID, ".csv")))
log_msg("National summary saved (", nrow(summary_national), " rows)")

# =============================================================================
# Aggregate across causes (total burden by year)
# =============================================================================

total_by_year <- burden[, .(deaths_total  = sum(deaths,        na.rm = TRUE),
                            deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
                            deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
                            deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                        by = year]

if (has_ylls) {
  yll_total <- ylls[, .(yll_heat   = sum(yll_heat,   na.rm = TRUE),
                        yll_cold   = sum(yll_cold,   na.rm = TRUE),
                        yll_nonopt = sum(yll_nonopt, na.rm = TRUE)),
                    by = year_id]
  setnames(yll_total, "year_id", "year")
  total_by_year <- merge(total_by_year, yll_total, by = "year", all.x = TRUE)
}

total_by_year[, location_id := LOCATION_ID]
fwrite(total_by_year,
       file.path(RESULTS_DIR, paste0("total_by_year_", LOCATION_ID, ".csv")))
log_msg("Annual totals saved")

# =============================================================================
# Diagnostic / summary plots
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating output plots")

  # Total attributable deaths over time
  p1 <- ggplot(total_by_year, aes(x = year)) +
    geom_line(aes(y = deaths_heat, color = "Heat"), linewidth = 1) +
    geom_line(aes(y = deaths_cold, color = "Cold"), linewidth = 1) +
    geom_line(aes(y = deaths_nonopt, color = "Non-optimal"), linewidth = 1, linetype = "dashed") +
    scale_color_manual(values = c("Heat" = "red", "Cold" = "blue", "Non-optimal" = "purple")) +
    labs(x = "Year", y = "Attributable deaths", color = "",
         title = paste("Total attributable deaths — Location", LOCATION_ID)) +
    theme_minimal()

  ggsave(file.path(FIGURES_DIR, paste0("total_deaths_loc", LOCATION_ID, ".png")),
         p1, width = 10, height = 6, dpi = 150)

  # Cause composition of attributable deaths (latest year, national)
  latest_year <- max(burden$year)
  burden_latest <- summary_national[year == latest_year & deaths_nonopt != 0]
  burden_latest[, acause := reorder(acause, deaths_nonopt)]

  p2 <- ggplot(burden_latest, aes(x = acause)) +
    geom_col(aes(y = deaths_heat, fill = "Heat"), alpha = 0.8) +
    geom_col(aes(y = deaths_cold, fill = "Cold"), alpha = 0.8) +
    scale_fill_manual(values = c("Heat" = "red", "Cold" = "blue")) +
    coord_flip() +
    labs(x = "", y = "Attributable deaths", fill = "",
         title = paste("Attributable deaths by cause,", latest_year,
                       "— Location", LOCATION_ID)) +
    theme_minimal()

  ggsave(file.path(FIGURES_DIR, paste0("cause_composition_loc", LOCATION_ID, ".png")),
         p2, width = 10, height = 7, dpi = 150)

  # PAF trends — pafs is now per (year, subloc, cause); show deaths-weighted
  # national average per (year, cause)
  pop_weights <- burden[, .(deaths = sum(deaths, na.rm = TRUE)),
                        by = .(year, subloc_id, acause)]
  paf_for_plot <- merge(pafs, pop_weights,
                        by = c("year", "subloc_id", "acause"), all.x = TRUE)
  paf_for_plot[is.na(deaths), deaths := 0]
  paf_natl <- paf_for_plot[, .(
    paf_heat = sum(paf_heat * deaths, na.rm = TRUE) /
               pmax(sum(deaths, na.rm = TRUE), 1),
    paf_cold = sum(paf_cold * deaths, na.rm = TRUE) /
               pmax(sum(deaths, na.rm = TRUE), 1)),
    by = .(year, acause)]

  p3 <- ggplot(paf_natl, aes(x = year)) +
    geom_line(aes(y = paf_heat, color = "Heat")) +
    geom_line(aes(y = paf_cold, color = "Cold")) +
    facet_wrap(~acause, scales = "free_y") +
    scale_color_manual(values = c("Heat" = "red", "Cold" = "blue")) +
    labs(x = "Year", y = "Annual PAF (deaths-weighted national avg)", color = "",
         title = paste("PAF trends — Location", LOCATION_ID)) +
    theme_minimal() +
    theme(strip.text = element_text(size = 7))

  ggsave(file.path(FIGURES_DIR, paste0("paf_trends_loc", LOCATION_ID, ".png")),
         p3, width = 14, height = 10, dpi = 150)

  log_msg("Output plots saved to", FIGURES_DIR)
}

log_msg("Output generation complete for location", LOCATION_ID)
