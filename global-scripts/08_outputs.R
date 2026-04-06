# 08_outputs.R — Generate summary tables and output files
#
# Aggregates results across causes, produces summary CSVs and maps.
#
# Input:  RESULTS_DIR/burden_{LOCATION_ID}.rds, pafs_{LOCATION_ID}.rds,
#         ylls_{LOCATION_ID}.rds, sevs_{LOCATION_ID}.rds
# Output: RESULTS_DIR/summary_{LOCATION_ID}.csv, figures

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

# =============================================================================
# Summary table: burden by cause and year
# =============================================================================

summary_table <- copy(burden)

if (has_ylls) {
  summary_table <- merge(summary_table,
                         ylls[, .(year_id, acause, yll_heat, yll_cold, yll_nonopt)],
                         by.x = c("year", "acause"),
                         by.y = c("year_id", "acause"),
                         all.x = TRUE)
}

if (has_sevs) {
  summary_table <- merge(summary_table,
                         sevs[, .(year, acause, sev)],
                         by = c("year", "acause"),
                         all.x = TRUE)
}

fwrite(summary_table,
       file.path(RESULTS_DIR, paste0("summary_", LOCATION_ID, ".csv")))
log_msg("Summary table saved")

# =============================================================================
# Aggregate across causes (total burden)
# =============================================================================

total_by_year <- burden[, .(deaths_total = sum(deaths, na.rm = TRUE),
                            deaths_heat = sum(deaths_heat, na.rm = TRUE),
                            deaths_cold = sum(deaths_cold, na.rm = TRUE),
                            deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                        by = year]

if (has_ylls) {
  yll_total <- ylls[, .(yll_heat = sum(yll_heat, na.rm = TRUE),
                        yll_cold = sum(yll_cold, na.rm = TRUE),
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

  # Cause composition of attributable deaths (latest year)
  latest_year <- max(burden$year)
  burden_latest <- burden[year == latest_year & deaths_nonopt != 0]
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

  # PAF trends
  p3 <- ggplot(pafs, aes(x = year)) +
    geom_line(aes(y = paf_heat, color = "Heat")) +
    geom_line(aes(y = paf_cold, color = "Cold")) +
    facet_wrap(~acause, scales = "free_y") +
    scale_color_manual(values = c("Heat" = "red", "Cold" = "blue")) +
    labs(x = "Year", y = "Annual PAF", color = "",
         title = paste("PAF trends — Location", LOCATION_ID)) +
    theme_minimal() +
    theme(strip.text = element_text(size = 7))

  ggsave(file.path(FIGURES_DIR, paste0("paf_trends_loc", LOCATION_ID, ".png")),
         p3, width = 14, height = 10, dpi = 150)

  log_msg("Output plots saved to", FIGURES_DIR)
}

log_msg("Output generation complete for location", LOCATION_ID)
