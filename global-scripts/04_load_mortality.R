# 04_load_mortality.R — Load cause-specific mortality for a location
#
# Loads GBD cause-specific mortality estimates (deaths by cause, year, age, sex).
# These are pre-processed GBD results, not raw vital registration data.
#
# Expected input format: CSV or RDS with columns:
#   location_id, year_id, age_group_id, sex_id, cause_id, acause, deaths
#   (Matches GBD Results Tool output format)
#
# Input:  MORTALITY_DIR/{LOCATION_ID}_mortality.csv (or .rds)
# Output: INTERMEDIATE_DIR/mortality.rds

source("config.R")

library(data.table)

log_msg("Loading mortality data for location", LOCATION_ID)

# --- Load mortality data ---
mort_file_rds <- file.path(MORTALITY_DIR, paste0(LOCATION_ID, "_mortality.rds"))
mort_file_csv <- file.path(MORTALITY_DIR, paste0(LOCATION_ID, "_mortality.csv"))

if (file.exists(mort_file_rds)) {
  mort <- readRDS(mort_file_rds)
  setDT(mort)
} else if (file.exists(mort_file_csv)) {
  mort <- fread(mort_file_csv)
} else {
  stop(paste("No mortality file found for location", LOCATION_ID,
             "- expected", mort_file_rds, "or", mort_file_csv))
}

log_msg("Loaded", nrow(mort), "mortality rows")

# --- Filter to study period and included causes ---
mort <- mort[year_id >= YEAR_START & year_id <= YEAR_END]
mort <- mort[acause %in% GBD_CAUSES]

# --- Validate ---
found_causes <- unique(mort$acause)
missing_causes <- setdiff(GBD_CAUSES, found_causes)
if (length(missing_causes) > 0) {
  warning(paste("Missing causes in mortality data:", paste(missing_causes, collapse = ", ")))
}

found_years <- sort(unique(mort$year_id))
expected_years <- YEAR_START:YEAR_END
missing_years <- setdiff(expected_years, found_years)
if (length(missing_years) > 0) {
  warning(paste("Missing years in mortality data:", paste(missing_years, collapse = ", ")))
}

log_msg("Filtered to", nrow(mort), "rows:",
        length(found_causes), "causes,",
        length(found_years), "years")

# --- Save ---
saveRDS(mort, file.path(INTERMEDIATE_DIR, "mortality.rds"))
log_msg("Mortality data saved to", file.path(INTERMEDIATE_DIR, "mortality.rds"))

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating mortality diagnostic plots")

  # Deaths by cause and year
  mort_summary <- mort[, .(deaths = sum(deaths, na.rm = TRUE)), by = .(year_id, acause)]

  p1 <- ggplot(mort_summary, aes(x = year_id, y = deaths, color = acause)) +
    geom_line() +
    labs(x = "Year", y = "Deaths", color = "Cause",
         title = paste("Deaths by cause — Location", LOCATION_ID)) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(size = 7))

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("mortality_causes_loc", LOCATION_ID, ".png")),
         p1, width = 12, height = 7, dpi = 150)

  # Deaths by year (total across causes)
  mort_total <- mort[, .(deaths = sum(deaths, na.rm = TRUE)), by = year_id]

  p2 <- ggplot(mort_total, aes(x = year_id, y = deaths)) +
    geom_col(fill = "steelblue") +
    labs(x = "Year", y = "Total deaths (17 causes)",
         title = paste("Total temperature-sensitive deaths — Location", LOCATION_ID)) +
    theme_minimal()

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("mortality_total_loc", LOCATION_ID, ".png")),
         p2, width = 8, height = 5, dpi = 150)

  log_msg("Mortality diagnostic plots saved")
}
