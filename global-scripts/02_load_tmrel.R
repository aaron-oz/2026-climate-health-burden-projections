# 02_load_tmrel.R — Load TMREL data for a location
#
# Loads year-specific TMRELs for the given LOCATION_ID.
# Supports two modes:
#   USE_DRAWS = TRUE:  keeps all 1000 TMREL draws per zone/year
#   USE_DRAWS = FALSE: uses summary (mean) TMREL values
#
# Input:  TMREL_DIR/tmrel_{LOCATION_ID}.csv or tmrel_{LOCATION_ID}_summaries.csv
# Output: INTERMEDIATE_DIR/tmrel.rds

source("config.R")

library(data.table)

log_msg("Loading TMRELs for location", LOCATION_ID)

if (USE_DRAWS) {
  tmrel_file <- file.path(TMREL_DIR, paste0("tmrel_", LOCATION_ID, ".csv"))
  if (!file.exists(tmrel_file)) stop(paste("TMREL draw file not found:", tmrel_file))

  tmrel <- fread(tmrel_file)
  tmrel <- tmrel[year_id >= YEAR_START & year_id <= YEAR_END]

  # Reshape to long: one row per (zone, year, draw)
  draw_cols <- paste0("tmrel_", 0:(N_DRAWS - 1))
  tmrel <- melt(tmrel,
                id.vars = c("location_id", "year_id", "meanTempCat"),
                measure.vars = draw_cols,
                variable.name = "draw",
                value.name = "tmrel",
                variable.factor = FALSE)
  tmrel[, draw := as.integer(gsub("tmrel_", "", draw))]
  # Convert TMREL to integer * 10 to match ERF daily_temp encoding
  tmrel[, tmrel := as.integer(round(tmrel * 10))]

  log_msg("Draw mode: loaded", nrow(tmrel), "TMREL draw rows")

} else {
  tmrel_file <- file.path(TMREL_DIR, paste0("tmrel_", LOCATION_ID, "_summaries.csv"))
  if (!file.exists(tmrel_file)) stop(paste("TMREL summary file not found:", tmrel_file))

  tmrel <- fread(tmrel_file)
  tmrel <- tmrel[year_id >= YEAR_START & year_id <= YEAR_END]
  tmrel[, tmrel_mean := round(tmrelMean, 1)]
  # Convert to integer * 10 to match ERF daily_temp encoding
  tmrel[, tmrel_mean_10 := as.integer(round(tmrelMean * 10))]

  log_msg("Summary mode: loaded", nrow(tmrel), "TMREL summary rows")
}

# Standardize column names
setnames(tmrel, "meanTempCat", "zone")
tmrel[, zone := as.integer(zone)]

# Save
saveRDS(tmrel, file.path(INTERMEDIATE_DIR, "tmrel.rds"))
log_msg("TMRELs saved to", file.path(INTERMEDIATE_DIR, "tmrel.rds"))

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating TMREL diagnostic plots")

  if (USE_DRAWS) {
    # Summarize draws for plotting
    tmrel_summary <- tmrel[, .(tmrel_mean = mean(tmrel / 10),
                               tmrel_lower = quantile(tmrel / 10, 0.025),
                               tmrel_upper = quantile(tmrel / 10, 0.975)),
                           by = .(zone, year_id)]
  } else {
    tmrel_summary <- copy(tmrel)
    setnames(tmrel_summary, "tmrelMean", "tmrel_mean")
    setnames(tmrel_summary, "tmrelLower", "tmrel_lower")
    setnames(tmrel_summary, "tmrelUpper", "tmrel_upper")
  }

  p <- ggplot(tmrel_summary, aes(x = year_id, y = tmrel_mean, color = as.factor(zone))) +
    geom_line() +
    geom_ribbon(aes(ymin = tmrel_lower, ymax = tmrel_upper, fill = as.factor(zone)),
                alpha = 0.1, color = NA) +
    labs(x = "Year", y = "TMREL (°C)",
         color = "Temp zone", fill = "Temp zone",
         title = paste("TMREL by zone — Location", LOCATION_ID)) +
    theme_minimal()

  ggsave(file.path(DIAGNOSTICS_DIR, paste0("tmrel_loc", LOCATION_ID, ".png")),
         p, width = 10, height = 6, dpi = 150)

  log_msg("TMREL diagnostic plot saved")
}
