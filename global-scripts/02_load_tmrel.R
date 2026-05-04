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

  # TMREL files have sparse years (1990, 2010, 2020) — fill missing study years
  avail_years <- sort(unique(tmrel$year_id))
  needed_years <- YEAR_START:YEAR_END
  missing_years <- setdiff(needed_years, avail_years)
  if (length(missing_years) > 0) {
    log_msg("TMREL draw years available:", paste(avail_years, collapse = ","),
            "— filling", length(missing_years), "missing study years by nearest-year")
    fill_rows <- rbindlist(lapply(missing_years, function(yr) {
      nearest <- avail_years[which.min(abs(avail_years - yr))]
      rows <- copy(tmrel[year_id == nearest])
      rows[, year_id := yr]
      rows
    }))
    tmrel <- rbind(tmrel, fill_rows)
  }
  tmrel <- tmrel[year_id >= YEAR_START & year_id <= YEAR_END]

  # TMREL files have 100 draws (tmrel_0 to tmrel_99). ERF curves have 1000.
  # Recycle TMREL draws: draw N uses tmrel_(N %% 100).
  n_tmrel_draws <- length(grep("^tmrel_\\d+$", names(tmrel)))
  tmrel_draw_cols <- paste0("tmrel_", 0:(n_tmrel_draws - 1))
  log_msg("TMREL file has", n_tmrel_draws, "draws; N_DRAWS =", N_DRAWS)

  # Reshape to long: one row per (zone, year, draw)
  tmrel <- melt(tmrel,
                id.vars = c("location_id", "year_id", "meanTempCat"),
                measure.vars = tmrel_draw_cols,
                variable.name = "draw",
                value.name = "tmrel",
                variable.factor = FALSE)
  tmrel[, draw := as.integer(gsub("tmrel_", "", draw))]

  # Recycle to N_DRAWS if needed
  if (n_tmrel_draws < N_DRAWS) {
    log_msg("Recycling", n_tmrel_draws, "TMREL draws to", N_DRAWS)
    tmrel_recycled <- rbindlist(lapply(n_tmrel_draws:(N_DRAWS - 1), function(d) {
      src_draw <- d %% n_tmrel_draws
      rows <- copy(tmrel[draw == src_draw])
      rows[, draw := d]
      rows
    }))
    tmrel <- rbind(tmrel, tmrel_recycled)
  }

  # Convert TMREL to integer * 10 to match ERF daily_temp encoding
  tmrel[, tmrel := as.integer(round(tmrel * 10))]

  setnames(tmrel, "meanTempCat", "zone")
  tmrel[, zone := as.integer(zone)]

  log_msg("Draw mode: loaded", nrow(tmrel), "TMREL draw rows")

} else {
  tmrel_file <- file.path(TMREL_DIR, paste0("tmrel_", LOCATION_ID, "_summaries.csv"))
  if (!file.exists(tmrel_file)) stop(paste("TMREL summary file not found:", tmrel_file))

  tmrel <- fread(tmrel_file)
  tmrel[, tmrel_mean := round(tmrelMean, 1)]
  # Convert to integer * 10 to match ERF daily_temp encoding
  tmrel[, tmrel_mean_10 := as.integer(round(tmrelMean * 10))]
  setnames(tmrel, "meanTempCat", "zone")
  tmrel[, zone := as.integer(zone)]

  if (COLOMBIA_VERIFICATION) {
    # Replicate Samuel (11_carga_atribuible.R:243-247): equal-weight mean of
    # ALL years in the source file (1990, 2010, 2020), then expand to study
    # period. Do NOT fill missing study years before averaging — that would
    # over-weight years adjacent to the study period (Samuel doesn't do that).
    src_years <- sort(unique(tmrel$year_id))
    log_msg("COLOMBIA_VERIFICATION: averaging TMREL across source years (",
            paste(src_years, collapse = ","), ") with equal weight, ",
            "then expanding to ", YEAR_START, "-", YEAR_END)
    tmrel_avg <- tmrel[, .(tmrel_mean = mean(tmrel_mean, na.rm = TRUE),
                           tmrel_mean_10 = as.integer(round(mean(tmrel_mean_10, na.rm = TRUE)))),
                       by = zone]
    tmrel <- CJ(zone = unique(tmrel$zone), year_id = YEAR_START:YEAR_END)
    tmrel <- merge(tmrel, tmrel_avg, by = "zone")
  } else {
    # Standard mode: TMREL files have sparse years (e.g. 1990, 2010, 2020).
    # For study years without an exact match, use the nearest available year.
    avail_years <- sort(unique(tmrel$year_id))
    needed_years <- YEAR_START:YEAR_END
    missing_years <- setdiff(needed_years, avail_years)
    if (length(missing_years) > 0) {
      log_msg("TMREL years available:", paste(avail_years, collapse = ","),
              "— filling", length(missing_years), "missing study years by nearest-year")
      fill_rows <- rbindlist(lapply(missing_years, function(yr) {
        nearest <- avail_years[which.min(abs(avail_years - yr))]
        rows <- copy(tmrel[year_id == nearest])
        rows[, year_id := yr]
        rows
      }))
      tmrel <- rbind(tmrel, fill_rows)
    }
    tmrel <- tmrel[year_id >= YEAR_START & year_id <= YEAR_END]
    log_msg("Summary mode: loaded", nrow(tmrel), "TMREL rows covering",
            length(unique(tmrel$year_id)), "years")
  }
}

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
    if ("tmrelMean" %in% names(tmrel_summary)) {
      plot_cols <- c("zone", "year_id", "tmrelMean", "tmrelLower", "tmrelUpper")
      tmrel_summary <- tmrel_summary[, ..plot_cols]
      setnames(tmrel_summary, c("tmrelMean", "tmrelLower", "tmrelUpper"),
               c("tmrel_mean", "tmrel_lower", "tmrel_upper"))
    } else {
      # COLOMBIA_VERIFICATION mode: only tmrel_mean available, no CI
      tmrel_summary <- tmrel_summary[, .(zone, year_id, tmrel_mean)]
      tmrel_summary[, `:=`(tmrel_lower = tmrel_mean, tmrel_upper = tmrel_mean)]
    }
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
