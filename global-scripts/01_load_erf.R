# 01_load_erf.R — Load and prepare Exposure-Response Function (ERF) curves
#
# Loads the 17 cause-specific ERF curve CSVs from Burkart et al. (2021).
# Supports two modes:
#   USE_DRAWS = TRUE:  keeps all 1000 draws for uncertainty propagation
#   USE_DRAWS = FALSE: computes summary statistics (mean, lower, upper)
#
# Input:  ERF_DIR/{cause}_curve_samples.csv (17 files, ~130MB each)
# Output: INTERMEDIATE_DIR/erf_curves.rds

source("config.R")

library(data.table)

# Cache key encodes the config that affects the saved object's schema.
# At cluster scale, build erf_curves once into a shared location and let
# subsequent workers reuse it — the CSV-read + log-RR-exponentiation takes
# ~90 s for 1000 draws and is the dominant per-process pipeline overhead.
mode_tag  <- if (USE_DRAWS) paste0("draws_N", N_DRAWS) else "summary"
cache_dir   <- file.path(ERF_DIR, "cache")
cache_file  <- file.path(cache_dir, paste0("erf_curves_", mode_tag, ".rds"))
temp_limits_cache <- file.path(cache_dir, paste0("temp_limits_", mode_tag, ".rds"))
target_erf    <- file.path(INTERMEDIATE_DIR, "erf_curves.rds")
target_limits <- file.path(INTERMEDIATE_DIR, "temp_limits.rds")

if (file.exists(cache_file) && file.exists(temp_limits_cache)) {
  log_msg("Cached ERF found at ", cache_file, "; skipping rebuild")
  if (!file.exists(target_erf) ||
      file.info(cache_file)$mtime > file.info(target_erf)$mtime) {
    file.copy(cache_file,        target_erf,    overwrite = TRUE)
    file.copy(temp_limits_cache, target_limits, overwrite = TRUE)
    log_msg("Copied cached ERF to ", target_erf)
  } else {
    log_msg("Intermediate ERF already current; nothing to copy")
  }
  # Skip diagnostic plots when reusing cache (they were generated at build
  # time); just exit cleanly so the rest of the pipeline can proceed.
  quit(save = "no")
}

log_msg("Loading ERF curves from", ERF_DIR, "(no cache; will populate ", cache_file, ")")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# Load all cause-specific curve files
erf <- rbindlist(lapply(GBD_CAUSES, function(cause) {
  f <- file.path(ERF_DIR, paste0(cause, "_curve_samples.csv"))
  if (!file.exists(f)) {
    warning(paste("ERF file not found:", f))
    return(NULL)
  }
  dt <- fread(f)
  dt[, acause := cause]
  dt
}))

log_msg("Loaded", nrow(erf), "rows across", length(GBD_CAUSES), "causes")

# Standardize column names
setnames(erf, "annual_temperature", "zone")
setnames(erf, "daily_temperature", "daily_temp")

# Convert daily temperature to integer * 10 (avoids floating point issues, per Burkart)
erf[, daily_temp := as.integer(round(daily_temp * 10))]
erf[, zone := as.integer(zone)]

# Draw columns
draw_cols <- paste0("draw_", 0:(N_DRAWS - 1))

if (USE_DRAWS) {
  # Exponentiate all draws (curves are stored in log-RR space)
  rr_cols <- paste0("rr_", 0:(N_DRAWS - 1))
  erf[, (rr_cols) := lapply(.SD, exp), .SDcols = draw_cols]
  erf[, (draw_cols) := NULL]

  # Reshape to long: one row per (zone, daily_temp, cause, draw)
  erf <- melt(erf,
              id.vars = c("zone", "daily_temp", "acause"),
              measure.vars = rr_cols,
              variable.name = "draw",
              value.name = "rr",
              variable.factor = FALSE)
  erf[, draw := as.integer(gsub("rr_", "", draw))]

  log_msg("Draw mode: reshaped to long format,", nrow(erf), "rows")

} else {
  # Summary mode: compute mean and quantiles, then exponentiate
  erf[, rr_mean  := exp(rowMeans(.SD)), .SDcols = draw_cols]
  erf[, rr_lower := exp(apply(.SD, 1, quantile, probs = 0.025)), .SDcols = draw_cols]
  erf[, rr_upper := exp(apply(.SD, 1, quantile, probs = 0.975)), .SDcols = draw_cols]
  erf[, rr_max   := exp(apply(.SD, 1, quantile, probs = 0.99)),  .SDcols = draw_cols]
  erf[, (draw_cols) := NULL]

  log_msg("Summary mode: computed mean/lower/upper/max RR")
}

# Store temperature limits per zone (for truncation in later scripts)
temp_limits <- erf[, .(min_temp = min(daily_temp), max_temp = max(daily_temp)), by = zone]

# Save to both the per-run intermediate path (consumed by 05 / 06) and the
# shared cache (reused by subsequent pipeline runs with the same N_DRAWS /
# USE_DRAWS configuration).
saveRDS(erf, target_erf)
saveRDS(temp_limits, target_limits)
saveRDS(erf, cache_file)
saveRDS(temp_limits, temp_limits_cache)

log_msg("ERF curves saved to ", target_erf,
        " and cached to ", cache_file)

# =============================================================================
# Diagnostic plots (if enabled)
# =============================================================================

if (RUN_DIAGNOSTICS) {
  library(ggplot2)
  log_msg("Generating ERF diagnostic plots")

  if (USE_DRAWS) {
    # Compute mean RR per zone/temp/cause for plotting
    plot_data <- erf[, .(rr_mean = mean(rr)), by = .(zone, daily_temp, acause)]
  } else {
    plot_data <- copy(erf)
  }

  # Plot curves for three representative zones
  plot_zones <- c(6, 14, 28)
  plot_data_sub <- plot_data[zone %in% plot_zones]
  plot_data_sub[, daily_temp_c := daily_temp / 10]  # convert back to Celsius for axis

  for (cause in unique(plot_data_sub$acause)) {
    p <- ggplot(plot_data_sub[acause == cause],
                aes(x = daily_temp_c, y = rr_mean, color = as.factor(zone))) +
      geom_line(linewidth = 0.5) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
      scale_color_manual(values = c("6" = "#1874CD", "14" = "#030303", "28" = "#CD2626")) +
      labs(x = "Daily mean temperature (°C)", y = "Relative Risk",
           color = "Mean annual\ntemp zone",
           title = paste("ERF:", cause)) +
      theme_minimal()

    ggsave(file.path(DIAGNOSTICS_DIR, paste0("erf_", cause, ".png")),
           p, width = 8, height = 5, dpi = 150)
  }

  log_msg("ERF diagnostic plots saved to", DIAGNOSTICS_DIR)
}
