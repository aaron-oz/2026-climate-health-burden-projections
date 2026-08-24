# 02_load_tmrel.R — Load TMREL data for a location
#
# Loads year-specific TMRELs for the given LOCATION_ID.
# Supports two modes:
#   USE_DRAWS = TRUE:  keeps all 1000 TMREL draws per zone/year
#   USE_DRAWS = FALSE: uses summary (mean) TMREL values
#
# Input:  TMREL_DIR/tmrel_{LOCATION_ID}.csv or tmrel_{LOCATION_ID}_summaries.csv
# Output: INTERMEDIATE_DIR/tmrel.rds

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))

library(data.table)

log_msg("Loading TMRELs for location", LOCATION_ID)

if (!TMREL_MODE %in% c("released_recycled", "derived_per_draw")) {
  stop("Unknown TMREL_MODE '", TMREL_MODE,
       "' (expected released_recycled or derived_per_draw)")
}
if (TMREL_MODE == "derived_per_draw" && !USE_DRAWS) {
  # Summary mode has no draw pairing to fix; it always uses the released
  # TMREL summaries, so the (default) derived mode simply does not apply.
  log_msg("Summary mode: TMREL_MODE = derived_per_draw applies to draw ",
          "mode only; using released TMREL summaries")
}

if (USE_DRAWS && TMREL_MODE == "derived_per_draw") {
  # ===========================================================================
  # Derive TMREL draw d as the argmin of ERF draw d's death-weighted RR curve
  # (IHME tmrelCalculator.R:126 definition), so each draw's reference is its
  # own curve minimum and rescaled curves never dip below 1 at the reference.
  # Search range 6.6-34.6 C (tmrelCalculator_launch.R), intersected with each
  # zone's modeled grid. Weights are the location's cause-death shares for
  # each study year: draw d's mortality shares weight ERF draw d when the
  # mortality input carries draws, else the year's point shares for all draws.
  # ===========================================================================
  mort_file <- file.path(INTERMEDIATE_DIR, "mortality.rds")
  if (!file.exists(mort_file))
    stop("derived_per_draw needs ", mort_file, " — run 04_load_mortality.R ",
         "first (run_location.R orders 04 ahead of 02)")
  mort <- setDT(readRDS(mort_file))
  if (nrow(mort) == 0 || sum(mort$deaths, na.rm = TRUE) <= 0)
    stop("derived_per_draw: the mortality input has no usable rows for ",
         "study years ", YEAR_START, "-", YEAR_END, " (04_load_mortality.R ",
         "filtered it to ", nrow(mort), " rows). The TMREL cause weights ",
         "cannot be formed, and burden would be empty anyway. Point ",
         "--mortality_file at data covering the study years.")

  has_mort_draws <- "draw" %in% names(mort)
  if (has_mort_draws) {
    cod <- mort[, .(deaths = sum(deaths, na.rm = TRUE)),
                by = .(year_id, acause, draw)]
    log_msg("Derived-TMREL weights: per-draw mortality shares (",
            uniqueN(cod$draw), " draws)")
  } else {
    cod <- mort[, .(deaths = sum(deaths, na.rm = TRUE)), by = .(year_id, acause)]
    log_msg("Derived-TMREL weights: point mortality shares (no mortality draws)")
  }
  years <- sort(unique(cod$year_id))
  missing_years <- setdiff(YEAR_START:YEAR_END, years)
  if (length(missing_years) > 0)
    log_msg("Derived-TMREL: mortality lacks years ",
            paste(missing_years, collapse = ","),
            " — nearest available year's weights are used for them")

  # ---------------------------------------------------------------------------
  # Per-(location, year) cache. Derived TMRELs depend only on the ERF draws,
  # the mortality weights, N_DRAWS, and the rounding flag — NOT on model or
  # scenario — so a production grid would otherwise re-derive the identical
  # TMRELs once per (model, scenario) combo. Each cache file stores the cod
  # weight rows it was derived from; a cache hit requires them to match the
  # current mortality input exactly, so a changed mortality file re-derives
  # rather than silently reusing stale TMRELs. Written atomically (concurrent
  # combos of one location may race here harmlessly).
  # ---------------------------------------------------------------------------
  cache_dir_tm <- file.path(TMREL_DIR, "derived_cache")
  dir.create(cache_dir_tm, showWarnings = FALSE, recursive = TRUE)
  cache_path <- function(yr) file.path(cache_dir_tm,
    sprintf("%d_%d_N%d%s.rds", LOCATION_ID, yr, N_DRAWS,
            if (isTRUE(TMREL_ROUND_WHOLE)) "_wholedeg" else ""))
  cod_for_year <- function(yr) {
    src_yr <- years[which.min(abs(years - yr))]
    setorderv(cod[year_id == src_yr], intersect(c("acause", "draw"), names(cod)))
  }
  cache_ok <- function(yr) {
    f <- cache_path(yr)
    if (!file.exists(f)) return(FALSE)
    stored <- tryCatch(readRDS(f), error = function(e) NULL)
    !is.null(stored) && isTRUE(all.equal(stored$cod, cod_for_year(yr),
                                         check.attributes = FALSE))
  }
  need_years <- Filter(function(yr) !cache_ok(yr), YEAR_START:YEAR_END)

  if (length(need_years) > 0) {
    erf_file <- file.path(INTERMEDIATE_DIR, "erf_curves.rds")
    if (!file.exists(erf_file))
      stop("derived_per_draw needs ", erf_file, " — run 01_load_erf.R first")
    erf <- setDT(readRDS(erf_file))  # long: zone, daily_temp (t10), acause, draw, rr
    SEARCH_MIN_10 <- 66L    # 6.6 C
    SEARCH_MAX_10 <- 346L   # 34.6 C
    erf_s <- erf[daily_temp >= SEARCH_MIN_10 & daily_temp <= SEARCH_MAX_10 &
                 zone >= TEMP_ZONE_MIN]
    rm(erf); invisible(gc(verbose = FALSE))

    draw_ids <- sort(unique(erf_s$draw))
    out <- vector("list", 0L)
    for (z in sort(unique(erf_s$zone))) {
      ez <- dcast(erf_s[zone == z], acause + daily_temp ~ draw, value.var = "rr")
      zcauses <- sort(unique(ez$acause))
      tvals   <- sort(unique(ez$daily_temp))
      dcols   <- as.character(draw_ids)
      # Per-cause (temps x draws) blocks on the zone's union grid; a cause
      # missing a grid point contributes RR = 1 there (neutral), matching the
      # replica instrument the fix was validated against.
      blocks <- lapply(zcauses, function(c_) {
        b <- as.matrix(ez[acause == c_][match(tvals, daily_temp), ..dcols])
        b[is.na(b)] <- 1
        b
      })
      for (yr in need_years) {
        wyr <- cod_for_year(yr)
        wpt <- wyr[, .(deaths = mean(deaths, na.rm = TRUE)), by = acause]
        wpt_v <- setNames(wpt$deaths, wpt$acause)[zcauses]
        wpt_v[is.na(wpt_v)] <- 0
        if (sum(wpt_v) <= 0)
          stop("Derived-TMREL: no deaths in any zone-", z, " cause for year ", yr)
        # cause x draw weight matrix, normalized within each draw over the
        # causes present in this zone
        wmat <- matrix(wpt_v, length(zcauses), length(draw_ids),
                       dimnames = list(zcauses, NULL))
        if (has_mort_draws) {
          wd <- wyr[acause %in% zcauses & draw %in% draw_ids]
          if (nrow(wd) > 0)
            wmat[cbind(match(wd$acause, zcauses), match(wd$draw, draw_ids))] <-
              wd$deaths
        }
        wmat <- sweep(wmat, 2, colSums(wmat), "/")
        W <- matrix(0, length(tvals), length(draw_ids))
        for (ci in seq_along(zcauses))
          W <- W + sweep(blocks[[ci]], 2, wmat[ci, ], "*")
        tm <- tvals[apply(W, 2, which.min)]
        if (isTRUE(TMREL_ROUND_WHOLE))
          tm <- pmin(pmax(10L * as.integer(round(tm / 10)), min(tvals)), max(tvals))
        out[[length(out) + 1]] <- data.table(zone = z, year_id = yr,
                                             draw = draw_ids, tmrel = tm)
      }
    }
    derived <- rbindlist(out)
    for (yr in need_years) {
      obj <- list(tmrel = derived[year_id == yr], cod = cod_for_year(yr))
      save_rds_atomic(obj, cache_path(yr))
    }
    log_msg("Derived-TMREL: derived ", length(need_years), " year(s), cached in ",
            cache_dir_tm)
  } else {
    log_msg("Derived-TMREL: all ", length(YEAR_START:YEAR_END),
            " year(s) served from cache (", cache_dir_tm, ")")
  }

  tmrel <- rbindlist(lapply(YEAR_START:YEAR_END,
                            function(yr) readRDS(cache_path(yr))$tmrel))
  tmrel[, location_id := LOCATION_ID]
  log_msg("Derived per-draw TMRELs: ", nrow(tmrel), " rows (",
          uniqueN(tmrel$zone), " zones x ", uniqueN(tmrel$year_id),
          " years x ", uniqueN(tmrel$draw), " draws); zone means [C]: ",
          paste(tmrel[, sprintf("%d:%.1f", zone[1], mean(tmrel) / 10),
                      by = zone]$V1, collapse = " "))

} else if (USE_DRAWS) {
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
