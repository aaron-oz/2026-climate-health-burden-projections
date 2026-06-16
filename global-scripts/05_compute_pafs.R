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

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))

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
  log_msg("Running in DRAW mode (", N_DRAWS, " draws, cause-chunked)")

  # --- Aggregate temperature once (no draw / cause dim) ---
  # temp does not carry a draw dim unless temperature uncertainty is enabled
  # (USE_DRAWS && temp_sd in the input). RR/TMREL draws and cause dim are
  # broadcast in inside the per-cause loop below.
  temp_agg <- temp[, .(pr = sum(pr, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)),
                   by = .(subloc_id, zone, daily_temp_10, year)]

  # Process one cause at a time. Peak memory at 1000 draws scales with
  # (zones * daily_temps * subloc * year * draws) per chunk — ~17x smaller
  # than materializing all causes at once, fitting on memory-constrained
  # hosts. The intermediate per-cause PAFs are collapsed before rbinding.
  causes <- sort(unique(erf$acause))
  paf_per_cause <- vector("list", length(causes))

  # Inner draw-chunk schedule: split draws into batches of DRAW_CHUNK_SIZE so
  # the (zones * daily_temps * draws) merge inside each cause stays within a
  # smaller memory envelope. Per-chunk overhead is small (a few dcasts and
  # gc cycles). Setting DRAW_CHUNK_SIZE = N_DRAWS recovers the unchunked
  # (cause-only) behavior.
  chunk_size <- max(1L, min(N_DRAWS, as.integer(DRAW_CHUNK_SIZE)))
  draw_starts <- seq(0L, N_DRAWS - 1L, by = chunk_size)
  draw_chunks <- lapply(draw_starts, function(s) s:min(s + chunk_size - 1L, N_DRAWS - 1L))
  log_msg(sprintf("Cause + draw chunked: %d causes x %d draw-chunks of <= %d draws each",
                  length(causes), length(draw_chunks), chunk_size))

  for (i in seq_along(causes)) {
    cause <- causes[i]
    t0 <- Sys.time()
    log_msg(sprintf("  [%d/%d] cause: %s", i, length(causes), cause))

    erf_c <- erf[acause == cause]
    paf_per_draw_chunk <- vector("list", length(draw_chunks))

    for (j in seq_along(draw_chunks)) {
      draws_keep <- draw_chunks[[j]]
      erf_cd  <- erf_c[draw %in% draws_keep]
      tmrel_d <- tmrel[draw %in% draws_keep]

      # Merge RR(cause, draw-batch) with TMREL(draw-batch)
      rr_cd <- merge(erf_cd, tmrel_d[, .(zone, year_id, draw, tmrel)],
                     by = c("zone", "draw"), allow.cartesian = TRUE)

      # Rescale RR to 1.0 at the TMREL
      if (COLOMBIA_VERIFICATION) {
        # verification: skip rescaling (Samuel uses raw RR)
      } else {
        rr_cd[, rr_ref := sum(rr * (daily_temp == tmrel), na.rm = TRUE),
              by = .(zone, draw, year_id)]
        rr_cd[rr_ref > 0, rr := rr / rr_ref]
        rr_cd[, rr_ref := NULL]
      }

      pafs_cd <- merge(temp_agg, rr_cd,
                       by.x = c("zone", "daily_temp_10", "year"),
                       by.y = c("zone", "daily_temp",    "year_id"),
                       all.x = TRUE, allow.cartesian = TRUE)
      pafs_cd[, risk := ifelse(daily_temp_10 < tmrel, "cold",
                               ifelse(daily_temp_10 > tmrel, "heat", NA_character_))]
      pafs_cd <- pafs_cd[!is.na(risk)]

      if (COLOMBIA_VERIFICATION) {
        pafs_cd[, paf_contrib := pmax(0, fifelse(rr >= 1,
                                                 pr * (rr - 1) / rr,
                                                 pr * -1 * ((1/rr) - 1) / (1/rr)))]
        paf_per_draw_chunk[[j]] <- pafs_cd[, .(acause = cause,
                                               paf = sum(paf_contrib, na.rm = TRUE)),
                                           by = .(risk, subloc_id, draw, year)]
      } else {
        paf_per_draw_chunk[[j]] <- pafs_cd[, .(acause = cause,
                                               paf = sum(ifelse(rr >= 1,
                                                                pr * (rr - 1) / rr,
                                                                pr * -1 * ((1/rr) - 1) / (1/rr)),
                                                         na.rm = TRUE)),
                                           by = .(risk, subloc_id, draw, year)]
      }

      rm(erf_cd, tmrel_d, rr_cd, pafs_cd)
      invisible(gc(verbose = FALSE))
    }

    paf_per_cause[[i]] <- rbindlist(paf_per_draw_chunk)
    rm(erf_c, paf_per_draw_chunk)
    invisible(gc(verbose = FALSE))
    log_msg(sprintf("    chunk done in %.1fs", as.numeric(Sys.time() - t0, units = "secs")))
  }

  if (COLOMBIA_VERIFICATION) {
    log_msg("COLOMBIA_VERIFICATION: PAF contributions floored at 0 per row before summing")
  }

  paf_results_draws <- rbindlist(paf_per_cause)
  rm(paf_per_cause); invisible(gc(verbose = FALSE))

  # Reshape draws to wide and summarize per (year, subloc, cause, risk)
  paf_wide <- dcast(paf_results_draws,
                    year + subloc_id + acause + risk ~ paste0("draw_", draw),
                    value.var = "paf")
  draw_cols <- paste0("draw_", 0:(N_DRAWS - 1))
  paf_wide[, paf_mean  := rowMeans(.SD, na.rm = TRUE), .SDcols = draw_cols]
  paf_wide[, paf_lower := apply(.SD, 1, quantile, 0.025, na.rm = TRUE), .SDcols = draw_cols]
  paf_wide[, paf_upper := apply(.SD, 1, quantile, 0.975, na.rm = TRUE), .SDcols = draw_cols]

  # Hand downstream code a frame keyed the same way summary mode does, with
  # paf_mean (and lower/upper for diagnostics). The per-draw long form remains
  # in paf_results_draws for per-draw burden computation below.
  paf_results <- paf_wide[, .(year, subloc_id, acause, risk,
                              paf_mean, paf_lower, paf_upper)]

  log_msg("Draw-level PAFs computed (cause-chunked)")

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
#
# Two paths:
#   (a) Production / annual: deaths_annual * paf_annual [* sev in verif].
#       PAFs are computed at (year, subloc, cause); deaths at full
#       (year, subloc, age, sex, cause); the merge broadcasts paf across age
#       and sex within each (year, subloc, cause).
#   (b) COLOMBIA_VERIFICATION daily branch (depto-day): computes daily PAFs at
#       (date, subloc, cause, risk), merges with daily mortality at
#       (date, subloc, age, sex, cause), multiplies by annual SEV, and sums
#       days to annual burden. This matches Samuel's pattern in
#       11_carga_atribuible.R, where attribution is daily so days with both
#       extreme temperature AND high mortality contribute proportionally
#       more than the annual-paf approximation captures.

do_daily_verif <- COLOMBIA_VERIFICATION && !USE_DRAWS &&
  file.exists(file.path(INTERMEDIATE_DIR, "mortality_daily.rds"))

if (do_daily_verif) {
  # ---------------------------------------------------------------------------
  # COLOMBIA_VERIFICATION daily branch (summary-mode RR only)
  # ---------------------------------------------------------------------------
  log_msg("COLOMBIA_VERIFICATION daily branch: computing depto-day PAFs and burden")

  # (1) Aggregate raw temperature to (date, subloc, zone, daily_temp_10, year);
  # within-(date, subloc) pop fractions become the daily PAF weights.
  temp_daily <- temp[, .(pop = sum(pop, na.rm = TRUE)),
                     by = .(date, subloc_id, year, zone, daily_temp_10)]
  temp_daily[, pop_subloc_date := sum(pop, na.rm = TRUE),
             by = .(date, subloc_id)]
  temp_daily[, pr_daily := pop / pop_subloc_date]

  # (2) Merge with the (already-rescaled-or-not) summary-mode RR. `rr` carries
  # tmrel_mean_10 from earlier, so heat/cold classification is direct.
  pafs_d <- merge(temp_daily, rr,
                  by.x = c("zone", "daily_temp_10", "year"),
                  by.y = c("zone", "daily_temp",   "year"),
                  all.x = TRUE, allow.cartesian = TRUE)

  pafs_d[, risk := ifelse(daily_temp_10 < tmrel_mean_10, "cold",
                          ifelse(daily_temp_10 > tmrel_mean_10, "heat", NA_character_))]
  pafs_d <- pafs_d[!is.na(risk) & !is.na(rr_mean)]

  # (3) Per-row PAF contribution, floored at 0 (Samuel's per-row floor).
  pafs_d[, paf_contrib := pmax(0, fifelse(rr_mean >= 1,
                                          pr_daily * (rr_mean - 1) / rr_mean,
                                          pr_daily * -1 * ((1/rr_mean) - 1) / (1/rr_mean)))]

  # (4) Sum within (date, subloc, cause, risk) → daily PAFs.
  paf_daily_long <- pafs_d[, .(paf = sum(paf_contrib, na.rm = TRUE)),
                           by = .(date, year, subloc_id, acause, risk)]

  paf_daily <- dcast(paf_daily_long,
                     date + year + subloc_id + acause ~ risk,
                     value.var = "paf", fill = 0)
  if (!"heat" %in% names(paf_daily)) paf_daily[, heat := 0]
  if (!"cold" %in% names(paf_daily)) paf_daily[, cold := 0]
  setnames(paf_daily, c("heat", "cold"), c("paf_heat", "paf_cold"))
  paf_daily[, paf_nonopt := paf_heat + paf_cold]
  paf_daily[, location_id := LOCATION_ID]

  saveRDS(paf_daily, file.path(RESULTS_DIR, paste0("pafs_daily_", LOCATION_ID, ".rds")))
  log_msg("Daily PAFs saved to ",
          file.path(RESULTS_DIR, paste0("pafs_daily_", LOCATION_ID, ".rds")),
          " (", nrow(paf_daily), " rows at date x subloc x cause)")

  # (5) Merge daily PAFs with daily mortality at (date, subloc, cause). The
  # PAF row is broadcast across all (age, sex) deaths on that day-depto-cause.
  mort_daily <- readRDS(file.path(INTERMEDIATE_DIR, "mortality_daily.rds"))
  setDT(mort_daily)
  mort_daily[, date := as.Date(date)]

  burden_daily <- merge(mort_daily[, .(date, year_id, subloc_id, age_group_id,
                                       sex_id, acause, deaths)],
                        paf_daily[, .(date, subloc_id, acause,
                                      paf_heat, paf_cold, paf_nonopt)],
                        by = c("date", "subloc_id", "acause"),
                        all.x = TRUE)
  burden_daily[is.na(paf_heat),   paf_heat   := 0]
  burden_daily[is.na(paf_cold),   paf_cold   := 0]
  burden_daily[is.na(paf_nonopt), paf_nonopt := 0]

  # (6) Apply annual SEV at (year, subloc, cause).
  sev_file <- file.path(RESULTS_DIR, paste0("sevs_", LOCATION_ID, ".rds"))
  if (!file.exists(sev_file)) {
    stop("COLOMBIA_VERIFICATION daily branch: SEV file not found at ", sev_file,
         ". Run 06_compute_sevs.R before 05_compute_pafs.R in verification mode.")
  }
  sevs_annual <- setDT(readRDS(sev_file))
  burden_daily <- merge(burden_daily,
                        sevs_annual[, .(year, subloc_id, acause, sev)],
                        by.x = c("year_id", "subloc_id", "acause"),
                        by.y = c("year",    "subloc_id", "acause"),
                        all.x = TRUE)
  burden_daily[is.na(sev), sev := 0]
  burden_daily[, `:=`(deaths_heat   = deaths * paf_heat   * sev,
                      deaths_cold   = deaths * paf_cold   * sev,
                      deaths_nonopt = deaths * paf_nonopt * sev)]

  # (7) Sum days → annual at (year, subloc, age, sex, cause). Schema matches
  # the production/annual burden output (07/08 only consume deaths_*).
  burden <- burden_daily[, .(deaths        = sum(deaths,        na.rm = TRUE),
                             deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
                             deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
                             deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                         by = .(year_id, subloc_id, age_group_id, sex_id, acause)]

  # Carry annual paf_combined values as diagnostic columns (the daily-rollup
  # implied effective PAF differs from these but the annual values are still
  # useful for cross-checks against pafs_<id>.rds).
  burden <- merge(burden,
                  paf_combined[, .(year, subloc_id, acause,
                                   paf_heat, paf_cold, paf_nonopt)],
                  by.x = c("year_id", "subloc_id", "acause"),
                  by.y = c("year",    "subloc_id", "acause"),
                  all.x = TRUE)
  burden <- merge(burden,
                  sevs_annual[, .(year, subloc_id, acause, sev)],
                  by.x = c("year_id", "subloc_id", "acause"),
                  by.y = c("year",    "subloc_id", "acause"),
                  all.x = TRUE)
  burden[is.na(paf_heat),   paf_heat   := 0]
  burden[is.na(paf_cold),   paf_cold   := 0]
  burden[is.na(paf_nonopt), paf_nonopt := 0]
  burden[is.na(sev),        sev        := 0]

  setnames(burden, "year_id", "year")
  burden[, location_id := LOCATION_ID]
  saveRDS(burden, file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))
  log_msg("Attributable burden (daily-rolled-up) saved to ",
          file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")),
          " (", nrow(burden), " rows at year x subloc x age x sex x cause)")
} else {
  # ---------------------------------------------------------------------------
  # Annual branch (production + verification fallback when daily mort missing)
  # ---------------------------------------------------------------------------
  # If mort carries per-draw rows (from IHME rate * pop per draw), collapse
  # to per-row mean for the summary-burden path so the merge with paf_combined
  # (one row per year/subloc/cause) doesn't cartesian-explode. The per-draw
  # rows are retained in `mort_full` for per-draw burden computation below.
  has_mort_draws <- "draw" %in% names(mort)
  if (has_mort_draws) {
    mort_full <- mort
    mort_for_summary <- mort[, .(deaths = mean(deaths, na.rm = TRUE)),
                             by = .(year_id, subloc_id, age_group_id, sex_id, acause)]
  } else {
    mort_full <- NULL
    mort_for_summary <- mort[, .(year_id, subloc_id, age_group_id, sex_id,
                                  acause, deaths)]
  }

  burden <- merge(mort_for_summary,
                  paf_combined[, .(year, subloc_id, acause, paf_heat, paf_cold, paf_nonopt)],
                  by.x = c("year_id", "subloc_id", "acause"),
                  by.y = c("year", "subloc_id", "acause"),
                  all.x = TRUE)
  # Guard against a silent resolution mismatch. If NO mortality row matched any
  # PAF row, every paf is NA, gets floored to 0 below, and the burden would be a
  # silent zero. This happens when subnational temperature/PAFs meet national
  # mortality (or vice versa). Fail loudly with the fix.
  if (nrow(burden) > 0 && all(is.na(burden$paf_nonopt))) {
    stop("Resolution mismatch: no PAF matched any mortality row. ",
         "Temperature/PAF subloc_ids {",
         paste(head(unique(as.character(paf_combined$subloc_id))), collapse = ","),
         "...} do not overlap mortality subloc_ids {",
         paste(head(unique(as.character(mort_for_summary$subloc_id))), collapse = ","),
         "...}. For national mortality run step 6a with --subnational=FALSE; ",
         "for subnational mortality run it with --subnational=TRUE.")
  }
  burden[is.na(paf_heat),   paf_heat   := 0]
  burden[is.na(paf_cold),   paf_cold   := 0]
  burden[is.na(paf_nonopt), paf_nonopt := 0]

  if (COLOMBIA_VERIFICATION) {
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

  # ---------------------------------------------------------------------------
  # Per-draw burden when both mortality and PAFs have per-draw rows
  # ---------------------------------------------------------------------------
  # We pair draws 1:1 between mortality and PAFs. Rate and PAF draws are
  # independent (IHME's rate draws vs our pipeline's ERF/TMREL draws), so
  # matched pairing is valid: the mean of the product equals the product of
  # the means (independent factors), and the variance reflects both sources
  # of uncertainty propagated together.
  #
  # If both inputs have draws, the canonical burden_<LOC>.rds carries a
  # `draw` column. Downstream (07/08) detects the column and either rolls
  # up to summary or carries draws through. Summaries can always be derived
  # from draws; the reverse is not true.
  if (USE_DRAWS && exists("paf_results_draws") && !is.null(mort_full) &&
      !COLOMBIA_VERIFICATION) {
    log_msg("Computing per-draw burden (mort_draws x paf_draws)")

    # paf_results_draws is long: (acause, risk, subloc_id, draw, year, paf)
    paf_combined_draws <- dcast(paf_results_draws,
                                year + subloc_id + acause + draw ~ risk,
                                value.var = "paf", fill = 0)
    if (!"heat" %in% names(paf_combined_draws)) paf_combined_draws[, heat := 0]
    if (!"cold" %in% names(paf_combined_draws)) paf_combined_draws[, cold := 0]
    setnames(paf_combined_draws, c("heat", "cold"), c("paf_heat", "paf_cold"))
    paf_combined_draws[, paf_nonopt := paf_heat + paf_cold]

    burden <- merge(
      mort_full[, .(year_id, subloc_id, age_group_id, sex_id, acause, draw, deaths)],
      paf_combined_draws[, .(year, subloc_id, acause, draw,
                             paf_heat, paf_cold, paf_nonopt)],
      by.x = c("year_id", "subloc_id", "acause", "draw"),
      by.y = c("year",    "subloc_id", "acause", "draw"),
      all.x = TRUE)
    burden[is.na(paf_heat),   paf_heat   := 0]
    burden[is.na(paf_cold),   paf_cold   := 0]
    burden[is.na(paf_nonopt), paf_nonopt := 0]
    burden[, `:=`(deaths_heat   = deaths * paf_heat,
                  deaths_cold   = deaths * paf_cold,
                  deaths_nonopt = deaths * paf_nonopt)]
    setnames(burden, "year_id", "year")
    burden[, location_id := LOCATION_ID]

    saveRDS(burden, file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))
    log_msg("Per-draw burden saved (", nrow(burden), " rows, ",
            uniqueN(burden$draw), " draws) -> ",
            file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))
  } else {
    # Summary path (no per-draw mort, or in verification mode)
    setnames(burden, "year_id", "year")
    burden[, location_id := LOCATION_ID]
    saveRDS(burden, file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")))
    log_msg("Attributable burden saved to ",
            file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds")),
            " (", nrow(burden), " rows at year x subloc x age x sex x cause)")
  }
}

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
