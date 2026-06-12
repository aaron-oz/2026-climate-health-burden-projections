# util_trace_pipeline.R — Walk a small set of pixels and one country
# aggregate through every stage of the burden pipeline, capturing
# intermediate values for hand-verification.
#
# Purpose: end-to-end correctness sanity check + methodology documentation.
# Lets a reader trace a single pixel's daily temperature → zone → RR lookup
# → TMREL → heat/cold classification → PAF contribution → aggregated PAF →
# attributable deaths, against the pipeline's own output, so that any
# off-by-one / wrong-index / lookup-mismatch bugs surface visibly.
#
# Reads only the pipeline's saved intermediate / results RDS files; does NOT
# re-run the pipeline. So this is fast (seconds) and runs against any
# already-computed pipeline output.
#
# Usage:
#   Rscript util_trace_pipeline.R \
#     --pixel_ids=494315,544743,580753 \
#     --year=2010 \
#     --causes=cvd_ihd,cvd_stroke \
#     [--draws_summary=summary|draws]   # default: summary mode
#     [--output=output/trace_report.md]
#
# Pick pixels with contrasting climate exposure to make the trace
# illuminating — e.g. high-altitude cool, lowland tropical hot, coastal
# moderate. Bogotá-area / Caribbean / Amazon trio for Colombia is a good
# default set.

source("config.R")
suppressPackageStartupMessages(library(data.table))

defaults <- list(
  PIXEL_IDS      = "",
  YEAR           = 2010L,
  CAUSES         = paste(GBD_CAUSES, collapse = ","),
  DRAWS_SUMMARY  = "summary",
  OUTPUT         = file.path(OUTPUT_DIR, "trace_report.md"),
  TEMP_INTERMEDIATE  = file.path(INTERMEDIATE_DIR, "temperature.rds"),
  ERF_INTERMEDIATE   = file.path(INTERMEDIATE_DIR, "erf_curves.rds"),
  TMREL_INTERMEDIATE = file.path(INTERMEDIATE_DIR, "tmrel.rds"),
  MORT_INTERMEDIATE  = file.path(INTERMEDIATE_DIR, "mortality.rds"),
  PAFS_RESULTS       = file.path(RESULTS_DIR, paste0("pafs_",   LOCATION_ID, ".rds")),
  BURDEN_RESULTS     = file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds"))
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

# =============================================================================
# Helpers
# =============================================================================

fmt_pct <- function(x) sprintf("%.4f%%", 100 * x)
fmt_num <- function(x) format(x, big.mark = ",", scientific = FALSE)

# A draws-aware summarizer: returns a single string with mean and [p2.5, p97.5]
# if the column carries draws, or a single value otherwise.
draws_summary <- function(values) {
  v <- values[!is.na(values)]
  if (length(v) == 0) return("NA")
  if (length(v) == 1) return(sprintf("%.6g", v))
  q <- quantile(v, c(0.025, 0.5, 0.975))
  sprintf("mean=%.6g [p2.5=%.6g, p50=%.6g, p97.5=%.6g, n=%d]",
          mean(v), q[1], q[2], q[3], length(v))
}

write_section <- function(con, title) {
  cat("\n## ", title, "\n\n", sep = "", file = con)
}

write_kv <- function(con, key, value) {
  cat("- **", key, ":** ", value, "\n", sep = "", file = con)
}

write_table_header <- function(con, cols) {
  cat("| ", paste(cols, collapse = " | "), " |\n", sep = "", file = con)
  cat("|", paste(rep("---", length(cols)), collapse = "|"), "|\n", sep = "", file = con)
}

write_table_row <- function(con, vals) {
  cat("| ", paste(vals, collapse = " | "), " |\n", sep = "", file = con)
}

# =============================================================================
# Main trace
# =============================================================================

trace_pipeline <- function() {
  pixel_ids <- as.integer(strsplit(as.character(PIXEL_IDS), ",", fixed = TRUE)[[1]])
  pixel_ids <- pixel_ids[!is.na(pixel_ids)]
  if (length(pixel_ids) == 0) {
    stop("No pixel_ids provided. Use --pixel_ids=p1,p2,p3")
  }
  year      <- as.integer(YEAR)
  causes    <- strsplit(as.character(CAUSES), ",", fixed = TRUE)[[1]]
  is_draws  <- identical(as.character(DRAWS_SUMMARY), "draws")

  log_msg("Trace report:")
  log_msg("  pixels: ", paste(pixel_ids, collapse = ","))
  log_msg("  year:   ", year)
  log_msg("  causes: ", paste(causes, collapse = ","))
  log_msg("  mode:   ", DRAWS_SUMMARY)
  log_msg("  output: ", OUTPUT)

  # Load
  temp   <- setDT(readRDS(TEMP_INTERMEDIATE))
  erf    <- setDT(readRDS(ERF_INTERMEDIATE))
  tmrel  <- setDT(readRDS(TMREL_INTERMEDIATE))
  mort   <- setDT(readRDS(MORT_INTERMEDIATE))
  pafs   <- setDT(readRDS(PAFS_RESULTS))
  burden <- setDT(readRDS(BURDEN_RESULTS))

  dir.create(dirname(OUTPUT), showWarnings = FALSE, recursive = TRUE)
  con <- file(OUTPUT, "w")
  on.exit(close(con))

  cat("# Pipeline trace report\n\n", file = con)
  write_kv(con, "Location ID", LOCATION_ID)
  write_kv(con, "Year",        year)
  write_kv(con, "Mode",        DRAWS_SUMMARY)
  write_kv(con, "Pixels",      paste(pixel_ids, collapse = ", "))
  write_kv(con, "Causes",      paste(causes, collapse = ", "))
  write_kv(con, "Generated",   format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))

  # ==========================================================================
  # Per-pixel trace: temperature → zone → daily_temp_10 → pop fraction
  # ==========================================================================
  write_section(con, "1. Pixel-level inputs and zone assignment")
  cat("For each traced pixel, summary across all days in the year:\n\n", file = con)
  write_table_header(con,
    c("pixel_id", "subloc_id", "zone", "annual mean (°C)",
      "min/max daily_temp_10", "pop", "n days"))

  pixel_summary <- temp[year == YEAR & pixel_id %in% pixel_ids,
                        .(subloc_id = first(subloc_id),
                          zone      = first(zone),
                          annual_mean_c = mean(daily_temp, na.rm = TRUE),
                          min_dt10  = min(daily_temp_10, na.rm = TRUE),
                          max_dt10  = max(daily_temp_10, na.rm = TRUE),
                          pop       = first(pop),
                          n_days    = .N),
                        by = pixel_id]
  for (i in seq_len(nrow(pixel_summary))) {
    p <- pixel_summary[i]
    write_table_row(con,
      c(p$pixel_id, p$subloc_id, p$zone, sprintf("%.2f", p$annual_mean_c),
        sprintf("%d / %d", p$min_dt10, p$max_dt10),
        fmt_num(round(p$pop)), p$n_days))
  }

  # ==========================================================================
  # Per-pixel-cause trace: RR lookup + TMREL + classification + PAF contrib
  # ==========================================================================
  write_section(con, "2. Per-pixel × per-cause math (one representative day)")
  cat("For each pixel × cause, we report values for a single representative",
      "day chosen as the day closest to the pixel's annual median daily_temp_10.",
      "This gives a typical-case trace rather than an extreme.\n\n",
      file = con)

  is_draws_erf <- "draw" %in% names(erf)
  rr_col       <- if (is_draws_erf) "rr" else "rr_mean"

  for (pid in pixel_ids) {
    pix_days <- temp[year == YEAR & pixel_id == pid]
    if (nrow(pix_days) == 0) next
    median_dt10 <- as.integer(median(pix_days$daily_temp_10, na.rm = TRUE))
    rep_day <- pix_days[which.min(abs(daily_temp_10 - median_dt10))[1]]
    # Local names prefixed `trace_*` to avoid data.table column-name collisions
    # in the i-clauses below; data.table's `..` prefix only works in j.
    trace_zone <- rep_day$zone
    trace_dt10 <- rep_day$daily_temp_10
    trace_pr   <- rep_day$pr
    trace_year <- year

    write_section(con,
      sprintf("2.%d Pixel %d on %s — zone=%d, daily_temp_10=%d (= %.1f °C)",
              which(pixel_ids == pid), pid, as.character(rep_day$date),
              trace_zone, trace_dt10, trace_dt10 / 10))
    write_kv(con, "Within-subloc pop fraction (pr)", sprintf("%.6f", trace_pr))

    write_table_header(con,
      c("cause", "RR (from ERF curve)", "TMREL", "risk",
        "PAF contribution = pr × (RR-1)/RR"))

    for (cause in causes) {
      erf_row <- erf[zone == trace_zone & daily_temp == trace_dt10 &
                       acause == cause]
      tmrel_row <- tmrel[zone == trace_zone & year_id == trace_year]
      if (nrow(erf_row) == 0 || nrow(tmrel_row) == 0) {
        write_table_row(con, c(cause, "NA (no ERF/TMREL match)", "—", "—", "—"))
        next
      }
      rr_vals    <- erf_row[[rr_col]]
      tmrel_vals <- if ("tmrel" %in% names(tmrel_row)) tmrel_row$tmrel else tmrel_row$tmrel_mean_10
      rr_summary <- draws_summary(rr_vals)
      tm_summary <- draws_summary(tmrel_vals)

      mean_tm <- mean(tmrel_vals, na.rm = TRUE)
      risk <- if (trace_dt10 < mean_tm) "cold" else if (trace_dt10 > mean_tm) "heat" else "at-tmrel"
      mean_rr <- mean(rr_vals, na.rm = TRUE)
      paf_contrib <- if (!is.na(mean_rr) && mean_rr > 0) trace_pr * (mean_rr - 1) / mean_rr else NA_real_

      write_table_row(con,
        c(cause, rr_summary, tm_summary, risk,
          if (is.na(paf_contrib)) "NA" else sprintf("%.6g", paf_contrib)))
    }
  }

  # ==========================================================================
  # Country-aggregate trace: PAFs and burden at country level
  # ==========================================================================
  write_section(con, "3. Country-aggregate PAFs (from pafs_<id>.rds)")
  pafs_yr <- pafs[year == YEAR & acause %in% causes]
  if (nrow(pafs_yr) == 0) {
    cat("(no PAF rows match)\n", file = con)
  } else {
    # Mortality-weighted aggregate across subloc (so country-level number)
    deaths_by <- mort[year_id == YEAR & acause %in% causes,
                      .(deaths = sum(deaths, na.rm = TRUE)),
                      by = .(subloc_id, acause)]
    paf_natl <- merge(pafs_yr, deaths_by, by = c("subloc_id", "acause"),
                      all.x = TRUE)
    paf_natl[is.na(deaths), deaths := 0]
    natl <- paf_natl[, .(paf_heat = sum(paf_heat * deaths, na.rm = TRUE) /
                                     pmax(sum(deaths, na.rm = TRUE), 1),
                         paf_cold = sum(paf_cold * deaths, na.rm = TRUE) /
                                     pmax(sum(deaths, na.rm = TRUE), 1)),
                     by = acause]
    write_table_header(con, c("cause", "country PAF heat", "country PAF cold",
                              "country PAF non-opt"))
    for (i in seq_len(nrow(natl))) {
      r <- natl[i]
      write_table_row(con, c(r$acause, fmt_pct(r$paf_heat),
                             fmt_pct(r$paf_cold),
                             fmt_pct(r$paf_heat + r$paf_cold)))
    }
  }

  # ==========================================================================
  # Country burden totals (from burden_<id>.rds)
  # ==========================================================================
  # If burden carries a draw column (per-draw output from 05), we sum within
  # draw and then take mean across draws. Plain sum across the whole frame
  # would double-count by N_DRAWS.
  write_section(con, "4. Country burden totals (from burden_<id>.rds)")
  has_draws <- "draw" %in% names(burden)
  burden_yr <- burden[year == YEAR & acause %in% causes]
  if (has_draws) {
    per_draw <- burden_yr[, .(deaths        = sum(deaths,        na.rm = TRUE),
                              deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
                              deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
                              deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                          by = .(acause, draw)]
    totals <- per_draw[, .(deaths        = mean(deaths,        na.rm = TRUE),
                           deaths_heat   = mean(deaths_heat,   na.rm = TRUE),
                           deaths_cold   = mean(deaths_cold,   na.rm = TRUE),
                           deaths_nonopt = mean(deaths_nonopt, na.rm = TRUE)),
                       by = acause]
  } else {
    totals <- burden_yr[, .(deaths        = sum(deaths,        na.rm = TRUE),
                            deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
                            deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
                            deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                        by = acause]
  }
  if (has_draws) {
    cat("(Burden carries draws; values below are mean across draws of the within-draw sum.)\n\n",
        file = con)
  }
  write_table_header(con, c("cause", "total deaths (pipeline input)",
                            "heat attributable", "cold attributable",
                            "non-optimal attributable"))
  for (i in seq_len(nrow(totals))) {
    r <- totals[i]
    write_table_row(con,
      c(r$acause, fmt_num(round(r$deaths)),
        fmt_num(round(r$deaths_heat)), fmt_num(round(r$deaths_cold)),
        fmt_num(round(r$deaths_nonopt))))
  }

  write_section(con, "5. Sanity checks")
  burden_yr_all <- burden[year == YEAR]
  if (has_draws) {
    per_draw_all <- burden_yr_all[, .(deaths        = sum(deaths,        na.rm = TRUE),
                                      deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
                                      deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
                                      deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                                  by = draw]
    all_totals <- per_draw_all[, .(deaths        = mean(deaths,        na.rm = TRUE),
                                   deaths_heat   = mean(deaths_heat,   na.rm = TRUE),
                                   deaths_cold   = mean(deaths_cold,   na.rm = TRUE),
                                   deaths_nonopt = mean(deaths_nonopt, na.rm = TRUE))]
  } else {
    all_totals <- burden_yr_all[, .(deaths        = sum(deaths,        na.rm = TRUE),
                                    deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
                                    deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
                                    deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE))]
  }
  consistency_err <- abs(all_totals$deaths_heat + all_totals$deaths_cold -
                           all_totals$deaths_nonopt)
  write_kv(con, "Sum(heat) + Sum(cold) matches Sum(non-opt)",
           if (consistency_err < 1) "YES (within rounding)" else
             sprintf("NO (off by %.2f)", consistency_err))
  write_kv(con, "Country-aggregate non-opt deaths (all causes in burden)",
           fmt_num(round(all_totals$deaths_nonopt)))
  write_kv(con, "Country deaths (pipeline mortality input, all causes)",
           fmt_num(round(all_totals$deaths)))

  invisible(NULL)
}

if (sys.nframe() == 0L) trace_pipeline()
