# util_workflow_b_ratio.R — Apply the Workflow B cross-scenario ratio to
# IHME-derived mortality counts and return attributable burden under scenario X.
#
# Reference: gbd/ihme-plan-b-prep.tex, "Workflow B: self-consistent ratio".
#
# Math, per cause:
#
#   For causes in CAUSES_IHME_TEMP_SCALED (IHME applied a temp-scalar to the
#   forecast for these):
#     scale_factor(c, l, t) = S_X(c, l, t) / S_SSP2(c, l, t)
#     m_X(c, l, a, s, t)    = m_IHME(c, l, a, s, t) * scale_factor(c, l, t)
#     deaths_heat_X         = m_X * paf_heat_X
#     deaths_cold_X         = m_X * paf_cold_X
#
#   For causes in CAUSES_IHME_NOT_TEMP_SCALED (IHME did not apply a temp
#   scalar), Workflow B's ratio is the identity (S_SSP2 = 1 conceptually
#   because there's no IHME temp signal to cancel), so:
#     scale_factor = 1
#     deaths_heat_X         = m_IHME * paf_heat_X
#     deaths_cold_X         = m_IHME * paf_cold_X
#
# S_Y is the pipeline's pop-weighted attributable-PAF aggregate at
# (year, subloc, cause) under scenario Y. The ratio cancels pipeline-specific
# calibration drift (different ERFs / bias correction / pop recipe than IHME)
# for the temp-scaled causes -- both numerator and denominator come from the
# same pipeline and the constant factor drops out.
#
# Heat and cold attributable burden are reported separately. Users can sum
# them downstream for non-optimal total; we keep them split because that's
# the more flexible reporting shape.
#
# Inputs:
#   --ref_burden=PATH        Pipeline burden output(s) for the reference
#                            scenario (SSP2-RCP4.5). RDS; comma-separated
#                            list supported (concatenated).
#   --target_burden=PATH     Same for the target scenario X.
#   --ihme_mortality=PATH    IHME-derived counts at (year, age, sex, acause)
#                            granularity. Output of util_convert_ihme_forecast.R.
#                            Comma-separated list supported (per-cause files).
#   --output=PATH            Where to save the Workflow B attributable
#                            burden RDS.
#
# Burden file schema (from 05_compute_pafs.R):
#   year, subloc_id, age_group_id, sex_id, acause, deaths, paf_heat, paf_cold,
#   paf_nonopt, deaths_heat, deaths_cold, deaths_nonopt, location_id

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(
  REF_BURDEN     = NULL,
  TARGET_BURDEN  = NULL,
  IHME_MORTALITY = NULL,
  OUTPUT         = NULL
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) {
    assign(k, defaults[[k]], envir = globalenv())
  }
}

load_rds_list <- function(paths_csv) {
  paths <- strsplit(as.character(paths_csv), ",", fixed = TRUE)[[1]]
  parts <- lapply(paths, function(p) {
    if (!file.exists(p)) stop(paste("File not found:", p))
    setDT(readRDS(p))
  })
  rbindlist(parts, use.names = TRUE, fill = TRUE)
}

# Pipeline-side S: pop-weighted attributable-PAF aggregate at (year, subloc,
# cause). Collapse age × sex from the burden frame because PAF is broadcast
# across them in the pipeline. We use sum(deaths_nonopt) / sum(deaths) so the
# aggregate is mortality-weighted across age/sex (matches what the pipeline
# would produce if you computed PAF at the country level directly).
compute_S <- function(b) {
  keys <- c("year", "subloc_id", "acause")
  if ("draw" %in% names(b)) keys <- c(keys, "draw")   # per-draw when available
  agg <- b[, .(num = sum(deaths_nonopt, na.rm = TRUE),
               den = sum(deaths,        na.rm = TRUE)), by = keys]
  agg[, S := num / pmax(den, 1)]
  agg[, .SD, .SDcols = c(keys, "S")]
}

# Per-cause PAFs (heat / cold) from the target burden, at (year, subloc,
# cause). The pipeline broadcasts these across age/sex, so first() within a
# group recovers the (year, subloc, cause) value.
compute_pafs <- function(b) {
  keys <- c("year", "subloc_id", "acause")
  if ("draw" %in% names(b)) keys <- c(keys, "draw")   # per-draw PAF (broadcast across age/sex within a draw)
  b[, .(paf_heat = first(paf_heat),
        paf_cold = first(paf_cold)),
    by = keys]
}

apply_ratio <- function(ref_burden     = REF_BURDEN,
                        target_burden  = TARGET_BURDEN,
                        ihme_mortality = IHME_MORTALITY,
                        output         = OUTPUT,
                        location_id    = LOCATION_ID,
                        verbose        = TRUE) {
  if (is.null(ref_burden) || is.null(target_burden) ||
      is.null(ihme_mortality) || is.null(output)) {
    stop("Required: ref_burden, target_burden, ihme_mortality, output")
  }

  if (verbose) log_msg("Loading reference-scenario burden(s)")
  ref <- load_rds_list(ref_burden)
  if (verbose) log_msg("Loading target-scenario burden(s)")
  tgt <- load_rds_list(target_burden)
  if (verbose) log_msg("Loading IHME mortality file(s)")
  ihme <- load_rds_list(ihme_mortality)

  S_ref <- compute_S(ref); setnames(S_ref, "S", "S_ref")
  S_tgt <- compute_S(tgt); setnames(S_tgt, "S", "S_tgt")
  paf_tgt <- compute_pafs(tgt)

  # Join keys carry the draw dimension when the burdens are draws-mode, so the
  # ratio and PAFs stay per-draw (paired by draw index, GBD convention) and the
  # output preserves uncertainty. Falls back to (year, subloc, cause) for
  # summary-mode burdens.
  jk <- intersect(c("year", "subloc_id", "acause", "draw"), names(S_ref))
  ratio_tbl <- merge(S_ref, S_tgt, by = jk, all = TRUE)
  # Identity ratio for non-temp-scaled causes -- IHME didn't bake in a temp
  # signal for these, so we don't divide out a denominator we don't have.
  ratio_tbl[, scale_factor := fifelse(
    acause %in% CAUSES_IHME_NOT_TEMP_SCALED, 1.0,
    fifelse(S_ref > 0, S_tgt / S_ref, NA_real_))]

  # Pull PAFs onto the same (year, subloc, cause[, draw]) frame.
  ratio_tbl <- merge(ratio_tbl, paf_tgt, by = jk, all.x = TRUE)
  ratio_tbl[is.na(paf_heat), paf_heat := 0]
  ratio_tbl[is.na(paf_cold), paf_cold := 0]

  # IHME mortality is at country grain (no subloc); broadcast the cause/subloc-
  # keyed pipeline rows across IHME's (age, sex) frame. Keep IHME's draw column
  # so it pairs with the pipeline draws by index (not a draw x draw cartesian).
  ihme_has_draw <- "draw" %in% names(ihme)
  ihme_cols <- c("year_id", "age_group_id", "sex_id", "acause",
                 if (ihme_has_draw) "draw", "deaths")
  ihme_keyed <- ihme[, ..ihme_cols]
  setnames(ihme_keyed, c("year_id", "deaths"), c("year", "ihme_deaths"))
  mk <- intersect(c("year", "acause", "draw"),
                  intersect(names(ihme_keyed), names(ratio_tbl)))
  out <- merge(ihme_keyed,
               ratio_tbl[, .SD, .SDcols = c(jk, "scale_factor", "paf_heat", "paf_cold")],
               by = mk, allow.cartesian = TRUE)

  out[, m_scaled := ihme_deaths * scale_factor]
  out[, `:=`(deaths_heat_attrib = m_scaled * paf_heat,
             deaths_cold_attrib = m_scaled * paf_cold)]
  out[, deaths_nonopt_attrib := deaths_heat_attrib + deaths_cold_attrib]
  out[, location_id := location_id]

  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)
  saveRDS(out, output)
  if (verbose) {
    log_msg("Saved Workflow B output (", nrow(out), " rows) -> ", output)
    log_msg(sprintf(
      "Totals: heat=%.0f  cold=%.0f  non-optimal=%.0f",
      sum(out$deaths_heat_attrib,   na.rm = TRUE),
      sum(out$deaths_cold_attrib,   na.rm = TRUE),
      sum(out$deaths_nonopt_attrib, na.rm = TRUE)))
    n_temp <- sum(out$acause %in% CAUSES_IHME_TEMP_SCALED)
    n_not  <- sum(out$acause %in% CAUSES_IHME_NOT_TEMP_SCALED)
    log_msg(sprintf(
      "Rows by cause-type: temp-scaled (ratio applied) = %d | not-temp-scaled (identity) = %d",
      n_temp, n_not))
  }
  invisible(out)
}

if (sys.nframe() == 0L) apply_ratio()
