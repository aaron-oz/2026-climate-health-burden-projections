# util_workflow_b_ratio.R — Apply the Workflow B cross-scenario ratio to
# IHME-derived mortality counts.
#
# Reference: gbd/ihme-plan-b-prep.tex, "Workflow B: self-consistent ratio".
#
# Math:
#   m^T_{c,X,l,a,s,t} = m^T_{c,SSP2,IHME}{l,a,s,t} * (S_X(l,t,c) / S_SSP2(l,t,c))
#
# where S_Y(l,t,c) is our pipeline's pop-weighted attributable-PAF aggregate
# at (location, year, cause) under scenario Y. The ratio cancels pipeline-
# specific drift (different ERFs / bias correction / pop recipe than IHME)
# because both numerator and denominator come from the same pipeline.
#
# Inputs:
#   --ref_burden=PATH        Pipeline burden output for the reference scenario
#                            (typically SSP2-RCP4.5). One file per year, RDS.
#                            Multiple files comma-separated; concatenated.
#   --target_burden=PATH     Same for the target scenario X.
#   --ihme_mortality=PATH    IHME-derived counts at (year, age, sex, acause)
#                            granularity. Output of util_convert_ihme_forecast.R.
#                            Comma-separated list supported (per-cause files).
#   --output=PATH            Where to save the Formulation-4 attributable
#                            burden RDS.
#
# Burden file schema (from 05_compute_pafs.R):
#   year, subloc_id, age_group_id, sex_id, acause, deaths, paf_heat, paf_cold,
#   paf_nonopt, deaths_heat, deaths_cold, deaths_nonopt, location_id
#
# The ratio uses deaths_nonopt (cause-specific, summed across heat+cold)
# divided by deaths (the cause-specific mortality denominator the pipeline
# saw). For Formulation 2 the answer is just `deaths_nonopt` from the target
# run; F4 adjusts for IHME's true reference counts being different from what
# the pipeline saw.

source("config.R")
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

apply_ratio <- function() {
  if (is.null(REF_BURDEN) || is.null(TARGET_BURDEN) ||
      is.null(IHME_MORTALITY) || is.null(OUTPUT)) {
    stop("Required: --ref_burden=... --target_burden=... ",
         "--ihme_mortality=... --output=...")
  }

  log_msg("Loading reference-scenario burden(s)")
  ref <- load_rds_list(REF_BURDEN)
  log_msg("Loading target-scenario burden(s)")
  tgt <- load_rds_list(TARGET_BURDEN)
  log_msg("Loading IHME mortality file(s)")
  ihme <- load_rds_list(IHME_MORTALITY)

  # Pipeline-side S: PAF aggregate at (year, subloc, acause). Collapse the
  # age x sex granularity from burden by summing deaths_nonopt / deaths.
  S <- function(b) {
    b[, .(num = sum(deaths_nonopt, na.rm = TRUE),
          den = sum(deaths,        na.rm = TRUE)),
      by = .(year, subloc_id, acause)][
      , .(year, subloc_id, acause, S = num / pmax(den, 1))]
  }
  S_ref <- S(ref); setnames(S_ref, "S", "S_ref")
  S_tgt <- S(tgt); setnames(S_tgt, "S", "S_tgt")

  ratio <- merge(S_ref, S_tgt,
                 by = c("year", "subloc_id", "acause"), all = TRUE)
  ratio[, ratio := fifelse(S_ref > 0, S_tgt / S_ref, NA_real_)]

  # Map IHME's year_id -> burden year. IHME mortality is at country grain,
  # so subloc_id from the burden frame broadcasts (we apply the same ratio
  # to all subloc within a country-year-cause).
  ihme_keyed <- ihme[, .(year = year_id, age_group_id, sex_id, acause,
                         ihme_deaths = deaths)]

  out <- merge(ihme_keyed, ratio,
               by = c("year", "acause"),
               allow.cartesian = TRUE)
  out[, deaths_nonopt_F4 := ihme_deaths * ratio]

  # Also compute Formulation 2 (target-burden-only) for comparison:
  # F2 attributable = pipeline target's deaths_nonopt directly, summed
  # to the same (year, subloc, age, sex, cause) grain.
  f2 <- tgt[, .(deaths_nonopt_F2 = sum(deaths_nonopt, na.rm = TRUE)),
            by = .(year, subloc_id, age_group_id, sex_id, acause)]
  out <- merge(out, f2,
               by = c("year", "subloc_id", "age_group_id", "sex_id", "acause"),
               all.x = TRUE)

  out[, location_id := LOCATION_ID]
  saveRDS(out, OUTPUT)
  log_msg("Saved Workflow B output (", nrow(out), " rows) -> ", OUTPUT)
  log_msg("F4 total attributable: ", round(sum(out$deaths_nonopt_F4, na.rm = TRUE), 1),
          " | F2 total attributable: ", round(sum(out$deaths_nonopt_F2, na.rm = TRUE), 1))
  invisible(out)
}

if (sys.nframe() == 0L) apply_ratio()
