# config.R — Global pipeline configuration
#
# Source this file at the top of every pipeline script.
# All paths, location settings, and flags are defined here.

# =============================================================================
# Location and time settings
# =============================================================================

# GBD location ID for this run (e.g., 125 = Colombia)
# Override via command line: Rscript script.R --location_id=125
LOCATION_ID <- 125

# Study period
YEAR_START <- 2010
YEAR_END   <- 2019

# Baseline period for resilience analysis (first N years)
BASELINE_YEARS <- c(2010, 2011, 2012)

# =============================================================================
# Paths
# =============================================================================

# Root directory for the project
# Project root: parent of global-scripts/. Set via environment variable or auto-detect.
PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT",
                           unset = normalizePath(file.path(getwd(), "..")))

# Input data directories
DATA_DIR       <- file.path(PROJECT_ROOT, "data")
ERF_DIR        <- file.path(DATA_DIR, "erf")            # ERF curve CSVs
TMREL_DIR      <- file.path(DATA_DIR, "tmrel")           # TMREL files
TEMP_DIR       <- file.path(DATA_DIR, "temperature")     # ERA5 temperature
POP_DIR        <- file.path(DATA_DIR, "population")      # WorldPop grids
MORTALITY_DIR  <- file.path(DATA_DIR, "mortality")        # GBD cause-specific deaths
LIFETABLE_DIR  <- file.path(DATA_DIR, "lifetables")       # Life tables
SHAPEFILE_DIR  <- file.path(DATA_DIR, "shapefiles")       # GBD shapefiles

# Output directories
OUTPUT_DIR     <- file.path(PROJECT_ROOT, "output")
INTERMEDIATE_DIR <- file.path(OUTPUT_DIR, "intermediate")
RESULTS_DIR    <- file.path(OUTPUT_DIR, "results")
FIGURES_DIR    <- file.path(OUTPUT_DIR, "figures")
DIAGNOSTICS_DIR <- file.path(OUTPUT_DIR, "diagnostics")

# =============================================================================
# Computation flags
# =============================================================================

# Use full 1000-draw uncertainty propagation (TRUE) or summary statistics (FALSE).
# Summary mode is faster but does not produce valid confidence intervals.
USE_DRAWS <- FALSE

# Number of draws (only used when USE_DRAWS = TRUE).
# ERF curves have 1000 draws, but TMREL files only have 100 (tmrel_0 to tmrel_99).
# When USE_DRAWS = TRUE, TMREL draws are recycled 10× to match. See 02_load_tmrel.R.
N_DRAWS <- 1000

# Run descriptive/diagnostic plots (set FALSE to skip for production runs)
RUN_DIAGNOSTICS <- TRUE

# Compute SEVs as a separate diagnostic output
COMPUTE_SEVS <- TRUE

# =============================================================================
# Colombia verification mode
# =============================================================================
#
# When TRUE, replicates Samuel's methodological choices that differ from
# Burkart et al. (2021). This allows us to reproduce Samuel's Colombia
# results for validation, then compare against the corrected pipeline.
#
# Differences toggled by this flag:
#   1. No RR rescaling at TMREL (raw RR used as-is)         [05/06]
#   2. PAF floored at zero (removes protective/negative PAFs) [05]
#   3. TMREL averaged across all years (single per zone)    [02]
#   4. Attributable burden = deaths × PAF × SEV (not just deaths × PAF) [05]
#   5. SEV computed as N-days × annual SEV, capped at 1 — replicates
#      Samuel's daily-summation bug in 11_carga_atribuible.R:444-469. [06]
#
# Note: USE_DRAWS = FALSE separately controls draw-level propagation
# (Samuel's critical issue #1). Temperature draws (issue #3) are controlled
# by whether temp_sd is present in the data.
#
# THIS FLAG MUST BE FALSE for any production / global / non-Colombia run.
# A guard below errors if it is TRUE with LOCATION_ID != 125.
COLOMBIA_VERIFICATION <- FALSE

# =============================================================================
# GBD cause list (17 temperature-sensitive causes from Burkart et al. 2021)
# =============================================================================

# GBD acause labels used in ERF filenames
GBD_CAUSES <- c(
  "ckd",              # Chronic kidney disease
  "cvd_cmp",          # Cardiomyopathy and myocarditis
  "cvd_htn",          # Hypertensive heart disease
  "cvd_ihd",          # Ischaemic heart disease
  "cvd_stroke",       # Stroke
  "diabetes",         # Diabetes mellitus
  "inj_animal",       # Animal-related injuries
  "inj_disaster",     # Disaster-related injuries
  "inj_drowning",     # Drowning
  "inj_homicide",     # Interpersonal violence
  "inj_mech",         # Mechanical injuries
  "inj_othunintent",  # Other unintentional injuries
  "inj_suicide",      # Self-harm
  "inj_trans_other",  # Other transport injuries
  "inj_trans_road",   # Road injuries
  "lri",              # Lower respiratory infections
  "resp_copd"         # Chronic obstructive pulmonary disease
)

# Temperature zone range (Burkart uses 6-28°C)
TEMP_ZONE_MIN <- 6
TEMP_ZONE_MAX <- 28

# =============================================================================
# Command-line argument parsing (override config values)
# =============================================================================

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  for (arg in args) {
    if (grepl("^--", arg)) {
      parts <- strsplit(sub("^--", "", arg), "=")[[1]]
      key <- parts[1]
      val <- parts[2]
      # Try numeric conversion, fall back to character
      val_num <- suppressWarnings(as.numeric(val))
      if (!is.na(val_num)) val <- val_num
      if (val == "TRUE") val <- TRUE
      if (val == "FALSE") val <- FALSE
      assign(toupper(key), val, envir = globalenv())
    }
  }
}

parse_args()

# =============================================================================
# Safety guards
# =============================================================================

if (isTRUE(COLOMBIA_VERIFICATION) && LOCATION_ID != 125) {
  stop("COLOMBIA_VERIFICATION = TRUE is only valid for LOCATION_ID = 125 ",
       "(Colombia). Current LOCATION_ID = ", LOCATION_ID, ". ",
       "This mode replicates known methodological bugs in Samuel's original ",
       "Colombia code (see config.R doc block) and must NEVER be used for ",
       "any other location or production run. Set COLOMBIA_VERIFICATION = FALSE.")
}

# =============================================================================
# Create output directories
# =============================================================================

for (d in c(OUTPUT_DIR, INTERMEDIATE_DIR, RESULTS_DIR, FIGURES_DIR, DIAGNOSTICS_DIR)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# =============================================================================
# Logging
# =============================================================================

log_msg <- function(...) {
  msg <- paste0("[", Sys.time(), "] ", paste(...))
  message(msg)
}

log_msg("Config loaded for location_id =", LOCATION_ID,
        "| years =", YEAR_START, "-", YEAR_END,
        "| use_draws =", USE_DRAWS)
