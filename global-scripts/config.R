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
PROJECT_ROOT <- here::here()

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

# Number of draws (only used when USE_DRAWS = TRUE)
N_DRAWS <- 1000

# Run descriptive/diagnostic plots (set FALSE to skip for production runs)
RUN_DIAGNOSTICS <- TRUE

# Compute SEVs as a separate diagnostic output
COMPUTE_SEVS <- TRUE

# Compute resilience index and counterfactuals
COMPUTE_RESILIENCE <- TRUE

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
