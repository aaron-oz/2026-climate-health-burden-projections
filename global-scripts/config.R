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
# PROJECT_ROOT is the parent of global-scripts/. Entry scripts set SCRIPTS_DIR
# from their own file location (--file=) before sourcing config, so this is
# independent of the working directory. Falls back to getwd()/.. if unset.
PROJECT_ROOT <- Sys.getenv(
  "PROJECT_ROOT",
  unset = if (exists("SCRIPTS_DIR")) dirname(normalizePath(SCRIPTS_DIR))
          else normalizePath(file.path(getwd(), "..")))

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
# ERF curves have 1000 draws, TMREL files have 100 (recycled to N_DRAWS), and
# IHME's mortality-rate forecasts come at 500 draws. Default to 500 to match
# the rate-limiting input — running 1000 ERF draws against 500-draw rates is
# wasted precision. Override via --n_draws=1000 if pure ERF-side uncertainty
# propagation is the goal.
N_DRAWS <- 500

# Inner draw-chunk size for 05_compute_pafs.R DRAW mode. The PAF computation
# is cause-chunked already (one cause at a time). Within each cause, the
# (zones × daily_temps × draws) merge can also be sub-chunked across draws.
# At full 500 draws on a medium country, cause-chunked alone peaks ~5 GB per
# worker; cause+draw-chunked (e.g., 5 chunks of 100 draws) peaks ~1 GB per
# worker, letting more workers fit in fixed cluster RAM. Trade-off is per-
# chunk loop overhead (small in practice). Set to N_DRAWS to disable draw
# chunking; smaller values increase worker concurrency at the cost of some
# total wall-clock.
DRAW_CHUNK_SIZE <- 100L

# Run descriptive/diagnostic plots (set FALSE to skip for production runs)
RUN_DIAGNOSTICS <- TRUE

# Compute SEVs as a separate diagnostic output
COMPUTE_SEVS <- TRUE

# Spatial resolution of the temperature/PAF aggregation. This MUST match the
# resolution of the mortality input, or the deaths x PAF merge in
# 05_compute_pafs.R finds no overlap and silently yields zero burden.
#   FALSE (default) -> national: every pixel inside the country is tagged with
#     subloc_id = LOCATION_ID. Use with national mortality (e.g. IHME forecast
#     files), which is the production case.
#   TRUE -> subnational: pixels are tagged to admin-1 (department/state) units
#     from the shapefile. Use only when the mortality is also subnational
#     (e.g. Samuel's Colombia department-level deaths).
# Only consumed by util_convert_cckp_temperature.R (step 6a). Override with
# --subnational=TRUE.
SUBNATIONAL <- FALSE

# Local CCKP mirror roots. If set and a NetCDF exists at the same bucket-
# relative path under the root, the pipeline symlinks it from there instead of
# downloading from public CCKP S3 (falls back to download on a miss, so a
# partial mirror is fine). Empty = always download.
#   CCKP_LOCAL_ROOT     -> temperature (cmip6-daily-x0.25/tas/...)
#   CCKP_POP_LOCAL_ROOT -> gridded population (pop-x0.25/popcount/...); falls
#     back to CCKP_LOCAL_ROOT when empty. Separate because a mirror may place
#     temp and pop under different roots (e.g. temp /data, pop /data/CRMe/data).
# Override: --cckp_local_root=/data --cckp_pop_local_root=/data/CRMe/data
CCKP_LOCAL_ROOT     <- Sys.getenv("CCKP_LOCAL_ROOT", "")
CCKP_POP_LOCAL_ROOT <- Sys.getenv("CCKP_POP_LOCAL_ROOT", "")

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
#   6. Department-specific life tables, age 0 -> 1 remap for the 0-4
#      group, ex - 2.5 for ages <80, ex = 10 for ages >=80 [07]
#   7. Depto-day attribution: daily PAFs (date x depto x cause x risk) merged
#      with daily mortality, multiplied by annual SEV, summed to annual
#      burden (matches Samuel's daily-attribution pattern). Requires
#      mortality_daily.rds; falls back to annual attribution if absent. [05]
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

# Subset of GBD_CAUSES for which IHME's GBD 2021 forecast applies a
# temperature-scalar adjustment internally. Workflow B's reference/target
# ratio (S_X / S_SSP2) is meaningful only for these causes -- it cancels
# out the temperature signal IHME already baked into the forecast and
# replaces it with our scenario-X signal. The other causes
# (CAUSES_IHME_NOT_TEMP_SCALED below) get the identity ratio (1.0) so the
# Workflow B formula collapses to plain m_IHME * PAF_X for them.
#
# Source: GBD 2021 forecast appendix (gbd/gbd-2021-forecast-appendix.pdf),
# Section 2.1.6.7 "Non-optimal temperature PAFs", which enumerates the 12
# IHME-forecast temperature-related causes:
#   "ischaemic heart disease, stroke, hypertensive heart disease, diabetes,
#    chronic kidney disease, lower respiratory infection, chronic obstructive
#    pulmonary disease, homicide, suicide, mechanical injuries,
#    transport-related injuries, and drowning."
# "Transport-related injuries" is IHME's umbrella cause; we split it into
# inj_trans_road and inj_trans_other, both treated as temp-scaled here.
# So 12 IHME causes -> 13 of our acause codes.
CAUSES_IHME_TEMP_SCALED <- c(
  "ckd",            # chronic kidney disease
  "cvd_htn",        # hypertensive heart disease
  "cvd_ihd",        # ischaemic heart disease
  "cvd_stroke",     # stroke
  "diabetes",       # diabetes
  "inj_drowning",   # drowning
  "inj_homicide",   # interpersonal violence (homicide)
  "inj_mech",       # mechanical injuries
  "inj_suicide",    # self-harm (suicide)
  "inj_trans_other",# transport-related injuries (other) — umbrella in IHME
  "inj_trans_road", # transport-related injuries (road) — umbrella in IHME
  "lri",            # lower respiratory infections
  "resp_copd"       # chronic obstructive pulmonary disease
)
CAUSES_IHME_NOT_TEMP_SCALED <- setdiff(GBD_CAUSES, CAUSES_IHME_TEMP_SCALED)
# Result: c("cvd_cmp", "inj_animal", "inj_disaster", "inj_othunintent")
# — 4 causes. Workflow B ratio is identity (1.0) for these; m_IHME * PAF_X
# applied directly.

# Temperature zone range (Burkart uses 6-28°C)
TEMP_ZONE_MIN <- 6
TEMP_ZONE_MAX <- 28

# =============================================================================
# CCKP CMIP6 model list — the 29 distinct models in CCKP's daily archive at
# https://wbg-cckp.s3.amazonaws.com/data/cmip6-daily-x0.25/ (verified
# 2026-06-03 from the daily-cmip6-download script). Each model appears with
# multiple scenario suffixes (historical, ssp126, ssp245, ssp370, ssp585) on
# S3 — we keep the bare model name here and the run-driver composes the full
# {model}-{scenario} URL fragment.
#
# Not every model publishes every scenario. Known gaps (as of 2026-06-03):
#   gfdl-cm4-r1i1p1f1          — only ssp245, ssp585 (no ssp126, ssp370)
#   hadgem3-gc31-ll-r1i1p1f3   — only ssp126, ssp245, ssp585 (no ssp370)
#   hadgem3-gc31-mm-r1i1p1f3   — only ssp126, ssp585
#   nesm3-r1i1p1f1             — no ssp370
#   taiesm1-r1i1p1f1           — no ssp245
# The run-driver should skip missing (model, scenario) pairs gracefully.
MODELS_ALL <- c(
  "access-cm2-r1i1p1f1",         "access-esm1-5-r1i1p1f1",
  "bcc-csm2-mr-r1i1p1f1",        "canesm5-r1i1p1f1",
  "cmcc-esm2-r1i1p1f1",          "cnrm-cm6-1-r1i1p1f2",
  "cnrm-esm2-1-r1i1p1f2",        "ec-earth3-r1i1p1f1",
  "ec-earth3-veg-lr-r1i1p1f1",   "fgoals-g3-r3i1p1f1",
  "gfdl-cm4-r1i1p1f1",           "gfdl-esm4-r1i1p1f1",
  "giss-e2-1-g-r1i1p1f2",        "hadgem3-gc31-ll-r1i1p1f3",
  "hadgem3-gc31-mm-r1i1p1f3",    "inm-cm4-8-r1i1p1f1",
  "inm-cm5-0-r1i1p1f1",          "ipsl-cm6a-lr-r1i1p1f1",
  "kace-1-0-g-r1i1p1f1",         "miroc6-r1i1p1f1",
  "miroc-es2l-r1i1p1f2",         "mpi-esm1-2-hr-r1i1p1f1",
  "mpi-esm1-2-lr-r1i1p1f1",      "mri-esm2-0-r1i1p1f1",
  "nesm3-r1i1p1f1",              "noresm2-lm-r1i1p1f1",
  "noresm2-mm-r1i1p1f1",         "taiesm1-r1i1p1f1",
  "ukesm1-0-ll-r1i1p1f2"
)

# =============================================================================
# IHME cause-file name map — maps our pipeline's acause codes to the filename
# IHME's GBD Results Tool downloads use. Caspar can edit if his download
# names don't match (IHME's UI sometimes appends timestamps or numbers).
# Placeholder list — names may need tweaking once we see the full set of
# downloaded CSVs.
IHME_CAUSE_FILES <- c(
  ckd              = "chronic_kidney_disease.csv",
  cvd_cmp          = "cardiomyopathy_and_myocarditis.csv",
  cvd_htn          = "hypertensive_heart_disease.csv",
  cvd_ihd          = "ischemic_heart_disease.csv",
  cvd_stroke       = "stroke.csv",
  diabetes         = "diabetes_mellitus.csv",
  inj_animal       = "animal_contact.csv",
  inj_disaster     = "exposure_to_forces_of_nature.csv",
  inj_drowning     = "drowning.csv",
  inj_homicide     = "interpersonal_violence.csv",
  inj_mech         = "exposure_to_mechanical_forces.csv",
  inj_othunintent  = "other_unintentional_injuries.csv",
  inj_suicide      = "self_harm.csv",
  inj_trans_other  = "other_transport_injuries.csv",
  inj_trans_road   = "road_injuries.csv",
  lri              = "lower_respiratory_infections.csv",
  resp_copd        = "chronic_obstructive_pulmonary_disease.csv"
)

# =============================================================================
# Command-line argument parsing (override config values)
# =============================================================================

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  for (arg in args) {
    if (grepl("^--", arg)) {
      parts <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
      key <- parts[1]
      # A bare flag (--foo, no =) means TRUE; otherwise take the value after =.
      if (length(parts) < 2 || is.na(parts[2])) {
        val <- TRUE
      } else {
        val <- parts[2]
        # Try numeric conversion, fall back to character / logical
        val_num <- suppressWarnings(as.numeric(val))
        if (!is.na(val_num)) val <- val_num
        else if (val == "TRUE") val <- TRUE
        else if (val == "FALSE") val <- FALSE
      }
      assign(toupper(key), val, envir = globalenv())
    }
  }
}

parse_args()

# Per-location scratch directory. The step-7 production fan-out runs one process
# per location concurrently; a shared output/intermediate/ would race on the
# location-specific files (temperature.rds, mortality.rds, tmrel.rds, ...). Give
# each location its own intermediate dir. Recomputed here so it picks up
# --location_id from parse_args. (Location-INDEPENDENT ERF curves live in the
# shared, read-only data/erf/cache, not here -- see 01_load_erf.R.)
INTERMEDIATE_DIR <- file.path(OUTPUT_DIR, "intermediate", as.character(LOCATION_ID))

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
