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

# --- renv activation (OPT-IN) ------------------------------------------------
# By default the pipeline uses the machine's own R library: the production
# machine manages R packages with Nix, and renv's project library would mask
# those packages. renv.lock stays in the repo as the record of the versions
# the pipeline was validated against.
#
# Set RENV_ACTIVATE_PROJECT=TRUE (e.g. in run_env.sh) to use renv's pinned
# project library instead; it needs a one-time renv::restore() from the repo
# root. Activation is keyed on PROJECT_ROOT rather than getwd() because R only
# reads .Rprofile from the startup directory, so cwd-based activation silently
# skips when a script is launched from elsewhere. renv's activate.R reads
# RENV_PROJECT and only falls back to getwd(), so setting it first targets the
# right project from any launch dir. Skipped when .Rprofile already activated
# this project's library. Must stay ahead of every library() call.
local({
  optin <- tolower(Sys.getenv("RENV_ACTIVATE_PROJECT", "FALSE")) %in%
             c("true", "t", "1")
  activate <- file.path(PROJECT_ROOT, "renv", "activate.R")
  lib      <- file.path(PROJECT_ROOT, "renv", "library")
  if (optin && file.exists(activate) &&
      !any(startsWith(normalizePath(.libPaths(), mustWork = FALSE),
                      normalizePath(lib, mustWork = FALSE)))) {
    Sys.setenv(RENV_PROJECT = PROJECT_ROOT)
    source(activate)   # local = FALSE: evaluates in globalenv, as at startup
  }
})

# Input data directories
DATA_DIR       <- file.path(PROJECT_ROOT, "data")
ERF_DIR        <- file.path(DATA_DIR, "erf")            # ERF curve CSVs
TMREL_DIR      <- file.path(DATA_DIR, "tmrel")           # TMREL files
TEMP_DIR       <- file.path(DATA_DIR, "temperature")     # ERA5 temperature
POP_DIR        <- file.path(DATA_DIR, "population")      # WorldPop grids
MORTALITY_DIR  <- file.path(DATA_DIR, "mortality")        # GBD cause-specific deaths
LIFETABLE_DIR  <- file.path(DATA_DIR, "lifetables")       # Life tables
SHAPEFILE_DIR  <- file.path(DATA_DIR, "shapefiles")       # GBD shapefiles

# Default GBD shapefile for geometry (pixel-to-country clipping). Prefer the
# augmented copy when it exists -- it adds 6 micro-nations that the GBD mapping
# file omits entirely (Maldives, Marshall Islands, Monaco, Nauru, Tokelau,
# Tuvalu; built by util_augment_shapefile.R). Non-breaking: the augmented file is
# a strict superset of the original, identical for the other ~198 locations.
DEFAULT_SHAPEFILE <- local({
  aug  <- file.path(SHAPEFILE_DIR, "GBD2023_mapping_final_augmented.shp")
  if (file.exists(aug)) aug else file.path(SHAPEFILE_DIR, "GBD2023_mapping_final.shp")
})

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

# TMREL source (draw mode only; uncertainty-draws fix, 2026-08-20 handoff).
#   "derived_per_draw" (DEFAULT since 2026-08-24, Aaron's ruling) — TMREL
#       draw d is derived as the argmin of ERF draw d's death-weighted RR
#       curve over the tmrelCalculator search range (6.6-34.6 C), the
#       definition in IHME's tmrelCalculator.R:126. Weights are the
#       location's cause-death shares for the study year, per-draw when the
#       mortality input carries draws (draw d's shares weight ERF draw d),
#       point otherwise. Requires 04_load_mortality.R to have run
#       (run_location.R orders 04 ahead of 02 for this). Summary mode
#       (USE_DRAWS = FALSE) has no draw pairing to fix and always uses the
#       released TMREL summaries.
#   "released_recycled" — LEGACY, kept only to reproduce the original ssp245
#       run and its review comparisons: IHME's released 100 TMREL draws,
#       recycled to N_DRAWS (draw N uses tmrel_(N %% 100)). The released
#       draws have no index linkage to the ERF draws, so a draw's reference
#       is never its own curve minimum and the draw-mean PAF is biased down
#       (one-signed; see ssp245 review section 6). Do not use for new
#       production runs.
# Override: --tmrel_mode=released_recycled
TMREL_MODE <- "derived_per_draw"

# Round derived TMRELs to whole degrees C (IHME's pafCalc_sevFix.R:94 does;
# we keep the ERF grid's 0.1 C resolution by default as more faithful to the
# argmin definition). Only consulted when TMREL_MODE = "derived_per_draw".
TMREL_ROUND_WHOLE <- FALSE

# Daily-temperature exposure uncertainty (2026-08-20 handoff step B).
#   "none"    — exposure used as-is (production behavior to date).
#   "era5_sd" — each pixel-day's exposure mass is spread over neighboring
#       0.1 C bins with a Gaussian kernel whose sd comes from the measured
#       ERA5 EDA-spread climatology at that (pixel, month), before zone-range
#       truncation. Distribution-level equivalent of IHME era2melt.R's
#       daily_temp + sd * N(0,1) draws, without a draw dimension.
# Override: --temp_noise_mode=era5_sd
TEMP_NOISE_MODE <- "none"
# Per-pixel-month sd climatology on the CCKP 0.25-degree grid, built by
# output/review-ssp245/{fetch_era5_spread.py,build_era5_sd.py}.
TEMP_SD_FILE <- file.path(DATA_DIR, "era5_sd",
                          "era5_t2m_spread_daily_clim_2022_cckp025.nc")
# Which intra-day aggregation bound to use: "sd_corr" (mean 3-hourly spread;
# errors fully correlated within the day) or "sd_indep" (spread of the daily
# mean under independent errors). Truth lies between; see the sd-field build
# script header.
TEMP_SD_VAR <- "sd_corr"

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
#
# Default the temperature root to a repo-relative data/cckp-mirror so operators
# can just symlink their local NetCDF tree there (data/cckp-mirror -> /wherever)
# with no env var. Non-breaking: if the path doesn't exist, ensure_download falls
# through to the S3 download exactly as before (unless CCKP_REQUIRE_LOCAL).
CCKP_LOCAL_ROOT     <- Sys.getenv("CCKP_LOCAL_ROOT", file.path(DATA_DIR, "cckp-mirror"))
CCKP_POP_LOCAL_ROOT <- Sys.getenv("CCKP_POP_LOCAL_ROOT", "")

# When TRUE, a local-mirror MISS is a hard error (naming the missing path)
# instead of silently downloading from S3. Set this for production runs where
# the data is local, so a misconfigured CCKP_LOCAL_ROOT / wrong layout fails in
# seconds rather than after hours of unintended downloads. Default FALSE keeps
# the download-fallback behaviour for machines without a mirror.
CCKP_REQUIRE_LOCAL  <- toupper(Sys.getenv("CCKP_REQUIRE_LOCAL", "FALSE")) %in%
                       c("TRUE", "1", "YES")

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
    if (!grepl("^--", arg)) next
    body <- sub("^--", "", arg)
    eq <- regexpr("=", body, fixed = TRUE)
    if (eq < 0L) {
      # A bare flag (--foo, no "=") means TRUE.
      key <- body
      val <- TRUE
    } else {
      key <- substr(body, 1L, eq - 1L)
      # substring, not strsplit: a value may legitimately contain "=" and
      # strsplit("a=b=c", "=")[[1]][2] silently truncated it to "b".
      val <- substring(body, eq + 1L)
      # "--foo=" with nothing after the "=" is an EMPTY value, not TRUE. The
      # old code split on "=", got a length-1 vector, and fell through to the
      # bare-flag branch. That turned callers passing an unset optional path
      # (util_run_global.R forwards --cckp_pop_local_root=$CCKP_POP_LOCAL_ROOT,
      # normally empty) into the literal TRUE, so the population mirror path
      # became "TRUE/pop-x0.25/..." and every combo whose population file was
      # not already in the cache was recorded as a missing model x scenario
      # rather than converted.
      if (nzchar(val)) {
        # Try numeric conversion, fall back to character / logical
        val_num <- suppressWarnings(as.numeric(val))
        if (!is.na(val_num)) val <- val_num
        else if (val == "TRUE") val <- TRUE
        else if (val == "FALSE") val <- FALSE
      }
    }
    assign(toupper(key), val, envir = globalenv())
  }
}

parse_args()

# =============================================================================
# Thread cap (critical for the parallel fan-out)
#
# The production run launches one OS process per location (GNU parallel). Each
# process's data.table defaults to ~50% of the machine's logical cores, so N
# concurrent locations request N x (cores/2) threads -- catastrophic
# oversubscription (observed load average ~660 on a box where RAM was idle: the
# run queue thrashed on context-switching instead of computing).
#
# Cap data.table to DT_THREADS threads per process (default 1) so N processes
# use N threads; then set -j to the physical core count. Override with
# DT_THREADS=<n> (env) or --dt_threads=<n>; DT_THREADS=0 means "all cores" (for
# a lone interactive run). BLAS threads are separate and set OUTSIDE R -- export
# OMP_NUM_THREADS=1 and OPENBLAS_NUM_THREADS=1 before launching.
if (!exists("DT_THREADS", envir = globalenv())) {
  DT_THREADS <- suppressWarnings(as.integer(Sys.getenv("DT_THREADS", "1")))
}
if (is.na(DT_THREADS)) DT_THREADS <- 1L
if (requireNamespace("data.table", quietly = TRUE)) {
  data.table::setDTthreads(as.integer(DT_THREADS))
}

# Per-location scratch directory. The step-7 production fan-out runs one process
# per location concurrently; a shared output/intermediate/ would race on the
# location-specific files (temperature.rds, mortality.rds, tmrel.rds, ...). Give
# each location its own intermediate dir. Recomputed here so it picks up
# --location_id from parse_args. (Location-INDEPENDENT ERF curves live in the
# shared, read-only data/erf/cache, not here -- see 01_load_erf.R.)
#
# PIPELINE_RUN_TAG narrows that scope one level further, from per-location to
# per-invocation. Per-location is only sufficient while a location runs its
# combos one at a time. Running several (model, scenario, year) combos of the
# SAME location concurrently would race on both this scratch dir and on the
# RESULTS_DIR staging files below, because neither carries the combo in its
# path. util_run_cckp_burden.R sets the tag to the combo id for exactly that
# reason. Empty tag (the default) reproduces the previous single-combo layout
# byte for byte, so single-location and validation runs are unaffected.
PIPELINE_RUN_TAG <- Sys.getenv("PIPELINE_RUN_TAG", "")
INTERMEDIATE_DIR <- file.path(OUTPUT_DIR, "intermediate", as.character(LOCATION_ID))
if (nzchar(PIPELINE_RUN_TAG)) {
  INTERMEDIATE_DIR <- file.path(INTERMEDIATE_DIR, PIPELINE_RUN_TAG)
}

# RESULTS_ROOT is the canonical, never-redirected results tree. The permanent
# per-combo output trees (results/cckp/...) and the run markers live here, so
# they stay in one place no matter where an individual invocation stages its
# working files. RESULTS_DIR is the staging target that scripts 05-08 write
# their *_{LOCATION_ID}.rds into, and it moves under the run tag.
RESULTS_ROOT <- RESULTS_DIR
if (nzchar(PIPELINE_RUN_TAG)) {
  RESULTS_DIR <- file.path(RESULTS_ROOT, "_staging",
                           as.character(LOCATION_ID), PIPELINE_RUN_TAG)
}

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

# =============================================================================
# Per-location run-progress markers (for monitoring long parallel fan-outs)
#
# Both phases of a location run -- temperature convert (util_run_cckp_pipeline.R)
# and burden (util_run_cckp_burden.R) -- overwrite a single per-location
# _progress.tsv so `cat output/results/cckp/<loc>/_progress.tsv` gives an
# instant status. The burden phase writes a terminal sentinel when its grid
# finishes: _DONE if every combo succeeded, _INCOMPLETE if it ended with
# failures/missing combos. So:
#   find output/results/cckp -name _DONE       | wc -l   # locations fully done
#   find output/results/cckp -name _INCOMPLETE          # locations needing attention
# The progress file appears as soon as Phase 1 starts, so a location's dir is no
# longer invisible until burden begins.
# =============================================================================
# `root` selects the output tree the markers live in, so the burden phase
# (results/cckp) and the Workflow B batch (results/workflow_b) keep separate
# progress + sentinel files rather than clobbering each other.
# RESULTS_ROOT, not RESULTS_DIR: markers must stay in the canonical tree even
# when an invocation stages its working files under a run tag.
cckp_marker_root <- function() file.path(RESULTS_ROOT, "cckp")
wfb_marker_root  <- function() file.path(RESULTS_ROOT, "workflow_b")
run_marker_dir   <- function(loc, root = cckp_marker_root()) file.path(root, loc)

# Overwrite the per-location progress file. `tally` is an optional named list of
# extra counters (ok/skip/fail/...); `last` is a human label for the most recent
# combo. Cheap enough to call every combo (one small file rewrite).
#
# Written via tmp + rename so a reader never catches a half-written file and so
# two workers refreshing it concurrently cannot interleave lines. Both compute
# the same tally from the same combo-status files, so whichever lands last is
# still correct.
write_run_progress <- function(loc, phase, done, total, tally = list(), last = "",
                               root = cckp_marker_root()) {
  d <- run_marker_dir(loc, root)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  lines <- c(paste0("phase\t",        phase),
             paste0("location_id\t",  loc),
             paste0("combos_done\t",  done),
             paste0("combos_total\t", total),
             paste0("pct\t", if (total > 0) sprintf("%.1f", 100 * done / total) else "NA"))
  for (nm in names(tally)) lines <- c(lines, paste0(nm, "\t", tally[[nm]]))
  lines <- c(lines,
             paste0("last_combo\t", last),
             paste0("updated\t",    format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
  atomic_write_lines(lines, file.path(d, "_progress.tsv"))
  # Also keep a copy per phase. _progress.tsv holds whichever phase wrote last,
  # so once burden starts, convert's own total is no longer recoverable from it
  # and a status view has nothing to compute a percentage against.
  atomic_write_lines(lines, file.path(d, paste0("_progress_", phase, ".tsv")))
}

# =============================================================================
# Per-combo status files
#
# One file per (phase, model, scenario, year). Exactly one worker ever writes a
# given combo's file, because the grid is partitioned across workers, so there
# is no lock and no contention: the previous design had every worker rewriting
# one shared _progress.tsv from its own in-memory tally, which meant a parallel
# run reported whichever worker wrote last rather than the true total.
#
# Progress and the terminal sentinel are DERIVED from these files rather than
# tracked in memory, so they are correct no matter how the work was divided up,
# and they survive a crash: a resumed run re-reads what actually completed.
# =============================================================================

# Atomic saveRDS. Combo outputs double as the resume marker: both runners skip a
# combo when its output file exists, without opening it. A plain saveRDS that is
# interrupted (Ctrl-C, OOM kill, node reboot) leaves a truncated file that still
# satisfies file.exists(), so the combo is skipped for good and the damage only
# surfaces later as an unreadable input. Writing to a temporary name and
# renaming means an interrupted write leaves either the previous file or
# nothing, and either is correct on resume.
save_rds_atomic <- function(object, path, ...) {
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  saveRDS(object, tmp, ...)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not move temporary file into place at ", path)
  }
  invisible(path)
}

# tmp + rename in the destination directory. rename(2) within a filesystem is
# atomic, so a concurrent reader sees either the old file or the new one.
atomic_write_lines <- function(lines, path) {
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  writeLines(lines, tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    warning("Could not atomically write ", path)
  }
  invisible(path)
}

combo_status_dir <- function(loc, phase, root = cckp_marker_root()) {
  file.path(run_marker_dir(loc, root), "status", phase)
}

# Combo id used as the status filename. Kept filesystem-safe and reversible:
# model / scenario / year are separated by "__" because model names themselves
# contain single dashes (e.g. access-cm2-r1i1p1f1).
combo_id <- function(model, scenario, year) {
  paste(model, scenario, year, sep = "__")
}

write_combo_status <- function(loc, phase, model, scenario, year, status,
                               elapsed_s = NA_real_, message = "",
                               root = cckp_marker_root()) {
  d <- combo_status_dir(loc, phase, root)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  atomic_write_lines(
    c(paste0("status\t",    status),
      paste0("model\t",     model),
      paste0("scenario\t",  scenario),
      paste0("year\t",      year),
      paste0("elapsed_s\t", if (is.na(elapsed_s)) "" else round(elapsed_s, 2)),
      paste0("message\t",   gsub("[\r\n\t]+", " ", message)),
      paste0("updated\t",   format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))),
    file.path(d, paste0(combo_id(model, scenario, year), ".tsv")))
}

# Read every combo status for one phase into a data.table (0 rows if none yet).
#
# A full grid is one file per (model, scenario, year), so a large location holds
# on the order of a thousand of them and this runs after every combo. One
# readLines() per file is unavoidable, but building a one-row data.table per
# file was costing about four times the I/O itself (measured on 841 files:
# 0.14 s the old way, 0.034 s this way). Parse into a plain character matrix and
# build the table once.
STATUS_FIELDS <- c("model", "scenario", "year", "status", "elapsed_s", "message")

read_combo_statuses <- function(loc, phase, root = cckp_marker_root()) {
  d <- combo_status_dir(loc, phase, root)
  f <- list.files(d, pattern = "\\.tsv$", full.names = TRUE)
  empty <- data.table::data.table(
    model = character(), scenario = character(), year = integer(),
    status = character(), elapsed_s = numeric(), message = character())
  if (length(f) == 0) return(empty)
  na_row <- setNames(rep(NA_character_, length(STATUS_FIELDS)), STATUS_FIELDS)
  m <- vapply(f, function(p) {
    ln <- tryCatch(readLines(p, warn = FALSE), error = function(e) character())
    if (length(ln) == 0) return(na_row)
    # Split each "key<TAB>value" line without strsplit()'s per-line list.
    unname(setNames(sub("^[^\t]*\t?", "", ln), sub("\t.*$", "", ln))[STATUS_FIELDS])
  }, character(length(STATUS_FIELDS)), USE.NAMES = FALSE)
  st <- data.table::data.table(
    model     = m[1, ],
    scenario  = m[2, ],
    year      = suppressWarnings(as.integer(m[3, ])),
    status    = m[4, ],
    elapsed_s = suppressWarnings(as.numeric(m[5, ])),
    message   = m[6, ])
  # An unreadable file has no status and is dropped, as before, rather than
  # counting toward the done total as a row of NAs.
  st[!is.na(status)]
}

# Refresh _progress.tsv from the combo-status files on disk. `total` is the size
# of the intended grid, passed in because the status dir only knows what has
# finished, not what was asked for.
refresh_run_progress <- function(loc, phase, total, last = "",
                                 root = cckp_marker_root()) {
  st <- read_combo_statuses(loc, phase, root)
  tally <- as.list(table(factor(
    st$status, levels = c("ok", "skip", "fail", "missing-on-s3", "missing-temp"))))
  # Carry the typical per-combo time in the progress file too. It is free here
  # (the statuses are already in hand) and it lets a monitoring view report
  # timing from this one small file instead of re-reading the whole status dir.
  ok_s <- st$elapsed_s[st$status == "ok"]
  tally[["median_ok_s"]] <- if (any(!is.na(ok_s))) round(median(ok_s, na.rm = TRUE), 1) else ""
  write_run_progress(loc, phase = phase, done = nrow(st), total = total,
                     tally = tally, last = last, root = root)
  invisible(st)
}

# Clear any stale terminal sentinels (call at the start of a (re)run so a prior
# _DONE/_INCOMPLETE can't be mistaken for the current run's state).
clear_done_sentinel <- function(loc, root = cckp_marker_root()) {
  unlink(file.path(run_marker_dir(loc, root), c("_DONE", "_INCOMPLETE")))
}

# Write the terminal sentinel: _DONE when complete (no failures/missing), else
# _INCOMPLETE. `summary` is a one-line human description of the final tally.
write_done_sentinel <- function(loc, complete, summary = "", root = cckp_marker_root()) {
  d <- run_marker_dir(loc, root)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  clear_done_sentinel(loc, root)
  f <- file.path(d, if (isTRUE(complete)) "_DONE" else "_INCOMPLETE")
  atomic_write_lines(c(paste0("location_id\t", loc),
                       paste0("status\t",   if (isTRUE(complete)) "DONE" else "INCOMPLETE"),
                       paste0("summary\t",  summary),
                       paste0("finished\t", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))),
                     f)
}

# Drop a phase's combo-status files at the start of a run. Without this, a run
# over a smaller grid than last time (say 14 models instead of 29) would still
# see the old grid's status files and report a done count above its own total.
# Nothing is lost: the output RDS files, not these markers, are what the skip
# logic consults, so a cleared status dir refills as the run proceeds.
clear_combo_statuses <- function(loc, phase, root = cckp_marker_root()) {
  unlink(combo_status_dir(loc, phase, root), recursive = TRUE)
}

# Terminal sentinel derived from the combo-status files rather than from an
# in-memory tally. `total` is the intended grid size. Complete means every
# intended combo has a status and none of them failed. A missing model x
# scenario on the mirror (a real CCKP gap) is not a failure: it is recorded and
# reported, but it does not hold a location back from DONE, because no rerun
# would ever fill it.
finalize_run_sentinel <- function(loc, phase, total, root = cckp_marker_root()) {
  st <- read_combo_statuses(loc, phase, root)
  n <- function(s) sum(st$status == s, na.rm = TRUE)
  n_fail <- n("fail")
  missing_lbl <- if (phase == "convert") "missing-on-s3" else "missing-temp"
  n_missing <- n(missing_lbl)
  complete <- nrow(st) >= total && n_fail == 0
  summary <- sprintf("%d ok, %d skip, %d fail, %d %s of %d combos",
                     n("ok"), n("skip"), n_fail, n_missing, missing_lbl, total)
  write_done_sentinel(loc, complete = complete, summary = summary, root = root)
  invisible(list(complete = complete, summary = summary, statuses = st))
}

log_msg("Config loaded for location_id =", LOCATION_ID,
        "| years =", YEAR_START, "-", YEAR_END,
        "| use_draws =", USE_DRAWS)

# Record the effective thread config so "is the cap in effect?" is answerable
# from the run log alone (no live vmstat needed). data.table cap is deterministic
# (default 1); OMP/OPENBLAS govern BLAS and must be exported outside R.
log_msg(sprintf("Threads: data.table=%s | OMP_NUM_THREADS=%s | OPENBLAS_NUM_THREADS=%s",
                if (requireNamespace("data.table", quietly = TRUE))
                  as.character(data.table::getDTthreads()) else "NA",
                Sys.getenv("OMP_NUM_THREADS", "unset"),
                Sys.getenv("OPENBLAS_NUM_THREADS", "unset")))
