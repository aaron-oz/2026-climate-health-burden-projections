# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

World Bank statistical consulting project quantifying mortality and morbidity (YLLs / AVPP) attributable to non-optimal temperatures. Currently validating with Colombia data (2010–2019), with the goal of scaling globally and projecting to 2100 under SSP scenarios. Based on the Burkart et al. (Lancet 2021) methodology and the GBD 2023 framework.

The stressor-resilience framework (a related but separate project) has been moved to `../2026-stressor-resilience-framework/`.

## Architecture

Pure R pipeline in `global-scripts/`. No build system, tests, or CI/CD. Scripts are run sequentially via `run_location.R`.

### Global Pipeline (global-scripts/)

Run order (managed by `run_location.R`):

1. `01_load_erf.R` — Load 17 cause-specific ERF curves (supports draw and summary modes)
2. `02_load_tmrel.R` — Load TMRELs for a location (sparse years filled by nearest-year; 100 TMREL draws recycled to match 1000 ERF draws)
3. `03_load_temperature.R` — Load daily temperature, assign zones, compute population weights
4. `04_load_mortality.R` — Load GBD cause-specific mortality
5. `06_compute_sevs.R` — Compute Summary Exposure Values (diagnostic metric, not used in burden calc)
6. `05_compute_pafs.R` — Compute PAFs and attributable burden (core calculation)
7. `07_compute_ylls.R` — Convert attributable deaths to YLLs using life tables
8. `08_outputs.R` — Summary tables and figures

Key config flags in `config.R`:
- `USE_DRAWS` — Full 1000-draw uncertainty propagation (TRUE) vs summary statistics (FALSE)
- `COLOMBIA_VERIFICATION` — Toggle Samuel's methodological choices for validation (see step2-comparison.md)
- `COMPUTE_SEVS` — Whether to compute SEVs

### Samuel's Original Colombia Scripts (from-samuel/Scripts/Colombia/)

The original 12-script Colombia pipeline in Spanish. Kept for reference. Not used directly — the global pipeline reimplements this with corrections.

### Utility Scripts

- `util_convert_samuel_colombia.R` — Convert Samuel's RDS/CSV data to pipeline format
- `util_generate_test_data.R` — Generate synthetic test data
- `util_parse_gbd_mortality.R` — Parse GBD Results Tool downloads
- `util_parse_un_lifetables.R` — Parse UN WPP life tables
- `util_download_un_lifetables.R` — Download UN life tables

### Key Data Inputs

- **ERF curves**: 17 cause-specific RR curves × 1000 draws in `data/erf/` (symlinked from `from-samuel/Info Burkart/ERF/`)
- **TMRELs**: 1,034 locations × 100 draws × 3 time points (1990, 2010, 2020) in `data/tmrel/`
- **Temperature**: Daily pixel-level temp + population in `data/temperature/`
- **Mortality**: GBD cause-specific deaths in `data/mortality/`
- **Life tables**: By location/age/sex/year in `data/lifetables/`
- **Colombia verification data**: Samuel's intermediate RDS files in `data/columbia-data-for-verifying-pipeline/`
- **GBD hierarchy**: `from-samuel/IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX`
- **Shapefiles**: GBD 2023 admin boundaries in `data/shapefiles/`

### Key Methodological Details

- **17 temperature-related causes** per GBD taxonomy (CVD subtypes, CKD, diabetes, LRI, COPD, injuries)
- **Uncertainty**: 1000 RR draws × 100 TMREL draws (recycled) × optional temperature draws
- **Temperature zones**: Annual mean temp per pixel (6–28°C), determines which ERF curve segment applies
- **TMREL**: Theoretical Minimum Risk Exposure Level — the death-weighted optimal temperature per location. Provided at decadal resolution; can be derived annually from cause-specific mortality + RR curves.

## Important Caveats

- **Large files**: ERF curve CSVs (~130 MB each), zipped mortality archives (~880 MB each), shapefiles (33 MB)
- **TMREL sparse years**: Files only contain 1990, 2010, 2020 — pipeline fills gaps by nearest-year
- **Colombia verification**: `COLOMBIA_VERIFICATION` flag toggles 4 methodological deviations from Burkart. Waiting on Samuel's output numbers for calibration.

## R Dependencies

Core: `data.table`, `ggplot2`
Geospatial (for outputs): `sf`, `raster`
