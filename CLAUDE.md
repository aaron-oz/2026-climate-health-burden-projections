# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

World Bank statistical consulting project quantifying mortality and morbidity (YLLs / AVPP) attributable to non-optimal temperatures. Currently validating with Colombia data (2010–2019), with the goal of scaling globally and projecting to 2100 under SSP scenarios. Based on the Burkart et al. (Lancet 2021) methodology and the GBD 2023 framework.

The stressor-resilience framework (a related but separate project) now lives in the Dropbox tree at `~/Dropbox/OZ-Labs/WorldBank/2025-2026-climate-resilience/2026-stressor-resilience-framework/` (it is no longer a sibling of this repo).

## Workspace Layout (Aaron's machine)

Since 2026-08-14 the canonical checkout is `/var/home/aoz/code/2026-climate-health-burden-projections/`. The old Dropbox copy (`~/Dropbox/OZ-Labs/WorldBank/2025-2026-climate-resilience/2026-climate-health-burden-projections/`) was moved here and no longer exists; do not recreate it.

Large untracked data lives at `/var/home/aoz/data/wb-temp-attr-projections/` and is symlinked into the checkout:

- `data/erf`, `data/cmip6-scratch`, `data/gbd-forecasts`, `GBD2023 Shape`: directory symlinks
- `from-samuel/`: real directory (31 tracked files) with symlinks for the big payloads (`Info Burkart/ERF`, `Info Burkart/TMRELs`, the DANE `*.zip` archives, `results`, large PDFs/CSVs)
- `output/summary/ssp245`: symlink to `review/ssp245/` under the data root, holding the summary tables from Caspar's ssp245 production run (~640 MB on disk, too large for git)

Worktrees for branch work go in `/var/home/aoz/code/2026-climate-health-burden-projections-worktrees/`.

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

## Credentials

### UN Data Portal API token (`UN_API_TOKEN`)

Needed only by `global-scripts/util_download_un_lifetables.R`, which fetches UN
WPP 2024 life tables into `data/lifetables/`.

**A token already exists. Do not ask for a new one before checking here.** It
lives in `run_env.sh` at the repo root, which is gitignored (`.gitignore:102`)
and mode 600. Load it with:

```bash
source ./run_env.sh
```

A second copy is still embedded in this checkout's Claude permissions allowlist,
`.claude/settings.local.json`, inside an allowed `Bash(export UN_API_TOKEN=...)`
rule. That was its only home until 2026-08-05, and repeated sessions failed to
find it there and wrongly concluded no token existed. Treat `run_env.sh` as
authoritative; the allowlist copy is a leftover that a `/permissions` cleanup
may delete at any time.

Facts verified 2026-08-05:

- The token is a JWT valid from 2026-04-07 to **2027-04-07** (`nbf` / `exp` claims).
  It is not expired.
- The token **value** has never been committed. `git log -S` on the value returns
  nothing across all branches. Only the variable name appears in tracked files
  (`util_download_un_lifetables.R`, commit 816b4ce).
- `.claude/settings.local.json` is ignored globally via
  `~/.config/git/ignore:1`, and `run_env.sh` via `.gitignore:102`.

**Do not paste the token value into chat, commit messages, or any tracked file.**

New tokens are free from https://population.un.org/dataportalapi/index.html.

### UN Data Portal outage (observed 2026-08-05)

The whole `population.un.org/dataportalapi` service was returning HTTP 502,
including its own `index.html`, while `population.un.org/wpp/` returned 200. So
a 502 from the life-table downloader means the service is down, not that the
token is bad. Check `curl -sI https://population.un.org/dataportalapi/index.html`
before spending time on auth.

**Token-free path, and the one to prefer: `util_fetch_un_lifetables_bulk.R`.**
It builds the same life tables from the public WPP bulk CSVs, needs no
credentials, makes two HTTP requests instead of ~204, and is not subject to the
API's rate limiting.

```bash
Rscript global-scripts/util_fetch_un_lifetables_bulk.R                  # all
Rscript global-scripts/util_fetch_un_lifetables_bulk.R --locations=14,413
```

Existing files are skipped unless `--force`; `--refetch` re-downloads the bulk
CSVs rather than reusing the ~290 MB cache under `data/un-wpp-bulk/` (override
the cache location with `UN_WPP_BULK_DIR`). Verified 2026-08-05: it reproduces
all 204 committed life tables exactly, zero difference in `ex` across 769,896
values.

The rest of this section documents the underlying data, for anyone needing to
change the script or reproduce it by hand.

The public WPP bulk CSVs need no auth:

```
https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/
  WPP2024_Life_Table_Abridged_Medium_1950-2023.csv.gz   (144,479,958 bytes)
  WPP2024_Life_Table_Abridged_Medium_2024-2100.csv.gz   (148,739,104 bytes)
```

Why the two sources agree despite reading differently named files: the API path
uses indicator 76 (complete, single-year ages) but keeps `ex` only at the 5-year
age starts 0, 5, ..., 80 (`AGE_STARTS`), and the *abridged* bulk table carries
`ex` at those same exact ages. So the abridged file is not a coarser
approximation of what the API returns, it is the same values.

Extraction recipe, if it needs repeating:

- Join on `ISO3_code` against `ihme_lc_id` from the augmented shapefile. All 204
  locations are present in the bulk files.
- Filter `Variant == "Medium"` (the only variant in these files), `SexID` in
  1, 2 (the files also carry 3 = Total; 1 = Male / 2 = Female already match the
  GBD `sex_id` convention, no remapping needed), `AgeGrpStart` in
  0, 5, ..., 80, and `Time` in 1990–2100.
- Age 0 appears twice per sex-year in an abridged table, as the span-1 infant
  row and inside a wider band. Keep `AgeGrpSpan == 1`. Skipping this silently
  doubles the age-0 rows.
- Emit `year_id, age_group_id, sex_id, ex, location_id`, which is the format
  `07_compute_ylls.R` merges on. Correct output is 3,774 rows per location
  (111 years x 17 age groups x 2 sexes).

## Life table coverage: 204 locations, not 198

The burden universe is **204** GBD locations. `GBD2023_mapping_final.shp` carries
only **198** level-3 rows: it has no geometry at all for Maldives (14), Marshall
Islands (24), Monaco (367), Nauru (369), Tokelau (413), Tuvalu (416).

Anything that derives a location list from the *un-augmented* shapefile will
silently drop those six. That is what happened to the life-table downloader
before 2026-08-05: the six were never in its UN crosswalk, so their life tables
were never requested, and every run of those locations died in
`07_compute_ylls.R` with "No life table found for location N", ~40 seconds of
PAF computation after the real problem.

Use `config.R::DEFAULT_SHAPEFILE`, which auto-prefers
`GBD2023_mapping_final_augmented.shp` when present. Build it once per machine
with `util_augment_shapefile.R`. `util_download_un_lifetables.R` now mirrors that
auto-prefer logic rather than hardcoding the original file.

UN WPP coverage is **not** the constraint for microstates: all 204 locations,
including the six, are present in the WPP 2024 bulk life tables.

Resolved 2026-08-05: all six were built from the WPP bulk CSVs (see the fallback
recipe above) and installed, so `data/lifetables/` now holds **204** tables.
These files are tracked in git (`.gitignore:59` un-ignores
`data/lifetables/*_lifetable.csv`), so collaborators get them on pull rather
than needing to re-download.

## R Dependencies

Core: `data.table`, `ggplot2`
Geospatial (for outputs): `sf`, `raster`
