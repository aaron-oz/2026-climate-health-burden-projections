# Data Directory

This directory contains all input data for the global climate-health burden pipeline.

## Directory Structure

```
data/
├── erf/              ✅ ERF curve CSVs (17 causes, symlinked from Info Burkart)
├── tmrel/            ✅ TMREL files (1034 locations, symlinked from Info Burkart)
├── shapefiles/       ✅ GBD 2023 shapefile (symlinked)
├── temperature/      ❌ NEEDED: ERA5 daily temperature by location
├── population/       ❌ NEEDED: WorldPop gridded population by location
├── mortality/        ❌ NEEDED: GBD cause-specific deaths by location
└── lifetables/       ❌ NEEDED: Life tables by location
```

## What's Available

### ERF Curves (`erf/`)
- 18 CSV files: `{cause}_curve_samples.csv`
- Columns: `annual_temperature`, `daily_temperature`, `draw_0` through `draw_999`
- Draws are in log-RR space (exponentiate to get RR)
- These are global — same curves apply to all locations

### TMRELs (`tmrel/`)
- 1034 location IDs, two files each:
  - `tmrel_{loc_id}.csv` — 1000 draws
  - `tmrel_{loc_id}_summaries.csv` — mean, lower, upper
- Columns: `location_id`, `year_id`, `meanTempCat`, `tmrel_0`...`tmrel_999`
- Years: 1990–2020

### GBD Shapefile (`shapefiles/`)
- `GBD2023_mapping_final.shp` — global administrative boundaries with `loc_id`

## What's Needed

### Temperature (`temperature/`)
Expected format: `{location_id}_daily_temp.csv` (or `.rds`) with columns:
- `pixel_id`: unique pixel identifier within the location
- `date`: daily date (YYYY-MM-DD)
- `daily_temp`: daily mean temperature in Celsius
- `pop`: pixel population
- `temp_sd`: (optional) daily temperature standard deviation for Monte Carlo draws

Source: ERA5 reanalysis (historical) or SSP/CMIP6 (projections)

### Mortality (`mortality/`)
Expected format: `{location_id}_mortality.csv` with columns:
- `location_id`, `year_id`, `age_group_id`, `sex_id`
- `cause_id`, `acause`
- `deaths`

Source: GBD Results Tool (run `00_download_gbd.R`) or IHME data request

### Population (`population/`)
Expected format: `{location_id}_population.csv` with columns:
- `pixel_id`, `year`, `pop`

Source: WorldPop (https://www.worldpop.org/)

### Life Tables (`lifetables/`)
Expected format: `{location_id}_lifetable.csv` with columns:
- `year_id`, `age_group_id` (or `age`), `sex_id`, `ex` (life expectancy)

Source: GBD or UN World Population Prospects
