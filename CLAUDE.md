# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

World Bank statistical consulting project quantifying mortality and morbidity (AVPP — Años de Vida Potencial Perdidos / Years of Potential Life Lost) attributable to non-optimal temperatures across Colombian departments for 2010–2019, with projections to 2050. Based on the Burkart et al. (Lancet 2021) methodology and the GBD 2023 framework.

## Architecture

This is a pure R epidemiological analysis pipeline with no build system, tests, or CI/CD. Scripts are run sequentially in RStudio.

### Pipeline (Scripts/Colombia/)

Scripts are numbered and must be run in order — each depends on outputs from prior steps:

1. `01_mortalidad_2010_2020.R` — Extract/clean 2010–2021 mortality data from DANE vital registration
2. `02_descriptivos_mortalidad.R` — Mortality descriptive statistics
3. `03_descriptivos_poblacion.R` — Population descriptive statistics
4. `04_proyecciones_poblacion.R` — DANE population projections (2010–2050) by department/age/sex
5. `05_proyecciones_mortalidad.R` — Mortality rate projections
6. `06_serie mortalidad.R` — Time series analysis of mortality
7. `07_estimaciones_AVPP.R` — AVPP calculations using life tables
8. `09_curvas_ER.R` — Process 19 environmental risk factor curves (CSVs → single RDS)
9. `10_ajuste_temperatura.R` — Temperature data alignment, Kelvin → Celsius conversion
10. `10_2_Proces_temp.R` — Additional temperature processing
11. `11_carga_atribuible.R` — Attributable burden via PAF (Population Attributable Fraction)
12. `12_salidas_graph_final.R` / `12_1_mapas_interactivos.R` — Output maps and figures

### Key Data Inputs

- **Mortality**: DANE death certificates with ICD-10 codes (format varies by year: xlsx, txt, csv). Source archives are the two `bod shared-*.zip` files (~880 MB each)
- **Temperature**: `temp_colombia_dias.csv` (daily) and `temp_colombia_horas.csv` (hourly) by department
- **Risk curves**: 19 cause-specific temperature-response curves with 1,000 Monte Carlo draws each, in `Info Burkart/ERF/`
- **TMREL**: Theoretical Minimum Risk Exposure Level data in `Info Burkart/TMRELs/`
- **GBD hierarchy**: `IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX`
- **Shapefiles**: Colombian admin boundaries in `GBD2023 Shape/`

### Key Methodological Details

- **18 temperature-related causes** per GBD taxonomy (CVD subtypes, CKD, diabetes, LRI, COPD, injuries)
- **Geographic unit**: 32 Colombian departments + DC Bogotá
- **Age groups**: 5-year bins (0–4 through 95+)
- **Uncertainty**: Propagated via 1,000 draws from risk curves
- **Location matching**: Scripts must reconcile DANE municipal codes, IHME location IDs, and raster grid indices

## Important Caveats

- **Hardcoded paths**: Scripts reference local `Bases/` directories relative to the RStudio project working directory. These paths will need updating on a new machine.
- **DANE format inconsistency**: Mortality data changes format across years, requiring year-specific parsing logic in the early scripts.
- **Large files**: Zipped mortality archives, CSV curve samples (130 MB+), and shapefiles (33 MB) are stored in the repo directory.
- **Language**: Script comments and variable names are in Spanish.

## R Dependencies

Core: `tidyverse`, `data.table`, `readxl`, `feather`, `janitor`
Geospatial: `sf`, `rgdal`, `raster`, `sp`, `mapview`, `ggspatial`
Visualization: `ggplot2`, `RColorBrewer`, `cowplot`, `classInt`
Reporting: `gtsummary`, `flextable`, `officer`
