# Pipeline state — temperature-attributable mortality projections

**Purpose.** Snapshot of where the global pipeline is as of 2026-05-06,
prepared for an alignment meeting with the World Bank Climate Change
Knowledge Portal (CCKP) team who will be running production at scale.
Intended audience: technical staff who haven't been embedded in the
methodology development.

**Status in one sentence.** The pipeline machinery is validated against
a known reference (Samuel's published Colombia 2010-2019 results) for a
single country at the national-annual aggregation level; several
methodology and architecture decisions remain open before it is ready
to run globally on real climate projections.

This document covers (i) where the pipeline is today, (ii) the data
interfaces in and out, (iii) what's locked vs still open, (iv) what
needs to land before the CCKP team can execute production runs, and
(v) a glossary of terms.

---

## 1. Methodology in brief

The deliverable is mortality (and Years of Life Lost — YLLs) attributable
to non-optimal ambient temperature, by location × age × sex × year ×
cause × scenario, projected forward under the Shared Socioeconomic
Pathway (SSP) and Representative Concentration Pathway (RCP) climate
scenarios.

Methodologically the pipeline follows Burkart et al. (Lancet 2021) and
the Global Burden of Disease (GBD) 2023 framework:

1. **Exposure-Response Functions (ERFs)** for 17 temperature-sensitive
   causes of death, fitted on ~58.9 million daily-resolution deaths from
   8-9 countries, at the (mean annual temperature zone × daily
   temperature × cause) level. Each ERF gives the relative risk (RR) of
   cause-specific death as a function of daily temperature within an
   annual mean-temperature zone, with 1000 posterior draws per cell.
2. **Theoretical Minimum Risk Exposure Levels (TMRELs)** by location and
   temperature zone — the death-weighted optimal temperature, derived
   from cause-of-death composition + ERFs.
3. **Population Attributable Fractions (PAFs)** computed per pixel-day
   from gridded daily temperature, ERFs, and TMRELs, then
   population-weighted within a location.
4. **Attributable burden** = population × PAF × cause-specific mortality
   rate. YLLs derived by multiplying attributable deaths by remaining
   life expectancy.

Key methodology decisions documented in `step2-comparison.md` and the
project's `comparison-report-pipeline-vs-samuel.md`.

---

## 2. Pipeline architecture

All code lives in `global-scripts/`. Pure R; no build system. Scripts
are run sequentially, orchestrated by `run_location.R`. Each script
sources `config.R` for paths, location, and flags.

### Pipeline scripts

| Script | Role |
|---|---|
| `00_download_gbd.R` | Bootstrap helper: download IHME GBD inputs (used during initial setup, not part of per-run flow) |
| `01_load_erf.R` | Load 17 cause-specific ERF curves; compute mean / lower / upper / 99th-percentile RR per (zone, daily_temp, cause) |
| `02_load_tmrel.R` | Load TMRELs for the location; fill missing study years by nearest-year; in COLOMBIA_VERIFICATION mode, average across source years |
| `03_load_temperature.R` | Load daily pixel temperature; assign mean-annual-temperature zones; truncate temps to ERF range; compute pixel population weights |
| `04_load_mortality.R` | Load cause-specific mortality at year × cause × age × sex |
| `05_compute_pafs.R` | Merge temperature × ERF × TMREL; compute PAFs (heat / cold); attributable burden = deaths × PAF (× SEV in verification mode) |
| `06_compute_sevs.R` | Compute Summary Exposure Values per (year, zone, cause); diagnostic output, not used in burden in production mode |
| `07_compute_ylls.R` | Convert attributable deaths to YLLs using life tables |
| `08_outputs.R` | Aggregate, summarize, write final CSVs and figures |
| `run_location.R` | Orchestrator: runs 01 → 08 in correct order for one location |

### Configuration flags (`config.R`)

| Flag | Default | Effect |
|---|---|---|
| `LOCATION_ID` | 125 | GBD location identifier |
| `YEAR_START` / `YEAR_END` | 2010 / 2019 | Study period |
| `USE_DRAWS` | FALSE | If TRUE, propagates 1000 ERF draws × 100 TMREL draws (recycled) end to end. If FALSE, uses summary statistics. |
| `N_DRAWS` | 1000 | Number of draws when `USE_DRAWS = TRUE` |
| `COMPUTE_SEVS` | TRUE | Whether to compute SEVs (diagnostic output) |
| `COLOMBIA_VERIFICATION` | FALSE | Replicates Samuel's methodological choices for Colombia validation. Errors in `config.R` if set TRUE with `LOCATION_ID != 125`. **Must be FALSE for any production run.** |

### Validation tooling

| Script | Role |
|---|---|
| `util_compare_to_samuel.R` | Hard-codes Samuel's published Colombia numbers; emits pass/fail diff at ±5% tolerance |
| `util_diff_cached_conversion.R` | Determinism check for the Samuel-data converter |
| `samuel-runner/run_samuel_with_checkpoints.R` | Re-implements Samuel's `11_carga_atribuible.R` from standardized data paths; saves 5 intermediate-stage outputs for diff against the global pipeline |

---

## 3. Data inputs

The pipeline reads from `data/` (project-relative). Currently each
required input is staged for one location (Colombia, GBD location ID
125) for validation purposes. Production runs require these inputs at
GBD-location granularity globally.

### 3.1 ERF curves

- **Location:** `data/erf/{cause}_curve_samples.csv`
- **One file per cause:** 17 causes (`ckd`, `cvd_cmp`, `cvd_htn`,
  `cvd_ihd`, `cvd_stroke`, `diabetes`, `inj_animal`, `inj_disaster`,
  `inj_drowning`, `inj_homicide`, `inj_mech`, `inj_othunintent`,
  `inj_suicide`, `inj_trans_other`, `inj_trans_road`, `lri`,
  `resp_copd`)
- **File size per cause:** ~130 MB (verified locally for
  `ckd_curve_samples.csv`)
- **Total ERF bundle size:** ~2.2 GB across all 17 causes (computed)
- **Format:** CSV; one row per (mean-annual-temperature zone, daily
  temperature in 0.1 °C units), columns are `meanTempCat`, `dailyTempCat`,
  and 1000 posterior draws (`draw_0` through `draw_999`) of log-RR
- **Source:** Burkart et al. 2021 supplementary material

### 3.2 TMRELs

- **Location:** `data/tmrel/tmrel_{LOCATION_ID}.csv` (1000-draw level)
  and `data/tmrel/tmrel_{LOCATION_ID}_summaries.csv` (mean, lower,
  upper)
- **One file per GBD location**
- **Format:** Sparse on year — each location has values at 1990, 2010,
  2020 only. The pipeline fills intermediate study years by
  nearest-year.
- **Source:** Burkart et al. 2021

### 3.3 Temperature

- **Pipeline-expected format:**
  `data/temperature/{LOCATION_ID}_daily_temp.rds` — one row per
  pixel-day with columns `pixel_id`, `date`, `daily_temp` (°C), `pop`
  (pixel population), optional `subloc_id`.
- **For Colombia validation:** ~5.46 million rows for 1{,}494 pixels ×
  3{,}650 days = 10 years (Samuel's bundled ERA5 historical at ~1 km,
  matching WorldPop grid).
- **Production source (CCKP CMIP6 daily, verified 2026-05-13):**
  - S3: `https://wbg-cckp.s3.amazonaws.com/data/cmip6-daily-x0.25/{var}/{model-scenario}/`
  - File-per-(model, scenario, year):
    `timeseries-tas-daily-mean_cmip6-daily-x0.25_{model-scenario}_timeseries_mean_{year}.nc`
    (~773 MB / file)
  - **Grid:** 1{,}440 lon × 721 lat at 0.25°. Lat range -90..90 south-to-north;
    lon range **-180..179.75** (CCKP applies `lonFlip`, not 0-360).
  - **Variable name** matches filename prefix: `timeseries-tas-daily-mean`.
  - **Units: °C** (not Kelvin — confirmed via annual-mean probe metadata).
  - **Missing value:** 1e+20.
  - **Calendar:** ⚠ varies by CMIP6 model (gregorian / 365_day / 360_day).
    The annual climatology probe reports `gregorian`; daily files
    per-model TBD until path-A daily file lands.
  - **Models:** 34 in CCKP's archive (ACCESS-CM2, ACCESS-ESM1-5,
    BCC-CSM2-MR, CanESM5, CMCC-ESM2, CNRM-CM6-1, EC-Earth3, …, UKESM1-0-LL).
  - **Scenarios:** historical (1850-2014) + ssp126 / ssp245 / ssp370 / ssp585
    (2015-2100).
- **Adapter:** `global-scripts/util_convert_cckp_temperature.R` (added 2026-05-13)
  converts NetCDF → pipeline RDS. Handles bbox subset to a `LOCATION_ID`,
  optional admin-1 tagging via shapefile, calendar variants. Pixel-id encoded
  as `lat_idx * 1440 + lon_idx` (stable across files / models).

### 3.4 Mortality

- **Location:** `data/mortality/{LOCATION_ID}_mortality.rds`
- **Format:** One row per (year, cause, age group, sex) with columns
  `year_id`, `acause`, `age_group_id`, `sex_id`, `deaths`,
  `location_id`
- **Source for production:** IHME GBD 2021 cause-specific mortality
  forecasts, draws by location × age × sex × year × scenario, all 17
  causes plus all-cause. **The exact format of this input will depend
  on the IHME data ask currently in flight** (see Section 7 below).

### 3.5 Life tables

- **Location:** `data/lifetables/{LOCATION_ID}_lifetable.rds`
- **Format:** One row per (year, age group, sex) with column `ex`
  (remaining life expectancy)
- **Source for production:** UN World Population Prospects, or
  IHME-Vollset, or local national life tables — to be locked

### 3.6 Population

- **Bundled with temperature input:** `pop` column on
  `data/temperature/{LOCATION_ID}_daily_temp.rds`.
- **Production source (CCKP gridded, verified 2026-05-13):**
  - S3: `https://wbg-cckp.s3.amazonaws.com/data/pop-x0.25/popcount/{dataset}-{scenario}/`
  - Dataset: `gpw-v4-rev11` (Gridded Population of the World v4 rev 11).
  - **SSP scenarios** in CCKP: ssp119 / ssp126 / ssp245 / ssp370 / ssp585 + historical.
  - **Same grid as CCKP CMIP6 temp** (1{,}440 × 721 at 0.25°) — no regridding needed.
  - File-per-(dataset, scenario, year-range):
    `climatology-popcount-annual-mean_pop-x0.25_{dataset}-{scenario}_climatology_mean_{ystart}-{yend}.nc`
    (~4.25 MB / file).
  - **Variable name:** `climatology-popcount-annual-mean`. **Units:** persons per pixel
    (count, not density). **Missing value:** 1e+20.
  - **Sanity check (2020-2039 ssp245):** global sum 8.25B, max-pop pixel 10.7M.
- **Two population roles in the math** (same SSP-gridded dataset serves both):
  1. **Gridded population** for spatial weighting in the within-subloc PAF.
  2. **Aggregated location × (age × sex) population** for converting rates
     to counts. For the age × sex decomposition we still need an age/sex
     distribution — open decision (Wittgenstein / IIASA SSP / IHME Vollset
     if shared).

---

## 4. Data outputs

Outputs land in `output/` with subdirectories `intermediate/`,
`results/`, `figures/`, `diagnostics/`. The `output/` directory is
gitignored.

| File | Granularity | Contents |
|---|---|---|
| `output/intermediate/erf_curves.rds` | (zone, daily_temp, cause) | Mean / lower / upper / 99th-percentile RR |
| `output/intermediate/tmrel.rds` | (zone, year) | TMREL summary per zone-year |
| `output/intermediate/temperature.rds` | (pixel, date) | Cleaned temperature input + assigned zones |
| `output/intermediate/mortality.rds` | (year, cause, age, sex) | Cleaned mortality input |
| `output/results/pafs_{LOCATION_ID}.rds` | (year, cause) | `paf_heat`, `paf_cold`, `paf_nonopt` |
| `output/results/burden_{LOCATION_ID}.rds` | (year, cause) | `deaths`, `deaths_heat`, `deaths_cold`, `deaths_nonopt` |
| `output/results/sevs_{LOCATION_ID}.rds` | (year, cause) | `sev` (diagnostic) |
| `output/results/ylls_{LOCATION_ID}.rds` | (year, cause) | `yll_heat`, `yll_cold`, `yll_nonopt` |
| `output/results/ylls_detail_{LOCATION_ID}.rds` | (year, cause, age, sex) | YLL with full age × sex detail |
| `output/results/summary_{LOCATION_ID}.csv` | (year, cause) | All measures merged |
| `output/results/total_by_year_{LOCATION_ID}.csv` | (year) | Aggregated across causes |
| `output/results/comparison_to_samuel_{LOCATION_ID}.csv` | metric | Validation diff table (Colombia only) |

---

## 5. Run instructions

Single-location run:

```
distrobox enter emacs-r -- Rscript global-scripts/run_location.R --location_id=125
```

The pipeline expects the working directory to be the project root.
`run_location.R` invokes scripts 01 → 06 → 05 → 07 → 08 in that order
(SEVs are computed before PAFs because in COLOMBIA_VERIFICATION mode
the burden formula needs them; in production mode the order is not
strictly required).

R is currently invoked through a Distrobox container (`emacs-r`,
R 4.5.2 — verified locally). The CCKP team's cluster R version, R
package availability, and any container/dependency requirements are
open questions for the meeting (Section 7).

For the Colombia validation run, after `run_location.R` completes:

```
distrobox enter emacs-r -- Rscript global-scripts/util_compare_to_samuel.R
```

emits the pass/fail diff against Samuel's published Colombia 2010-2019
numbers.

---

## 6. Compute footprint

All numbers in this section are **observations on a local
workstation**, not extrapolations to a cluster context. Cluster
behavior (memory limits, scheduler overhead, parallel run patterns)
will need its own benchmarking.

- **Single-location run (Colombia, summary mode):** ~1 minute wall time
  (verified, multiple runs)
- **Samuel-runner reference implementation:** ~90 seconds, ~11 GB peak
  RAM (reported by the Samuel-checkpoint validation agent — magnitude
  is approximate, not independently verified)
- **Storage per location-run:** intermediate + results files are tens
  of MB per location; temperature input is the largest at ~20 MB per
  location-decade RDS. Rough guess for global runs at GBD-location
  granularity (~1,034 locations × 17 causes × 30 forecast years × 4
  scenarios × 500 draws): the input/output volume could be substantial
  — needs scoping with CCKP infrastructure team. **Storage estimate
  not verified.**
- **Draw-mode (`USE_DRAWS = TRUE`) runtime:** not measured for full
  ERF × TMREL × temperature draws. Likely materially slower than
  summary mode given the order-of-magnitude increase in row counts.
  **Not verified.**

---

## 7. Decisions: locked vs open

### Locked

- **ERF source:** Burkart et al. 2021 (17 causes), used as-is.
- **PAF formula:** `(RR-1)/RR` for harmful effects, `-((1/RR)-1)/(1/RR)`
  for protective effects, per Burkart 2021 and IHME GBD 2021 forecast
  appendix.
- **Mortality anchor for projections:** rates, not counts (decided
  2026-05-06 based on cohort-component analysis of IHME's Vollset
  population integration).
- **COLOMBIA_VERIFICATION mode:** strictly for replicating Samuel's
  Colombia bugs; safety-guarded against use with any other location.
- **Validation reference:** Samuel's Colombia 2010-2019 published
  numbers. After the subnational refactor and depto-day verification
  branch (2026-05-07), 18 of 19 headline metrics pass at ±5% tolerance:
  totals within ~2–3%, top-3 cause rankings exact, only `inj_trans_road`
  heat YLL still fails at -11.76%. See `colombia-validation-state.tex`
  (or `.pdf`) for the snapshot.
- **Subnational dimension (was O1, closed 2026-05-07):** PAFs computed
  at (year, subloc_id, cause); mortality at full
  (year, subloc_id, age, sex, cause); merged at full granularity. For
  non-Colombia locations, `subloc_id` falls back to a single value =
  `LOCATION_ID` if input lacks a subnational column.

### Open — affects production runs

| # | Decision | Notes |
|---|---|---|
| ~~O1~~ | ~~Subnational aggregation level~~ | **Closed 2026-05-07.** Subnational dimension carried through PAF / SEV / burden / YLL. Verification mode adds a depto-day attribution branch. See locked decisions above. |
| O2 | Heat / cold split for projections | IHME does not separate them. Our pipeline does. To report heat-attributable and cold-attributable separately, we apply our pipeline's split proportions to the combined IHME-anchored projection. To be confirmed in the deliverable spec. |
| O3 | 17-vs-12 cause coverage | Burkart fits 17 causes; IHME's GBD 2021 forecast module fits 12. Five Burkart causes have no temperature signal in IHME's forecasts. Decision: include forward-only via our pipeline (with disclosure) or exclude. |
| O4 | SSP-aligned gridded population source | CCKP publishes GPW v4 rev 11 with SSP scenarios on the same 0.25° grid as their CMIP6 temperature (verified 2026-05-13) — natural plug-in choice for grid consistency. Other options still on the table: WorldPop fixed-2020, Jones-O'Neill / NCAR, Murakami. Impact on within-country spatial weighting is bounded but unverified across choices. |
| O5 | SSP-aligned location × age × sex population for rates → counts | Vollset (from IHME if shared, otherwise external Wittgenstein / IIASA SSP database). Determines whether projections include SSP demographic differences in the level. |
| O6 | TMREL zone migration approach for forward projections | Pixels can move between mean-annual-temperature zones under warming. IHME uses a 10-year rolling mean to assign zones plus inter-zone interpolation for boundary cases. Our pipeline currently uses an annual mean per pixel-year with no inter-zone smoothing. Open methodology design. |
| O7 | Bias-correction recipe for CMIP6 → ERA5 | CCKP's `cmip6-daily-x0.25` product is already downscaled and bias-corrected to 0.25° (verified 2026-05-13: tas attributes mention `tasmax & dtr` derivation and CMOR processing). IHME aligns at 2021 with their own bias-correction layer — open question whether CCKP's pre-applied correction matches IHME's, and how to reconcile if not. |
| O8 | Forecast horizon | 2050 matches IHME's appendix horizon. 2100 requires a Wittgenstein-demographics + GBD-Foresight-cause-shares + Burkart-ERFs hybrid approach (sketched in `gbd/ihme-plan-b-prep.tex`, not implemented). |
| O9 | Uncertainty representation in deliverable | Full draws? Mean + 95% CI? At what aggregation level? |
| O10 | Workflow B mechanics | Anchor on IHME rates, apply our pipeline's cross-SSP scalar ratio, multiply by SSP-aligned population. Several open methodology questions for IHME tied to this — see `handoff-ihme-temp-unscaling-validity.md` and the in-flight rate ask. |

### Open — affects handoff process to CCKP

- **Branch / merge strategy.** Subnational refactor is currently on a
  feature branch. Merge to `main` should follow the refactor and a
  re-validation run.
- **Container / environment.** Project currently uses Distrobox locally
  (R 4.5.2 in the `emacs-r` container). Cluster-portable equivalents
  (Singularity / Apptainer image, `renv` lockfile) not yet built.
- **Methodology document.** No standalone document covers all the
  decisions in Section 7 in one place. Either we write that or extract
  it from existing documents (`step2-comparison.md`,
  `comparison-report-pipeline-vs-samuel.md`,
  `ihme-unscaling-validity-report.md`).

---

## 8. Validation status

- **Colombia 2010-2019 (national, annual):** pipeline reproduces
  Samuel's published numbers to within 5–10% on totals and most
  per-cause metrics, with residual gap traced to the subnational
  aggregation difference (Section 7, O1). All known toggleable
  methodology differences between Samuel and the global pipeline have
  been replicated under `COLOMBIA_VERIFICATION = TRUE` and patches
  documented in `comparison-report-pipeline-vs-samuel.md`.
- **Other locations:** not yet run on real data. Synthetic test data
  (`util_generate_test_data.R`) confirms end-to-end pipeline mechanics
  but does not validate scientific results.
- **Forward projections (CMIP6 inputs):** not yet implemented — the
  pipeline currently expects historical temperature. Forward-projection
  capability is open work.

---

## 9. Open work items before production handoff

In rough priority order:

1. ~~**Subnational refactor.** Add depto / admin-1 dimension through
   PAF, SEV, burden, YLL. Closes the 5–10% Colombia validation gap and
   is independently needed for global accuracy.~~ **Done 2026-05-07.**
2. **Forward-projection inputs.** CCKP CMIP6 daily temperature + GPW
   SSP gridded population formats verified 2026-05-13; adapter scaffolded
   at `global-scripts/util_convert_cckp_temperature.R`. Calendar-variant
   handling (gregorian / 365_day / 360_day) drafted but pending verification
   against an actual daily file. Per-location run-driver loop (over years,
   models, scenarios) still TBD.
3. **IHME data integration.** Once the rate-level data ask is fulfilled,
   the Workflow B mechanics need to be wired into the pipeline (apply
   our cross-SSP scalar ratio to IHME's published rates, multiply by
   our chosen population denominator). This depends on which decisions
   in O4 / O5 / O10 land.
4. **Cluster-portable environment.** `renv` lockfile or container image
   to ensure CCKP's cluster R doesn't differ from the validated local
   environment.
5. **Methodology document.** One place that covers all the decisions
   above with the supporting reasoning. The CCKP team will want this
   for their own review and reproducibility records.
6. **Output specification freeze.** Once the heat/cold split, cause
   coverage, and uncertainty decisions are made (O2, O3, O9), the
   final output schema can be locked.

---

## 10. Known issues / honest caveats

- ~~The 5–10% validation gap on Colombia is structural (subnational
  aggregation), not a bug. The fix is the refactor in item 1 above.~~
  Closed 2026-05-07: subnational refactor + depto-day verification branch
  bring 18 of 19 headline metrics within ±5%.
- One Colombia per-cause metric (`inj_trans_road` heat YLL) still fails
  at -11.76%. Hypothesized causes (mortality imputation, ERF
  point-estimate convention, garbage-code redistribution, age
  restrictions) documented in `colombia-validation-state.tex` Section 5.
- Some methodology decisions hinge on what IHME shares (rates
  available? Vollset populations? alignment shift inputs?). The IHME
  data ask is in flight; depending on responses, several of the open
  decisions in Section 7 may need to be revisited.
- The pipeline uses age-invariant ERFs (same RR for all ages), matching
  Burkart 2021 and IHME GBD 2021. This is a known methodology
  simplification — temperature mortality risk almost certainly varies
  by age, but neither published reference has age-stratified ERFs.
- Bias correction for CMIP6 → ERA5 has not been finalized. IHME's
  approach (anchored at 2021) is the current reference but the
  recipe specifics from their appendix are not exhaustive.

---

## 11. Glossary

- **CCKP** — Climate Change Knowledge Portal (World Bank)
- **CMIP6** — Coupled Model Intercomparison Project Phase 6 (climate
  model ensemble)
- **DALY** — Disability-Adjusted Life Year (sum of YLL and YLD)
- **ERF** — Exposure-Response Function (relationship between exposure
  and mortality risk)
- **ERA5** — ECMWF Reanalysis v5 (historical climate reanalysis used
  for ERF fitting)
- **GBD** — Global Burden of Disease (IHME's flagship project)
- **IHME** — Institute for Health Metrics and Evaluation (University of
  Washington; produces GBD)
- **IIASA** — International Institute for Applied Systems Analysis
  (publishes the SSP database)
- **MR-BRT** — Meta-Regression Bayesian Regularized Trimmed (statistical
  framework used by IHME for ERF fitting)
- **PAF** — Population Attributable Fraction (fraction of disease
  burden attributable to a risk factor)
- **PYPLL** — Productive Years of Potentially Lost Life (Spanish: AVPP;
  used in Samuel's report — equivalent to YLL for working-age range)
- **RCP** — Representative Concentration Pathway (greenhouse-gas
  concentration scenarios)
- **RR** — Relative Risk (ratio of risk under exposure to risk at
  reference exposure)
- **S** / **scalar** — Risk scalar = 1 / (1 − PAF_total); the
  multiplicative factor between risk-deleted and total mortality
- **SDI** — Socio-Demographic Index (IHME composite of income,
  education, fertility)
- **SEV** — Summary Exposure Value (population exposure intensity,
  bounded [0, 1])
- **SSP** — Shared Socioeconomic Pathway (socioeconomic narrative
  scenarios, e.g. SSP1 sustainability through SSP5 fossil-fueled)
- **TMREL** — Theoretical Minimum Risk Exposure Level (death-weighted
  optimal temperature)
- **WHO** — World Health Organization
- **WorldPop** — Public 1 km gridded population product (worldpop.org)
- **YLL** — Years of Life Lost (sum of attributable deaths × remaining
  life expectancy)
- **YLD** — Years Lived with Disability
- **Vollset (2020)** — IHME's demographic forecasting methodology
  reference (Lancet 2020, Vollset et al.)
- **Workflow B** — Plan to anchor projections on IHME's published rates,
  apply our pipeline's cross-SSP temperature scalar ratio, multiply
  by SSP-aligned population externally. Documented in
  `gbd/ihme-plan-b-prep.tex` (terminology there is "rescaling" /
  "back-out and re-multiply").
