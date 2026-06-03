# Caspar's workflow and TODOs

This document is the operational handoff for running the production
pipeline. It lives in the repo (rather than only in email) so it can be
edited as questions come up, mistakes get found, or steps get refined.

Companion docs: [`README.md`](README.md) for project orientation,
[`pipeline-state-for-cckp.md`](pipeline-state-for-cckp.md) for architecture
and methodology details.

---

You can start some of these now (1) and we'll get (2)–(3) to you soon. After
that you should be able to try out (4)–(6). (7)–(9) are for the full
production run.

## 1. Download IHME forecast CSVs

Download the 17 IHME cause-specific rate CSVs + the population forecast CSV
into `data/gbd-forecasts/` (or any directory of your choice — you'll point
the converter at it in step 4). We've already validated the converter on
`ischemic_heart_disease.csv`.

## 2. Clone the pipeline repo

```
https://github.com/aaron-oz/2026-climate-health-burden-projections
```

## 3. Set up the R environment

We'll provide an `renv` lockfile and/or a container image to keep
R / `data.table` / `sf` versions matching what we validated on. Main
dependencies: `data.table`, `ncdf4`, `sf`, `readxl`.

## 4. Convert IHME CSVs to pipeline-format mortality

Run the converter once to turn IHME's rate × pop format into the pipeline's
mortality RDS files for every (cause, location) pair:

```bash
Rscript global-scripts/util_convert_ihme_batch.R \
  --source_dir=/path/to/where/caspar/downloaded/the/csvs
```

The script reads each cause CSV once, iterates all ~205 IHME locations
internally, and writes
`data/mortality/{LOC}_mortality_ihme_{acause}{,_draws}.rds` for every
(cause, location) pair. It is idempotent (skips combos whose RDS exists in
case you need to rerun).

Up front it prints a discovery table showing which file matched which
cause, so any mismatches are visible before the heavy reading work starts.
If IHME's UI gave you filenames different from what we guessed in
`config.R::IHME_CAUSE_FILES`, either rename or edit that list.

## 5. Pull a single CMIP6 NetCDF for the benchmark

Pull a single (model, scenario, year) NetCDF from your S3 archive into a
local cache directory — used by the next step.

## 6. Single-combo benchmark on your hardware

One location, one model, one year, full draws:

```bash
# Step 6a: Pull the CCKP CMIP6 NetCDF + GPW population for one combo and
# convert to the pipeline's pixel-day RDS format
Rscript global-scripts/util_run_cckp_pipeline.R \
  --location_id=125 \
  --models=access-cm2-r1i1p1f1 \
  --scenarios=ssp245 \
  --years=2022 \
  --shapefile=data/shapefiles/GBD2023_mapping_final.shp

# Step 6b: Run the burden pipeline (01-08) on that combo using IHME's
# cvd_ihd rates as the mortality input
Rscript global-scripts/util_run_cckp_burden.R \
  --location_id=125 \
  --models=access-cm2-r1i1p1f1 \
  --scenarios=ssp245 \
  --years=2022 \
  --use_draws=TRUE \
  --n_draws=500 \
  --mortality_file=data/mortality/125_mortality_ihme_cvd_ihd_draws.rds
```

**Key choices, in case you want to swap anything:**

- `--location_id=125` is Colombia. Any of the 205 IHME locations works;
  Colombia gives a "medium country" timing that we can compare directly to
  our desktop measurements. Brazil (135) or USA (102) would give a
  larger-country number; small-island countries like Tuvalu would give a
  low-end number.
- `--models=access-cm2-r1i1p1f1` is a single model — one combo only. We've
  verified ACCESS-CM2 works end-to-end on our side (it was our first
  probe), so any timing peculiarity here is genuinely about the pipeline,
  not the data format. Should produce ~30–40 s of pipeline wall-clock on
  top of the NetCDF download.
- `--years=2022` is the first IHME forecast year, single year. The CCKP
  NetCDF for this is ~750 MB, so the bulk of the wall-clock for the
  benchmark will actually be the download — worth noting separately when
  reporting back.
- `--n_draws=500` matches IHME's forecast draw count, what we'll run in
  production.
- `--mortality_file=...cvd_ihd_draws.rds` — you need to have run the
  converter for at least `cvd_ihd` before this command (step 4). One cause
  is enough for the timing test; the pipeline runs the cause-chunked PAF
  math for all 17 regardless of which causes have mortality input, so
  timing is representative even with one cause.

**Send us the wall-clock times from that run.** That lets us confirm our
total time estimates:

- The total wall-clock printed by `util_run_cckp_burden.R` (line like
  `[1/1] access-cm2-r1i1p1f1/ssp245/2022 -> ok (XX.Xs)`)
- The download timing logged by `util_run_cckp_pipeline.R` (the
  `Downloaded ... in <X>s` lines)

## 7. Launch the full SSP2-RCP4.5 reference run

All 200 locations, 29 models, 29 years (2022–2050).

Single-location version:

```bash
Rscript global-scripts/util_run_global.R \
  --location_id=125 \
  --scenarios=ssp245 \
  --years=2022-2050
```

Parallel across all ~205 locations using GNU parallel (one process per
location, 125 concurrent):

```bash
parallel -j 125 \
  'Rscript global-scripts/util_run_global.R --location_id={} --scenarios=ssp245 --years=2022-2050' \
  ::: $(Rscript -e 'cat(readRDS("output/intermediate/ihme_loc_map.rds")$loc_id)')
```

Each invocation does (a) download + convert CCKP CMIP6 NetCDFs for all 29
models × the specified scenario × years, then (b) run the burden pipeline
for each combo with the IHME mortality files as input. Per-location
wall-clock ~17 hours for 29 models × 29 years × 500 draws; with 125
parallel locations the global run completes in ~1 day per scenario.

For all four SSPs, change `--scenarios=ssp245` to
`--scenarios=ssp126,ssp245,ssp370,ssp585`. The runner is idempotent within
and across scenarios — the SSP2-RCP4.5 combos that double as both reference
and target only get computed once, and it should be easy to pick up if
something errors without needing to redo everything.

## 8. Spot check

We'll spot-check a sample of outputs against expected ranges, then give the
green light for the three target-scenario runs (SSP1-RCP1.9, SSP3-RCP7.0,
SSP5-RCP8.5). Each of those runs shares the reference run's denominator, so
no rerunning SSP2.

## 9. Apply the Workflow B ratio

After all four runs finish, combine each target with the reference into
per-scenario attributable burden:

Single-location version:

```bash
Rscript global-scripts/util_apply_workflow_b_batch.R \
  --location_id=125 \
  --ref_scenario=ssp245 \
  --target_scenarios=ssp126,ssp245,ssp370,ssp585 \
  --years=2022-2050
```

Parallel across all locations:

```bash
parallel -j 125 \
  'Rscript global-scripts/util_apply_workflow_b_batch.R --location_id={} --target_scenarios=ssp126,ssp245,ssp370,ssp585' \
  ::: $(Rscript -e 'cat(readRDS("output/intermediate/ihme_loc_map.rds")$loc_id)')
```

Per-combo work is just I/O + a small ratio multiplication, so this runs
orders of magnitude faster than the burden pipeline itself — probably
~minutes per location, ~hour total for all 200 locations × 29 models × 29
years × 4 targets in parallel. Outputs land at
`output/results/workflow_b/{LOC}/{model}-{target}/wb_{year}.rds`.
