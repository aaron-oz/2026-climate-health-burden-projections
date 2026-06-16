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

The repo ships an `renv` lockfile (`renv.lock`) pinning every R package
version we validated against. R version: **4.5.2**. Main runtime
dependencies: `data.table`, `ncdf4`, `sf`, `readxl`, `ggplot2`,
`jsonlite`, `RColorBrewer`, plus all transitive deps (~40 packages
total).

When you start R in the project directory, `.Rprofile` automatically
activates renv.

R-package-level reproducibility is handled by renv. **System-level
libraries** (GDAL, GEOS, PROJ, libudunits2, libnetcdf) that `sf` and
`ncdf4` depend on are NOT something renv can pin — those need to be
present on the OS. Three options for handling that, in increasing order
of effort:

### Option A — Try `renv::restore()` and see what happens

If your workstation already has compatible versions of `libgdal`,
`libgeos`, `libproj`, `libudunits2`, and `libnetcdf` (likely, given you
run CMIP6 work), the restore just works. Most R-on-Linux setups that
have ever touched climate or geospatial data already have these:

```bash
# From inside the project directory, first time only:
Rscript -e 'install.packages("renv", repos="https://cloud.r-project.org")'
Rscript -e 'renv::restore(prompt = FALSE)'
```

The two packages that would fail loudly if system libs are missing are
`sf` and `ncdf4`. If those install cleanly, everything else will too.

**Effort: zero.** Just try it.

### Option B — Install the system libs via OS package manager, then restore

If `sf` or `ncdf4` fail during option A, install the missing system
libraries first. One command per OS:

**Ubuntu / Debian:**
```bash
sudo apt install libgdal-dev libgeos-dev libproj-dev \
                 libudunits2-dev libnetcdf-dev
```

**RHEL / Rocky / Alma / Fedora:**
```bash
sudo dnf install gdal-devel geos-devel proj-devel \
                 udunits2-devel netcdf-devel
```

**Arch:**
```bash
sudo pacman -S gdal geos proj udunits netcdf
```

Then re-run `renv::restore(prompt = FALSE)`. The R-package install will
succeed against the freshly-installed system libs.

**Effort: ~5 minutes** if you have sudo on the workstation. This is the
standard approach for R + geospatial work on Linux and what we'd
recommend by default if A doesn't immediately work.

### Option C — Singularity / Apptainer container

Maximum reproducibility: a container recipe pins the OS, R version, all
R packages, and all system libraries. You build the image once (~30 min)
and run the pipeline inside it. The image is portable; if you ever move
to a different machine, the container goes with it.

We haven't built the container recipe yet. Worth it if (1) you want
exact reproducibility for the deliverable, (2) the workstation might be
reimaged or shared, or (3) the pipeline gets handed to a third party who
may not have the system libs. Not necessary for a single-shot analysis
run on a known machine.

**Effort: ~half a day** of build + smoke test on your end after we ship
a `Singularity.def`. Ping us if you want this.

### Recommendation

**Try A first; fall back to B if it errors.** Container (C) only if
there's a specific reason to want full OS-level reproducibility.

### Bypassing renv

If you'd rather use system-level R packages directly instead of renv's
isolated library, delete `.Rprofile` from your clone and the
auto-activation won't trigger. Then ensure your system R has compatible
package versions (the lockfile lists them).

## 3.5 Install the input-data bundle

The large input datasets (ERF curves, TMRELs, GBD shapefile) are not in git
(too large / redistribution). We ship them as a separate zip,
`wb-pipeline-data.zip` (~1.1 GB). Unzip it at the **root** of your clone — it
already contains the correct `data/` subpaths:

```bash
cd /path/to/2026-climate-health-burden-projections
unzip /path/to/wb-pipeline-data.zip   # populates data/erf, data/tmrel, data/shapefiles
```

Life tables are already in the repo (`data/lifetables/`, UN WPP, all 198
nations, 1990–2100) — nothing to install. The only inputs you still fetch
yourself are the IHME forecast CSVs (step 1) and the CMIP6/population NetCDFs
(downloaded automatically in step 6, see below).

Note: the scripts resolve their own location, so you can run them from the repo
root (as shown here) or from inside `global-scripts/` — either works.

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

## 5. CMIP6 temperature + gridded population (automatic)

You do **not** pre-stage these. Step 6's `util_run_cckp_pipeline.R` downloads
both the CMIP6 daily `tas` temperature and the GPW gridded population NetCDFs
itself, via `curl`, from the **public** CCKP bucket
(`https://wbg-cckp.s3.amazonaws.com/data`) — no credentials needed. Files are
cached under `data/cmip6-scratch/cckp/` (skip-if-present), retried on transient
errors, and combos that genuinely don't exist on S3 are logged `missing-on-s3`
and skipped (known gaps: gfdl-cm4 lacks ssp126/ssp370; hadgem3-gc31-mm lacks
ssp245/ssp370; nesm3 lacks ssp370; taiesm1 lacks ssp245).

Requirements: `curl` on PATH and internet access.

**If you have a local CCKP mirror** (faster than the public download), point the
pipeline at it — it symlinks from your disk instead of downloading, falling back
to the public bucket on any miss:

```bash
--cckp_local_root=/data --cckp_pop_local_root=/data/CRMe/data
```

(Separate roots because temp and pop may live under different prefixes; the
relative paths below them must match the public CCKP layout
`cmip6-daily-x0.25/tas/...` and `pop-x0.25/popcount/...`.)

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
  --shapefile=data/shapefiles/GBD2023_mapping_final.shp \
  --subnational=FALSE
  # --subnational=FALSE (the default) tags pixels at national resolution to
  #   match the national IHME mortality; use TRUE only with subnational deaths.
  # Add --cckp_local_root=... --cckp_pop_local_root=... if using a local mirror.

# Step 6b: Run the burden pipeline (01-08) on that combo using IHME's
# cvd_ihd rates as the mortality input
Rscript global-scripts/util_run_cckp_burden.R \
  --location_id=125 \
  --models=access-cm2-r1i1p1f1 \
  --scenarios=ssp245 \
  --years=2022 \
  --use_draws_run=TRUE \
  --n_draws_run=500 \
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
- `--n_draws_run=500` matches IHME's forecast draw count, what we'll run in
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

## 6.5. Trace-verify the math on the benchmark output

Before scaling out to the full production run, walk one pixel × one
cause through every stage of the pipeline and verify each value by hand.
This catches lookup / index / formula bugs that would otherwise hide
behind plausible-looking aggregates. **This step was your idea.**

```bash
Rscript global-scripts/util_trace_pipeline.R \
  --pixel_ids=<comma,separated,pixel_ids> \
  --year=2022 \
  --causes=cvd_ihd \
  --draws_summary=summary \
  --output=output/trace_benchmark.md
```

The script reads the saved intermediate / results RDS files (no re-running
of the pipeline needed) and writes a structured markdown report with:

- Per-pixel inputs and zone assignment
- Per-(pixel × cause) RR lookup, TMREL, heat/cold classification, and PAF
  contribution
- Country-aggregate PAFs and burden totals
- Sanity checks: sum(heat) + sum(cold) should equal sum(non-opt)

To pick illuminating pixels, choose a contrasting set — e.g., a
high-altitude metro (cool, cold-attributable PAF should dominate), a
tropical lowland (warm, heat side dominates), and a moderate climate.
You can get good candidates with:

```r
library(data.table)
t <- setDT(readRDS("output/intermediate/temperature.rds"))
yr <- t[year == 2022, .(annual_mean = mean(daily_temp),
                        pop = first(pop), zone = first(zone)), by = pixel_id]
# Cold/high-pop pixel:
head(yr[zone <= 14][order(-pop)], 1)
# Hot/high-pop pixel:
head(yr[zone >= 25][order(-pop)], 1)
# Moderate-zone:
head(yr[zone %in% 20:22][order(-pop)], 1)
```

For a worked example of hand verification of one row,
[`pipeline-math-verification.md`](pipeline-math-verification.md) walks
through pixel 527 in Colombia 2010 with cvd_ihd, showing the
recomputation for every one of the 8 stages between raw input and per-row
PAF contribution. All 8 match the trace tool's reported values.

You don't need to redo this hand verification on every combo — once on a
representative sample (say 3–5 pixels in your benchmark run) is enough to
confirm the math hasn't drifted on your hardware.

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
