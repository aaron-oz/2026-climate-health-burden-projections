# Climate Health Burden Projections

A statistical pipeline for quantifying mortality and morbidity (years of life
lost) attributable to non-optimal temperature, under Shared Socioeconomic
Pathway (SSP) × Representative Concentration Pathway (RCP) climate scenarios,
out to 2050.

This is the World Bank's projection workstream, built on the
Burkart et al. (*Lancet*, 2021) methodology and the IHME GBD 2021 forecast
framework, with bias-corrected CMIP6 temperature and SSP-aligned gridded
population from the World Bank Climate Change Knowledge Portal (CCKP).

## Where to start

- **Production workflow / step-by-step commands (start here if you're Caspar):**
  [`caspar-workflow.md`](docs/ops/caspar-workflow.md)
- **Overview, architecture, and run instructions:**
  [`pipeline-state-for-cckp.md`](docs/ops/pipeline-state-for-cckp.md)
- **Methodology memo (Workflow B, ratio approach):**
  [`gbd/ihme-plan-b-prep.tex`](gbd/ihme-plan-b-prep.tex)
- **Validation status (Colombia 2010–2019):**
  [`colombia-validation-state.tex`](docs/validation/colombia-validation-state.tex)
  (18 of 19 headline metrics within ±5% of Samuel Cuervo's published numbers)
- **End-to-end math verification (worked-example walkthrough):**
  [`pipeline-math-verification.md`](docs/validation/pipeline-math-verification.md) —
  hand-verifies all 8 stages between raw input and per-row PAF
  contribution; produced by `util_trace_pipeline.R` then independently
  recomputed
- **Data inputs and layout:**
  [`data/README.md`](data/README.md)
- **ssp245 validation review (August 2026): report, slides, and full notes:**
  [`docs/reviews/`](docs/reviews/)

## Documentation layout

Long-form documents live under `docs/`, one subdirectory per kind:

| directory | contents |
|---|---|
| `docs/reviews/` | run validation reviews (agenda, findings, report, slides) |
| `docs/validation/` | Colombia-era verification reports and comparisons |
| `docs/ops/` | production workflow, migration, and architecture docs |
| `docs/handoffs/` | session handoff briefs |
| `docs/meetings/` | meeting notes and agendas |
| `docs/methodology/` | methodology walkthrough (LaTeX) |
| `docs/notes/` | personal working notes (gitignored) |

## Pipeline summary

The pipeline is pure R (no build system, no tests, no CI), executed via
`global-scripts/run_location.R`. Eight scripts run in sequence:

1. Load exposure-response function (ERF) curves
2. Load temperature minimum risk exposure level (TMREL) values
3. Load and prepare daily temperature
4. Load mortality
5. Compute attributable burden (PAFs × counts) [cause-chunked, draw-chunked]
6. Compute Summary Exposure Values (diagnostic)
7. Compute years of life lost (YLLs)
8. Aggregate to summary tables

Three utility / driver scripts orchestrate the projection workflow on
CCKP-derived inputs:

- `util_convert_cckp_temperature.R` — CCKP CMIP6 NetCDF + GPW population
  → pipeline pixel-day RDS
- `util_run_cckp_pipeline.R` — iterate (model × scenario × year) grid,
  download cached, write per-combo RDS
- `util_run_cckp_burden.R` — drive scripts 1–8 against each combo,
  per-(combo) output tree

Plus an IHME-side converter and a Workflow B post-processor:

- `util_convert_ihme_forecast.R` — IHME rate × pop forecast → pipeline
  mortality format (per-draw long form + mean back-compat)
- `util_workflow_b_ratio.R` — apply the reference / target scenario ratio
  to IHME-anchored counts

## Repository layout

```
global-scripts/        R pipeline + utilities
data/                  Inputs: ERF curves, TMREL, temperature, mortality,
                       population, life tables, shapefiles (most contents
                       gitignored; data/README.md documents expected files)
output/                Intermediates and results (gitignored)
from-samuel/           Original Colombia pipeline scripts + reference
                       materials (Samuel Cuervo's work, used for validation)
gbd/                   Methodology memo + IHME GBD reference materials
                       (PDFs gitignored; the .tex source for our memo
                       is tracked)
```

## Environment

R 4.5.2, exercised locally via the `emacs-r` Distrobox container. Key
dependencies: `data.table`, `ncdf4`, `sf`, `readxl`, `ggplot2` (for
diagnostics).

A `renv` lockfile and Singularity / Apptainer container image for
reproducible cluster execution are TBD (one of the handoff items in
[`pipeline-state-for-cckp.md`](docs/ops/pipeline-state-for-cckp.md) §9).

## Validation

Colombia 2010–2019 verification mode replicates Samuel Cuervo's published
methodology choices to enable direct numerical comparison. Current state:
**18 of 19 headline metrics within ±5%**. The one remaining cause-specific
gap (`inj_trans_road` heat YLL, −11.76%) is traced to upstream mortality-
imputation and garbage-code-redistribution hypotheses being followed up
separately. See `colombia-validation-state.tex` for the full breakdown.

## License and contributors

World Bank statistical consulting project. Internal collaboration — not
for public redistribution. Reference materials from third parties (IHME
forecast appendix, Burkart paper PDFs) are gitignored.
