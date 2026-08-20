# Handoff: Run Samuel's Colombia pipeline with intermediate checkpoints

## Mission

Run Samuel's original Colombia pipeline (`from-samuel/Scripts/Colombia/11_carga_atribuible.R`) against the same input data the global pipeline uses, save intermediate-stage outputs, and diff them against the global pipeline's outputs to localize where they diverge.

The global pipeline (with `COLOMBIA_VERIFICATION = TRUE`) reproduces Samuel's published Colombia totals to within 5–10% on most metrics, but heat-side per-cause PAFs are still 24-39% off (heat homicide -24%, heat road -39%). An independent code-comparison agent enumerated 36 differences in `../validation/comparison-report-pipeline-vs-samuel.md`. We've patched the toggleable ones (Batches A and B in the git history). The remaining gap is hypothesized to come from the depto × day aggregation difference (Samuel keeps department × day granularity through PAF/SEV/burden; we collapse to year × cause early). **We need to confirm that hypothesis** — that the residual divergence is structural, not a hidden bug — before investing in a depto-day refactor of our pipeline.

The way to confirm: run Samuel's calculation and compare at intermediate stages, not just final totals.

## Context

- **Working directory:** `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/`
- **Branch:** `samuel-verification-patches` (already checked out). Commit your work on this branch.
- **Samuel's scripts are at** `from-samuel/Scripts/Colombia/`, vendored in commit `0bdec33`. The script that does the burden calculation is `11_carga_atribuible.R` (~600 lines, in Spanish). The earlier scripts (01-10) are data prep that we don't need — we already have the cleaned inputs.
- **The global pipeline has already been validated** end-to-end by running through `global-scripts/run_location.R` with `COLOMBIA_VERIFICATION = TRUE` and comparing against Samuel's published numbers. See `../validation/comparison-report-pipeline-vs-samuel.md` for the detailed difference enumeration.
- **R is available only inside the `emacs-r` distrobox container.** Use `distrobox enter emacs-r -- Rscript ...` to invoke R. The host has no R installed.
- **Samuel's script uses `tidyverse` / `dplyr` syntax**; the global pipeline uses `data.table`. The Samuel-checkpoint runner you build can use either.

## Approach

You'll write a sibling R script (suggested location: `samuel-runner/run_samuel_with_checkpoints.R`) that re-implements the data flow of `11_carga_atribuible.R` but:

1. **Reads from the global pipeline's standardized inputs** rather than Samuel's hardcoded `Bases/...` paths. The Samuel script expects R objects in the workspace (`df_temperatura`, `mortalidad_dia`, `curves_er`, `curves_max`, `poblacion`, `pob_san_a`, `df_tmrel`); you'll need to load equivalents from our data directories.

   Mapping (verified):
   - `df_temperatura` (pixel × day temperature) ← `data/columbia-data-for-verifying-pipeline/colombia/temperatura_diaria_pixel.rds`
   - `poblacion` (year-varying WorldPop) ← `data/columbia-data-for-verifying-pipeline/colombia/WorldPop_2010_2019_pixel.csv`
   - `mortalidad_dia` (daily mortality with depto/sex/age/cause) ← `data/columbia-data-for-verifying-pipeline/colombia/mortalidad_diaria_DANE_2010_2019_imput.rds`
   - `df_tmrel` ← `data/tmrel/tmrel_125_summaries.csv`
   - `curves_er` (ERF curves with rr_mean, rr_max per zone × daily_temp × cause) ← `data/erf/{cause}_curve_samples.csv` files (samples are in raw form; you'll need to compute mean and 99th-percentile yourself; or read the global pipeline's already-processed `output/intermediate/erf_curves.rds`)
   - DANE life tables (used for AVPP/YLL) ← `data/columbia-data-for-verifying-pipeline/colombia/Tablas_vida_DANE_2005_2050.rds`

   `pob_san_a` is San Andrés-specific population substitution that Samuel uses where WorldPop is missing for pixel 1499. You may or may not need to replicate it depending on how clean your runs need to be (it's ~0.15% of population).

2. **Saves intermediate outputs at five checkpoints** so you can diff against the global pipeline's outputs. Suggested checkpoints (numbered for matching diff-stage):

   - **Checkpoint 1 — RR/TMREL inputs after merge.** Per (zone, daily_temp, cause), the values of rr_mean, rr_max, and tmrelMean used. (Confirms input identity.)
   - **Checkpoint 2 — Per-day per-depto PAF.** For each (date, dept, cause, heat/cold), the PAF value before any aggregation. Save at full granularity and also a national-annual aggregation by `mean(PAF) by (year, cause, risk)` for direct comparison to the global pipeline's annual PAF.
   - **Checkpoint 3 — Per-(year, dept, cause) SEV.** Before zone aggregation. Plus the zone-aggregated per-(year, dept, cause) SEV. Plus a deaths-weighted national-annual aggregation for direct comparison.
   - **Checkpoint 4 — Per-(year, dept, sex, age, cause) attributable deaths** (deaths × PAF × SEV). Plus the year × cause aggregation summed across dept/sex/age.
   - **Checkpoint 5 — YLL** (avpp = attrib_deaths × (ev - 2.5), with the >80 cap). Plus the year × cause aggregation.

   Save each as RDS to `output/samuel-runner/checkpoint_N_*.rds`.

3. **Diffs against the global pipeline's outputs at each checkpoint.** The global pipeline's verification-mode outputs are at:
   - `output/intermediate/erf_curves.rds` (cp1)
   - `output/results/pafs_125.rds` (cp2 aggregated)
   - `output/results/sevs_125.rds` (cp3 aggregated)
   - `output/results/burden_125.rds` (cp4 aggregated)
   - `output/results/ylls_125.rds` (cp5 aggregated)

   For each checkpoint, produce a diff table: for each (year × cause × heat/cold) cell in the global-pipeline output, what does Samuel's national-aggregated equivalent show? Report mean abs % diff, max abs % diff, and identify which (year, cause) cells diverge most.

   At the deeper-granularity level (cp2 per-day, cp3 per-zone, cp4 per-dept-sex-age), there's nothing in our pipeline to diff against — those are saved for diagnostic / future use.

## Deliverable

**Two artifacts:**

1. **The runner script** (`samuel-runner/run_samuel_with_checkpoints.R`) committed to `samuel-verification-patches`. It should be self-contained: invokable via `distrobox enter emacs-r -- Rscript samuel-runner/run_samuel_with_checkpoints.R`. Should produce all 5 checkpoints.

2. **A markdown report** at `/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/../validation/samuel-runner-checkpoint-report.md` containing:

   - **Executive summary** (3-5 bullets): which checkpoint shows the first meaningful divergence? Is the divergence consistent with the depto × day aggregation hypothesis, or does it suggest a different / additional cause?
   - **Per-checkpoint diff tables** with the metrics mentioned above (mean abs % diff, max, top-divergent cells).
   - **Specific findings on the heat-side gap.** Heat homicide PAF is -24% in our output vs Samuel's published numbers; heat road PAF is -39%. Does Samuel's intermediate output at checkpoint 2 (per-day PAF aggregated to year) match our annual PAF for these causes? If not, by how much, and is that gap consistent with sample covariance arithmetic (Samuel's `Σ(deaths_d × paf_d)` vs ours `mean(paf) × Σ(deaths_d)`)?
   - **Conclusion on the structural hypothesis:** does the checkpoint diff support, refute, or partially support the claim that the residual divergence is the depto × day aggregation difference?
   - **Open questions / surprises** discovered along the way.

## Constraints

- **Read-only for the global pipeline.** Do not modify anything in `global-scripts/`. You may modify Samuel's scripts in `from-samuel/Scripts/Colombia/` if needed (for path adjustments etc.), but prefer wrapping them — copy the relevant transformations into your runner rather than editing `11_carga_atribuible.R` directly.
- **Working directory should be the project root.** Your script can `setwd()` if needed.
- **Be explicit about deviations from Samuel.** If you have to deviate from Samuel's exact code (because the path doesn't exist, or some R object isn't reproducible from our inputs), say so in the report and justify the deviation.
- **The mortality data has a known filter difference** that the previous validation already addressed: Samuel filters `codptore != 75` (residents abroad). Apply that filter in your runner.
- **Don't try to fix the gap.** Your job is to localize it, not close it. The team will decide what to do based on your findings.

## Background context the team has

- `../validation/step2-comparison.md` — Samuel vs Burkart 2021 reference comparison (different from your mission, but shows the methodology landscape).
- `../validation/comparison-report-pipeline-vs-samuel.md` — the independent code review that triggered this work. Reference items 12, 15, 20, 21, 24, 25 (depto-day aggregation) and 9 (year-varying WorldPop) are the structural items hypothesized to cause the residual.
- `handoff-pipeline-vs-samuel-comparison.md` — the brief that produced the comparison report.
- The git log on `samuel-verification-patches` shows the chain of fixes already applied.
- `ihme-unscaling-validity-report.md` (may not exist yet — being produced by another agent in parallel; ignore unless you happen to need IHME-forecasting context, which you shouldn't).

## Why this matters

The team's pipeline will produce attributable mortality projections to 2100 for the World Bank. Real spending decisions will hinge on these numbers. Before committing engineering effort to a depto-day refactor of the global pipeline, the team needs confidence that the residual gap is *only* the depto-day aggregation difference and not some other hidden bug. Your work bisects the gap precisely.

Take the time you need. Be thorough. Spanish→English glossary (in case useful):
- `muertes` = deaths; `avpp` = YLL; `temperatura` = temperature; `pob`/`poblacion` = population; `fecha` = date; `ano` = year; `c_muerte` = cause of death; `cod_depto` = department code; `zona` = temperature zone; `pr_zona` = population proportion in zone; `paf` = PAF; `paf2` = PAF without zero-floor; `rr_mean` = mean RR; `rr_max` = max RR (for SEV); `tmrelMean` = mean TMREL.
