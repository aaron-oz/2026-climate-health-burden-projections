# Handoff: Independent comparison of global pipeline vs Samuel's Colombia code

## Mission

Compare two implementations of the same temperature-attributable burden
methodology and produce an itemized list of behavioral differences. The
goal is to explain why their numerical outputs disagree.

The two implementations:

1. **Global pipeline (this repo, verification mode)** — `global-scripts/`,
   run via `Rscript run_location.R --location_id=125`. Set
   `COLOMBIA_VERIFICATION <- TRUE` in `global-scripts/config.R` before
   running. The verification flag toggles known methodology changes
   intended to mirror implementation #2.

2. **Samuel's original Colombia scripts** — `from-samuel/Scripts/Colombia/`.
   The relevant file for the attributable-burden calculation is
   `11_carga_atribuible.R`. Earlier scripts in that directory handle data
   prep (mortality cleaning, ERF curve loading, temperature processing).

Both implementations consume the same source data:
- ERF curves: `from-samuel/Info Burkart/ERF/` (symlinked into `data/erf/`)
- TMREL summaries: `from-samuel/Info Burkart/TMRELs/tmrel_125_summaries.csv`
  (also at `data/tmrel/tmrel_125_summaries.csv` — verified byte-identical)
- Temperature, mortality, life tables, divipola: Samuel's RDS files in
  `data/columbia-data-for-verifying-pipeline/colombia/`, transformed by
  `global-scripts/util_convert_samuel_colombia.R` into the format the
  global pipeline expects.

## Current numerical state

After running with `COLOMBIA_VERIFICATION = TRUE`, the global pipeline
produces output that differs from Samuel's published numbers as follows
(Samuel's targets come from the World Bank report at
`from-samuel/results/WB_climatechange_Colombia.pdf` and its executive
summary; the comparison script `global-scripts/util_compare_to_samuel.R`
produces this table automatically):

| Metric | Pipeline | Samuel | % diff |
|---|---|---|---|
| Total attributable deaths (10-yr sum) | 6,675 | 9,472 | -29.5% |
| Heat-attributable deaths | 990 | 2,302 | -57.0% |
| Cold-attributable deaths | 5,625 | 7,170 | -21.6% |
| Total YLL | 117,061 | 172,870 | -32.3% |
| Heat YLL | 30,385 | 63,556 | -52.2% |
| Cold YLL | 86,676 | 109,313 | -20.7% |
| Cold IHD YLL | 45,134 | 48,907 | -7.7% |
| Cold stroke YLL | 9,794 | 10,372 | -5.6% |
| Heat IHD YLL | 2,393 | 9,408 | -74.6% |
| Heat homicide YLL | 17,606 | 30,045 | -41.4% |
| Heat road traffic YLL | 3,743 | 10,937 | -65.8% |
| 17-cause mortality denominator | 884,628 | 884,628 | 0.0% (PASS) |
| Top-3 cold YLL causes (rank match) | 3/3 | — | exact |
| Top-3 heat YLL causes (rank match) | 2/3 | — | partial |

Full diff table: `output/results/comparison_to_samuel_125.csv` (regenerated
each run). Tolerance for pass/fail is ±5%. Currently 1/25 PASS.

The 17-cause mortality denominator (884,628) matches exactly. This
confirms the input mortality data flowing through both pipelines is the
same. Discrepancies are downstream of mortality.

## What's already been investigated and ruled in / out

These items have been examined and either fixed in the verification
pathway or confirmed not to be the cause. Do not re-investigate unless
you find evidence to the contrary.

**Fixed in verification mode (replicating Samuel's behavior):**
- RR rescaling at TMREL: skipped in verification mode
  (`05_compute_pafs.R`, `06_compute_sevs.R`)
- PAF flooring at zero: applied in verification mode (`05_compute_pafs.R`)
- TMREL averaging: equal-weight mean of source years (1990, 2010, 2020)
  before expanding to study period (`02_load_tmrel.R`)
- Burden formula: `deaths × PAF × SEV` in verification mode
  (`05_compute_pafs.R`)
- SEV computation: replicates Samuel's daily-summation-and-cap pattern
  (`06_compute_sevs.R`); see `../validation/step2-comparison.md` section 2b for the
  derivation.

**Confirmed not to be the cause:**
- Source TMREL file: byte-identical between
  `from-samuel/Info Burkart/TMRELs/tmrel_125_summaries.csv` and
  `data/tmrel/tmrel_125_summaries.csv`.
- Samuel-data converter (`util_convert_samuel_colombia.R`) is deterministic;
  re-running produces byte-identical output to the cached version.
  Verified by `util_diff_cached_conversion.R`.

## Scope boundaries

**In scope:**
- Any line-by-line behavioral difference between the two implementations
  that could affect numerical output.
- Data flow: how each pipeline aggregates across pixels, days, years,
  zones, departments, sexes, ages, and causes — at what point each
  dimension is summed, averaged, or dropped.
- Order of operations (e.g., flooring before vs. after summing; averaging
  before vs. after merging).
- Cause mapping: confirm the 17 GBD `acause` codes used by the global
  pipeline correspond 1:1 to Samuel's 17 `c_muerte` Spanish labels
  (mapping is in `util_convert_samuel_colombia.R:50-63`).
- ERF curve handling and the `rr_max` (RR_max for SEV) computation.
- TMREL handling beyond the averaging fix already applied.
- Any data filter, threshold, truncation, or imputation step that one
  implementation does and the other does not.

**Out of scope:**
- Performance, code style, idiom modernization.
- Whether either methodology is "correct" relative to Burkart et al. 2021.
  That comparison is documented separately in `../validation/step2-comparison.md`.
- Department-level (subnational) breakdowns. The global pipeline does not
  currently track department through the burden calculation; Samuel does.
  Note this as a known structural difference but do not propose
  refactoring the global pipeline to add it.

## How to run

1. Confirm `global-scripts/config.R` has `COLOMBIA_VERIFICATION <- TRUE`
   and `USE_DRAWS <- FALSE`. (Default is `FALSE`; flip for this work.)
2. If converted Colombia data is missing (`data/temperature/125_daily_temp.rds`,
   `data/mortality/125_mortality.rds`, `data/lifetables/125_lifetable.rds`),
   regenerate it: `distrobox enter emacs-r -- Rscript util_convert_samuel_colombia.R`
   from `global-scripts/`.
3. Run the pipeline:
   `distrobox enter emacs-r -- Rscript run_location.R --location_id=125`
   from `global-scripts/`. ~1 min.
4. Run the comparison:
   `distrobox enter emacs-r -- Rscript util_compare_to_samuel.R`
   from `global-scripts/`. Produces
   `output/results/comparison_to_samuel_125.csv`.

R is in the `emacs-r` distrobox container; the host has no R installed.

## Suggested deliverable

A markdown report listing each behavioral difference you find, in a table
with these columns:

| # | Location in global pipeline | Location in Samuel | Difference | Plausible numerical impact |

For "plausible numerical impact," indicate direction (would close the gap
/ widen it / unclear) and rough magnitude category (large / moderate /
small / negligible) without quantitative analysis. Reserve quantitative
testing for follow-up work; the goal of this pass is to enumerate
candidate causes, not to rank them.

Order the table by line number in the global pipeline.

If you encounter ambiguity in either codebase that prevents you from
classifying a difference, flag it as "unclear" and describe what
additional information would resolve it.

## Reference: file map

Global pipeline (this repo, `global-scripts/`):
- `config.R` — configuration and the `COLOMBIA_VERIFICATION` flag
- `run_location.R` — orchestrator, fixes script execution order
- `01_load_erf.R` — load ERF (RR) curves
- `02_load_tmrel.R` — load and process TMRELs
- `03_load_temperature.R` — load and prepare daily temperature
- `04_load_mortality.R` — load mortality (already aggregated by
  `util_convert_samuel_colombia.R` to year × cause × age × sex)
- `06_compute_sevs.R` — Summary Exposure Values (must run before 05 in
  verification mode because 05 needs SEVs for the burden formula)
- `05_compute_pafs.R` — PAFs and attributable burden
- `07_compute_ylls.R` — multiply attributable deaths by life expectancy
- `08_outputs.R` — summary tables and plots
- `util_convert_samuel_colombia.R` — Samuel-data ingestion
- `util_compare_to_samuel.R` — the comparison harness

Samuel's pipeline (`from-samuel/Scripts/Colombia/`, all in Spanish):
- `01_mortalidad_2010_2020.R` through `08_*.R` — data prep
- `09_curvas_ER.R` — ERF curve processing (collapses 1000 draws to summary
  statistics; relevant for understanding `rr_max`)
- `10_ajuste_temperatura.R`, `10_2_Proces_temp.R` — temperature processing
- `11_carga_atribuible.R` — the main attributable-burden script (the
  highest-density file for behavioral comparison)
- `12_*.R` — output figures

Source data:
- `from-samuel/Info Burkart/` — Burkart et al. 2021 reference materials
  (ERF curves, TMRELs, methods PDFs)
- `data/columbia-data-for-verifying-pipeline/colombia/` — Samuel's
  intermediate RDS files (temperature, mortality, life tables, divipola)

Reference doc already in repo:
- `../validation/step2-comparison.md` — earlier comparison of Samuel vs the Burkart
  reference implementation. Useful background but covers a different
  comparison than this mission. The 7 issues listed there are about
  Samuel-vs-Burkart; your job is global-pipeline-vs-Samuel.

The Spanish→English glossary for Samuel's variable names:
- `muertes` = deaths; `avpp` = YLL; `temperatura` = temperature;
- `pob`/`poblacion` = population; `fecha` = date; `ano` = year;
- `c_muerte` = cause of death; `cod_depto` = department code;
- `zona` = temperature zone; `pr_zona` = population proportion in zone;
- `paf` = PAF; `paf2` = PAF without zero-floor;
- `rr_mean` = mean RR; `rr_max` = max RR (for SEV);
- `tmrelMean` = mean TMREL; `tmrelLower`/`tmrelUpper` = TMREL CI bounds.
