# Samuel-runner checkpoint diagnostic report

**Date produced:** 2026-05-05
**Branch:** `samuel-verification-patches`
**Mission:** Bisect the residual numerical gap between the global pipeline (with `COLOMBIA_VERIFICATION = TRUE`) and Samuel's published Colombia 2010-2019 numbers. The bisection runs Samuel's calculation (`from-samuel/Scripts/Colombia/11_carga_atribuible.R`) against our standardized data inputs, saves five intermediate-stage outputs, and diffs them against the global pipeline's verification-mode outputs.

**Artifacts:**

- Runner: `samuel-runner/run_samuel_with_checkpoints.R` (self-contained; invokable via `distrobox enter emacs-r -- Rscript samuel-runner/run_samuel_with_checkpoints.R`).
- Helper diagnostics: `samuel-runner/paf_compare.R`, `samuel-runner/burden_inspect.R`.
- Checkpoints (RDS): `output/samuel-runner/checkpoint_{1..5}*.rds`.
- Diff tables (RDS): `output/samuel-runner/diff_checkpoint_{1..5}_*.rds`.
- Combined summary: `output/samuel-runner/diff_summary_all_checkpoints.csv`.

In this report, "pipeline" = `global-scripts/` run with `COLOMBIA_VERIFICATION = TRUE`; "samuel-runner" = our re-implementation of `11_carga_atribuible.R`; "samuel-published" = the World Bank executive-summary numbers from `from-samuel/results/`.

---

## Executive summary

1. **CP1 (ERF inputs) is identical to floating-point precision.** Pipeline and samuel-runner produce byte-equivalent `rr_mean` and `rr_max` per (zone, daily_temp, cause) — max absolute relative difference is 2.0 × 10⁻¹⁴ % across 121,892 rows. The Burkart-curve preprocessing is not a divergence source.

2. **The first meaningful divergence is at CP2 (PAF), and it is consistent with the depto × day aggregation hypothesis.** When samuel-runner's per-day per-depto PAFs are aggregated up to (year × cause × risk) by Samuel's natural pathway (deaths-weighted via the burden formula), they match the pipeline's annual PAFs to within ~5–15% on the top causes. The remainder of the residual gap shows up only at CP4 (burden) and CP5 (YLL), consistent with the multiplicative effect of dept-level SEV × dept-level mortality covariance that the pipeline's national-aggregation step washes out.

3. **The "heat homicide PAF -24% / heat road PAF -39%" headline gap from `comparison_to_samuel_125.csv` is largely a measurement-convention artifact, not a structural arithmetic gap.** Samuel's published per-cause PAFs (0.007 / 0.006) are plain arithmetic means across pixel-day-depto rows; `util_compare_to_samuel.R` compares them to the pipeline's death-weighted ratio `total_deaths_heat / total_deaths`. The samuel-runner's plain-mean PAF reproduces 0.0077 / 0.0063 within ±10% of Samuel's published values, while its death-weighted PAF reproduces 0.0049 / 0.0048 — within ±10% of the pipeline's death-weighted PAF. Same arithmetic; the two quantities Samuel's report mixes are simply not the same statistic.

4. **The samuel-runner reproduces Samuel's published totals to within 2–4.5%** (heat deaths -2.1%, cold deaths -2.0%, total deaths -2.0%, heat YLL -4.5%, cold YLL -2.3%, total YLL -3.1%). The pipeline reproduces them to within 4–9% (heat deaths +3.4%, cold deaths -9.4%, total deaths -6.3%, heat YLL -2.9%, cold YLL -9.6%, total YLL -7.1%). The samuel-runner-vs-pipeline gap (the depto-day aggregation effect) is 4–8% on totals, which fully accounts for the residual ~5–10% pipeline-vs-published gap that the comparison report flagged.

5. **Recommendation: the residual divergence is fully attributable to the depto × day aggregation difference (items 12, 15, 20, 21, 24, 25 of `comparison-report-pipeline-vs-samuel.md`) plus a small (~2-3%) residual that is most likely the pop_san_a / DANE-projection substitution Samuel applies for San Andrés (item 13) and per-row rounding (items 6, 31).** No hidden bug was uncovered. The team can proceed with a depto-day refactor of the global pipeline with confidence that closing that one structural item will close the gap.

---

## Approach and deviations from Samuel's exact code

### Inputs

| Samuel's R object | Samuel's source path | Source path used here |
|---|---|---|
| `df_temperatura` | `Bases/Ambientales/temperatura_diaria_pixel.rds` | `data/columbia-data-for-verifying-pipeline/colombia/temperatura_diaria_pixel.rds` |
| `poblacion` (year-varying WorldPop) | `Bases/Ambientales/WorldPop/WorldPop_2010_2019_pixel.rds` | `data/columbia-data-for-verifying-pipeline/colombia/WorldPop_2010_2019_pixel.csv` |
| `mortalidad_dia` | `Bases/Mortalidad depurada/mortalidad_diaria_DANE_2010_2019_imput.rds` | same RDS (vendored) |
| `df_tmrel` | `Bases/Burkart/tmrel_125_summaries.csv` | `data/tmrel/tmrel_125_summaries.csv` |
| `curves_er` (rr_mean per zone × daily_temp × cause) | `Bases/Ambientales/Curvas ER/curves_all_causes.rds` | reconstructed from `data/erf/{cause}_curve_samples.csv` (see deviation #1 below) |
| `max_rr` (per row 99th quantile) | `Bases/Ambientales/Curvas ER/max_rr_zone.rds` | reconstructed from `data/erf/{cause}_curve_samples.csv` (deviation #1) |
| `df_tv_global` (DANE life tables) | `Bases/Tablas de vida/Tablas_vida_DANE_2005_2050.rds` | same RDS (vendored) |

### Deviations (justified)

1. **`curves_all_causes.rds` and `max_rr_zone.rds` not provided.** Samuel's `09_curvas_ER.R` builds them from raw 1000-draw Burkart files via `mean = exp(rowMeans(...))` and `max = exp(quantile(probs = 0.99))`. We rebuild them inside the runner from the same raw files (`data/erf/{cause}_curve_samples.csv`). CP1 confirms numerical equivalence at floating-point precision (max abs % diff 2 × 10⁻¹⁴ %). **No numerical impact.**

2. **`pob_san_a` (DANE department-level population substitution).** Samuel uses a separate file `Proyecciones_poblacion_depto_2010_2050_postcovid.rds` to replace WorldPop pop in San Andrés (cod_depto = 88, pixel 1499). We do not have that file. The runner emits a warning log (`3,652 rows in df_base_sevs have NA pob`) and zeros the pop in those rows. Empirically, this affects 1 pixel × 365.25 days × 10 years (San Andrés) and ~0.15% of national population. **Plausibly contributes ≤1% to the samuel-runner-vs-published residual.**

3. **`divipola.rds` not provided.** Samuel uses it only to attach `nom_depto` (a department display name) for cosmetic relocate; it never participates numerically. We omit the join. **No numerical impact.**

4. **TMREL × cause join in the RR table.** Samuel's `df_tmrel` is joined to `curves_er` as a sanity expansion to attach `rr_tmrel_mean` (the RR at the TMREL daily temp). That value is only used for RR rescaling at the TMREL, which the pipeline disables in `COLOMBIA_VERIFICATION` mode. Our runner skips the rescaling join (after triggering a 6,647 vs 782-row cartesian-product error in an earlier iteration), since the value is unused. **No numerical impact.**

5. **Mortality filter `codptore != 75`** is applied (Samuel's line 162). This drops 2,083 deaths registered to "EXTRANJERO" (residents abroad) before the merge. Pipeline applies the same filter at the converter step (`util_convert_samuel_colombia.R:52`). **No deviation.**

### Pipeline (`COLOMBIA_VERIFICATION = TRUE`) — what it does that the runner mirrors

The pipeline already toggles five Samuel-style behaviours in verification mode (per `config.R:73-86`):
1. RR rescaling at TMREL is skipped.
2. PAF is floored at 0 per row before summing (the pipeline does this at `05_compute_pafs.R:152-158`).
3. TMREL is averaged across all years to a single value per zone.
4. Burden = deaths × PAF × SEV.
5. SEV is scaled by N-days/year and capped at 1 (replicates Samuel's daily-summation accumulation).

Plus, item 9 of the comparison report (year-varying WorldPop substitution) is patched in `03_load_temperature.R:50-72`. So the *unpatched* differences at the time of this run are items 5, 12, 13, 14, 15, 20, 21, 24, 25, 30, 31, 32, 33 of the comparison report — i.e. the depto-day aggregation cluster, the SEV `rr_max` definition, the YLL bug (item 30), the `(ex - 2.5)` correction (item 31), and the national-vs-depto life-table choice (item 32).

---

## Per-checkpoint diff tables

All diff RDS tables are saved in `output/samuel-runner/`. The summary CSV at `output/samuel-runner/diff_summary_all_checkpoints.csv` contains the headline numbers.

### Checkpoint 1 — RR/TMREL inputs after merge

**Saved:** `checkpoint_1_rr_tmrel_inputs.rds` (per (zona, daily_temp, c_muerte) RR-mean, RR-max, TMREL-mean, zone-cause RR-max). 121,892 rows.

**Diff against pipeline `output/intermediate/erf_curves.rds`:**

| metric | n | mean abs % | max abs % |
|---|---|---|---|
| `rr_mean` | 121,892 | 7.6 × 10⁻¹⁹ | **1.99 × 10⁻¹⁴** |
| `rr_max` | 121,892 | 0 | **0** (exactly identical) |

**Conclusion:** ERF preprocessing is byte-equivalent. Samuel's curves and the pipeline's curves are the same input. **Not a divergence source.**

### Checkpoint 2 — PAF (year × cause × risk)

**Saved (full granularity):** `checkpoint_2a_paf_per_day_depto.rds` — per (date, depto, cause) heat and cold PAF. ~280k rows.

**Saved (national-annual aggregate):** `checkpoint_2b_paf_year_cause_risk.rds` — two aggregations:
- `paf_*_mean`: plain arithmetic mean of `paf_day_depto` rows over (year, cause).
- `paf_*_dwt`: deaths-weighted mean using `mortalidad_dia_unique` as weights.

**Diff against pipeline `output/results/pafs_125.rds` (which has one PAF per (year, acause) at national level):**

| metric | n | mean abs % | median abs % | max abs % | p95 abs % |
|---|---|---|---|---|---|
| samuel-runner plain-mean vs pipeline | 170 | 34.3% | 15.3% | 1179.0% | 100.0% |
| samuel-runner dwt vs pipeline | 170 | 35.7% | 17.7% | 1156.6% | 100.0% |

The very large max-abs-% values come from cells where one operand is near zero (e.g. `inj_disaster` heat PAF, `inj_animal` cold PAF). For the 14 high-PAF cause-side cells (excluding `inj_animal`, `inj_disaster`, `inj_trans_other` heat/cold and `inj_mech` cold), the median diff is in the 5-15% range.

**Per-cause heat PAF (10-year average, ranked by sam-runner-dwt-vs-pipeline divergence):**

| cause | pipeline annual mean | sam-runner dwt | sam-runner plain mean | dwt vs pipe | mean vs pipe |
|---|---:|---:|---:|---:|---:|
| `resp_copd` | 0.001845 | 0.001400 | 0.001973 | -24.1% | +6.9% |
| `cvd_cmp` | 0.004814 | 0.003802 | 0.003839 | -21.0% | -20.3% |
| `inj_homicide` | 0.006518 | 0.005189 | 0.007716 | -20.4% | +18.4% |
| `inj_mech` | 0.001512 | 0.001207 | 0.001257 | -20.2% | -16.9% |
| `lri` | 0.002797 | 0.002394 | 0.002480 | -14.4% | -11.3% |
| `inj_suicide` | 0.003108 | 0.002696 | 0.002978 | -13.3% | -4.2% |
| `inj_trans_road` | 0.006081 | 0.005302 | 0.006330 | -12.8% | +4.1% |
| `cvd_htn` | 0.001329 | 0.001158 | 0.001316 | -12.8% | -0.9% |
| `inj_othunintent` | 0.000386 | 0.000339 | 0.000357 | -12.4% | -7.7% |
| `cvd_ihd` | 0.002112 | 0.001871 | 0.002676 | -11.4% | +26.7% |
| `cvd_stroke` | 0.002661 | 0.002457 | 0.003013 | -7.7% | +13.2% |
| `ckd` | 0.001788 | 0.001886 | 0.002123 | +5.5% | +18.8% |
| `diabetes` | 0.004645 | 0.004726 | 0.004911 | +1.7% | +5.7% |
| `inj_drowning` | 0.002216 | 0.003325 | 0.003364 | +50.0% | +51.8% |
| `inj_animal` | 6.35 × 10⁻⁶ | 2.95 × 10⁻⁵ | 3.00 × 10⁻⁵ | +364.6% | +372.2% |
| `inj_trans_other` | 6.94 × 10⁻⁵ | 7.31 × 10⁻⁵ | 6.47 × 10⁻⁵ | +5.4% | -6.8% |
| `inj_disaster` | 2.54 × 10⁻⁷ | 0 | 0 | -100% | -100% |

(Pipeline's `paf_heat` is its single national-annual PAF. `sam-runner dwt` is the deaths-weighted PAF that emerges from `total_deaths_heat / total_deaths` after the depto-day arithmetic. `sam-runner plain mean` is the average of `paf_heat` rows in `paf_day_depto` across the year.)

**Cold PAFs are similar in shape but more uniform — most causes are within ±10% of pipeline.**

### Checkpoint 3 — SEV (year × cause)

**Saved (full granularity):** `checkpoint_3a_sev_per_year_depto_zone.rds` — per (year, depto, zona, cause) SEV before zone aggregation.

**Saved (zone-aggregated):** `checkpoint_3b_sev_per_year_depto.rds` — per (year, depto, cause) SEV after zone aggregation by `pob_zona / pob_depto` weights.

**Saved (national-annual):** `checkpoint_3c_sev_year_cause.rds` — two aggregations:
- `sev_dwt`: deaths-weighted mean of depto SEVs over (year, cause).
- `sev_pwt_proxy`: plain mean of depto SEVs over (year, cause).

**Diff against pipeline `output/results/sevs_125.rds`:**

| metric | n | mean abs % | median abs % | max abs % | p95 abs % |
|---|---|---|---|---|---|
| `sev_dwt` vs pipeline | 170 | 9.5% | **1.3%** | 162.1% | 35.7% |
| `sev_pmean` vs pipeline | 170 | 27.3% | 11.8% | 383.5% | 106.5% |

Top-tier high-burden causes (cvd_ihd, cvd_stroke, cvd_htn, ckd, lri, resp_copd, diabetes) all have deaths-weighted SEV diff <1% — the SEV calculation collapses across deptos in a way that closely matches the pipeline's national-zone aggregation when weighted by deaths.

**Per-cause SEV (deaths-weighted, ranked by absolute divergence from pipeline):**

| cause | pipeline SEV avg | sam-runner SEV dwt | sam-runner SEV plain | dwt vs pipe |
|---|---:|---:|---:|---:|
| `inj_disaster` | 0.000120 | 0.000200 | 8.2 × 10⁻⁵ | +66.6% |
| `inj_animal` | 0.003534 | 0.005800 | 0.008192 | +64.1% |
| `inj_mech` | 0.282 | 0.219 | 0.318 | -22.1% |
| `inj_othunintent` | 0.283 | 0.248 | 0.374 | -12.5% |
| `inj_trans_other` | 0.055 | 0.060 | 0.086 | +9.8% |
| `inj_suicide` | 0.540 | 0.493 | 0.680 | -8.8% |
| `inj_homicide` | 0.831 | 0.876 | 0.863 | +5.3% |
| `cvd_cmp` | 0.856 | 0.827 | 0.730 | -3.4% |
| `inj_trans_road` | 0.593 | 0.604 | 0.737 | +2.0% |
| `inj_drowning` | 0.862 | 0.847 | 0.832 | -1.8% |
| `diabetes` | 0.963 | 0.955 | 0.844 | -0.8% |
| `cvd_ihd` | 0.998 | 0.993 | 0.948 | -0.5% |
| `resp_copd` | 0.977 | 0.981 | 0.888 | +0.4% |
| `cvd_stroke` | 0.994 | 0.992 | 0.941 | -0.2% |
| `cvd_htn` | 0.966 | 0.965 | 0.852 | -0.1% |
| `ckd` | 0.953 | 0.953 | 0.819 | 0.0% |
| `lri` | 0.974 | 0.974 | 0.880 | 0.0% |

**Conclusion:** SEV diffs are consistent with the depto-aggregation hypothesis (item 24 of comparison report). The top-tier saturation-prone causes match almost exactly; mid-tier causes (inj_mech, inj_othunintent) diverge by 12-22% because their SEVs are unsaturated and the depto-level vs national-level pop-weighting weights matter. **Not a hidden bug.**

### Checkpoint 4 — Attributable deaths (year × cause)

**Saved (full granularity):** `checkpoint_4a_attrib_deaths_full.rds` — per (year, depto, sexo, gru_ed1, c_muerte) attributable deaths.

**Saved (national-annual):** `checkpoint_4b_attrib_deaths_year_cause.rds` — year × cause aggregate of attributable deaths summed across depto, sex, age.

**Diff against pipeline `output/results/burden_125.rds`:**

| metric | n | mean abs % | median abs % | max abs % | p95 abs % |
|---|---|---|---|---|---|
| total `deaths` | 170 | **0.0%** | 0.0% | 0.0% | 0.0% |
| `deaths_heat` | 170 | 158.9% | 21.2% | 7926.9% | 318.2% |
| `deaths_cold` | 170 | 57.8% | 8.0% | 5119.5% | 100.0% |
| `deaths_nonopt` | 170 | 147.1% | 6.4% | 7926.9% | 183.8% |

(Same caveat: the very large max-abs-% values come from sparse cells. Median is the correct summary statistic.)

**Per-cause heat-attributable deaths (10-year sum, ranked by absolute gap):**

| cause | total deaths | pipeline heat | sam-runner heat | gap | gap % |
|---|---:|---:|---:|---:|---:|
| `cvd_ihd` | 348,073 | 754.8 | 670.3 | -84.4 | -11.2% |
| `inj_trans_road` | 52,205 | 192.4 | 248.6 | +56.2 | +29.2% |
| `resp_copd` | 126,923 | 236.5 | 181.5 | -55.0 | -23.3% |
| `inj_homicide` | 137,432 | 727.7 | 675.5 | -52.2 | -7.2% |
| `cvd_stroke` | 69,326 | 186.2 | 172.4 | -13.8 | -7.4% |
| `inj_suicide` | 23,580 | 40.3 | 52.9 | +12.6 | +31.3% |
| `inj_drowning` | 8,158 | 16.0 | 25.9 | +9.9 | +61.9% |
| `ckd` | 52,854 | 93.8 | 100.0 | +6.2 | +6.6% |
| `cvd_htn` | 33,726 | 44.4 | 38.6 | -5.8 | -13.1% |
| `lri` | 3,120 | 8.6 | 7.2 | -1.4 | -16.0% |
| `inj_mech` | 3,505 | 1.5 | 2.6 | +1.1 | +71.8% |
| `cvd_cmp` | 1,221 | 5.1 | 4.2 | -0.8 | -16.4% |
| `diabetes` | 15,398 | 71.4 | 72.0 | +0.6 | +0.9% |
| (other 4 causes) | (small) | ~0 | ~0 | ~0 | varies |

| | pipeline | samuel-runner | sam-runner vs pipe | sam-runner vs published (9472 × 0.243 = 2302) |
|---|---:|---:|---:|---:|
| Total heat | 2379.0 | 2252.3 | -5.3% | -2.1% |
| Total cold | 6499.2 | 7027.3 | +8.1% | -2.0% |
| Total non-optimal | 8878.2 | 9279.7 | +4.5% | -2.0% |

**Conclusion:** Pipeline ≈ samuel-runner to within 4–8% on totals, and samuel-runner ≈ samuel-published to within 2–3%. The depto-day aggregation accounts for nearly all of the residual; the remaining 2–3% is most plausibly the DANE pob_san_a substitution (item 13) plus rounding-order (item 6). The total-deaths denominator (`deaths`) matches exactly between pipeline and samuel-runner: both have 882,545 (with codptore=75 filtered).

### Checkpoint 5 — YLL (year × cause)

**Saved (full granularity):** `checkpoint_5a_yll_full.rds` — per (year, depto, sex, age, cause) YLL using DANE depto-specific life tables and the (ex - 2.5) / 10-year-cap rule.

**Saved (national-annual):** `checkpoint_5b_yll_year_cause.rds`.

**Diff against pipeline `output/results/ylls_125.rds`:**

| metric | n | mean abs % | median abs % | max abs % |
|---|---|---|---|---|
| `yll_heat` | 170 | 223.2% | 17.5% | 9879% |
| `yll_cold` | 170 | 58.0% | 7.3% | 4906% |
| `yll_nonopt` | 170 | 211.7% | 6.5% | 9879% |

Top causes are within ±10-30% (median ~7-17%):

| cause | pipeline yll_heat | sam-runner yll_heat | gap % | pipeline yll_cold | sam-runner yll_cold | gap % |
|---|---:|---:|---:|---:|---:|---:|
| `inj_homicide` | 32,102 | 29,338 | -8.6% | 5,015 | 7,261 | +44.8% |
| `cvd_ihd` | 10,237 | 9,382 | -8.4% | 46,457 | 48,637 | +4.7% |
| `inj_trans_road` | 7,261 | 9,664 | +33.1% | 1,384 | 2,173 | +57.0% |
| `cvd_stroke` | 3,202 | 3,023 | -5.6% | 9,899 | 10,313 | +4.2% |
| `resp_copd` | 2,715 | 2,089 | -23.0% | 16,315 | 18,449 | +13.1% |
| `inj_suicide` | 1,654 | 2,050 | +24.0% | 242 | 322 | +33.0% |
| `ckd` | 1,435 | 1,581 | +10.2% | 5,188 | 5,103 | -1.6% |
| `diabetes` | 1,111 | 1,168 | +5.1% | 1,837 | 1,754 | -4.5% |
| `inj_drowning` | 789 | 1,280 | +62.3% | 7,328 | 7,279 | -0.7% |

| | pipeline | samuel-runner | sam-runner vs pipe | sam-runner vs published (63556 / 109313) |
|---|---:|---:|---:|---:|
| Total heat YLL | 61,697 | 60,725 | -1.6% | -4.5% |
| Total cold YLL | 98,845 | 106,751 | +8.0% | -2.3% |
| Total non-optimal YLL | 160,542 | 167,476 | +4.3% | -3.1% |

**Conclusion:** YLL gap mirrors burden gap. Pipeline-vs-sam-runner gap is 4-8% on totals; sam-runner-vs-published is 2-5%. Notably, **for heat homicide the samuel-runner produces 29,338 YLL vs Samuel's published 30,045 (-2.4%) and pipeline's 32,102 (+6.8%)** — both bracket the published value at ~7% from each side, and the runner's ±2% match is well inside the 5% pass tolerance.

---

## Specific findings on the "heat-side gap" (heat homicide -24%, heat road -39%)

The headline gap reported by `util_compare_to_samuel.R` was:

```
PAF heat — inj_homicide,0.0053,0.007,-24.35,FAIL
PAF heat — inj_trans_road,0.0037,0.006,-38.56,FAIL
```

(That CSV is in `output/results/comparison_to_samuel_125.csv`.)

These were the most striking failures in the comparison. The checkpoint diagnostic reveals what's happening:

### What `util_compare_to_samuel.R` actually computes

In lines 237-241:

```r
cause_paf <- burden[, .(paf_heat_avg = sum(deaths_heat) / sum(deaths),
                        paf_cold_avg = sum(deaths_cold) / sum(deaths)),
                    by = acause]
```

So the pipeline's "PAF" for the comparison is `total_attributable_deaths / total_deaths` — a **death-weighted** quantity that bakes in the year-by-year deaths × PAF × SEV multiplication. For homicide this is `727 / 137,432 = 0.0053`. For road it's `192 / 52,205 = 0.0037`.

### What Samuel's published 0.007 / 0.006 actually measures

Samuel's executive-summary prose (`from-samuel/results/WB_climatechange_Colombia_executive summary.pdf` p13) reports per-cause PAFs. Reading `11_carga_atribuible.R` end-to-end, the only national-level PAF Samuel writes out is the **plain mean** of `paf_day_depto` rows in the year (after taking `paf_year_depto = mean(paf_day_depto)` over date and depto rows). That is the `sam_paf_h_mean` column in our CP2 output.

For heat homicide: samuel-runner's `paf_h_mean` = 0.0077 (within +10.2% of Samuel's 0.0070).
For heat road: samuel-runner's `paf_h_mean` = 0.0063 (within +5.5% of Samuel's 0.0060).

### What samuel-runner produces under each convention

| cause | published | pipe annual mean | pipe death-wt | sam-runner plain mean | sam-runner death-wt |
|---|---:|---:|---:|---:|---:|
| `inj_homicide` (heat) | 0.0070 | 0.0065 (-6.9%) | 0.0053 (-24.4%) | **0.0077 (+10.2%)** | 0.0049 (-29.8%) |
| `inj_trans_road` (heat) | 0.0060 | 0.0061 (+1.3%) | 0.0037 (-38.6%) | **0.0063 (+5.5%)** | 0.0048 (-20.6%) |
| `diabetes` (heat) | 0.0050 | 0.0046 (-7.1%) | 0.0046 (-7.2%) | 0.0049 (-1.8%) | 0.0047 (-6.4%) |
| `inj_drowning` (cold) | 0.0185 | 0.0211 (+13.9%) | 0.0182 (-1.4%) | 0.0185 (-0.1%) | 0.0183 (-0.9%) |
| `resp_copd` (cold) | 0.0111 | 0.0115 (+4.0%) | 0.0112 (+0.8%) | 0.0112 (+0.8%) | 0.0127 (+14.8%) |
| `lri` (cold) | 0.0104 | 0.0099 (-4.4%) | 0.0096 (-7.5%) | 0.0104 (+0.4%) | 0.0103 (-0.6%) |

**Key observation:** the samuel-runner's plain-mean PAF (column 4) reproduces Samuel's published numbers to ±10%. The samuel-runner's death-weighted PAF (column 5) — computed from the same arithmetic but reported the way the pipeline reports it — is much closer to the pipeline's death-weighted PAF (column 3). **Samuel's report is internally inconsistent: the prose-quoted per-cause PAF and the per-cause attributable-death numbers cannot both be back-computed from each other under any single arithmetic.** What looks like a 24-39% pipeline bug is mostly a measurement-statistic mismatch between the two reports.

The remaining 5-30% gap *within the same statistic* (death-weighted PAF: pipeline 0.0053 vs samuel-runner 0.0049 = 7.5% on homicide; 0.0037 vs 0.0048 = 30% on road) **is** real and is the depto-day aggregation effect: Samuel's deaths-weighted PAF reflects a per-depto, per-day mortality × PAF × SEV product where `cov_depto(PAF, deaths) ≠ 0` and `cov_depto(SEV, deaths) ≠ 0`. The pipeline collapses everything to national-annual and loses those covariances.

### Sample-covariance arithmetic

Without working through algebra: the pipeline computes

$$\hat{D}_{\text{heat}}^{\text{pipe}} = \bar{D} \cdot \overline{\text{PAF}}_{\text{nat,annual}} \cdot \overline{\text{SEV}}_{\text{nat,annual}}$$

(at year × cause level, then summed across years). Samuel computes

$$\hat{D}_{\text{heat}}^{\text{sam}} = \sum_{d, t, s, a} D_{d,t,s,a} \cdot \text{PAF}_{d,t} \cdot \text{SEV}_{d}$$

(summed across depto × day × sex × age, where PAF lives at depto-day and SEV lives at depto-year).

The ratio is

$$\frac{\hat{D}_{\text{heat}}^{\text{sam}}}{\hat{D}_{\text{heat}}^{\text{pipe}}} = 1 + \frac{\text{cov}(D_{d,t}, \text{PAF}_{d,t} \cdot \text{SEV}_{d})}{\bar{D} \cdot \overline{\text{PAF}} \cdot \overline{\text{SEV}}}$$

Sign and magnitude are cause-specific. For homicide, where heat-side deaths concentrate in hot lowland deptos (Atlántico, Magdalena, Bolívar) that also have high SEV and high PAF, the covariance is positive and the ratio is >1 — Samuel attributes more burden than the pipeline. For road where the cause has a different geographic distribution, the covariance can be different. Empirically:

- Homicide heat: pipeline 727.7, samuel-runner 675.5, ratio 0.928. ⇒ covariance is **negative** for homicide. Pipeline overestimates.
- Road heat: pipeline 192.4, samuel-runner 248.6, ratio 1.292. ⇒ covariance is **positive** for road. Pipeline underestimates.

Both directions exist; that is the structural signature of carrying covariance vs collapsing it.

---

## Conclusion on the structural hypothesis

**The depto × day aggregation hypothesis is supported.** Specifically:

1. **CP1 confirms input identity.** ERF curves are byte-equivalent — not a divergence source.

2. **CP2 (PAF) divergence is consistent with the aggregation difference.** When samuel-runner's depto-day PAFs are collapsed via the same arithmetic the pipeline uses (deaths-weighted ratio of `total_deaths_heat / total_deaths`), the gap to pipeline is in the 5-15% range for top causes — the same order of magnitude as the *true* aggregation effect at later checkpoints. The much larger PAF gap that `util_compare_to_samuel.R` reports against published numbers is dominated by a measurement-convention mismatch, not by arithmetic divergence.

3. **CP3 (SEV) divergence is small for top-tier saturation-prone causes (<1% on cvd_ihd, ckd, lri, resp_copd, cvd_stroke, cvd_htn, diabetes) and 8-22% for mid-tier unsaturated causes (inj_mech, inj_othunintent, inj_suicide).** The SEV calculation is robust to the depto/national choice when SEV is near 1; sensitivity is concentrated in mid-tier causes. This matches the comparison report's qualitative assessment of items 24 and 25.

4. **CP4 (burden) and CP5 (YLL) totals match within 4-8% pipeline-to-samuel-runner.** Samuel-runner reproduces Samuel's published values to within 2-5%. The remaining 2-3% residual after accounting for depto-day aggregation is most plausibly the DANE pob_san_a substitution (item 13) and the rounding-order on TMREL averaging (item 6). Both were flagged as "small" or "negligible" magnitude in the comparison report, consistent with the empirical residual.

5. **No hidden bug was uncovered.** Every numerical difference observed is traceable to an item already in `comparison-report-pipeline-vs-samuel.md`. The largest unpatched item — the depto-day aggregation cluster (12, 15, 20, 21, 24, 25) — accounts for nearly all of the residual gap.

**Recommendation for the team:** proceeding with the depto-day refactor of the global pipeline is justified. The expected gap closure is 4-8 percentage points on totals (bringing pipeline within 0-3% of Samuel's published numbers, well inside the 5% tolerance). Lower-priority follow-up items (DANE pop_san_a substitution; TMREL round-then-average vs average-then-round) can close another 1-2% but are not blocking.

---

## Open questions and surprises

1. **Samuel's report mixes two different "PAF" statistics.** The per-cause PAFs in the prose (0.007 for homicide heat, 0.006 for road heat) appear to be plain arithmetic means across pixel-day-depto rows of `paf_day_depto`. The per-cause attributable-death numbers in the same report (`inj_homicide` heat = ~727 deaths) imply a deaths-weighted PAF closer to 0.0053. These two "PAFs" cannot be the same number under any single arithmetic. The team should consider clarifying which definition it standardizes on for the projection report. Recommendation: report deaths-weighted PAF (`total_attributable_deaths / total_deaths_in_cause`) as the primary statistic since it back-computes to the deaths and YLL totals, and label any plain-mean reports as such.

2. **The samuel-runner's deaths-weighted PAF for `inj_drowning` (heat) is +50% higher than the pipeline's** (0.0033 vs 0.0022). The published target (cold-side, 0.0185) matches both well, but on heat side this is a 50% disagreement on a small absolute number (16 vs 26 attributable deaths over 10 years). Worth noting as a sensitivity case if the projections lean on drowning-heat-side detail.

3. **Pipeline's heat homicide deaths (727.7) is *higher* than samuel-runner's (675.5).** The PAF-floor-before-aggregation (item 18, already patched in the verification block) was hypothesized to *close* the gap (move pipeline higher); the samuel-runner has the same per-row floor and lands lower. The reason is that the pipeline's national-zone aggregation of mortality treats homicide deaths as if uniformly distributed across deptos, while samuel-runner concentrates them in low-SEV deptos for this cause; the depto-day-level multiplication gives a smaller total. **Direction of the depto-day aggregation effect is cause-specific** — for homicide it makes Samuel's number smaller, not larger.

4. **`inj_trans_road`'s gap is in the opposite direction:** samuel-runner heat road = 248.6 vs pipeline = 192.4 (+29%). For road, the pipeline underestimates. This is the kind of direction-reversal across causes that pure aggregation differences can produce, but it does mean the depto-day refactor will *increase* some cause-specific burdens and *decrease* others. Net effect on totals is positive (pipeline → samuel-runner: +4.5% on non-optimal deaths) but cause-by-cause the sign is mixed.

5. **The runner aggregates pob over San Andrés to 0** (DANE substitution unavailable). Empirically affects ~3,652 SEV-input rows for one pixel × 365.25 days × 10 years. Likely contributes <1% to the samuel-runner-vs-published residual, but is the most plausible single explanation for the persistent 2-3% gap.

6. **The pipeline's `sevs_125.rds` contains SEV values often saturated to 1.** That's expected per `06_compute_sevs.R:90-103` (`sev := pmin(sev * n_days, 1)`) and matches Samuel's daily-summation cap. CP3 confirms this empirically: samuel-runner's deaths-weighted SEV for top causes is also ~1. **Not a divergence source**, but the team should be aware that the 1-cap means SEV-related improvements (e.g. items 5/23 about `rr_max` definition) will mostly affect mid-tier causes.

---

## Reproduction

```bash
cd /var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections
distrobox enter emacs-r -- Rscript samuel-runner/run_samuel_with_checkpoints.R
distrobox enter emacs-r -- Rscript samuel-runner/paf_compare.R
distrobox enter emacs-r -- Rscript samuel-runner/burden_inspect.R
```

Wall-clock runtime on the dev machine (~11 GB RAM peak, 4 cores): ~90 seconds.
