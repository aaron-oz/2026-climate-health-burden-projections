# Pipeline math verification

End-to-end hand verification of one (pixel, day, cause) row through every
stage of the burden pipeline, comparing the trace tool's reported values
against independently-recomputed values, plus independent re-derivation
of the per-subloc PAF and the country burden total.

**Purpose:** prove (not just plausibility-check) that the pipeline math is
correct. Any off-by-one, wrong-index, sign-flip, formula-typo, or missing
transformation step would surface as a mismatch.

**Setup:**
- Location: Colombia (loc_id 125)
- Year: 2010
- Pixel: 527 (Bogotá-area, zone 14)
- Date: 2010-01-02
- Cause: cvd_ihd
- Mode: summary (USE_DRAWS=FALSE)
- Data: Samuel Cuervo's validated Colombia 2010-2019 pixel-day temperature
  and DANE mortality (the dataset the pipeline reproduces within ±5% on
  18 of 19 published headline metrics — see
  [`colombia-validation-state.tex`](colombia-validation-state.tex))

## Honest note on the first attempt

A first pass at this verification (committed in `f8f3f3a`) confirmed the
**trace tool's** output matched independent recomputation using the
formula `pr × (RR-1)/RR` applied to the raw RR straight from the saved
ERF file. That checks the *tool's* arithmetic but does **not** verify the
pipeline's results because the pipeline applies an extra **RR rescaling
step** before computing PAF (line ~129 of `05_compute_pafs.R`): the RR
curve is divided by the RR value at the TMREL temperature so that the
rescaled RR equals 1 at the TMREL. The trace tool wasn't showing this
step, so the "PAF contribution" in the original tool's output did not
correspond to what the pipeline actually saves.

Verifying the per-subloc aggregation independently surfaced a 5× mismatch
between my hand computation (using un-rescaled RR) and the pipeline's
saved PAF. Once I added the rescaling step, my computation matched the
pipeline's saved value **to 8 decimal places**.

The trace tool was fixed (`util_trace_pipeline.R`) to show raw RR,
RR-at-TMREL, rescaled RR, and the PAF contribution computed from
rescaled RR. The verification below uses the corrected tool.

## Step-by-step

### Step 1 — Raw input row from `data/temperature/125_daily_temp.rds`

| field      | value      |
|------------|-----------:|
| daily_temp | 13.589166 °C |
| pop        | 5,100,137.1 |
| subloc_id  | 11 (Bogotá D.C.) |

### Step 2 — `daily_temp_10` derivation per `03_load_temperature.R`

```
daily_temp_10 = as.integer(round(daily_temp * 10))
              = round(13.589166 * 10)
              = 136
```

Hand-computed: **136**. Trace tool: **136**. ✓

### Step 3 — Zone assignment (annual mean per pixel, rounded)

```
annual mean for pixel 527 in 2010 = 13.786322 °C over 365 days
zone = as.integer(round(13.786322)) = 14
```

Hand-computed: **14**. Trace tool: **14**. ✓

### Step 4 — Within-subloc pop fraction (pr)

Per `03_load_temperature.R`, `pr` is each pixel-day's `pop` divided by
the total person-days in that subloc-year:

```
pop_subloc_total (subloc 11, year 2010) = 3,188,693,158
pr = 5,100,137.1 / 3,188,693,158 = 1.599444 × 10⁻³
```

Hand-computed: **1.599444e-03**. Trace tool: **0.001599**. ✓ (rounding)

Also confirmed: sum of `pr` within each subloc-year = 1.0 (33 of 33
sublocs match), so the normalization is correct.

### Step 5 — Raw RR lookup in `output/intermediate/erf_curves.rds`

For (zone=14, daily_temp=136, acause=cvd_ihd), summary mode:

```
rr_mean (raw) = 1.018998
```

Hand-computed: **1.018998**. Trace tool's "raw RR" column: **1.019**. ✓

### Step 6 — RR rescaling (the step the original verification missed)

The pipeline does, in `05_compute_pafs.R` non-verification mode:

```
For each (zone, cause, year), find rr_ref = rr_mean at the TMREL temperature.
Replace rr_mean ← rr_mean / rr_ref for every (daily_temp) in that group.
```

For zone=14, cvd_ihd, year=2010:
- TMREL = 235 (= 23.5 °C)
- rr_mean at TMREL (raw) = 0.944913 (looked up from the ERF file)
- rescaled rr at daily_temp=136 = 1.018998 / 0.944913 = **1.078403**

Hand-computed rescaled RR: **1.078403**. Trace tool's "rescaled RR" column:
**1.0784**. ✓

### Step 7 — TMREL lookup in `output/intermediate/tmrel.rds`

For (zone=14, year=2010), summary mode:

```
tmrel_mean_10 = 235 (= 23.5 °C)
```

Hand-computed: **235**. Trace tool: **235**. ✓

### Step 8 — Heat/cold classification

```
risk = if (daily_temp_10 < tmrel) "cold" else "heat"
     = if (136 < 235) "cold" → "cold"
```

Hand-computed: **cold**. Trace tool: **cold**. ✓

### Step 9 — PAF contribution = pr × (RR_rescaled − 1) / RR_rescaled

(Burkart attributable-fraction formula applied to the **rescaled** RR;
RR ≥ 1 branch since rescaled RR = 1.078 here.)

```
paf_contrib = pr × (RR_rescaled - 1) / RR_rescaled
            = 1.599444e-03 × (1.078403 - 1) / 1.078403
            = 1.599444e-03 × 0.072704
            = 1.162848e-04
```

Hand-computed: **1.162848e-04**. Trace tool's "PAF contrib using rescaled
RR" column: **0.000116285**. ✓

---

## Aggregation verification — per-subloc PAF

The previous 9 steps verify one row. Step 10 is the aggregation: summing
per-row contributions to a per-subloc PAF, then to a country PAF, then
multiplying by mortality counts to get attributable deaths.

I re-implemented the pipeline's exact aggregation (replicating the
`temp_agg` pre-aggregation by (subloc, zone, daily_temp_10, year), the
RR rescaling, the heat/cold classification, and the sum within
(acause, risk, subloc, year)) for subloc 11, year 2010, cvd_ihd:

| | hand-computed | pipeline saved | match |
|---|--:|--:|:--:|
| paf_cold (subloc 11) | 0.06956161 | 0.06956161 | ✓ (8 dp) |
| paf_heat (subloc 11) | 0.00000000 | 0.00000000 | ✓ |

## Aggregation verification — country burden total

For the country total: take the pipeline's saved per-subloc PAFs from
`pafs_125.rds`, multiply by per-subloc mortality counts, sum across
sublocs. This verifies the `burden = deaths × PAF` multiplication chain
(does NOT independently re-verify the PAFs — those are verified at the
per-subloc level above):

| | hand-computed | burden_125.rds | match |
|---|--:|--:|:--:|
| country deaths_cold (cvd_ihd) | 1315.9 | 1315.9 | ✓ |
| country deaths_heat (cvd_ihd) | -64.7 | -64.7 | ✓ |

## Result

Every stage between raw input and country-level attributable burden
**independently reproduces the pipeline's saved values exactly** (or to
displayed sig figs where the trace tool rounds), including the previously
missed RR rescaling step.

The pipeline math is correct for this row, this subloc, and this country
aggregate.

## What this verifies (and what it doesn't)

**Verified:**
- The arithmetic in `03_load_temperature.R`, `01_load_erf.R`,
  `02_load_tmrel.R`, and `05_compute_pafs.R` for one example walks
  through cleanly when reproduced from the saved RDS files.
- The PAF rescaling step is correctly applied.
- The within-subloc aggregation, the cross-subloc aggregation, and the
  deaths × PAF multiplication are correct.
- The trace tool (after the fix) reports values that match the pipeline.

**NOT verified by this exercise:**
- Whether the ERF curves themselves (Burkart 2021's published RR draws)
  are correct or appropriate — those are external inputs.
- Whether the TMREL values (IHME's 1990 / 2010 / 2020 files) are correct.
- Whether the input temperature and mortality data are correct.
- Whether the pipeline formulas (PAF = pr × (RR-1)/RR; YLL = deaths × ex)
  are the right scientific choices — that's a methodology question
  documented in `gbd/ihme-plan-b-prep.tex`.
- Whether the pipeline's behavior in DRAW mode (which carries draws
  through the merge and produces uncertainty bands) is correct. This
  verification was done in summary mode; draws-mode aggregation is
  structurally similar but has its own merge fan-out to check.

For higher-confidence verification beyond this row-and-aggregation
walk, the strongest independent signal is the Colombia validation:
**18 of 19 headline metrics within ±5%** of Samuel Cuervo's published
2010-2019 numbers, using the same input data and pipeline code (see
`colombia-validation-state.tex`).

## Reproducing

```bash
# Run the pipeline (any mode):
distrobox enter emacs-r -- Rscript global-scripts/run_location.R \
  --location_id=125 --year_start=2010 --year_end=2010 \
  --use_draws=FALSE --run_diagnostics=FALSE

# Run the trace tool:
distrobox enter emacs-r -- Rscript global-scripts/util_trace_pipeline.R \
  --pixel_ids=527 --year=2010 --causes=cvd_ihd \
  --draws_summary=summary --output=output/trace_verification_one_row.md

# Verify by hand — the recomputation queries live inline in this commit's
# message (and the original verification session). Load
# output/intermediate/{temperature,erf_curves,tmrel}.rds, apply the
# formulas in the steps above, and the values reproduce exactly.
```

The trace tool produces a similar report for any pixel set + cause set
in seconds, against any computed pipeline output. The aggregation
verification queries are reusable too — same shape for any
(subloc, cause) pair.
