# Pipeline math verification

End-to-end hand verification of one (pixel, day, cause) row through every
stage of the burden pipeline, comparing the trace tool's reported values
against independently-recomputed values.

**Purpose:** prove (not just plausibility-check) that the pipeline math is
correct row-by-row. Any off-by-one, wrong-index, sign-flip, or
formula-typo would surface as a mismatch on one of these steps.

**Setup:**
- Location: Colombia (loc_id 125)
- Year: 2010
- Pixel: 527 (Bogotá-area metro, zone 14)
- Date: 2010-01-02
- Cause: cvd_ihd
- Mode: summary (USE_DRAWS=FALSE)
- Data: Samuel Cuervo's validated Colombia 2010-2019 pixel-day temperature
  and DANE mortality (the dataset the pipeline reproduces within ±5% on
  18 of 19 published headline metrics — see
  [`colombia-validation-state.tex`](colombia-validation-state.tex))

**Method:** run the pipeline, then `util_trace_pipeline.R`, then
independently recompute each value from the raw saved RDS files and
compare.

---

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

| computed | trace reports | match |
|---------:|--------------:|:-----:|
| 136      | 136           | ✓     |

### Step 3 — Zone assignment (annual mean per pixel, rounded)

```
annual mean for pixel 527 in 2010 = mean over 365 days = 13.786322 °C
zone = as.integer(round(13.786322)) = 14
```

| computed | trace reports | match |
|---------:|--------------:|:-----:|
| 14       | 14            | ✓     |

### Step 4 — Within-subloc pop fraction (pr)

Per `03_load_temperature.R`, `pr` is each pixel-day's `pop` divided by
the total person-days in that subloc-year:

```
pop_subloc_total (subloc 11, year 2010) = sum across all pixel-days
                                        = 3,188,693,158
pr = 5,100,137.1 / 3,188,693,158
   = 1.599444 × 10⁻³
```

| computed       | trace reports | match |
|---------------:|--------------:|:-----:|
| 1.599444e-03   | 0.001599      | ✓     |

### Step 5 — RR lookup in `output/intermediate/erf_curves.rds`

For (zone=14, daily_temp=136, acause=cvd_ihd), summary mode:

```
erf_row = erf[zone == 14 & daily_temp == 136 & acause == "cvd_ihd"]
rr_mean = 1.018998
```

| computed       | trace reports                  | match |
|---------------:|-------------------------------:|:-----:|
| 1.018998       | 1.019 (3 sig figs displayed)   | ✓     |

### Step 6 — TMREL lookup in `output/intermediate/tmrel.rds`

For (zone=14, year=2010), summary mode:

```
tmrel_row = tmrel[zone == 14 & year_id == 2010]
tmrel_mean_10 = 235  (= 23.5 °C)
```

| computed | trace reports | match |
|---------:|--------------:|:-----:|
| 235      | 235           | ✓     |

### Step 7 — Heat/cold classification per `05_compute_pafs.R`

```
risk = if (daily_temp_10 < tmrel) "cold"
       else if (daily_temp_10 > tmrel) "heat"
       else NA
     = if (136 < 235) "cold" → "cold"
```

| computed | trace reports | match |
|---------:|--------------:|:-----:|
| cold     | cold          | ✓     |

### Step 8 — PAF contribution = pr × (RR − 1) / RR

(Standard Burkart attributable-fraction formula; RR ≥ 1 branch since
RR = 1.019 here.)

```
paf_contrib = pr × (RR - 1) / RR
            = 1.599444e-03 × (1.018998 - 1) / 1.018998
            = 1.599444e-03 × 0.018644
            = 2.9819465495e-05
```

| computed             | trace reports     | match |
|---------------------:|------------------:|:-----:|
| 2.981947e-05         | 2.98195e-05       | ✓     |

---

## Result

All 8 steps match independent recomputation **exactly** (or to displayed
significant figures where the trace tool rounds). The pipeline arithmetic
for this row is correct end-to-end.

This verifies the per-pixel math up to and including the per-row PAF
contribution. The next aggregation step (sum of per-row contributions
across pixel-days within a subloc-year, producing a country-level PAF
that then multiplies cause-specific mortality counts to attributable
deaths) is structurally verified by the pipeline's `sum(heat) +
sum(cold) = sum(non-opt)` sanity check (in section 5 of the trace
report) and by the 18-of-19 PASS validation against Samuel's published
Colombia numbers.

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

# Hand-verify (the recomputation script lives inline in this commit's
# message and in the original verification session); the values above
# can be reproduced by loading temperature.rds, erf_curves.rds, and
# tmrel.rds from output/intermediate/ and applying the formulas
# step-by-step.
```

The trace tool produces a similar report for any chosen pixel set + cause
set in a few seconds, against any computed pipeline output. Caspar's
production runs should generate one of these per representative
(location, cause) sample for spot-checking before greenlight.
