# Step 2: Comparison of Colombia Scripts vs Burkart Reference Implementation

## Overview

This document compares the Colombia pipeline (`global-scripts/`) against the Burkart et al. reference code (`from-samuel/Info Burkart/environmental_risk_factors-temperature_lancet_2021/`). The goal is to identify discrepancies that could affect results or need resolution before scaling globally.

**Bottom line:** The Colombia pipeline produces reasonable estimates but takes several methodological shortcuts relative to the reference. The most consequential are (1) not propagating uncertainty through draws, (2) the `× SEV` multiplier in the attributable burden formula, and (3) the temperature data lacking Monte Carlo draws. These are ordered below by severity.

---

## CRITICAL ISSUES

### 1. Uncertainty propagation: summary statistics vs 1000 draws

**Burkart reference (`pafCalc_sevFix.R`):**
- Loads raw RR curve CSVs with 1000 draws (`draw_0` through `draw_999`) in log space
- Exponentiates all 1000 draws (line 69)
- Reshapes to long format: one row per (zone, daily_temp, cause, **draw**) (line 78)
- Loads TMRELs as 1000 draws per location/year (`tmrel_0` through `tmrel_999`) (line 92)
- Merges RR draws to TMREL draws **by draw number** (line 97)
- Rescales each RR draw to equal 1.0 at that draw's TMREL (lines 101-102)
- Computes PAFs **per draw** (line 212)
- Reshapes PAFs back to wide (1000 draw columns) and computes summary statistics (mean, 2.5th, 97.5th percentile) only at the very end (lines 223-225)

**Colombia script (`09_curvas_ER.R` + `11_carga_atribuible.R`):**
- Loads the same raw CSVs with 1000 draws
- **Immediately computes summary statistics** (mean, 25th percentile, 97.5th percentile, 99th percentile) across draws (line 29-34 of 09)
- Exponentiates the summaries (line 38-44 of 09)
- Discards the 1000 draws entirely — all downstream computation uses `rr_mean`, `rr_lower`, `rr_upper`
- TMREL is also loaded as summary only (`tmrelMean`, `tmrelLower`, `tmrelUpper`) and then **averaged across all years** into a single value per zone (lines 243-249 of 11)

**Impact:** This is the most consequential difference. By summarizing draws upfront:
- Uncertainty intervals on PAFs/attributable burden are **not propagated** — they cannot be computed correctly from point estimates of RR
- The mean of exponentiated draws ≠ the exponentiate of the mean of log-draws (Jensen's inequality). The order of operations matters: Burkart exponentiates each draw then summarizes; Samuel summarizes log-space draws then exponentiates the summary. For convex functions (exp), this systematically **underestimates the mean RR**
- The TMREL is draw-specific in Burkart. Each draw has its own optimal temperature, and the RR curve is rescaled relative to that draw's TMREL. Using a single average TMREL for all draws conflates the uncertainty in the optimal temperature with the uncertainty in the curve shape.

**Recommendation:** For the global pipeline, propagate all 1000 draws through PAF/SEV calculation, matching the Burkart approach. This is computationally heavier but essential for valid uncertainty intervals.

---

### 2. The `× SEV` multiplier in attributable burden

**Burkart reference (`pafCalc_sevFix.R`):**
- PAFs and SEVs are computed **separately** and saved to separate output files (lines 303-306)
- PAFs are the output used for burden attribution in the GBD framework
- SEVs are a summary exposure metric used for **reporting and comparison** — they are NOT multiplied into the attributable burden calculation
- The attributable burden calculation happens downstream: `attributable_deaths = total_deaths × PAF`

**Colombia script (`11_carga_atribuible.R`):**
- Computes PAFs and SEVs, then calculates:
  ```r
  muertes_cold = muertes * paf_cold * sev    # line 552
  muertes_heat = muertes * paf_heat * sev    # line 553
  ```
- The `× sev` multiplier reduces the attributable burden below what PAF alone would give

**Impact:** This is methodologically incorrect relative to the GBD/Burkart framework. The PAF already captures the population-level fraction of deaths attributable to temperature. Multiplying by SEV introduces a second scaling factor that:
- Double-counts the exposure distribution (which is already embedded in the PAF calculation)
- Systematically **underestimates** attributable burden, since SEV ∈ [0, 1]

Samuel appears to have adopted this approach deliberately (there's also a `carga_atriuible_nosevs` version computed at lines 584-598 without the SEV multiplier), possibly as a conservative adjustment. But it does not match the reference methodology.

**Recommendation:** The standard GBD formula is `attributable_burden = total_burden × PAF`. Remove the `× SEV` multiplier. Keep SEV as a separate diagnostic/reporting metric.

#### 2b. Samuel's SEV computation itself is wrong (additional bug found 2026-05-04)

Even within the choice to multiply by SEV, the way Samuel *computes* SEV does not produce a valid 0–1 exposure fraction. In `11_carga_atribuible.R:444-469`:

```r
base_pobtemp <- df_base_sevs %>%
  group_by(fecha, cod_depto, c_muerte, zona, temperatura) %>%   # PER DAY
  summarise(pob_temp = sum(pob), pob_zona = mean(pob_zona), ...) %>%
  mutate(pr_zona = pob_temp / pob_zona)                          # snapshot pop denom

sev <- base_pobtemp %>%
  mutate(sev = ifelse(rr_max <=1 | rr_mean<=1, 0,
                      pr_zona * (rr_mean - 1)/(rr_max-1))) %>%
  group_by(ano, cod_depto, c_muerte, zona) %>%
  summarise(sev = sum(sev), ...) %>%                             # SUM ACROSS DAYS
  mutate(sev = ifelse(sev > 1, 1, sev))                          # CAP AT 1
```

`pob_zona` is a single-day Jan-1 snapshot of zone population (line 320-333). `pr_zona = pob_temp / pob_zona` is therefore a *daily* fraction. Summing across all 365 days gives values up to ~365, which Samuel then caps at 1.

**Effective behavior:** for any cause where temperature has a non-trivial effect (RR meaningfully > 1) in any high-population zone, the daily sum saturates to 1 within the first few days, so Samuel's "SEV" effectively becomes an indicator of "does this cause have any temperature exposure in this zone-year?" weighted by RR-intensity. For top causes (IHD, stroke, COPD, drowning), it sits at 95–100%, which is what the report shows in Tables A1.15–A1.21.

**Comparison with our pipeline:** our SEV in `06_compute_sevs.R` follows the conventional definition — aggregate to person-time at each (zone, temp, year) and divide by total person-time → a true [0,1] fraction. The ratio Samuel/Ours ≈ N days/year, capped at 1.

**Impact:** When run with our (correct) SEV definition, Samuel's `× SEV` multiplier shrinks attributable burden by ~5×. With his (saturated) SEV, the multiplier is mostly a no-op for top causes. This is why Samuel's reported total (9,472 deaths) is ~6× our verification-mode result before the fix.

**Replicated in our pipeline (verification-mode only):** `06_compute_sevs.R` multiplies the per-zone year-level SEV by N days in the year and caps at 1, mirroring Samuel. Gated on `COLOMBIA_VERIFICATION = TRUE` and `LOCATION_ID = 125`; `config.R` errors if either condition is violated.

---

### 3. Temperature data: point estimates vs Monte Carlo draws

**Burkart reference (`era2melt.R`):**
- For each pixel-day, has both a temperature mean and a standard deviation (from ERA5)
- Creates **1000 Monte Carlo temperature draws** per pixel-day: `dailyTempCat = round(sd × rnorm(N, 0, 1) + dailyTempCat)` (line 238)
- Collapses population by (zone, daily_temp_draw, draw_number)
- Output: `melt_{loc}_{year}.csv` with columns `meanTempCat`, `dailyTempCat`, `pop`, `draw`
- These temperature draws are matched to RR draws by draw number in `pafCalc_sevFix.R`

**Colombia script (`10_ajuste_temperatura.R`, `10_2_Proces_temp.R`):**
- Uses point estimates of daily temperature per pixel (no SD, no draws)
- Temperature is a single value per pixel-day — no uncertainty

**Impact:** Temperature measurement uncertainty is not propagated into PAF estimates. For Colombia this may be acceptable since ERA5 temperature data for a well-observed tropical country has relatively low uncertainty. For global application (especially in data-sparse regions like sub-Saharan Africa), temperature uncertainty could be substantial and should be propagated.

**Recommendation:** For the global pipeline, implement temperature draws matching the Burkart approach. This also aligns the draw-level merge with RR draws.

---

## MODERATE ISSUES

### 4. RR rescaling at the TMREL

**Burkart reference:**
- After merging RR draws with TMREL draws, finds the RR at the TMREL for each draw:
  ```r
  rr[, rrRef := sum(rr * (dailyTempCat==tmrel), na.rm = T),
     by = c("meanTempCat", "acause", "draw", "location_id", "year_id")]
  rr[, rr := rr / rrRef]    # lines 101-102
  ```
- This ensures RR = 1.0 exactly at the TMREL for every draw
- The rescaling is **location-specific and year-specific** (because TMREL varies by location and year)

**Colombia script:**
- Joins the TMREL to the RR table and records the RR at the TMREL (`rr_tmrel_mean`), but **does not divide the RR curves by this reference value**
- Instead, the PAF formula handles the offset implicitly: `PAF = pr × (RR - 1) / RR`
- The PAF formula is mathematically equivalent to `1 - 1/RR` when `pr = 1`, which assumes the reference RR is already 1.0

**Impact:** If the raw exponentiated RR at the TMREL temperature is not exactly 1.0 (which it generally won't be — the MR-BRT model estimates log(RR) on a spline, and the TMREL-temperature RR won't be exactly 0 in log space), then the PAF formula gives slightly wrong results. The bias depends on how far from 1.0 the raw RR is at the TMREL.

**Recommendation:** Explicitly rescale RR curves to equal 1.0 at the TMREL before computing PAFs, matching the reference.

---

### 5. PAF formula implementation

**Burkart reference:**
```r
pafs <- pafs[, lapply(.SD, function(x) {
    as.double(sum(ifelse(x>=1, pr*(x-1)/x, pr*-1*((1/x)-1)/(1/x))))
  }), .SDcols = "rr",
  by = c("acause", "risk", "draw", "year_id", "location_id")]
```
- `pr` is the population proportion for each pixel-day combination
- This sums across all pixel-days within a location-year-cause-draw-risk combination
- PAFs can be negative (protective effects of cold on external causes)
- The PAF formula itself `(RR-1)/RR` for RR≥1 and `-((1/RR)-1)/(1/RR)` for RR<1 is **identical** in both implementations

**Colombia script:**
```r
paf = case_when(
    rr_mean >= 1 ~ pr*(rr_mean -1)/rr_mean,
    TRUE ~ pr* -1*((1/rr_mean)-1)/(1/rr_mean)
),
paf = ifelse(paf < 0, 0, paf)    # line 379: floors PAF at 0
```
- Same formula, but applied to `rr_mean` instead of individual draws
- **Floors PAF at 0** — removes protective effects

**Impact:** The PAF floor at 0 removes the (biologically real) protective effects that cold temperatures have on external causes like drowning, homicide, and suicide. In Burkart's results, these protective effects are clearly visible (negative cold-attributable burden for external causes in several countries, Table 1). Samuel's pipeline would attribute zero cold burden to these causes instead of a protective (negative) burden.

Note: Samuel also computes `paf2` without the floor, but uses `paf` (floored) in the main attributable burden calculation.

**Recommendation:** Do not floor PAFs at zero. Allow negative PAFs for protective effects, consistent with the Burkart methodology.

---

### 6. TMREL: averaged across years vs year-specific

**Burkart reference:**
- TMREL files contain year-specific values (`year_id` column, 1990–2020)
- TMRELs vary by location, year, and temperature zone
- Loaded and merged by `(location_id, year_id, meanTempCat, draw)` in `pafCalc_sevFix.R`

**Colombia script:**
- Loads the TMREL summary file for location 125 (Colombia)
- **Averages across all years** into a single value per temperature zone (lines 243-249 of script 11)
- Uses this static TMREL for all years 2010-2019

**Impact:** The TMREL can shift over time as the composition of causes of death changes (since it's the death-weighted minimum-risk temperature). By averaging, you lose the ability to detect temporal trends in the optimal temperature. For the retrospective resilience analysis, year-specific TMRELs are particularly important since we're comparing across time.

**Recommendation:** Use year-specific TMRELs. The TMREL files in `Info Burkart/TMRELs/` already provide year-specific values for many locations.

---

### 7. ICD code mapping and garbage code redistribution

**Burkart reference (`clean_vr.R`, `apply_rd.R`):**
- Maps ICD-10 codes to GBD causes using a standardized package map
- Redistributes "garbage codes" (ill-defined or non-specific ICD codes) proportionally to mapped causes
- This is critical for accurate cause-of-death attribution, especially in countries with high proportions of garbage-coded deaths

**Colombia script (`01_mortalidad_2010_2020.R`):**
- Maps ICD-10 codes directly to the 17 temperature-sensitive causes using regex pattern matching on `c_bas1` (the underlying cause of death field)
- **No garbage code redistribution** — deaths coded to non-specific codes are simply excluded
- Uses `missRanger` for imputing missing demographic fields (age, sex, department), not for cause redistribution

**Impact:** Deaths coded to garbage codes (e.g., "unspecified cardiac arrest", "senility") that should be redistributed to specific causes like IHD or stroke are lost. This systematically underestimates the total cause-specific death counts, and the bias varies by cause. The magnitude depends on Colombia's garbage code fraction (typically 15-30% in Latin American vital registration systems).

**Recommendation:** For global runs, implement garbage code redistribution following the Burkart/GBD approach. For Colombia specifically, assess the garbage code fraction to determine the magnitude of underestimation.

---

## NON-ISSUES (verified as equivalent)

- **Temperature zone truncation**: Both implementations cap zones to 6–28°C and truncate daily temperatures to curve boundaries. Samuel's remapping of zones 29–31 → 28 is correct.
- **Population weighting**: Both use pixel-level population proportions summed within locations. Equivalent approach at similar spatial resolution.
- **SEV formula**: Same formula in both. Samuel's use of summary statistics rather than draws is a downstream consequence of critical issue #1, not a separate problem.

---

## SUMMARY TABLE

| # | Issue | Severity | Direction of bias | Effort to fix |
|---|-------|----------|-------------------|---------------|
| 1 | No draw-level uncertainty propagation | **Critical** | Underestimates mean RR (Jensen's ineq.); no valid CIs | High |
| 2 | `× SEV` multiplier on attributable burden | **Critical** | Underestimates burden | Low |
| 3 | No temperature draws | **Critical** | Ignores measurement uncertainty | High |
| 4 | No RR rescaling at TMREL | Moderate | Small bias depending on raw RR at TMREL | Low |
| 5 | PAF floored at zero | Moderate | Removes protective effects for external causes | Low |
| 6 | TMREL averaged across years | Moderate | Loses temporal signal | Low |
| 7 | No garbage code redistribution | Moderate | Underestimates cause-specific deaths | High (global) |

## RECOMMENDED FIX ORDER

1. **Remove `× SEV` multiplier** from attributable burden formula (quick fix, large impact)
2. **Remove PAF floor at zero** — allow negative/protective PAFs (quick fix)
3. **Add RR rescaling at TMREL** (quick fix)
4. **Use year-specific TMRELs** (quick fix — data already available)
5. **Propagate 1000 draws** through entire pipeline (large refactor, needed for valid CIs)
6. **Add temperature draws** (needed for global, can start with point estimates for Colombia)
7. **Implement garbage code redistribution** (needed for global, less critical for Colombia)
