# Handoff: uncertainty-draws fix plan (written 2026-08-20)

For the next Claude session (and Aaron). Read this top to bottom; steps are
in execution order. The session that wrote this ran the full ssp245 review
(2026-08-14 to 2026-08-20); its condensed state also lives in project memory
(`ssp245-review-state.md` under the Claude memory dir for this repo).

## 1. Where the project stands

- The ssp245 production run (Caspar's machine, 204 countries x 27 models x
  2022-2050, 500 draws) passed all integrity gates and validates well
  against the GBD 2019 temperature-attributable estimates (global 0.999,
  159/204 inside published UIs; the apparent hot-belt gap vs GBD 2023 is
  GBD's own between-release revision). Full story:
  `docs/reviews/ssp245-review-findings.org` (internal log, complete
  history including dead ends) and `docs/reviews/ssp245-review-summary.tex`
  (the polished 7-page report Aaron sends to Charlie/Caspar; its Section 6
  addendum states the pairing defect).
- **Launch of the remaining scenarios (ssp370/585/126) is HELD.** Reason:
  two uncertainty-propagation defects must be resolved first, because
  fixing either forces an ssp245 burden-step rerun (~the compute of one
  scenario's burden pass; temperature conversions are reusable).
- Aaron emailed Charlie and Caspar (2026-08-20) saying exactly this. His
  framing: the pairing fix is decided; the open work is assessing whether
  and how to include daily temperature uncertainty.

## 2. The two defects (background you must understand)

**Defect 1: TMREL draw pairing (decided: fix it).** The method defines
TMREL draw d as the argmin of RR draw d's death-weighted curve (IHME's
released `tmrelCalculator.R:126` in
`from-samuel/Info Burkart/environmental_risk_factors-temperature_lancet_2021/`).
IHME released 1,000 ERF draws but only 100 TMREL draws with no index
linkage (verified: released TMREL draws correlate ~0 with per-draw argmins
in any order). Our `02_load_tmrel.R:42-66` recycles the 100 across the 500
production draws; an arbitrarily paired reference is never the draw's own
minimum, so rescaled curves dip below 1 and the draw-mean PAF is biased
DOWN, one-signed, worst where curves are flat near the optimum (the
tropical low cluster: Haiti 0.35, Malaysia 0.45, Indonesia 0.56 etc. vs
GBD 2019). Experimentally confirmed: a replica with recycled pairing
reproduces production essentially exactly (median 1.00, global to 0.004%);
per-draw argmin TMRELs move 14 of 21 cluster members into the GBD 2019 UIs
(from 5) but shift overall levels +7%, leaving an unattributed positive
residual (candidates: IHME's draw-specific TMREL weights, their
whole-degree TMREL rounding vs our 0.1 C, their subnational aggregation).
Aaron's ruling: fix it on coherence grounds (a proven one-signed bias is
not retained because it cancels against unknowns); GBD 2019 is a
legitimacy check, NOT a calibration target (standing project framing).

**Defect 2: no daily temperature uncertainty (open; the active work).**
IHME's `era2melt.R` built 1,000 exposure draws as
`daily_temp + sd * N(0,1)` with sd = ERA5's published analysis-uncertainty
field (ERA5's 10-member ensemble-of-data-assimilations spread, 0.5 deg),
paired by index with RR and TMREL draws. Our CCKP/NEX-GDDP input has no
uncertainty field, so production has no such term (the latent path in
`03_load_temperature.R` was broken and is now guarded with a stop()).
Noise widens the daily distribution and raises burdens, most in
narrow-spread mid-TMREL countries. A tuned scalar (1.0 C) was tested and
rejected as a free dial; the decision must rest on the MEASURED field.

## 3. The plan (execution order)

### Step A. Get the measured ERA5 uncertainty field
Download ERA5 ensemble spread for 2m temperature (CDS API; credentials in
`~/.cdsapirc`, working; dataset `reanalysis-era5-single-levels`,
product_type `ensemble_spread`, variable `2m_temperature`, 3-hourly at
0.5 deg; the ARCO archive used for ERA5 means may not carry EDA spread, so
check ARCO first: `gs://gcp-public-data-arco-era5/...`, anon access, venv
`/var/home/aoz/data/venvs/arco/bin/python`, but plan on CDS). A single
reference year (or a few months across seasons) suffices to build a
per-pixel sd climatology; aggregate 3-hourly spread to a DAILY-MEAN
uncertainty (note: spread of the daily mean is smaller than mean 3-hourly
spread if errors decorrelate intra-day; IHME's exact aggregation is
unknown; compute both, report the choice). Regrid 0.5 -> 0.25 by nearest
or bilinear onto the CCKP grid. Save to
`/var/home/aoz/data/wb-temp-attr-projections/era5_sd/` with a small build
script committed to `output/review-ssp245/`.

### Step B. Use the PER-PIXEL sd field in the instrument tests
Aaron's explicit instruction: do not just measure the sd; RUN the replica
experiments with the spatially varying per-pixel sd. Concretely, extend
`analysis16_2x2_all204.R`: its noise convolution currently applies one
scalar sd per country at the (zone, t10) aggregate level. Replace with
per-pixel convolution: each pixel's daily values get its own Gaussian
kernel (sd from the Step-A field at that pixel) BEFORE truncation and
aggregation. Implementation hint that keeps it fast: round pixel sds to
0.1 C bins, aggregate exposure to (zone, sd-bin, t10), convolve each
(zone, sd-bin) slice with its kernel, then sum -- mathematically identical
to per-pixel and ~100x cheaper. Rerun the 2x2 as A, B, A+sd(x), B+sd(x)
(A = recycled pairing = production; B = per-draw argmin TMRELs). Compare
against `output/review-ssp245/pairing2x2_all204.csv` (the tuned-scalar
run). Report the same metrics (in-UI counts, median ratio, cluster
improvement, defectors) plus the pop-weighted distribution of the measured
sd itself, so the noise decision reads "measured sd is X, its effect is
Y" with no free parameter.

### Step C. Residual decomposition (replica, cheap)
Explain B's +7% residual mechanically as far as possible:
1. TMREL rounding: variant of B with TMRELs rounded to whole degrees
   (IHME's `pafCalc_sevFix.R:94` does this; we keep 0.1 C). If it moves
   the residual materially, the rounding choice becomes a documented
   decision (Aaron leans 0.1 C as more faithful to the definition).
2. Draw-specific TMREL weights: derive TMREL draw d using MORTALITY draw
   d's cause-death shares (we have 500 mortality draws per cause in
   `data/mortality/{loc}_mortality_ihme_{cause}_draws.rds`), replacing the
   fixed 2022 point weights used so far. This removes the last
   approximation we can remove; IHME's actual weights (GBD 2019 CoD
   draws) are not reconstructible.
Whatever residual remains is documented as external (their subnational
aggregation, round-internal differences), not chased further.

### Step D. Implement the pipeline fix behind a config flag
Per-draw TMREL derivation in the production pipeline: `util_derive_tmrel.R`
is a faithful reimplementation of `tmrelCalculator.R` (verified by the
2026-08-19 audit) but is unused; wire it in (config flag, e.g.
`TMREL_MODE = "released_recycled" | "derived_per_draw"`), with
draw-specific mortality weights per Step C.2, seeded and reproducible.
Keep the released-TMREL path intact for comparability.

### Step E. Validate the flagged pipeline locally
Run the REAL pipeline (not the replica) with the flag on for a validation
set (suggest: Haiti 114, Nicaragua 131, DRC 171, Indonesia 11, Colombia
125, China 6, Kuwait 145, plus 2-3 more cluster members), single model
(access-cm2-r1i1p1f1), 2022. Compare against replica-B predictions
(`pairing2x2_all204.csv` col B) and the GBD 2019 values
(`revision_all204.csv`). Also rerun the Colombia historical validation
suite to confirm nothing else moved. Note: local runs work (smoke-tested
2026-08-20: `util_run_cckp_burden.R --location_id=125 --years=2010
--force=TRUE` in draws mode, 82 s/combo); converted CCKP temperature
exists locally only for a few Colombia-area combos, so for other countries
first convert from the local NetCDFs
(`--cckp_local_root=/var/home/aoz/data/wb-temp-attr-projections/cckp-test`,
which holds access-cm2 + canesm5 ssp245 2022/2050 + GPW pop).

### Step F. Decision memo, then rerun
Write the decision memo for Aaron as an .org file (his format preference)
in `docs/reviews/`: pairing fix (decided, validated), noise
include-or-document verdict from Step B with the measured-sd evidence,
rounding choice from Step C, the residual statement, compute cost, and
updated Caspar instructions. After Aaron's sign-off: update the report
addendum + findings.org, redraft the Caspar rerun email (burden step only;
conversions reusable; `df -h /data` check; the summarizer rerun follows),
then the easier re-review, then scenario launch.

## 4. Decisions already made (do not relitigate)

- Per-draw TMREL fix: adopted in principle (Aaron, 2026-08-20).
- Goal framing: best predictions, NOT recreating GBD/Burkart; GBD
  comparisons are legitimacy checks only (Aaron, 2026-08-19).
- YLLs: keep UN WPP national projected life expectancies as primary; GBD
  reference-table version is a cheap retrofit if colleagues want it
  (pending Aaron's colleague conversations; non-blocking).
- ERA5 re-referencing of exposure (quantile mapping to ERA5): tested,
  mixed, NOT adopted.
- Samuel is away for months; decisions are Aaron + Claude. American
  spelling always. Docs Aaron reads: .org preferred.

## 5. Tooling and gotchas (hard-won; read before touching the replica)

- **Replica** = local reimplementation of the PAF stage used for
  experiments; validated to reproduce production (median 1.00). Core
  machinery in `output/review-ssp245/analysis13/14/16*.R`. PAF formula:
  signed per-draw contributions `pr*(rel-1)/rel` if rel>=1 else
  `pr*(rel-1)`; heat = bins above TMREL; burden = per-cause PAF x
  production cause deaths from `by_cause.csv` (access-cm2 2022 rows).
- **Pixel map**: `output/review-ssp245/pixel_loc_map.rds`, built by
  `build_pixel_map.R`. MUST be restricted to the 204 national polygons;
  the shapefile has 838 subnational polygons that otherwise overwrite
  national ids (the 2026-08-20 instrument bug; China shrank to 5 pixels).
- **ERF caches**: `data/erf/cache/erf_curves_draws_N500.rds` is LONG
  format (zone, daily_temp, acause, draw 0-499, rr on the RR scale);
  `erf_curves_summary.rds` is wide with draw_500-999 columns in LOG scale
  plus rr_mean. Zone grids can be ragged per cause (match temps per cause,
  do not slice arithmetically). Zone 28's grid ends at 32.1 C.
- **TMREL files**: `data/tmrel/tmrel_{loc}.csv` (tmrel_0..99, deg C, years
  1990/2010/2020 only) and `_summaries.csv` (tmrelMean).
- **Production reference values**: `~/data/wb-temp-attr-projections/review/
  ssp245/national_by_year.csv` and `by_cause.csv` (access-cm2 2022 rows);
  GBD 2019 comparisons in `output/review-ssp245/revision_all204.csv`.
- R runs via `distrobox enter emacs-r -- Rscript ...`; heavy jobs
  background. LaTeX: `TEXINPUTS=/var/home/aoz/Dropbox/OZ-Labs/templates//:
  xelatex` in the `bittensor-ubuntu` distrobox, run twice.
- ERA5 extracts live in `/var/home/aoz/data/era5_raw/{loc}_{year}_t2m.nc`
  (hourly, Kelvin, 17 countries for 2022); fetched via
  `output/review-ssp245/fetch_era5_batch.py` (ARCO store
  `ar/full_37-1h-0p25deg-chunk-1.zarr-v3`, anon).
- Audit report (full findings list with file:line): in the 2026-08-19
  session transcript; headline items are reproduced in the findings.org
  and the report addendum. The three audit bugs are FIXED (commit 3a2dbb5).

## 6. Open questions safe to park

- The +7% residual's external share (their subnational aggregation etc.).
- Sub-grid 29 island countries: still flagged, no direct ERA5 test.
- Zone-28 truncation: sensitivity band deferred until projection results
  are drafted (local 2-model computation suffices; no Caspar compute).
- GBD 2023-round curves: not published; a data request to IHME is the
  path if ever wanted as better inputs.