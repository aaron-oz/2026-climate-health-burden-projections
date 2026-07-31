# Published benchmarks

Values published by others, transcribed for comparison against our own output.
Nothing here is computed by this pipeline, and nothing here should ever be
overwritten by it.

## burkart2021_table1.csv

Burkart et al., "Estimating the cause-specific relative risks of non-optimal
temperature and related deaths", *The Lancet* 398 (2021), Table 1, page 690.
Transcribed 2026-07-30 from `from-samuel/burkart-paper.txt`, a verified text
extraction of the paper held in this repo.

Attributable deaths, population attributable fractions (PAF) and attributable
rates per 100,000, for high temperature, low temperature and non-optimal
temperature, in 1990 and 2019, for the nine countries in the study.

Columns: `location_id` (GBD, verified against
`data/shapefiles/GBD2023_mapping_final.dbf`), `location_name`, `year`, `risk`
(`high` / `low` / `nonopt`), `deaths` with `deaths_lower` / `deaths_upper` (95%
uncertainty interval), `paf_pct` with bounds, `rate_per_100k` with bounds.

All nine countries are in our 204: Brazil (135), Chile (98), China (6),
Colombia (125), Guatemala (128), Mexico (130), New Zealand (72), South Africa
(196), United States (102).

Transcription check: `paf_pct` for high plus low reconciles to `nonopt` within
0.02 percentage points across all 18 country-years, which is rounding. The
death counts do not sum exactly (up to 0.5% for China), which is expected:
the published non-optimal figure is estimated jointly rather than as the sum of
its two parts, and the table is rounded.

**Comparability caveats, which matter before anyone reads a difference as an
error.** These are 2019 and 1990 estimates built on observed ERA5 temperature
and GBD 2019 mortality. Our projections start in 2022, use CMIP6 model
temperature with no baseline shift to ERA5 (IHME applies one; see
`gbd/gbd-appendix.txt` Section 2.1.6.7), and use IHME forecast mortality. The
PAF here is a share of all-cause deaths, whereas a PAF derived from our summary
output has the 17 temperature-related causes as its denominator. Treat this as
a check on magnitude, on the heat-to-cold balance, and on the ordering across
countries, not as a target to match to the digit.
