# Handoff: options for running the pipeline retrospectively (historical burden)

**Status as of 2026-07-31: parked, not started.** Charlie raised in a meeting
that the 2022--2050 projections would be more useful with a historical burden
series to compare against. The decision was to wait and see how the forward
results look before committing to this. These notes exist so whoever picks it
up does not have to re-derive the situation.

Nothing in this document has been built. It is an assessment of what is
already on disk, what is genuinely missing, and three routes with different
costs.

## The short version

"We can't run retrospectively without mortality draws" is too strong. The
pipeline already has a working no-mortality-draws path. What is missing is not
the ability to run, it is the ability to put a full uncertainty interval on
the historical burden.

## What is already in place for historical years

Verified on disk 2026-07-31:

- **Life tables**: `data/lifetables/` has 199 files, each covering 1990--2100
  at 17 age groups. Checked file by file, all 199 have the same 111-year span.
  The projection-year life-table gap recorded in the production-run blockers
  does not apply going backwards.
- **TMREL draws**: `data/tmrel/` has 1,034 locations x 100 draws at years
  1990, 2010, and 2020 (verified: all 1,034 draw files carry exactly those
  three years, nothing in between). Because 1990 and 2010 are real anchors,
  `02_load_tmrel.R`'s nearest-year fill is interpolating across the historical
  period rather than extrapolating.
- **ERF curves**: `data/erf/`, 1,000 draws, no year dimension at all. They
  apply unchanged to any year.
- **Temperature**: Caspar has historical model data, bias-aligned on average
  to ERA5 but retaining model-to-model variation. See the open question in the
  last section about how that variation should enter.

So mortality is the only input that is not already historical-capable.

## What is actually missing, and the two gaps are very different

**Gap 1: global historical mortality at summary level. This is a download, not
an IHME ask.** All three GBD CSVs in `data/mortality/` are Colombia only
(one location, the 17 causes split across the three files, 17 age groups,
1990--2023, `val`/`upper`/`lower`). The public GBD Results Tool serves the same
thing for every location. `global-scripts/00_download_gbd.R` already has the
17-cause `CAUSE_MAP` with GBD cause IDs and was written for exactly this.

**Gap 2: draw-level historical mortality. This needs an IHME request.** GBD
does not publish draws publicly. Note that our documented ask to IHME was
2022--2050 both times it was written down, in `gbd/ihme-plan-b-prep.tex`
(lines 116-117) and in the draft letter in `ihme-unscaling-validity-report.md`
(line 310). There is no record in this repo of ever asking for historical
draws, so assume a fresh ask is needed. (If it was raised by email, that is not
visible from the repo.)

## Three routes, cheapest first

### Route A: summary mortality, point-estimate burden. Works today, no new code.

`05_compute_pafs.R:394` checks `"draw" %in% names(mort)` and takes a summary
path when the column is absent. With summary mortality you get
`burden = deaths x paf_mean`, which completes cleanly. Cost is one GBD Results
Tool download. You get no uncertainty interval on the historical burden.

### Route B: burden draws from PAF draws alone. Small code change.

PAF draws exist independently of mortality draws, so `burden_d = deaths x paf_d`
is computable by broadcasting scalar deaths across the PAF draws. The gate at
`05_compute_pafs.R:466` currently blocks this because it also requires
`!is.null(mort_full)`. Relaxing that gate gives a historical interval
reflecting ERF and TMREL uncertainty (and temperature uncertainty if enabled),
missing only mortality-count uncertainty.

How much that omission costs: for Colombia, GBD's own mortality intervals have
a median relative width of 0.26, computed as `(upper-lower)/val` across 8,636
rows in `IHME-GBD_2023_DATA-a0955ea2-1.csv`, with p10 = 0.16 and p90 = 0.42.
That is not negligible. **This has not been compared against the PAF interval
widths from an existing run**, so which source dominates is currently unknown.
That comparison is cheap and should be the first thing done if Route B is
being considered seriously, because it determines whether Route B is good
enough or whether Route C is required.

### Route C: ask IHME for historical draws. Slowest, most complete.

Only route that gives a historical interval directly comparable to the
2022--2050 runs. See the comparability caveat below for why that matters.

## Comparability caveat, and it bites the exact use case Charlie described

The 2022--2050 runs use IHME's 500 mortality draws, so their intervals include
mortality-count uncertainty. A historical run built via Route A or B would have
intervals that are narrower **by construction**, not because the past is better
known. Putting the two side by side without saying so would misrepresent the
comparison.

Two ways to handle it if Route C is not taken:

1. State the asymmetry explicitly wherever historical and future appear
   together.
2. Deliberately drop mortality draws from the future run when building the
   comparison figure, so both sides carry only ERF/TMREL uncertainty. Less
   informative but honest.

## A comparator that needs no run at all

If what is wanted is mainly a historical anchor rather than our own pipeline's
retrospective estimate, two things are already on disk:

- `data/benchmarks/burkart2021_table1.csv`: GBD's published attributable
  deaths for non-optimal temperature, 1990 and 2019, nine countries, with
  uncertainty intervals.
- `../2026-stressor-resilience-framework/data/gbd-risk-high-temp/`: a GBD
  high-temperature risk extract.

These come from GBD's method and GBD's temperature inputs, not ours, so they
answer a different question than a retrospective run of our pipeline would.
Worth being clear about which question is being asked before choosing.

## Open question: how Caspar's model-to-model spread should enter

The historical model data carries model-to-model variation. The pipeline only
opens a temperature draw dimension when `temp_sd` is present in the input
(see `03_load_temperature.R` and the `USE_DRAWS` handling in
`05_compute_pafs.R`). Three options, none decided:

- Fold the spread into `temp_sd` and let it propagate as temperature draws.
- Run each model separately and treat the spread across runs as a separate
  axis, which is what the forward CCKP runs already do.
- Ignore it and use the ensemble mean.

This changes what the historical interval *means*, so it should be settled
before generating anything, not after.

## Related

- `pipeline-state-for-cckp.md` for the forward-run state.
- `handoff-cckp-run-failures-2026-07-31.md` for the current forward-run
  failures, which are unrelated to this but worth reading first if you are
  assessing "how the results look."
