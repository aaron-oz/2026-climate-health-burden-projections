# Handoff: triage of Caspar's CCKP run failures (logs received 2026-07-31)

Source: `failure-logs.tar.gz` from Caspar, 84 `run_2022.log` files under
`output/results/cckp/{110,349,413}/{model-scenario}/`. Runs executed on the
HPC at `/data/HEAT/Burkart/FHS/2026-climate-health-burden-projections/`, which
may be ahead of this repo (see the caveat on `setDT(dat)` below). All logs are
single-year, 2022, `use_draws=TRUE`, `n_draws=500`.

Locations, identified from `data/gbd-comparison/gbd_F_denominator_2019_2022.csv`:
110 = Dominica, 349 = Greenland, 413 = Tokelau.

## Summary

| Location | Logs | Completed | Failed | Cause |
|---|---|---|---|---|
| 110 Dominica | 30 | 16 | 14 | temperature input has 0 rows |
| 349 Greenland | 27 | 27 | 0 | no error in any log, see below |
| 413 Tokelau | 27 | 0 | 27 | no life table on disk |

Two genuinely distinct bugs, plus one open question.

## Bug 1: empty temperature input crashes with an unrelated error message

For location 110, the failure is perfectly correlated with the temperature
input being empty. Every one of the 14 failing logs contains:

```
Loaded 0 pixel-day rows
Temperature data saved: 0 rows
```

and every one of the 16 successful logs contains 360 or 365 pixel-day rows.
Dominica is a single-pixel location, so 365 pixel-days is one pixel over a
365-day calendar and 360 is one pixel over a 360-day-calendar model.

`03_load_temperature.R` accepts the empty input, writes a zero-row file, and
exits successfully. The run then proceeds through mortality loading and all 17
PAF cause-chunks before dying in `05_compute_pafs.R` with:

```
All elements in argument 'x' to 'setDT' must be of equal length,
but input 6 has length 0 whereas the first non-empty input had length 1
```

That message says nothing about temperature, and it arrives roughly 40 seconds
of compute after the actual problem. Anyone debugging from the tail of the log
will look in the wrong place.

**Caveat on locating the fix:** `setDT(dat)` does not appear anywhere in this
repo's R sources. The HPC checkout is either ahead of this branch or sources a
helper that is not committed here. Confirm which version Caspar is running
before patching.

**Suggested fix:** make `03_load_temperature.R` hard-error on zero rows, the
same way `07_compute_ylls.R` already hard-errors on a missing life table, with
a message naming the temperature file. Commit e0c3fd6 ("guard empty
conversions") addressed the conversion side; this is the consumption side.

**Still unexplained:** why the CCKP conversion yields 0 rows for 14 of 30
model-scenario combinations at the same location. The failing set is
alphabetically contiguous (`access-cm2` through `gfdl-cm4`, with `gfdl-esm4`
onward all succeeding), which may only reflect the order in which retries were
issued. Failing logs are dated 2026-07-21, 07-22, 07-23, 07-29 and 07-31 while
successes are dated 07-28 and 07-29, so this is not a single bad code version.
Combos retried on 07-31 still produced 0 rows, so the condition looks
persistent per combo rather than transient. **From the logs alone it cannot be
determined whether the 16 currently-successful combos would still succeed if
rerun today**, since each combo has only its most recent log. Worth rerunning
one known-good combo to check before drawing conclusions.

Single-pixel locations are the suspect class. Tokelau is also single-pixel and
would plausibly hit the same thing once its life table exists.

## Bug 2: missing life tables are discovered mid-run instead of pre-flight

All 27 Tokelau runs failed with:

```
ERROR in 07_compute_ylls.R: No life table found for location 413
```

Tokelau's temperature is fine (360 or 365 pixel-days, same as Dominica). The
only problem is that `data/lifetables/413_lifetable.csv` does not exist. The
directory holds 199 files and the location IDs jump from 396 to 422.

The hard error in `07_compute_ylls.R` is deliberate and correct, and its
message says so. The problem is upstream:
`global-scripts/util_download_un_lifetables.R` has two non-fatal skip paths.
Line 130-133 emits a warning when a requested `loc_id` has no UN crosswalk
entry, and line 155-158 prints "No data returned, skipping" when the UN API
returns nothing. Neither aborts. A location silently absent from the download
is only discovered when a full pipeline run reaches step 07, after the
expensive PAF computation has already been done.

Tokelau's population is small enough that a missing UN WPP series is a
plausible explanation, but **that is a hypothesis, not verified**. Check which
of the two skip paths fired.

**Suggested fix:** a pre-flight check in the runner, before any location is
dispatched. Against whatever location list the global run uses:

```bash
for l in $(cat locations.txt); do
  [ -f "data/lifetables/${l}_lifetable.csv" ] || echo "MISSING lifetable: $l"
done
```

This could not be run here because `output/cckp_run_manifest.csv` is keyed by
model, not location, so the run's location universe is not available in this
repo. It lives on the HPC.

For locations where UN WPP genuinely has no life table, a decision is needed:
borrow a regional or neighbouring-country life table, or exclude the location
and record the exclusion. Either is defensible, but it should be an explicit
recorded choice rather than a run that dies.

## Open question: why is Greenland in the failure bundle?

All 27 Greenland logs report `Pipeline complete`, no errors, roughly 3.88M
pixel-days (10,627 pixels over 365 days; 3,825,720 over 360 for the
360-day-calendar models). Nothing in the logs marks them as failed.

Possibilities not distinguishable from the logs: the runner's status file
marks them failed for a reason not written to the log, they were bundled for
comparison, or an output collision occurred. Note that the Dominica and
Greenland logs from 07-29 write to `output/results/burden_{loc}.rds` while the
Tokelau logs from 07-30 write to `output/results/_staging/{loc}/{combo}/`, so
staging was introduced between those dates. Before staging existed, concurrent
model-scenario combos at one location would overwrite each other's output at
the same path. That is a plausible reason a "complete" run would still be
recorded as bad, but it is speculation.

**Ask Caspar** what marked the Greenland runs as failures.

## Suggested order of work

1. Ask Caspar about Greenland, since it may be a third bug or a non-issue.
2. Pre-flight life-table check across the full location list. Cheap, and
   likely catches more locations than Tokelau alone.
3. Make `03_load_temperature.R` fail loudly on zero rows.
4. Then investigate the zero-row CCKP conversions for single-pixel locations,
   which is the only one of the four requiring real diagnosis.
