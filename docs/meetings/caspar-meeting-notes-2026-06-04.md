# Caspar meeting notes — 2026-06-04

Working agenda for the touch-base with Caspar. He hasn't sent a status
update since the email went out, so we're entering blind on his progress.
Use these as prompts; reorder freely.

## Status questions (find out where he is)

1. **Has he started?** If yes, which step (1–9 from
   [`../ops/caspar-workflow.md`](../ops/caspar-workflow.md)) is he currently on? If not,
   what's blocking him from starting (data download timing, system access,
   competing priorities)?

2. **`renv::restore()` outcome.** Did it complete cleanly? Specifically did
   `sf` and `ncdf4` install without errors? Those two are the canaries — if
   they installed, the system libs (`libgdal`, `libgeos`, `libproj`,
   `libudunits2`, `libnetcdf`) are present and we're past the trickiest
   environment hurdle. If they failed, he's on Option B (apt/dnf install
   the missing libs, retry).

3. **Single-combo benchmark.** Has he run step 6? The wall-clock from his
   hardware is the single most useful data point we can get from this
   meeting — it anchors the production schedule for everything else.

## Decisions he needs to make

4. **Storage strategy.** Cache the full ~3 TB of CCKP NetCDFs locally for
   the production run, or stream-and-delete (download → convert → delete
   the NetCDF, lower disk peak but no caching across reruns)? We have soft
   defaults but he picks.

5. **NUMA pinning.** Given the dual-socket workstation, does he plan to
   pin workers to sockets via `numactl --cpunodebind=X --membind=X`? Worth
   ~30–50% wall-clock if applied. No code changes needed from us — pure
   ops on his side.

6. **Singularity / Apptainer container (Option C).** Does he want one?
   ~half a day of work on our side. Only worth landing if he's worried
   Options A and B (renv-only + system-libs-only) won't hold up. Default
   answer is no — most CMIP6-running workstations already have the system
   libs and renv handles the R side.

## Things to flag proactively (he probably doesn't know yet)

7. **Graceful 404 handling landed.** The production run won't crash on the
   known (model, scenario) gaps in CCKP's archive:
   - `gfdl-cm4-r1i1p1f1` has no ssp126 or ssp370
   - `hadgem3-gc31-mm-r1i1p1f3` has no ssp245 or ssp370
   - `nesm3-r1i1p1f1` has no ssp370
   - `taiesm1-r1i1p1f1` has no ssp245

   Missing combos get logged to the manifest as `missing-on-s3` and the
   run continues. He doesn't need to filter or special-case anything.

8. **Per-draw burden is canonical now.** `burden_<LOC>.rds` carries a
   `draw` column whenever both inputs have draws. One file per combo
   instead of two. Summaries can be derived from draws; reverse isn't true.

9. **`MODELS_ALL` is 29 models** (not 34 as I'd been quoting earlier —
   I was over-counting by including scenario suffixes). If CCKP's daily
   archive has more models than what we extracted from the download
   script, he should let us know which to add.

10. **`IHME_CAUSE_FILES`** — the batch converter uses substring matching
    against expected filenames. If his actual downloaded filenames don't
    contain the disease name as a substring (e.g., something like
    `GBD_RESULTS_001.csv`), he'd need to either rename or edit the
    `IHME_CAUSE_FILES` map in `config.R`. Worth a 30-second sanity check
    before he runs the batch converter (the converter prints a file
    discovery table up front, so any mismatches surface visibly).

## Things to listen for from him

- **`CAUSES_IHME_TEMP_SCALED` framing.** We split IHME's "transport-related
  injuries" umbrella into both `inj_trans_road` and `inj_trans_other` and
  treat both as temp-scaled. If he has different framing or knows IHME
  differentiates these, worth knowing.
- **Timeline concerns.** Our estimate is ~3–4 days wall-clock for all 4
  SSPs once the production run starts. Does he have a deadline that
  pressures this?
- **Anything weird about the CCKP daily files we missed.** Calendar
  variants, unusual fill-value conventions, anything about the bias-
  correction layer that differs from what we documented in
  `../ops/pipeline-state-for-cckp.md` §3.3.

## On the pixel/country trace tool

`util_trace_pipeline.R` exists and is tested. Mostly **Caspar should be
the one to run it** on his production output once a single combo is
through. Reasons:

- The trace's job is to verify the pipeline on the data it actually ran
  on, against the hardware it actually ran on. His output is the relevant
  artifact.
- Anything we trace on our side now uses either (a) Samuel's historical
  Colombia data — already validated at 18/19 PASS, nothing new to learn —
  or (b) the limited cvd_ihd IHME data we have, which is one cause out
  of 17. We can't trace cause math we haven't run.
- We'd have value in pre-validating the workflow B ratio applicator on
  something other than smoke-test data, but that needs real CCKP
  future-year temp + multi-cause IHME mortality, both of which Caspar
  will produce as part of his first scenario run.

**One thing we could do now**: run the trace on his preliminary Brazil drop
(the 101-day Apr-Jul 2012 ACCESS-CM2 file from 2026-05-28). That'd give
us a representative-shape trace report we could show him as an example
output. ~30 min of work; not urgent.

## Coming out of the meeting

After he reports status, the typical next steps are:

- **If he hit no issues:** wait for him to finish the SSP2-RCP4.5 reference
  run, spot-check the output, green-light the other three SSPs.
- **If he hit setup issues:** triage whether it's a known fix (probably
  Option B systemd packages) or something we need to dig into. Container
  (Option C) becomes more likely.
- **If he hit pipeline bugs:** he sends us the failing run's log, we
  reproduce and fix.

Nothing on our side is urgent or blocked beyond his progress.
