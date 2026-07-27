# Benchmark + relaunch plan (thread-oversubscription fix)

**Purpose.** The current run is slow because the machine is CPU-thread
oversubscribed, not I/O- or memory-bound. A snapshot showed **load average ~660
with RAM idle** (55 of 503 GB used, swap untouched) and only ~44 R processes:
each process's `data.table` was grabbing ~half the machine's cores, so ~20
concurrent locations requested hundreds of threads and the machine spent its
cycles context-switching instead of computing.

The fix is merged to `main`: cap `data.table` to 1 thread per process
(`DT_THREADS`, default 1) and set `-j` to the physical core count. This plan (a)
proves the fix and measures real timing on a small benchmark, then (b) after we
vet the outputs together, relaunches the whole job with the optimal settings.

Do **not** start the full run until we've reviewed the benchmark outputs
together. All commands run from the repo root.

---

## Step 0 — Pull the fix and tell us your core count

```bash
git pull origin main            # should include commit c0df922 or later
nproc                           # physical/logical cores -- send us this number
```

Everything downstream sets `-j` from `nproc`. Below, substitute your core count
for `<CORES>` (start with physical cores; if `nproc` reports logical/hyperthreaded
cores, use about half).

---

## Step 1 — Stop the current jobs and quiesce the machine

```bash
# If launched under GNU parallel / a shell, Ctrl-C that terminal first, then:
pkill -f util_run_global.R
pkill -f run_location.R
pkill -f util_run_cckp
sleep 5
pgrep -af Rscript | head            # expect empty (or only your shell)
uptime                              # load average should start falling toward ~0
```

Wait until `uptime` shows the load dropping and `pgrep Rscript` is essentially
empty before continuing. Nothing already computed is lost: partial outputs on
disk are valid and will be reused.

---

## Step 2 — Set the environment for every launch

```bash
export CCKP_LOCAL_ROOT=/data        # your local CMIP6/pop mirror (already working)
export CCKP_REQUIRE_LOCAL=TRUE       # NEW: hard-fail if a file isn't local, instead
                                     #      of silently downloading from S3
export DT_THREADS=1                  # NEW: 1 data.table thread per process
export OMP_NUM_THREADS=1             # NEW: 1 BLAS thread per process
export OPENBLAS_NUM_THREADS=1        # NEW
```

Put these in the shell you launch from (or `.bashrc`). At the start of each
convert phase you should now see a **preflight** line like
`Preflight: N/N temperature combos resolvable from local mirror ...` -- if it
says any "would download", the mirror path/layout is off and it will stop rather
than download.

---

## Step 3 (optional but recommended) — Prove the fix causally

Run one identical tiny slice twice on the now-idle machine and watch `uptime` in
a second terminal. This isolates the thread fix from everything else.

```bash
# A) UNCAPPED (worst case): DT_THREADS=0 means "all cores per process" -- the
#    oversubscription we're fixing. Several small locations run at once so the
#    effect is visible.
DT_THREADS=0 \
parallel -j <CORES> \
  'Rscript global-scripts/util_run_global.R --location_id={} --models=access-cm2-r1i1p1f1,bcc-csm2-mr-r1i1p1f1 --scenarios=ssp245 --years=2030-2034' \
  ::: 106 116 117 107
# watch `uptime` in another terminal -- expect load to spike well above <CORES>

# (let it finish, then)

# B) WITH the cap (DT_THREADS=1 already exported): same-size slice, different
#    small locations so nothing is skipped.
parallel -j <CORES> \
  'Rscript global-scripts/util_run_global.R --location_id={} --models=access-cm2-r1i1p1f1,bcc-csm2-mr-r1i1p1f1 --scenarios=ssp245 --years=2030-2034' \
  ::: 105 112 115 119
# expect load to settle near <CORES>, and B to finish faster in wall-clock
```

All eight are tiny island nations (few pixels), distinct from each other and
from the Step 4 benchmark locations, so both arms do equal fresh work.

Send us the wall-clock of each and the peak `uptime` load for each. If B is
clearly faster with load near `<CORES>`, the fix is confirmed and we proceed.

---

## Step 4 — The measured benchmark (fresh, size-varied locations)

These locations are **not** in your started set, so nothing is skipped and the
timing is clean. They span the size range (burden time scales with pixel count):

| role | loc_id | country | approx size |
|------|--------|---------|-------------|
| small  | 305 | Bermuda   | tiny |
| medium | 131 | Nicaragua | medium |
| large  | 163 | India     | very large |

We include a **scenario-gap model** (`gfdl-cm4-r1i1p1f1`, which lacks some SSPs
on CCKP) to exercise the `missing-on-s3` path, alongside two normal models, and
**all 4 scenarios** so the Workflow B ratio runs on real cross-scenario data for
the first time.

**Years span the horizon on purpose.** Near-term scenarios (2030) barely differ,
so early years can't tell us whether warming actually raises heat-attributable
burden -- that check is only meaningful once the scenarios diverge. So we run a
few near years (stable per-combo timing) **and** the far end (2049-2050), where
the warming signal is testable.

```bash
parallel -j <CORES> \
  'Rscript global-scripts/util_run_global.R \
      --location_id={} \
      --models=access-cm2-r1i1p1f1,bcc-csm2-mr-r1i1p1f1,gfdl-cm4-r1i1p1f1 \
      --scenarios=ssp245,ssp370,ssp585,ssp126 \
      --years=2030,2031,2040,2049,2050' \
  ::: 305 131 163
```

That is 3 locations x 3 models x 4 scenarios x 5 years = 180 combos (a few will
be `missing-on-s3` for gfdl-cm4, as expected). 500 draws throughout (no
skimping). Note the large location (163, India) runs its combos serially, so it
is the pacing item -- its per-combo time is the number that governs the full-run
ETA (see the timing note at the end).

### Monitor while it runs

```bash
# per-location live status (updates every combo):
cat output/results/cckp/163/_progress.tsv        # phase, combos_done/total, pct
# real per-combo seconds (this is the number that sets the full-run ETA):
tail output/cckp_burden_manifest.csv             # elapsed_s column
# machine health:
uptime                                           # load should be ~<CORES>, not 660
# completion:
find output/results/cckp -name _DONE             # a location's sentinel = fully done
```

### When burden finishes, run Workflow B for the same locations

```bash
parallel -j <CORES> \
  'Rscript global-scripts/util_apply_workflow_b_batch.R \
      --location_id={} --ref_scenario=ssp245 \
      --target_scenarios=ssp370,ssp585,ssp126 --years=2030,2031,2040,2049,2050' \
  ::: 305 131 163
```

---

## Step 5 — Vet the NUMBERS, not just the files (do not start the full run yet)

This is the real gate. "It ran" is not "it's right."

### 5a. Automated checks

```bash
Rscript global-scripts/util_vet_benchmark.R --locations=305,131,163
```

- **Tier 1 (hard, must all PASS -- a FAIL is a bug):** `paf_nonopt == paf_heat +
  paf_cold`; `deaths_nonopt == heat + cold`; `|paf_nonopt| <= 1` (attributable
  never exceeds total); no NA/Inf; total deaths >= 0; draws present and each mean
  inside its own 95% interval.
- **Tier 2 (directional, for our + Samuel's judgment):** does heat-attributable
  PAF **rise** and cold **fall** as the scenario warms (ssp126<245<370<585)? --
  reported by year, so you see it emerge in 2049-2050 (2030 will look like a
  coin-flip, and that's expected). Plus ssp585-ssp245 divergence by year (should
  grow), heat/cold composition by location (tropical -> heat-leaning), top-3
  causes, and a PAF magnitude summary (believable is single-to-low-double-digit
  %; Colombia validation anchor ~4% for cvd_ihd).

### 5b. One per-pixel trace on a new location (the Colombia hand-check, redone)

```bash
Rscript global-scripts/util_trace_pipeline.R \
  --location_id=163 --model=access-cm2-r1i1p1f1 --scenario=ssp585 \
  --year=2050 --causes=cvd_ihd
```
Confirms zone -> TMREL -> RR-rescale -> PAF arithmetic is correct on a location we
haven't hand-verified (India), the same way we validated Colombia. (Needs 163's
per-combo intermediate present; run it on the machine that produced the
benchmark. `--model`/`--scenario` point it at the CCKP combo output.)

### 5c. Send us the bundle

```bash
{ echo "### cores/load"; nproc; uptime
  echo; echo "### burden per-combo times (India = pacing item)"; tail -60 output/cckp_burden_manifest.csv
  echo; echo "### convert per-combo times"; tail -30 output/cckp_run_manifest.csv
  echo; echo "### sentinels"; find output/results/cckp output/results/workflow_b -name _DONE -o -name _INCOMPLETE
  echo; echo "### VETTING"; Rscript global-scripts/util_vet_benchmark.R --locations=305,131,163
} > ~/benchmark_report_$(date +%Y%m%d_%H%M).txt
cat ~/benchmark_report_*.txt
```

We (and ideally Samuel for the epi read) will confirm: Tier 1 all PASS; Tier 2
directions right at the far horizon; magnitudes plausible; load settled near
`<CORES>`; per-combo time dropped materially vs the ~8 min under thrash. From the
size-varied per-combo times we size-weight an honest full-run ETA and pick `-j`.

---

## Step 6 — (We do this together, after vetting) Backfill + full relaunch

Once the benchmark checks out, before the full run we backfill the two new
columns onto your existing partial outputs (so the whole dataset is uniform
without recomputing what's already done):

```bash
Rscript global-scripts/util_backfill_scenario_columns.R --dry_run=TRUE   # preview
Rscript global-scripts/util_backfill_scenario_columns.R                  # apply
```

Then the full run, idempotent (it resumes, skipping finished combos), with the
chosen `-j` and all four scenarios by default:

```bash
parallel -j <CORES> \
  'Rscript global-scripts/util_run_global.R --location_id={} --years=2022-2050' \
  ::: $(Rscript -e 'cat(readRDS("output/intermediate/ihme_loc_map.rds")$loc_id)')
```

Progress at any time: `find output/results/cckp -name _DONE | wc -l` (locations
fully done) and `find output/results/cckp -name _INCOMPLETE` (needing attention).

---

## After the call — Caspar, solo, over the next few hours

The benchmark keeps running after we hang up. Steps 1-4 are yours; step 5 is a
hard stop.

1. **Let all three benchmark locations finish burden.** Check every so often:
   ```bash
   find output/results/cckp -name _DONE            # expect 305, then 131, then 163
   find output/results/cckp -name _INCOMPLETE      # any of these = failures, see below
   uptime                                          # load should stay ~<CORES>
   cat output/results/cckp/163/_progress.tsv       # India: watch it go convert -> burden
   ```
   - India (163) sits in `phase convert` a while before `phase burden` -- that's
     expected (it converts all its combos first).
   - If load creeps back toward 660, the thread cap isn't in effect: confirm
     `echo $DT_THREADS $OMP_NUM_THREADS` are set in the shell you launched from.
   - If an `_INCOMPLETE` appears, grab the tail of that location's newest log:
     ```bash
     tail -40 "$(find output/results/cckp/<loc> -name 'run_*.log' -printf '%T+ %p\n' | sort | tail -1 | cut -d' ' -f2)"
     ```
     and send it to us; don't try to fix it blind.

2. **When 305, 131, 163 all show burden `_DONE`, run Workflow B for all three**
   (the Step 4 Workflow B command, `::: 305 131 163`).

3. **Run the full vetting** (Step 5a on all three) **and the India per-pixel
   trace** (Step 5b).

4. **Send us the report bundle** (Step 5c) -- to Aaron, and cc Samuel for the
   epidemiological read on Tier 2 magnitudes and heat/cold composition.

5. **STOP. Do not start the full run.** Wait for our sign-off. We need to confirm
   Tier 1 all PASS, Tier 2 directions sane at 2049-2050, magnitudes plausible,
   and that India's per-combo time gives an acceptable full-run ETA. This is the
   whole point of the benchmark; launching the 200-location run before it clears
   risks days of compute producing wrong or unusable numbers.

## After we sign off (together)

6. **Backfill** the model/scenario columns onto your existing partial outputs
   (Step 6 backfill commands), so the full dataset ends up uniform.

7. **Launch the full run.** The exact form depends on India's measured per-combo
   time:
   - **India was fast:** the simple per-location fan-out (Step 6 launch command).
   - **India was slow (minutes/combo):** we'll hand you a big-country-split
     launch (each large country as several processes split by scenario) so no
     single serial chain dominates. We decide this from the benchmark number, not
     now.

8. **Monitor the full run:**
   ```bash
   find output/results/cckp -name _DONE | wc -l    # locations fully done
   find output/results/cckp -name _INCOMPLETE      # needing attention
   uptime                                          # load ~<CORES>
   ```

---

## Notes

- Nothing here reduces draws; all runs are the full 500.
- The old downloaded NetCDFs in `data/cmip6-scratch/cckp` are harmless leftovers
  (same data as the mirror). If you want them gone, `rm -rf data/cmip6-scratch/cckp`
  is safe: already-converted temperature RDS are skipped regardless, and new
  combos will symlink from `/data`.
- If the preflight or a run stops with a `CCKP_REQUIRE_LOCAL` error, that is the
  guardrail working: it means a NetCDF isn't where the mirror layout expects it
  (`/data/cmip6-daily-x0.25/tas/<model>-<scenario>/...`). Fix the path rather
  than unsetting the guard.

---

## Timing: what governs the full-run ETA

One number decides whether the full run is days or weeks: **the largest
country's per-combo time.** The pipeline runs one process per location and, within
a location, combos run **serially**. So the full run's wall-clock has a hard floor:

```
floor  >=  (combos per location, ~3,300)  x  (per-combo time of the biggest country)
```

`-j` parallelizes *across* locations but cannot speed up that one serial chain.
That is why the benchmark includes India (163) and why its per-combo time is the
pacing item we wait for.

- If the large per-combo time is small (seconds), the full run is days -> launch
  as-is.
- If it is minutes, the big countries (China, India, US, Canada, Brazil, Russia,
  DRC) dominate, and the fix is **intra-location parallelism**: launch each big
  country as several processes split by scenario (and/or year range), e.g.
  `--scenarios=ssp245` in one process and `--scenarios=ssp370` in another. The
  runner already supports this; we decide once we see the number.

Expected arrival of signals (paced by India): fix-works (load ~`<CORES>`) within
minutes; small/medium timing + a Workflow B check within the hour; India's
convert-then-burden per-combo times over ~1-3 hours. Full-confidence launch comes
after India's number lands **and** Step 5 vetting passes -- realistically later
the same day, not during the first call.
