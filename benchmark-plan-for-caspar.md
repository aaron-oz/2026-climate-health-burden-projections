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

```bash
parallel -j <CORES> \
  'Rscript global-scripts/util_run_global.R \
      --location_id={} \
      --models=access-cm2-r1i1p1f1,bcc-csm2-mr-r1i1p1f1,gfdl-cm4-r1i1p1f1 \
      --scenarios=ssp245,ssp370,ssp585,ssp126 \
      --years=2030-2035' \
  ::: 305 131 163
```

That is 3 locations x 3 models x 4 scenarios x 6 years = 216 combos (a few will
be `missing-on-s3` for gfdl-cm4, as expected). 500 draws throughout (no
skimping).

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
      --target_scenarios=ssp370,ssp585,ssp126 --years=2030-2035' \
  ::: 305 131 163
```

---

## Step 5 — Send us the results to vet (do not start the full run yet)

```bash
{ echo "### cores"; nproc
  echo; echo "### load"; uptime
  echo; echo "### burden per-combo times"; tail -40 output/cckp_burden_manifest.csv
  echo; echo "### sentinels"; find output/results/cckp output/results/workflow_b -name _DONE -o -name _INCOMPLETE
  echo; echo "### one burden schema"; Rscript -e 'x<-readRDS(Sys.glob("output/results/cckp/163/*-ssp245/burden_2030.rds")[1]); cat(paste(names(x),collapse=", "),"\n"); cat("draws:", length(unique(x$draw)), " model/scen:", x$model[1], x$scenario[1], "\n")'
  echo; echo "### one workflow_b schema + ratio"; Rscript -e 'x<-readRDS(Sys.glob("output/results/workflow_b/163/*-ssp585/wb_2030.rds")[1]); cat(paste(names(x),collapse=", "),"\n"); cat("scale_factor range:", paste(round(range(x$scale_factor,na.rm=TRUE),4),collapse=".."),"\n")'
} > ~/benchmark_report_$(date +%Y%m%d_%H%M).txt
cat ~/benchmark_report_*.txt
```

We will check: load settled near `<CORES>`; per-combo time dropped materially vs
the ~8 min you were seeing; 500 draws present; `model`/`scenario` columns
stamped; Workflow B `scale_factor` is a sensible spread around 1 (not all 1, not
absurd); burden magnitudes plausible. From the size-varied per-combo times we
size-weight an honest full-run ETA and pick the final `-j`.

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
