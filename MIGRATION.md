# Migrating to the fixed pipeline

Five steps, in order. Steps 1 to 4 are done once; step 5 is how you check on it
from then on. Everything after step 5 is reference material for when something
looks odd, not part of the migration.

Every command below assumes you are in the project directory. Set that first
and stay there:

```bash
cd /data/HEAT/Burkart/FHS/2026-climate-health-burden-projections   # adjust to yours
```

---

## Step 1. Stop the run that is going now

```bash
# Ctrl-C the running job, or:
pkill -f util_run_global.R
```

Stop it rather than waiting for it to finish. The run in flight cannot produce
a complete result: it is working from a model list filtered per scenario to
dodge a bug that is now fixed, so 15 or more models per scenario are missing by
construction. Letting it finish only delays the restart.

Nothing computed so far is lost or has to be redone:

- **Converted temperature files stay valid.** The conversion was rewritten for
  speed, but its output is byte-identical, verified against the old version on
  six locations. Existing files are recognised and skipped.
- **Burden outputs stay valid.** They are incomplete, not wrong. Each file
  holds the correct numbers for the model and scenario it names.
- **Old progress markers are harmless.** They get rewritten on the next run.

Killing a run can leave a file half-written. Under the old code such a file
looked finished to the resume check and would never have been recomputed. Step
4 clears any of those automatically, so there is nothing for you to do about
it, and new writes are atomic and cannot be left half-finished at all.

## Step 2. Update the code

```bash
git pull
```

## Step 3. Set up run_env.sh

```bash
cp run_env.sh.example run_env.sh
```

Then open `run_env.sh` and check **one** setting, the first one in the file:

```bash
export CCKP_LOCAL_ROOT=/data
```

It must point at the directory that holds `cmip6-daily-x0.25/` and
`pop-x0.25/`. If the value shipped in the example is already correct for this
machine, you are done: change nothing else. Every other setting has a working
default and is documented in place in that file.

`run_env.sh` is gitignored, so it survives future `git pull`s and will never
cause a merge conflict.

## Step 4. Launch

```bash
./run_production.sh
```

This checks the machine first and refuses to launch if anything is wrong, so it
is the only command needed to go from a fresh pull to a running job. What it
prints before launching is described under "What you should see" below.

This is also the resume command. Re-run it any time, after a crash, a Ctrl-C,
or a later `git pull`, and it skips everything already computed and fills only
what is missing. There is no separate resume step and nothing to clean up
first.

## Step 5. Watch it

```bash
./status.sh
```

Read-only and safe to run as often as you like while the run is going. The
`./` matters: these are scripts sitting in the project directory, not commands
on your PATH. To check from some other directory, give the full path, which
works from anywhere:

```bash
/data/HEAT/Burkart/FHS/2026-climate-health-burden-projections/status.sh
```

---

# Reference

## What you should see

`./run_production.sh` prints a block of PASS lines before it launches anything:

```
[PASS] renv library is active
[PASS] pinned packages load                             data.table 1.18.4
[PASS] runs from any working directory
[PASS] shapefile resolves and covers all locations      204 level-3 | 6/6 micro-nations
[PASS] CCKP local mirror is readable                    /data (29 model-scenario dirs)
[PASS] grid completes past an absent model x scenario   6/6 combos recorded
[PASS] per-combo status files and progress are written
[PASS] parallel grid matches serial grid                4 workers: identical statuses
[PASS] concurrent combos get private scratch dirs
[PASS] parallel burden matches serial on real data      loc 125, 2 models, 2022: identical
[PASS] mask-first conversion matches the old output     loc 125 + 22 (wrapped): identical
=== 11 of 11 checks passed ===
```

If any line says FAIL it stops without launching and tells you which log to
send. Do not work around a FAIL; it means something on the machine is off.

Some lines may say PASS with "SKIPPED" and a reason, usually that there is
nothing converted yet to compare against. That is expected on a first run and
is not a failure.

## Reading ./status.sh

One row per location per phase.

```
      loc   phase  done total   pct    ok  skip  fail   gap  median_s  state
1:     62 convert  2314  3364  68.8  2314     0     0     0      31.2  (running)
2:    125  burden   841   841 100.0   841     0     0     0       4.1  DONE
```

- **convert** builds daily temperature files from the CCKP NetCDFs; **burden**
  runs the pipeline over them.
- **gap** is a model and scenario combination CCKP does not publish. Expected,
  not an error, and rerunning will never fill it. Four models have such gaps.
- **state** is DONE only when every intended combo finished with no failures.

## When something fails

Look before re-running:

```bash
./status.sh --failures
```

Re-running fixes a failure that was circumstantial: the run was killed, a
worker was short of memory, a file was caught mid-write. It will not fix a real
bug, and re-running a genuine error just reproduces it. If the same combos fail
twice with the same message, or the message reads like an R error rather than a
resource problem, stop and send that output to Aaron rather than looping.

## _INCOMPLETE does not block anything

`_DONE` and `_INCOMPLETE` are status only. Nothing reads them to decide what to
run, so there is never a need to delete one by hand. A location marked
`_INCOMPLETE` is retried in full on the next launch exactly like any other, and
the marker is rewritten from what actually finished.

What a location does skip is any combo whose **output file already exists**.
That is the only thing that governs resumption, and it is per combo, not per
location or per model.

So if a location seemed not to start on a restart, the marker was not the
cause. Two real causes existed, both now fixed, and both are described under
"What changed" below.

## Running one scenario, or a few locations

To finish one scenario before starting the others:

```bash
SCENARIOS=ssp245 ./run_production.sh
```

or set `SCENARIOS=ssp245` in `run_env.sh`. Drop the setting later and re-run to
pick up the remaining three; everything ssp245 already produced is skipped.

To retry a handful of locations without walking the whole list:

```bash
LOCATIONS="62 102" ./run_production.sh
```

There is nothing to do about the model list. `run_production.sh` takes no model
setting at all: every run uses all 29 models from `config.R`, and the ones a
scenario does not publish are recorded as gaps and stepped over. The
hand-filtered `--models=` list you needed before is not part of the workflow
any more. Do not add one back; it would silently drop models.

One thing to expect when running a subset: `./status.sh` reflects the scope of
the most recent launch, not everything ever computed. After an ssp245-only run
it shows the ssp245 grid. When you later run all four, the ssp245 combos
reappear as `skip` and the totals cover the full grid again. Nothing was lost
in between; the outputs on disk are what count.

## Workers, and what the settings actually do

The run is one process per location, `JOBS` of them at a time. That is the main
dial and it has not changed. One GNU parallel job is one location, so
per-location and per-job mean the same thing throughout.

What is new is that a single location can now work on several of its
(model, scenario, year) combos at once, instead of strictly one at a time.
That applies to the **convert** step, and it is not applied evenly: the count
is sized from how large the location is on the grid, because that is where the
imbalance is. Of 204 locations, 180 get one worker, 13 get two, 8 get three,
and 3 get four. The US, Russia and New Zealand are the three.

`MAX_WORKERS_PER_LOCATION` (default 4) is a **ceiling on that sizing, not a
number applied to everything**. Setting it to 2 means "no location gets more
than 2"; small locations still get one either way. Setting it to 1 switches the
feature off.

The **burden** step is left at one combo at a time per location, set by
`BURDEN_WORKERS` (default 1). It costs about the same per combo everywhere, so
splitting it does not correct an imbalance the way it does for convert; it only
raises throughput, and `JOBS` already controls that at lower risk. It is also
the memory-hungry phase. Measured in the full production configuration, 17
causes and 500 draws:

| location | burden peak per combo | time per combo |
|---|---|---|
| Colombia | 3.7 GB | 71s |
| United States | 4.7 GB | 90s |

Each extra burden worker therefore costs about as much memory as another whole
job. `BURDEN_WORKERS=2` turns it on if you want one location to finish sooner,
but for general throughput raise `JOBS` instead.

**`JOBS` is the setting that matters most here.** Steady-state memory is
roughly `JOBS` times the per-combo burden peak, because locations spend most of
their time in the burden step. On this machine's 512 GB:

| JOBS | worst-case resident | |
|---|---|---|
| 125 | ~590 GB | over capacity, will swap |
| 100 | ~470 GB | 92% of RAM, little left for page cache |
| 80 | ~375 GB | ~135 GB spare, the default |

The spare memory is not wasted. Every combo reads a roughly 770 MB NetCDF, and
page cache is what keeps those reads off the disk; a run that swaps is far
slower than one with fewer jobs. This is worth knowing because the previous
`-j 125` was at or over the edge, and time lost to swapping would have looked
like slow locations rather than a memory problem.

`./status.sh` now prints a memory line every time you run it, and warns if
free memory drops under 8%. If it warns, lower `JOBS` in `run_env.sh` and
relaunch; nothing is lost.

Thread counts are pinned to one everywhere, and every worker inherits that, so
workers add processes but never multiply threads. You do not set this and it is
not in `run_env.sh`: `run_production.sh` exports `OMP_NUM_THREADS=1` and
`OPENBLAS_NUM_THREADS=1`, and `config.R` caps data.table at one thread by
default.

If the machine runs short of memory, lower `JOBS` first, since it is by far the
larger multiplier, and only then `MAX_WORKERS_PER_LOCATION`.

## What changed

1. **The convert phase used to stop at the first missing model and scenario**
   and still exit 0. On a full four-scenario run it quit at gfdl-cm4/ssp370,
   which is 65% of the grid and 18 of 29 models never converted. This is why
   removing hadgem3-gc31-mm from the front of your list made it work: with no
   gap to hit, the loop ran to the end.

2. **An unset `CCKP_POP_LOCAL_ROOT` was passed down as the literal `TRUE`**, so
   the population file was looked for under a directory named `TRUE/`. Any
   combo whose population file was not already cached was recorded as a gap
   rather than converted. If most of a location's combos came back as gaps for
   no obvious reason, this was why.

3. **Runs now work from any directory.** Previously renv only activated when R
   started in the project root, so launching elsewhere silently used different
   package versions, or none.

4. **Large locations are much cheaper.** The convert step used to build a table
   over the whole bounding box and throw most of it away. Only 5.8% of the US
   bounding box is inside the US, because the Aleutians straddle the dateline
   and Hawaii and Point Barrow stretch the latitudes. It now selects the cells
   inside the country first. Measured on real data, output unchanged:

   | location | time | peak memory |
   |---|---|---|
   | United States | 51.8s -> 10.7s | 20.5 GB -> 2.9 GB |
   | Russia | 56.3s -> 20.6s | 14.3 GB -> 4.2 GB |
   | Brazil | 11.2s -> 7.0s | 1.8 GB -> 1.0 GB |

5. **Progress is tracked per combo**, so the numbers in `./status.sh` are
   correct no matter how the work was split, and they survive a restart.

## One thing to know about earlier output

Nothing produced before this change is wrong, but a lot of it is incomplete,
because of items 1 and 2. Re-running fills the gaps. The exception worth
flagging: any ensemble figure averaged across models before the re-run was
averaged over a truncated, alphabetically-first subset rather than all models,
so regenerate those after the run finishes.
