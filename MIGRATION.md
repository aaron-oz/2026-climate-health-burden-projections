# Migrating to the fixed pipeline

## If a run is currently going, stop it

```bash
# Ctrl-C the running ./run_production.sh, or:
pkill -f util_run_global.R
```

Stop it rather than waiting for it to finish. The run in flight cannot produce
a complete result: it is working from a model list filtered per scenario to
dodge the convert-abort bug, so 15 or more models per scenario are missing by
construction. Letting it finish only delays the restart.

Nothing computed so far is lost or has to be redone:

- **Converted temperature files stay valid.** The conversion was rewritten for
  speed, but its output is byte-identical, verified against the old version on
  six locations. Existing files are recognised and skipped.
- **Burden outputs stay valid.** They are incomplete, not wrong. Each file
  holds the correct numbers for the model and scenario it names.
- **Old progress markers are harmless.** They get rewritten on the next run.

Killing a run can leave a file half-written. Under the old code such a file
looked finished to the resume check and would never have been recomputed;
`./run_production.sh` now scans for them and clears them before it launches, so
there is nothing for you to do about it. New writes are atomic and cannot be
left half-finished at all.

Optional tidy-up, not required: the old code left staging files at
`output/results/burden_<loc>.rds` and similar. Nothing reads them now. Delete
them if you like the tree clean.

## Then, three commands total. Everything else is automatic.

Everything below assumes you are in the project directory:

```bash
cd /data/HEAT/Burkart/FHS/2026-climate-health-burden-projections   # adjust to yours
```

Then:

```bash
git pull
cp run_env.sh.example run_env.sh
./run_production.sh
```

`run_env.sh` needs exactly one line checked, the first setting in the file:

```bash
export CCKP_LOCAL_ROOT=/data
```

That must be the directory holding `cmip6-daily-x0.25/` and `pop-x0.25/`. If
the shipped value is already right for your machine, change nothing else; every
other setting in the file has a working default and is documented in place.

To watch it, from the project directory:

```bash
./status.sh
```

The `./` matters: these are scripts in the project directory, not commands on
your PATH. From elsewhere, give the full path, which works from any directory:

```bash
/data/HEAT/Burkart/FHS/2026-climate-health-burden-projections/status.sh
```

`./run_production.sh` is also the resume command: re-run it any time, after a
crash, a Ctrl-C, or another `git pull`, and it skips everything already
computed and fills only what is missing. There is no separate resume step and
nothing to clean up first.

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
- **fail** is the column that needs attention. Always look before re-running:

  ```bash
  ./status.sh --failures
  ```

  Re-running fixes a failure that was circumstantial: the run was killed, a
  worker was short of memory, a file was mid-write. It will not fix a real
  bug, and re-running a genuine error just reproduces it. If the same combos
  fail twice with the same message, or the message looks like an R error
  rather than a resource problem, stop and send that output to Aaron rather
  than looping.
- **state** is DONE only when every intended combo finished with no failures.

## What changed, briefly

1. **The convert phase used to stop at the first missing model and scenario**
   and still exit 0. On a full four-scenario run it quit at gfdl-cm4/ssp370,
   which is 65% of the grid and 18 of 29 models never converted. This is why
   removing hadgem3-gc31-mm from the front of your list made it work: with no
   gap to hit, the loop ran to the end. **You no longer need to filter models
   per scenario.** Use the full list; gaps are recorded and stepped over.

2. **Runs now work from any directory.** Previously renv only activated when R
   started in the project root, so launching elsewhere silently used different
   package versions, or none.

3. **Large locations are much cheaper, and a few of them get extra workers.**
   The convert step used to build a table over the whole bounding box and throw
   most of it away. Only 5.8% of the US bounding box is actually inside the US,
   because the Aleutians straddle the dateline and Hawaii and Point Barrow
   stretch the latitudes. It now selects the cells inside the country first.
   Measured on real data, output unchanged:

   | location | time | peak memory |
   |---|---|---|
   | United States | 51.8s -> 10.7s | 20.5 GB -> 2.9 GB |
   | Russia | 56.3s -> 20.6s | 14.3 GB -> 4.2 GB |
   | Brazil | 11.2s -> 7.0s | 1.8 GB -> 1.0 GB |

   Separately, the handful of geographically largest locations now convert
   several combos at once. This applies to 24 of 204 locations and only to the
   conversion step; the median location is unaffected and still runs one combo
   at a time. You do not configure it.

4. **Progress is tracked per combo**, so the numbers in `./status.sh` are
   correct no matter how the work was split, and they survive a restart.

## Finishing one scenario before starting the others

```bash
SCENARIOS=ssp245 ./run_production.sh
```

or set `SCENARIOS=ssp245` in `run_env.sh`. Drop the setting later and re-run to
pick up the remaining three; everything ssp245 already produced is skipped.

There is nothing to do about the model list. `run_production.sh` does not take
a model setting at all: every run uses all 29 models from `config.R`, and the
ones a scenario does not publish are recorded as gaps and stepped over. The
hand-filtered `--models=` list you needed before is simply not part of the
workflow any more. Do not add one back; it would silently drop models.

One thing to expect: `./status.sh` reflects the scope of the most recent
launch, not everything ever computed. After an ssp245-only run it shows the
ssp245 grid. When you later run all four, the ssp245 combos reappear as `skip`
and the totals cover the full grid again. Nothing was lost in between; the
outputs on disk are what count, and the status view is derived from the run you
last asked for.

## Running only some locations

```bash
LOCATIONS="62 102" ./run_production.sh
```

Useful for retrying a handful without walking the whole list.

## _INCOMPLETE does not block anything

`_DONE` and `_INCOMPLETE` are status only. Nothing reads them to decide what to
run, so there is never a need to delete one by hand. A location marked
`_INCOMPLETE` is retried in full on the next launch exactly like any other, and
the marker is rewritten from what actually finished.

What a location does skip is any combo whose **output file already exists**.
That is the only thing that governs resumption, and it is per combo, not per
location or per model.

So if a location seemed not to start on a restart, the marker was not the
cause. Two real causes existed, both now fixed:

- The convert phase stopped at the first model and scenario missing from the
  mirror and still exited 0, abandoning every combo after it (item 1 above).
- An unset `CCKP_POP_LOCAL_ROOT` was being passed down as the literal string
  `TRUE`, so the population file was looked for under a directory named
  `TRUE/`. Any combo whose population file was not already cached was recorded
  as a gap rather than converted. If most of a location's combos came back as
  gaps for no obvious reason, this was why.

## Workers, and what the settings actually do

The run is one process per location, `JOBS` of them at a time. That is the main
dial and it has not changed.

On top of that, a location may split its own conversion work across a few
workers. This is not applied evenly: it is sized from how large the location is
on the grid, because that is where the imbalance is. Of 204 locations, 180 get
one worker, 13 get two, 8 get three, and 3 get four. The US, Russia and New
Zealand are the three.

`MAX_WORKERS_PER_LOCATION` (default 4) is a **ceiling on that sizing, not a
number applied to everything**. Setting it to 2 means "no location gets more
than 2"; small locations still get 1 either way. Setting it to 1 switches the
feature off. Since one GNU parallel job is one location, per-location and
per-job mean the same thing here.

The burden step is deliberately left at one worker per location, set by
`BURDEN_WORKERS` (default 1). Two reasons. It costs about the same per combo
everywhere, so extra workers there do not fix an imbalance, they just raise
throughput, and `JOBS` already controls that. And it is the memory-hungry
phase: one burden combo peaked at 3.7 GB measured on Colombia in summary mode
with a single cause, so the production configuration will be at least that.
Multiplying it per location is the fastest way to run the machine out of RAM.

Thread counts are pinned to 1 everywhere (data.table, OpenMP, OpenBLAS) and
that is inherited by every worker, so workers add processes but never
multiply threads. Verified.

If the machine runs short of memory, lower `JOBS` first, since it is the larger
multiplier, then `MAX_WORKERS_PER_LOCATION`.

## One thing to know about earlier output

Nothing produced before this change is wrong, but a lot of it is incomplete,
because of item 1. Re-running fills the gaps. The exception worth flagging:
any ensemble figure averaged across models before the re-run was averaged over
a truncated, alphabetically-first subset rather than all models, so regenerate
those after the run finishes.
