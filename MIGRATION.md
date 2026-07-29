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

```bash
cd <project>
git pull

cp run_env.sh.example run_env.sh     # first time only: set CCKP_LOCAL_ROOT
$EDITOR run_env.sh

./run_production.sh                  # verifies, then launches or resumes
```

Then, whenever you want to know how it is going, from anywhere:

```bash
./status.sh
```

That is the whole workflow. `./run_production.sh` is also the resume command:
re-run it any time, after a crash, a Ctrl-C, or another `git pull`, and it
skips everything already computed and fills only what is missing. There is no
separate resume step and nothing to clean up first.

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
- **fail** is the only column that needs action, and the action is always the
  same: re-run `./run_production.sh`. Use `./status.sh --failures` to see why
  before you do.
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

3. **Large locations are much cheaper, and get extra workers automatically.**
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

   Large locations also run several combos at once now. You do not configure
   either of these.

4. **Progress is tracked per combo**, so the numbers in `./status.sh` are
   correct no matter how the work was split, and they survive a restart.

## Finishing one scenario before starting the others

```bash
SCENARIOS=ssp245 ./run_production.sh
```

or set `SCENARIOS=ssp245` in `run_env.sh`. Drop the setting later and re-run to
pick up the remaining three; everything ssp245 already produced is skipped.

Use the full model list. The per-scenario filtering you had to do before is no
longer needed, and leaving it in place would silently omit models.

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

## If the machine runs short of memory

Lower `MAX_WORKERS_PER_LOCATION` in `run_env.sh` to 2, or 1 to turn the extra
workers off entirely, and re-run `./run_production.sh`. Nothing else changes.
For reference, 4 workers on the US now use about 12 GB against the 20 GB a
single worker used before the in-country selection landed.

## One thing to know about earlier output

Nothing produced before this change is wrong, but a lot of it is incomplete,
because of item 1. Re-running fills the gaps. The exception worth flagging:
any ensemble figure averaged across models before the re-run was averaged over
a truncated, alphabetically-first subset rather than all models, so regenerate
those after the run finishes.
