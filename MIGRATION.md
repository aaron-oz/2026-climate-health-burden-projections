# Migrating to the fixed pipeline

Three commands total. Everything else is automatic.

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
=== 10 of 10 checks passed ===
```

If any line says FAIL it stops without launching and tells you which log to
send. Do not work around a FAIL; it means something on the machine is off.

The last check is skipped on a machine with nothing converted yet. That is
expected on a first run and is not a failure.

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

## One thing to know about earlier output

Nothing produced before this change is wrong, but a lot of it is incomplete,
because of item 1. Re-running fills the gaps. The exception worth flagging:
any ensemble figure averaged across models before the re-run was averaged over
a truncated, alphabetically-first subset rather than all models, so regenerate
those after the run finishes.
