#!/usr/bin/env bash
#
# run_production.sh — Launch (or resume) the full global projection run.
#
#   ./run_production.sh
#
# Safe to re-run at any time. Everything already computed is skipped, so a
# re-run after a crash, a kill, or a code update picks up where it left off and
# only fills what is missing. There is no separate "resume" command.
#
# It self-checks before launching and refuses to start if the machine is not
# set up correctly, so this is the only command needed to go from a fresh pull
# to a running job. Watch it with ./status.sh
#
# Settings, override by exporting before running or by editing run_env.sh:
#   JOBS       concurrent locations                  (default 80)
#   SCENARIOS  comma separated                       (default all four)
#   YEARS      range                                 (default 2022-2050)
#   LOCATIONS  space/comma separated loc ids to run, instead of all of them.
#              Use it to retry or spot-check a few, e.g. LOCATIONS="62 102"
#   CCKP_LOCAL_ROOT      path to the local CCKP mirror        (required)
#   CCKP_POP_LOCAL_ROOT  if population sits elsewhere         (optional)
#   MAX_WORKERS_PER_LOCATION  cap on workers per location     (default 4)

set -euo pipefail
cd "$(dirname "$0")"
PROJECT_ROOT="$(pwd)"
export PROJECT_ROOT

# Machine-specific settings live here, outside git.
[ -f ./run_env.sh ] && . ./run_env.sh

JOBS="${JOBS:-80}"
export MAX_WORKERS_PER_LOCATION="${MAX_WORKERS_PER_LOCATION:-4}"
export BURDEN_WORKERS="${BURDEN_WORKERS:-1}"
SCENARIOS="${SCENARIOS:-ssp245,ssp370,ssp585,ssp126}"
YEARS="${YEARS:-2022-2050}"

# The mirror is the whole input; never silently fall back to downloading ~200
# locations' worth of NetCDFs from the public bucket.
export CCKP_REQUIRE_LOCAL=TRUE

# One BLAS thread per process. Each location is already its own process, so
# letting BLAS fan out too would oversubscribe the machine badly (a previous
# run reached load average ~660 while RAM sat idle).
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

RUN_STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
LOG_DIR="output/logs/${RUN_STAMP}"
mkdir -p "$LOG_DIR"

say() { printf '%s\n' "$*"; }
die() { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

command -v parallel >/dev/null || die "GNU parallel not found on PATH."
[ -n "${CCKP_LOCAL_ROOT:-}" ] || die "CCKP_LOCAL_ROOT is not set. Put it in run_env.sh."
[ -d "${CCKP_LOCAL_ROOT}" ] || die "CCKP_LOCAL_ROOT is not a directory: ${CCKP_LOCAL_ROOT}"

# Build the augmented shapefile if it is not there yet. Cheap, idempotent, and
# without it six micro-nations produce no output at all.
if [ ! -f data/shapefiles/GBD2023_mapping_final_augmented.shp ]; then
  say ">>> Building augmented shapefile (adds 6 micro-nations)..."
  Rscript global-scripts/util_augment_shapefile.R 2>&1 | tail -5
fi

# A run killed mid-write can leave a truncated output file, which would look
# finished to the resume check and never be recomputed. Only worth looking for
# when the previous run did not exit cleanly, which the marker below tells us:
# it is written at launch and removed on normal completion. Checking
# unconditionally cost minutes of silence on a full output tree for nothing.
RUN_MARKER=output/.run_active
if [ -f "$RUN_MARKER" ]; then
  say ">>> The previous run did not exit cleanly (started $(cat "$RUN_MARKER" 2>/dev/null))."
  say ">>> Checking its most recent output files for truncation..."
  Rscript global-scripts/util_repair_outputs.R 2>&1 | tail -6
  say ""
else
  # First launch under this version: earlier code wrote outputs in place, so a
  # small check covers anything a past interruption left behind. Bounded low
  # enough to be unnoticeable.
  Rscript global-scripts/util_repair_outputs.R --recent=50 --quiet 2>&1 | tail -4
fi

if [ "${1:-}" != "--skip-verify" ]; then
  say ">>> Verifying this machine before launching..."
  say ""
  if ! Rscript global-scripts/util_verify_install.R 2>&1 | tee "$LOG_DIR/verify.log"; then
    say ""
    die "Verification failed. Nothing was launched. Send $LOG_DIR/verify.log to Aaron."
  fi
fi

if [ -n "${LOCATIONS:-}" ]; then
  LOC_LIST="$(printf '%s' "$LOCATIONS" | tr ',' ' ')"
else
  LOC_MAP=output/intermediate/ihme_loc_map.rds
  [ -f "$LOC_MAP" ] || die "$LOC_MAP not found. Run util_convert_ihme_batch.R first, or set LOCATIONS."
  LOC_LIST="$(Rscript -e "cat(readRDS('$LOC_MAP')\$loc_id)")"
fi
N_LOC="$(printf '%s\n' $LOC_LIST | wc -w)"
[ "$N_LOC" -gt 0 ] || die "No locations to run."

say ""
say "=============================================================="
say " Launching production run"
say "   locations      : $N_LOC"
say "   scenarios      : $SCENARIOS"
say "   years          : $YEARS"
say "   concurrency    : $JOBS locations at a time"
say "   CCKP mirror    : $CCKP_LOCAL_ROOT"
say "   convert workers: up to $MAX_WORKERS_PER_LOCATION for the largest locations (most get 1)"
say "   burden workers : $BURDEN_WORKERS per location"
say "   logs           : $LOG_DIR/"
say "=============================================================="
say ""
say "The few geographically largest locations convert several combos at once;"
say "the other ~180 run one at a time, as before."
say ""
say "Monitor with:   ./status.sh"
say "Stop with:      Ctrl-C, then re-run this script to resume."
say ""

# --joblog gives one row per location with its exit code, so a location that
# died is visible without grepping every log.
date -u +%Y-%m-%dT%H:%M:%SZ > "$RUN_MARKER"
# Remove the marker however we exit, so only a hard kill leaves it behind. That
# is exactly the case where the truncation check above is worth running.
trap 'rm -f "$RUN_MARKER"' EXIT

parallel -j "$JOBS" --joblog "$LOG_DIR/joblog.tsv" --line-buffer \
  "Rscript global-scripts/util_run_global.R --location_id={} \
     --scenarios=$SCENARIOS --years=$YEARS \
     > $LOG_DIR/loc_{}.log 2>&1" \
  ::: $LOC_LIST

say ""
say ">>> Fan-out finished. Final status:"
say ""
Rscript global-scripts/util_run_status.R || true
say ""
say "Per-location logs: $LOG_DIR/loc_<id>.log"
say "Exit codes:        $LOG_DIR/joblog.tsv"
say ""
say "If any location shows FAILED combos, just re-run ./run_production.sh"
say "Completed work is skipped, so it only retries what is missing."
