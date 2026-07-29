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
#   JOBS       concurrent locations                  (default 100)
#   SCENARIOS  comma separated                       (default all four)
#   YEARS      range                                 (default 2022-2050)
#   CCKP_LOCAL_ROOT      path to the local CCKP mirror        (required)
#   CCKP_POP_LOCAL_ROOT  if population sits elsewhere         (optional)

set -euo pipefail
cd "$(dirname "$0")"
PROJECT_ROOT="$(pwd)"
export PROJECT_ROOT

# Machine-specific settings live here, outside git.
[ -f ./run_env.sh ] && . ./run_env.sh

JOBS="${JOBS:-100}"
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

if [ "${1:-}" != "--skip-verify" ]; then
  say ">>> Verifying this machine before launching..."
  say ""
  if ! Rscript global-scripts/util_verify_install.R 2>&1 | tee "$LOG_DIR/verify.log"; then
    say ""
    die "Verification failed. Nothing was launched. Send $LOG_DIR/verify.log to Aaron."
  fi
fi

LOC_LIST="$(Rscript -e 'cat(readRDS("output/intermediate/ihme_loc_map.rds")$loc_id)')"
N_LOC="$(printf '%s\n' $LOC_LIST | wc -w)"
[ "$N_LOC" -gt 0 ] || die "No locations found in output/intermediate/ihme_loc_map.rds"

say ""
say "=============================================================="
say " Launching production run"
say "   locations      : $N_LOC"
say "   scenarios      : $SCENARIOS"
say "   years          : $YEARS"
say "   concurrency    : $JOBS locations at a time"
say "   CCKP mirror    : $CCKP_LOCAL_ROOT"
say "   logs           : $LOG_DIR/"
say "=============================================================="
say ""
say "Big locations (the US, Russia and a few others) automatically get extra"
say "workers of their own; everything else runs single-threaded as before."
say ""
say "Monitor with:   ./status.sh"
say "Stop with:      Ctrl-C, then re-run this script to resume."
say ""

# --joblog gives one row per location with its exit code, so a location that
# died is visible without grepping every log.
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
