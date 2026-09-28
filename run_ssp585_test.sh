#!/usr/bin/env bash
#
# run_ssp585_test.sh: small end-to-end test of SSP5-RCP8.5 (ssp585) with the
# Workflow B scaling, on 12 locations. Three steps, run in order:
#
#   ./run_ssp585_test.sh run         # 1. burden pipeline, ssp585, the 12 locations
#   ./run_ssp585_test.sh scale       # 2. Workflow B scaling against the ssp245 run
#   ./run_ssp585_test.sh summarize   # 3. tables, QA reports, and a bundle to send
#
# Each step is safe to re-run: finished work is skipped. Step 1 is the long one;
# watch it with ./status.sh. Steps 2 and 3 take minutes.
#
# Prerequisite: the ssp245 run for these locations is already on disk (it is the
# Workflow B reference), along with run_env.sh and the CCKP mirror used for it.
#
# Settings, override by exporting before running:
#   BURDEN_WORKERS   combos run at once within each location in step 1 (default 4;
#                    12 locations x 4 = 48 concurrent combos, below the 80 of the
#                    ssp245 production run)
#   YEARS            default 2022-2050
#   SCALE_JOBS       locations scaled at once in step 2 (default 12)

set -euo pipefail
cd "$(dirname "$0")"
export PROJECT_ROOT="$(pwd)"
# Read the caller's override before run_env.sh (which sets BURDEN_WORKERS=1 for
# the production run) can replace it.
TEST_BURDEN_WORKERS="${BURDEN_WORKERS:-4}"
[ -f ./run_env.sh ] && . ./run_env.sh

# The 12 test locations (GBD location_id):
#   163 India           required (Samuel); hot, largest forecast population
#     6 China           large, spans cold north to subtropical south
#   102 United States   largest compute of the set; cold-dominated
#    81 Germany         temperate Europe, cold-dominated
#   135 Brazil          large, tropical to subtropical
#   125 Colombia        validation country; we can re-run it locally
#    11 Indonesia       equatorial islands, near-constant temperature
#   145 Kuwait          extreme heat
#   214 Nigeria         West Africa, large population, heat
#   190 Uganda          equatorial highlands
#   114 Haiti           known inj_disaster shock tail (derived TMREL at the
#                       search floor in some draws); tests the ratio there
#    14 Maldives        micro-nation from the augmented shapefile, tiny grid
# TEST_LOCATIONS overrides the list, for a dry run on fewer locations.
LOCATIONS="${TEST_LOCATIONS:-163 6 102 81 135 125 11 145 214 190 114 14}"
LOC_CSV="$(printf '%s' "$LOCATIONS" | tr ' ' ',')"
YEARS="${YEARS:-2022-2050}"
REF=ssp245
TARGET=ssp585
TEST_DIR=output/ssp585_test
mkdir -p "$TEST_DIR"

say() { printf '%s\n' "$*"; }
die() { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

case "${1:-}" in

run)
  # The first launch records its start time; the summarize step uses it to
  # confirm every ssp585 combo was computed by this run.
  [ -f "$TEST_DIR/started_at" ] || date -u +%Y-%m-%dT%H:%M:%S > "$TEST_DIR/started_at"
  say ">>> ssp585 test: burden pipeline for 12 locations (started $(cat "$TEST_DIR/started_at") UTC)"
  # run_env.sh (sourced above) may set JOBS, BURDEN_WORKERS, SCENARIOS or
  # LOCATIONS for the production run. The values below replace them for this
  # test, and RUN_ENV_LOADED stops run_production.sh from re-sourcing run_env.sh
  # and putting them back.
  RUN_ENV_LOADED=1 LOCATIONS="$LOCATIONS" SCENARIOS="$TARGET" YEARS="$YEARS" JOBS=12 \
    BURDEN_WORKERS="$TEST_BURDEN_WORKERS" ./run_production.sh
  ;;

scale)
  command -v parallel >/dev/null || die "GNU parallel not found on PATH."
  STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
  LOG_DIR="output/logs/ssp585_test_scale_${STAMP}"
  mkdir -p "$LOG_DIR"
  say ">>> ssp585 test: Workflow B scaling, target $TARGET against reference $REF"
  # Target ssp585: every year. Target ssp245 (the reference against itself)
  # for two years only: its scale factors must all be exactly 1 and its output
  # must reproduce the unscaled ssp245 burden, which the summary checks.
  parallel -j "${SCALE_JOBS:-12}" --joblog "$LOG_DIR/joblog.tsv" --line-buffer \
    "Rscript global-scripts/util_apply_workflow_b_batch.R --location_id={} \
       --ref_scenario=$REF --target_scenarios=$TARGET --years=$YEARS \
       > $LOG_DIR/loc_{}_${TARGET}.log 2>&1 && \
     Rscript global-scripts/util_apply_workflow_b_batch.R --location_id={} \
       --ref_scenario=$REF --target_scenarios=$REF --years=2022,2050 \
       > $LOG_DIR/loc_{}_${REF}.log 2>&1" \
    ::: $LOCATIONS
  say ""
  say ">>> Scaling finished. Per-location logs: $LOG_DIR/"
  say ">>> Last line of each (expect 'missing-ref' only for models that have no $REF run):"
  for l in $LOCATIONS; do
    printf '  loc %-4s %s\n' "$l" "$(tail -1 "$LOG_DIR/loc_${l}_${TARGET}.log" | sed 's/^\[[^]]*\] //')"
  done
  ;;

summarize)
  SINCE="$(cut -c1-10 "$TEST_DIR/started_at" 2>/dev/null || true)"
  [ -n "$SINCE" ] || die "$TEST_DIR/started_at not found; run step 1 first."
  say ">>> 1/3 Summarizing the unscaled ssp585 pipeline output (with provenance check since $SINCE)"
  Rscript global-scripts/util_summarize_run.R --scenarios="$TARGET" --locations="$LOC_CSV" \
    --years="$YEARS" --since="$SINCE" --out="$TEST_DIR/summary_pipeline"
  say ""
  say ">>> 2/3 Summarizing the Workflow B (scaled) output"
  Rscript global-scripts/util_summarize_workflow_b.R --scenarios="$REF,$TARGET" \
    --ref_scenario="$REF" --locations="$LOC_CSV" --years="$YEARS" \
    --out="$TEST_DIR/summary_workflow_b"
  say ""
  say ">>> 3/3 Packing the bundle"
  B="$TEST_DIR/bundle"
  rm -rf "$B"; mkdir -p "$B/summary_pipeline" "$B/summary_workflow_b" "$B/manifests" "$B/sample"
  cp "$TEST_DIR"/summary_pipeline/*.csv "$TEST_DIR"/summary_pipeline/*.txt "$B/summary_pipeline/" 2>/dev/null || true
  cp "$TEST_DIR"/summary_workflow_b/*.csv "$TEST_DIR"/summary_workflow_b/*.txt "$B/summary_workflow_b/"
  cp "$TEST_DIR/started_at" "$B/manifests/"
  # Manifest rows for this test only (the full files hold the ssp245 run too).
  [ -f output/cckp_burden_manifest.csv ] && \
    awk -F, -v s="$TARGET" 'NR==1 || $2==s' output/cckp_burden_manifest.csv > "$B/manifests/burden_manifest_${TARGET}.csv"
  [ -f output/workflow_b_batch_manifest.csv ] && cp output/workflow_b_batch_manifest.csv "$B/manifests/"
  # One combo's raw files (Colombia, access-cm2, 2022) so we can recompute the
  # scaling independently from the Colombia inputs we already have.
  M=access-cm2-r1i1p1f1
  for f in "cckp/125/$M-$REF/burden_2022.rds" "cckp/125/$M-$TARGET/burden_2022.rds" \
           "workflow_b/125/$M-$TARGET/wb_2022.rds" "workflow_b/125/$M-$REF/wb_2022.rds"; do
    if [ -f "output/results/$f" ]; then
      mkdir -p "$B/sample/$(dirname "$f")"; cp "output/results/$f" "$B/sample/$f"
    else
      say "  (sample file not found, skipped: output/results/$f)"
    fi
  done
  tar czf "$TEST_DIR/ssp585_test_bundle.tar.gz" -C "$TEST_DIR" bundle
  say ""
  say ">>> Done. Please send: $TEST_DIR/ssp585_test_bundle.tar.gz ($(du -h "$TEST_DIR/ssp585_test_bundle.tar.gz" | cut -f1))"
  ;;

*)
  sed -n '3,21p' "$0" | sed 's/^# \{0,1\}//'
  exit 1
  ;;
esac
