#!/usr/bin/env bash
# End-to-end test of the final pass on a small fixture (3 locations, 29 years, 5 climate models,
# 4 scenarios, 40 draws) built from the real Colombia sample and scaled with the production
# Workflow B code. Run from the repo root inside the emacs-r distrobox:
#   bash output/final-summary-pass-2026-11/test/run_tests.sh <bundle-v2 sample dir> <scratch dir> <pop_by_loc.rds> [historical csv]
# The standard population here is uniform and for testing only.
set -euo pipefail
SAMPLE=$1; FX=$2/fx; POPRDS=$3; HIST=${4:-}; T=output/final-summary-pass-2026-11
Rscript $T/test/make_fixture.R "$SAMPLE" "$FX" 40
PROJECT_ROOT=$FX Rscript global-scripts/util_export_compact.R --out=$FX/compact --jobs=3 > $FX/compact.log 2>&1
grep -E 'PASS|FAIL|locations:' $FX/compact/report.txt
mkdir -p $FX/inputs
Rscript $T/prepare_inputs.R "$POPRDS" $FX/inputs > $FX/inputs.log 2>&1
{ echo "age_group_id,weight"; for a in $(seq 0 5 80); do echo "$a,1"; done; } > $FX/inputs/std_pop_TEST_ONLY_uniform.csv
awk -F, 'NR==1||$1==125||$1==163||$1==102' $FX/inputs/groups.csv > $FX/inputs/groups_fx.csv
PROJECT_ROOT=$FX Rscript global-scripts/util_final_tables.R --compact=$FX/compact --out=$FX/tables \
  --pop=$FX/inputs/population.csv --std_pop=$FX/inputs/std_pop_TEST_ONLY_uniform.csv --groups=$FX/inputs/groups_fx.csv \
  --names=$FX/inputs/names.csv --lifetables=$(pwd)/data/lifetables ${HIST:+--historical=$HIST} --jobs=3 > $FX/tables.log 2>&1
grep -E 'units:' $FX/tables/run_info.txt
Rscript $T/test/check_tables.R $FX $FX/inputs/std_pop_TEST_ONLY_uniform.csv
