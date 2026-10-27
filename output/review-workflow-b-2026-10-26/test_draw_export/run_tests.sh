#!/usr/bin/env bash
# End-to-end test of the summarizer's draw export and the pooling script on a
# small fixture built from the accepted ssp585 test bundle's Colombia sample.
# Run from the repo root, inside an R environment (here: the emacs-r distrobox):
#   bash output/review-workflow-b-2026-10-26/test_draw_export/run_tests.sh <sample dir> <scratch dir>
set -euo pipefail
SAMPLE=$1; FX=$2/fx; REPO=$(pwd); T=output/review-workflow-b-2026-10-26/test_draw_export
Rscript $T/make_fixture.R "$SAMPLE" "$FX" "$REPO"
export PROJECT_ROOT=$FX
Rscript global-scripts/util_summarize_workflow_b.R --scenarios=ssp126,ssp585 --ref_scenario=ssp245 \
  --years=2022,2023,2030 --jobs=2 --draw_periods=2022-2023,2030 --out=$FX/output/summary_workflow_b > $FX/summarize.log 2>&1
Rscript $T/check_draws.R "$FX"
printf 'location_id,group\n125,A\n126,A\n125,B\n' > $FX/groups.csv
for ms in paired common; do
  Rscript global-scripts/util_pool_workflow_b_draws.R --draws_dir=$FX/output/summary_workflow_b/draws \
    --model_set=$ms --groups=$FX/groups.csv --out=$FX/output/pool_$ms > $FX/pool_$ms.log 2>&1
done
Rscript $T/check_pool.R "$FX"
Rscript $T/check_incomplete.R "$FX" "$REPO"
# Annual export, periods formed by the pooling script (one period per pass):
# must reproduce the period export's pooled results.
Rscript global-scripts/util_summarize_workflow_b.R --scenarios=ssp126,ssp585 --ref_scenario=ssp245 \
  --years=2022,2023,2030 --jobs=2 --draw_periods=annual --out=$FX/output/summary_annual > $FX/summarize_annual.log 2>&1
for ms in paired common; do
  Rscript global-scripts/util_pool_workflow_b_draws.R --draws_dir=$FX/output/summary_annual/draws \
    --periods=2022-2023,2030 --period_batch=1 --model_set=$ms --groups=$FX/groups.csv \
    --out=$FX/output/pool_annual_$ms > $FX/pool_annual_$ms.log 2>&1
done
Rscript $T/check_annual.R "$FX"
