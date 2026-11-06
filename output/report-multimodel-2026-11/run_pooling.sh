#!/bin/bash
# Production pooling for the 2026-11 report: totals under three model sets, cause and
# age breakdowns on the 24 common models, reporting decades, income groups plus World.
# Run inside the emacs-r distrobox from the repo root. Uses 8 of 16 cores.
# Arguments: <summary_dir with draws/, draws_cause/, draws_age/> <out dir>
set -e
S=$1; O=$2
G=output/report-multimodel-2026-11/income_groups.csv
P=2022-2030,2031-2040,2041-2050
for set in common paired all; do
  nice -n 10 Rscript global-scripts/util_pool_workflow_b_draws.R "--draws_dir=$S/draws" --model_set=$set \
    --periods=$P --period_batch=3 --groups=$G --jobs=8 "--out=$O/totals_$set"
done
for b in age cause; do
  /usr/bin/time -v nice -n 10 Rscript global-scripts/util_pool_workflow_b_draws.R "--draws_dir=$S/draws_$b" --model_set=common \
    --periods=$P --period_batch=3 --groups=$G --jobs=8 "--out=$O/$b"
done
