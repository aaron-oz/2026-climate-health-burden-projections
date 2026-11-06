#!/bin/bash
# Pool the yearly totals with the production script, common 24 and paired, for
# decades. Run inside the emacs-r distrobox from the repo root.
# Arguments: <summary_dir> <out_dir>
set -e
S=$1; O=$2
for set in common paired; do
  Rscript global-scripts/util_pool_workflow_b_draws.R \
    --draws_dir="$S/draws" --model_set=$set \
    --periods=2022-2030,2031-2040,2041-2050 --period_batch=3 \
    --out="$O/pool_totals_$set"
done
