#!/usr/bin/env bash
# run_final_pass.sh: the final summary pass on the production machine, both stages.
# Run from the repo root (current main), with the scaled scenario output under output/results.
#
#   bash output/final-summary-pass-2026-11/run_final_pass.sh a    # stage A: the compact copy (long; restartable)
#   bash output/final-summary-pass-2026-11/run_final_pass.sh b    # stage B: the tables, from the compact copy
#   bash output/final-summary-pass-2026-11/run_final_pass.sh pack # bundle the tables and reports to send back
#
# Settings (environment variables):
#   JOBS_A  workers for stage A (default 16). Each holds one location, under 1 GB.
#   JOBS_B  workers for stage B (default 8). Each needs about 7 GB of memory (measured
#           6.6 GB at full size), so JOBS_B x 7 GB must fit in free memory.
#   COMPACT where the compact copy goes (default output/compact; about 65 GB).
#   TABLES  where the tables go (default output/final_tables; a few GB).
# Both stages skip what they have already finished, so either can be rerun after an interruption.
set -euo pipefail
STAGE=${1:?usage: run_final_pass.sh a|b|pack}
JOBS_A=${JOBS_A:-16}; JOBS_B=${JOBS_B:-8}
COMPACT=${COMPACT:-output/compact}; TABLES=${TABLES:-output/final_tables}
IN=output/final-summary-pass-2026-11/inputs
case "$STAGE" in
  a)
    Rscript global-scripts/util_export_compact.R --out="$COMPACT" --jobs="$JOBS_A" 2>&1 | tee -a final_pass_stage_a.log
    echo; cat "$COMPACT/report.txt"
    ;;
  b)
    [ -f "$IN/std_pop.csv" ] || { echo "missing $IN/std_pop.csv (pull the latest main)"; exit 1; }
    Rscript global-scripts/util_final_tables.R --compact="$COMPACT" --out="$TABLES" \
      --pop="$IN/population.csv" --std_pop="$IN/std_pop.csv" --groups="$IN/groups.csv" --names="$IN/names.csv" \
      --historical="$IN/historical_gbd2019_table.csv" --jobs="$JOBS_B" 2>&1 | tee -a final_pass_stage_b.log
    ;;
  pack)
    out=final_pass_$(date +%Y%m%d).tar
    tar -cf "$out" "$TABLES" "$COMPACT/report.txt" "$COMPACT/manifest.csv" final_pass_stage_*.log
    ls -l "$out"; echo "Send $out (tables and reports). The compact copy itself stays on this machine until we agree where it goes."
    ;;
  *) echo "usage: run_final_pass.sh a|b|pack"; exit 1 ;;
esac
