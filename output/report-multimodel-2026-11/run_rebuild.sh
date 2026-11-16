#!/bin/bash
# run_rebuild.sh: rebuild every pooled table, extra and figure after a change of groups,
# in the order the scripts depend on each other. Run inside the emacs-r distrobox from
# the repo root. Pooling uses 8 of 16 cores (run_pooling.sh).
# Arguments: <summary_dir with draws/, draws_cause/, draws_age/> <review_dir (pop_by_loc.rds,
#            model_sets.rds, independent check files)> <shapefile.shp> <out dir>
# Writes everything under <out dir>: the pooled output, tidy_common.rds, extras/,
# tables/, tex-tables/, figures-report/, figures-slides/, and a log per step. Nothing in
# the repo is overwritten; copy tables and figures into docs/ when the report is revised.
set -e
S=$1; R=$2; SHP=$3; O=$4
D=output/report-multimodel-2026-11
mkdir -p "$O"
bash $D/run_pooling.sh "$S" "$O" > "$O/log_pooling.txt" 2>&1
Rscript $D/build_tables.R "$O" "$R" "$O/tables" $D/income_groups_by_location.csv > "$O/log_build_tables.txt" 2>&1
nice -n 10 Rscript $D/country_extras.R "$S" $D/income_groups.csv "$O/extras" "$R/model_sets.rds" > "$O/log_country_extras.txt" 2>&1
Rscript $D/make_figures.R "$O" "$R" "$SHP" "$O/figures-report" "$O/figures-slides" > "$O/log_make_figures.txt" 2>&1
Rscript $D/make_report_tables.R "$O" "$O/tex-tables" $D/income_groups_by_location.csv "$O/tables" > "$O/log_make_report_tables.txt" 2>&1
echo done
