#!/bin/bash
# Step E (2026-08-20 handoff): run the REAL pipeline for the validation set
# under three configurations and stash labeled burden outputs:
#   modeA  = default (released_recycled TMRELs, no noise)  -> production behavior
#   modeB  = --tmrel_mode=derived_per_draw                  -> the pairing fix
#   modeBc = derived + --temp_noise_mode=era5_sd (sd_corr)  -> fix + measured noise
# Mortality: {loc}_mortality_validation.rds (production by_cause 2022 totals,
# point weights) so pipeline-B weights match replica-B by construction.
# access-cm2-r1i1p1f1, ssp245, 2022. Outputs land in
# output/review-ssp245/stepE/{mode}/burden_{loc}.rds
set -u
cd /var/home/aoz/code/wbg-climate-health-burden-projections
LOCS="114 131 145 13 190 11 125 135 171 6"
OUT=output/review-ssp245/stepE
mkdir -p $OUT/modeA $OUT/modeB $OUT/modeBc

run_mode () {
  mode=$1; shift
  extra="$*"
  for loc in $LOCS; do
    echo "=== $mode loc $loc ==="
    distrobox enter emacs-r -- Rscript global-scripts/util_run_cckp_burden.R \
      --location_id=$loc --models=access-cm2-r1i1p1f1 --scenarios=ssp245 \
      --years=2022 --force=TRUE \
      --mortality_file=data/mortality/${loc}_mortality_validation.rds \
      $extra 2>&1 | grep -E "ok |fail|missing" | tail -1
    src=output/results/cckp/$loc/access-cm2-r1i1p1f1-ssp245/burden_2022.rds
    if [ -f "$src" ]; then
      cp "$src" $OUT/$mode/burden_${loc}.rds
    else
      echo "MISSING burden for loc $loc in $mode"
    fi
  done
}

run_mode modeA
run_mode modeB  --tmrel_mode=derived_per_draw
run_mode modeBc --tmrel_mode=derived_per_draw --temp_noise_mode=era5_sd
echo "step E runs complete"
