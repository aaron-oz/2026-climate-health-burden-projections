#!/usr/bin/env Rscript
# Helper: inspect CP4 heat/cold burden gaps by cause.

library(data.table)
ROOT <- "/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/output/samuel-runner"

d <- as.data.table(readRDS(file.path(ROOT, "diff_checkpoint_1_erf.rds")))
cat(sprintf("CP1: %d (zone, daily_temp, cause) rows compared. Max abs%% rr_mean: %g, rr_max: %g\n",
            nrow(d), max(abs(d$rr_mean_pct), na.rm = TRUE),
            max(abs(d$rr_max_pct), na.rm = TRUE)))

cp4 <- as.data.table(readRDS(file.path(ROOT, "diff_checkpoint_4_burden.rds")))
agg4 <- cp4[, .(pipe_dh = sum(pipe_deaths_heat), sam_dh = sum(sam_deaths_heat),
                pipe_dc = sum(pipe_deaths_cold), sam_dc = sum(sam_deaths_cold),
                pipe_d  = sum(pipe_deaths),       sam_d  = sum(sam_deaths)),
            by = acause]
agg4[, gap_h := sam_dh - pipe_dh]
agg4[, gap_c := sam_dc - pipe_dc]
agg4[, n_total_deaths := pipe_d]
agg4 <- agg4[order(-abs(gap_h))]
cat("\n=== CP4: ranked by absolute gap in heat-attributable deaths ===\n")
print(agg4[, .(acause, n_total_deaths,
               pipe_dh = round(pipe_dh, 1), sam_dh = round(sam_dh, 1),
               gap_h = round(gap_h, 1),
               pipe_dc = round(pipe_dc, 1), sam_dc = round(sam_dc, 1),
               gap_c = round(gap_c, 1))])
cat(sprintf("\nTotal heat: pipe=%.1f, sam=%.1f, gap=%.1f (%+.1f%%)\n",
    sum(agg4$pipe_dh), sum(agg4$sam_dh), sum(agg4$gap_h),
    (sum(agg4$sam_dh)/sum(agg4$pipe_dh) - 1) * 100))
cat(sprintf("Total cold: pipe=%.1f, sam=%.1f, gap=%.1f (%+.1f%%)\n",
    sum(agg4$pipe_dc), sum(agg4$sam_dc), sum(agg4$gap_c),
    (sum(agg4$sam_dc)/sum(agg4$pipe_dc) - 1) * 100))
