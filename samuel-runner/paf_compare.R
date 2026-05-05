#!/usr/bin/env Rscript
# Helper: compares per-cause PAFs across measurement conventions.
# Run after run_samuel_with_checkpoints.R.

library(data.table)
ROOT <- "/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections/output/samuel-runner"
PROJ <- "/var/home/aoz/Dropbox/OZ-Labs/WorldBank/2026-climate-health-burden-projections"

cat("=== Per-cause PAFs: published vs pipe vs sam-runner ===\n")
cp2 <- as.data.table(readRDS(file.path(ROOT, "diff_checkpoint_2_paf.rds")))
cp4 <- as.data.table(readRDS(file.path(ROOT, "diff_checkpoint_4_burden.rds")))
agg_pdwt <- cp4[, .(
  pipe_paf_h_dwt = sum(pipe_deaths_heat) / sum(pipe_deaths),
  sam_paf_h_dwt  = sum(sam_deaths_heat)  / sum(sam_deaths),
  pipe_paf_c_dwt = sum(pipe_deaths_cold) / sum(pipe_deaths),
  sam_paf_c_dwt  = sum(sam_deaths_cold)  / sum(sam_deaths)
), by = acause]
agg_cp2 <- cp2[, .(
  pipe_paf_h_avg = mean(pipe_paf_heat),
  pipe_paf_c_avg = mean(pipe_paf_cold),
  sam_paf_h_mean = mean(sam_paf_heat_mean),
  sam_paf_c_mean = mean(sam_paf_cold_mean)
), by = acause]
agg <- merge(agg_pdwt, agg_cp2, by = "acause")
pub_h <- list(inj_homicide = 0.007, inj_trans_road = 0.006, diabetes = 0.005)
pub_c <- list(inj_drowning = 0.0185, resp_copd = 0.0111, lri = 0.0104)

cat("\n--- Heat causes ---\n")
for (cause in names(pub_h)) {
  rr <- agg[acause == cause]
  if (nrow(rr) == 0) next
  cat(sprintf("\n%s (heat) - Samuel-published PAF target: %.4f\n",
              cause, pub_h[[cause]]))
  cat(sprintf("  pipeline annual mean PAF (= mean across yrs of paf_heat): %.4f (vs pub: %+5.1f%%)\n",
              rr$pipe_paf_h_avg, (rr$pipe_paf_h_avg/pub_h[[cause]]-1)*100))
  cat(sprintf("  pipeline death-weighted PAF (= total deaths_heat / total deaths): %.4f (vs pub: %+5.1f%%)\n",
              rr$pipe_paf_h_dwt, (rr$pipe_paf_h_dwt/pub_h[[cause]]-1)*100))
  cat(sprintf("  samuel-runner plain-mean PAF (mean across pixel-day-depto): %.4f (vs pub: %+5.1f%%)\n",
              rr$sam_paf_h_mean, (rr$sam_paf_h_mean/pub_h[[cause]]-1)*100))
  cat(sprintf("  samuel-runner death-weighted PAF (= total deaths_heat / total deaths): %.4f (vs pub: %+5.1f%%)\n",
              rr$sam_paf_h_dwt, (rr$sam_paf_h_dwt/pub_h[[cause]]-1)*100))
}
cat("\n--- Cold causes ---\n")
for (cause in names(pub_c)) {
  rr <- agg[acause == cause]
  if (nrow(rr) == 0) next
  cat(sprintf("\n%s (cold) - Samuel-published PAF target: %.4f\n",
              cause, pub_c[[cause]]))
  cat(sprintf("  pipeline annual mean PAF: %.4f (vs pub: %+5.1f%%)\n",
              rr$pipe_paf_c_avg, (rr$pipe_paf_c_avg/pub_c[[cause]]-1)*100))
  cat(sprintf("  pipeline death-weighted PAF: %.4f (vs pub: %+5.1f%%)\n",
              rr$pipe_paf_c_dwt, (rr$pipe_paf_c_dwt/pub_c[[cause]]-1)*100))
  cat(sprintf("  samuel-runner plain-mean PAF (mean across pixel-day-depto): %.4f (vs pub: %+5.1f%%)\n",
              rr$sam_paf_c_mean, (rr$sam_paf_c_mean/pub_c[[cause]]-1)*100))
  cat(sprintf("  samuel-runner death-weighted PAF: %.4f (vs pub: %+5.1f%%)\n",
              rr$sam_paf_c_dwt, (rr$sam_paf_c_dwt/pub_c[[cause]]-1)*100))
}
