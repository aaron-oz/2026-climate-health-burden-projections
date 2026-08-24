# Memo-ready summaries from analysis17_2x2_sdfield_all204.csv: per-country
# noise effects (both bounds), the C.1/C.2 deltas, and the countries where
# each choice matters most.
suppressPackageStartupMessages(library(data.table))
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
R <- fread("output/review-ssp245/analysis17_2x2_sdfield_all204.csv")
cat("countries:", nrow(R), "\n\n")

cat("== noise effect on burden, per country (ratio to same pairing, raw) ==\n")
for (p in c("A", "B")) for (b in c("c", "i")) {
  v <- R[[paste0(p, b)]] / R[[p]]
  lbl <- sprintf("%s%s/%s (%s bound)", p, b, p, ifelse(b == "c", "corr", "indep"))
  cat(sprintf("%-22s median %.3f | p90 %.3f | max %.3f (%s)\n", lbl,
              median(v), quantile(v, 0.9), max(v),
              R$location_name[which.max(v)]))
}

cat("\n== C.1 rounding: Br/B per country ==\n")
v <- R$Br / R$B
cat(sprintf("median %.4f | min %.3f (%s) | max %.3f (%s)\n", median(v),
            min(v), R$location_name[which.min(v)],
            max(v), R$location_name[which.max(v)]))
cat("global Br/B:", round(sum(R$Br) / sum(R$B), 4), "\n")

cat("\n== C.2 draw-specific weights ==\n")
has_ihd <- file.exists(sprintf("data/mortality/%d_mortality_ihme_cvd_ihd_draws.rds",
                               R$location_id))
cat("countries with exact cvd_ihd draws:", sum(has_ihd), "of", nrow(R), "\n")
v <- R$Bw[has_ihd] / R$B[has_ihd]
cat(sprintf("Bw/B (exact subset):  median %.4f | min %.4f | max %.4f\n",
            median(v), min(v), max(v)))
v <- R$Bs / R$B
cat(sprintf("Bs/B (stress, all):   median %.4f | min %.4f | max %.4f\n",
            median(v), min(v), max(v)))
cat("global (exact subset): Bw", round(sum(R$Bw[has_ihd])),
    "vs B", round(sum(R$B[has_ihd])),
    "ratio", round(sum(R$Bw[has_ihd]) / sum(R$B[has_ihd]), 4), "\n")

cat("\n== measured sd: pop-weighted per-country distribution ==\n")
for (vv in c("sd_corr_pw", "sd_indep_pw")) {
  x <- R[[vv]]
  cat(sprintf("%-12s min %.2f | p25 %.2f | median %.2f | p75 %.2f | max %.2f C (max: %s)\n",
              vv, min(x), quantile(x, .25), median(x), quantile(x, .75), max(x),
              R$location_name[which.max(x)]))
}

cat("\n== biggest noise-driven changes (corr bound), top 10 by |Ac/A - 1| ==\n")
R[, noise_eff := Ac / A - 1]
print(R[order(-abs(noise_eff))][1:10, .(location_name, A = round(A), Ac = round(Ac),
      eff_pct = round(100 * noise_eff, 1), sd_corr_pw = round(sd_corr_pw, 2))])

cat("\n== low-cluster detail ==\n")
M <- fread("output/review-ssp245/revision_all204.csv")
lowc <- R[location_id %in% M[val_nonopt > 50 & ours_nonopt < lower_nonopt, location_id]]
print(lowc[, .(location_name, paper = round(paper), lo = round(lo), hi = round(hi),
               A = round(A), B = round(B), Bc = round(Bc),
               inB = B >= lo & B <= hi, inBc = Bc >= lo & Bc <= hi)])
