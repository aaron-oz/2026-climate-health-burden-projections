# All-204 comparison of our ssp245 2022 ensemble against published GBD 2022
# non-optimal-temperature burden (GBD Results tool export in data/gbd-comparison/).
suppressPackageStartupMessages(library(data.table))

OUT <- "/var/home/aoz/code/2026-climate-health-burden-projections/output/review-ssp245"
gbd <- fread("data/gbd-comparison/gbd_A_anchor_2019_2022.csv")
gbd <- gbd[year == 2022 & metric_name == "Number"]
gbd[, risk := fcase(rei_name == "High temperature", "heat",
                    rei_name == "Low temperature", "cold",
                    rei_name == "Non-optimal temperature", "nonopt")]
gbd[, meas := fifelse(measure_name == "Deaths", "d", "y")]
gw <- dcast(gbd, location_id ~ meas + risk, value.var = c("val", "lower", "upper"))

ens  <- fread(file.path(OUT, "ensemble_by_location_year.csv"))[year == 2022]
meta <- fread(file.path(OUT, "location_meta.csv"))
m <- merge(merge(gw, ens, by = "location_id"),
           meta[, .(location_id, location_name, lat, cells = cells_off125)],
           by = "location_id")
cat("joined locations:", nrow(m), "\n")

cat("\n== global sums, 2022: GBD published vs our ssp245 ensemble ==\n")
cat(sprintf("deaths nonopt: GBD %.0f  ours %.0f  (ratio %.3f)\n",
            sum(m$val_d_nonopt), sum(m$nonopt), sum(m$nonopt)/sum(m$val_d_nonopt)))
cat(sprintf("deaths heat  : GBD %.0f  ours %.0f  (ratio %.3f)\n",
            sum(m$val_d_heat), sum(m$heat), sum(m$heat)/sum(m$val_d_heat)))
cat(sprintf("deaths cold  : GBD %.0f  ours %.0f  (ratio %.3f)\n",
            sum(m$val_d_cold), sum(m$cold), sum(m$cold)/sum(m$val_d_cold)))
cat(sprintf("YLL nonopt   : GBD %.0f  ours %.0f  (ratio %.3f)\n",
            sum(m$val_y_nonopt), sum(m$yll_nonopt), sum(m$yll_nonopt)/sum(m$val_y_nonopt)))

m[, `:=`(ratio = nonopt / val_d_nonopt,
         in_ui = nonopt >= lower_d_nonopt & nonopt <= upper_d_nonopt,
         in_ui_heat = heat >= lower_d_heat & heat <= upper_d_heat,
         in_ui_cold = cold >= lower_d_cold & cold <= upper_d_cold,
         sign_ok = sign(nonopt) == sign(val_d_nonopt))]
cat("\n== per-location agreement (n = 204) ==\n")
cat("Spearman rank corr, nonopt deaths:", round(cor(m$nonopt, m$val_d_nonopt, method="spearman"), 3), "\n")
cat("within GBD 95% UI: nonopt", sum(m$in_ui), " heat", sum(m$in_ui_heat),
    " cold", sum(m$in_ui_cold), "\n")
cat("same sign as GBD (nonopt):", sum(m$sign_ok), "\n")
big <- m[val_d_nonopt > 100]
cat("ratio quantiles (", nrow(big), "locations with GBD nonopt > 100 deaths):\n")
print(round(quantile(big$ratio, c(.05,.25,.5,.75,.95)), 2))

cat("\n== sub-grid group vs resolved: ours/GBD ratio ==\n")
m[, grp := fifelse(cells <= 1, "subgrid", "resolved")]
print(m[val_d_nonopt > 20, .(n = .N, ratio_median = round(median(ratio), 2),
        ratio_q25 = round(quantile(ratio, .25), 2),
        ratio_q75 = round(quantile(ratio, .75), 2),
        in_ui = sum(in_ui)), by = grp])

cat("\n== largest disagreements (GBD nonopt > 100; top 14 by |log ratio|) ==\n")
big[, alr := abs(log(pmax(ratio, 1e-6)))]
print(big[order(-alr)][1:14, .(location_name, cells,
      gbd = round(val_d_nonopt), gbd_lo = round(lower_d_nonopt),
      gbd_hi = round(upper_d_nonopt), ours = round(nonopt),
      ours_min = round(nonopt_min), ours_max = round(nonopt_max),
      ratio = round(ratio, 2))])

fwrite(m, file.path(OUT, "gbd2022_comparison.csv"))
cat("\nwrote gbd2022_comparison.csv\n")
