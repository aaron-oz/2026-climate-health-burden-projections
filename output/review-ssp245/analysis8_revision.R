# All-204 GBD round-revision measurement, using the paper-era companion
# dataset (GHDx "Mortality Burden Attributable to Non-Optimal Temperature
# 1990-2019", DOI 10.6069/7QWW-PF74) downloaded 2026-08-17.
# revision = current-round (GBD 2023) 2019 value / paper-era 2019 value.
suppressPackageStartupMessages(library(data.table))

OUT <- "/var/home/aoz/code/2026-climate-health-burden-projections/output/review-ssp245"
P <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/gbd-2019-results/IHME_MORTALITY_TEMPERATURE_1990_2019_DATA_Y2021M09D27.CSV")
cat("rows:", nrow(P), "| measures:", paste(unique(P$measure_name), collapse=","),
    "| metrics:", paste(unique(P$metric_name), collapse=","),
    "| years:", paste(sort(unique(P$year_id)), collapse=","), "\n")
cat("loc types:", paste(unique(P$location_type), collapse=","),
    "| sexes:", paste(unique(P$sex), collapse=","),
    "| ages:", paste(unique(P$age_group_name), collapse=","),
    "| reis:", paste(unique(P$rei_name), collapse=","), "\n")

# Paper-era file has Rate (per 100k) and Percent (PAF of all-cause) only, no
# counts. Reconstruct counts by anchoring on the current-round download:
# population implied from its Number/Rate, all-cause deaths from Number/Percent.
p19 <- P[year_id == 2019 & measure_name == "Deaths" &
         age_group_name == "All Ages" & location_type %in% c("admin0", "nonsovereign")]
p19[, risk := fcase(rei_name %like% "High", "heat", rei_name %like% "Low", "cold",
                    default = "nonopt")]
pw <- dcast(p19, location_id ~ risk + metric_name,
            value.var = c("val", "lower", "upper"))

g <- fread("data/gbd-comparison/gbd_A_anchor_2019_2022.csv")
g19 <- g[year == 2019 & measure_name == "Deaths"]
g19[, risk := fcase(rei_name %like% "High", "heat", rei_name %like% "Low", "cold",
                    default = "nonopt")]
ga <- dcast(g19, location_id ~ risk + metric_name, value.var = "val")
# gbd_A units: Rate per 100k, Percent a fraction (verified on Niger 2019)
ga[, `:=`(pop = 1e5 * nonopt_Number / nonopt_Rate,
          allcause = nonopt_Number / nonopt_Percent)]
gw <- ga[, .(location_id, r23_nonopt = nonopt_Number, r23_heat = heat_Number,
             r23_cold = cold_Number, pop, allcause)]

pw <- merge(pw, gw[, .(location_id, pop, allcause)], by = "location_id")
# paper-file units: Rate is per PERSON, Percent a fraction (verified on Niger)
pw[, `:=`(val_nonopt = val_nonopt_Rate * pop,
          lower_nonopt = lower_nonopt_Rate * pop,
          upper_nonopt = upper_nonopt_Rate * pop,
          val_heat = val_heat_Rate * pop,
          val_cold = val_cold_Rate * pop,
          val_nonopt_viapaf = val_nonopt_Percent * allcause)]
cat("paper-era countries:", nrow(pw),
    "| global nonopt (rate-based):", round(sum(pw$val_nonopt)),
    "| (paf-based):", round(sum(pw$val_nonopt_viapaf)), "\n")
cat("reconstruction agreement: median |paf-based/rate-based - 1| =",
    round(median(abs(pw$val_nonopt_viapaf / pw$val_nonopt - 1), na.rm = TRUE), 3), "\n")
pw[, c("pop", "allcause") := NULL]

ens <- fread(file.path(OUT, "ensemble_by_location_year.csv"))[year == 2022]
meta <- fread(file.path(OUT, "location_meta.csv"))
gc2 <- fread(file.path(OUT, "gbd2022_comparison.csv"))

M <- Reduce(function(a, b) merge(a, b, by = "location_id"),
            list(pw, gw, ens[, .(location_id, ours_nonopt = nonopt, ours_heat = heat,
                                 ours_cold = cold)],
                 meta[, .(location_id, location_name)],
                 gc2[, .(location_id, ratio_vs_r23 = ratio)]))
cat("joined:", nrow(M), "locations\n")

M[, `:=`(rev_nonopt = r23_nonopt / val_nonopt, rev_heat = r23_heat / val_heat,
         ours_vs_paper = ours_nonopt / val_nonopt,
         in_paper_ui = ours_nonopt >= lower_nonopt & ours_nonopt <= upper_nonopt)]
fwrite(M, file.path(OUT, "revision_all204.csv"))

cat("\n== revision factor (round23 2019 / paper 2019), nonopt ==\n")
print(round(quantile(M$rev_nonopt, c(.05,.25,.5,.75,.95), na.rm = TRUE), 2))
cat("global: round23", round(sum(M$r23_nonopt)), "/ paper", round(sum(M$val_nonopt)),
    "=", round(sum(M$r23_nonopt)/sum(M$val_nonopt), 3), "\n")

cat("\n== ours(2022) vs PAPER-ERA(2019): all 204 ==\n")
cat("Spearman:", round(cor(M$ours_nonopt, M$val_nonopt, method = "spearman"), 3),
    "| inside paper 95% UI:", sum(M$in_paper_ui, na.rm = TRUE), "of", nrow(M), "\n")
big <- M[val_nonopt > 100]
cat("ratio quantiles (", nrow(big), "locs, paper nonopt > 100):\n")
print(round(quantile(big$ours_vs_paper, c(.05,.25,.5,.75,.95)), 2))
cat("global ours / global paper:", round(sum(M$ours_nonopt)/sum(M$val_nonopt), 3), "\n")

sf <- c("Niger","Haiti","Sudan","South Sudan","Kuwait","Iraq","Nigeria","India",
        "Chad","Mali","Oman","United Arab Emirates","Pakistan","Cambodia","Mauritania")
cat("\n== shortfall + case-study countries ==\n")
print(M[location_name %in% sf,
        .(location_name, paper19 = round(val_nonopt), r23_19 = round(r23_nonopt),
          revision = round(rev_nonopt, 2), ours22 = round(ours_nonopt),
          vs_paper = round(ours_vs_paper, 2), vs_r23 = round(ratio_vs_r23, 2),
          in_paper_ui)][order(vs_r23)])

cat("\n== heat-side revision, same countries ==\n")
print(M[location_name %in% sf,
        .(location_name, paper_heat = round(val_heat), r23_heat = round(r23_heat),
          rev_heat = round(rev_heat, 2), ours_heat = round(ours_heat))][order(-rev_heat)])

# YLL-per-death in the paper era (rates ratio; population cancels per location,
# global uses implied population weights)
py <- P[year_id == 2019 & metric_name == "Rate" & age_group_name == "All Ages" &
        location_type %in% c("admin0", "nonsovereign") & rei_name %like% "Non-optimal"]
pyw <- dcast(py, location_id ~ measure_name, value.var = "val")
setnames(pyw, c("location_id", "d_rate", "y_rate"))
pyw <- merge(pyw, ga[, .(location_id, pop)], by = "location_id")
cat("\npaper-era global YLL per attributable death:",
    round(sum(pyw$y_rate * pyw$pop) / sum(pyw$d_rate * pyw$pop), 1),
    "y  (per-person rates; population weights cancel scale)\n")
