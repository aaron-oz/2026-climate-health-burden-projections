suppressPackageStartupMessages(library(data.table))
SP <- "output/review-ssp245/round2/work"
REV <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"  # untracked analysis CSVs from the round-1 review live in the main checkout
R2 <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/review_bundle"
options(width = 220)
m <- readRDS(file.path(SP, "m.rds"))
stale <- c(50, 89, 101, 126)
e <- m[year == 2022, .(new = mean(deaths_nonopt_mean_2), newmed = mean(deaths_nonopt_median), old = mean(deaths_nonopt_mean_1),
                       newh = mean(deaths_heat_mean_2), oldh = mean(deaths_heat_mean_1), newc = mean(deaths_cold_mean_2), oldc = mean(deaths_cold_mean_1),
                       newlo = mean(deaths_nonopt_lower_2), newhi = mean(deaths_nonopt_upper_2)), by = location_id]
rv <- fread(file.path(REV, "revision_all204.csv"))[, .(location_id, location_name, paper = val_nonopt, plo = lower_nonopt, phi = upper_nonopt)]
g23 <- fread(file.path(REV, "gbd2022_comparison.csv"))[, .(location_id, g_nonopt = val_d_nonopt, g_lo = lower_d_nonopt, g_hi = upper_d_nonopt, g_heat = val_d_heat, g_cold = val_d_cold)]
z <- merge(merge(e, rv, by = "location_id"), g23, by = "location_id")
sc <- function(v, ref, lo, hi, lab) {
  ok <- !is.na(ref) & ref > 0
  cat(sprintf("%-26s n=%d global ratio %.3f | median ratio %.3f | Spearman %.3f | in ref UI %d | sign ok %d\n", lab, sum(ok),
              sum(v[ok]) / sum(ref[ok]), median(v[ok] / ref[ok]), cor(v[ok], ref[ok], method = "spearman"),
              sum(v[ok] >= lo[ok] & v[ok] <= hi[ok]), sum(sign(v[ok]) == sign(ref[ok]))))
}
cat("== vs GBD 2019 paper-era (Burkart 2021 companion), ensemble mean 2022 ==\n")
sc(z$old, z$paper, z$plo, z$phi, "old (round 1)")
sc(z$new, z$paper, z$plo, z$phi, "new (round 2)")
sc(z$newmed, z$paper, z$plo, z$phi, "new, draw median")
cat("== vs GBD 2023 round, year 2022 ==\n")
sc(z$old, z$g_nonopt, z$g_lo, z$g_hi, "old")
sc(z$new, z$g_nonopt, z$g_lo, z$g_hi, "new")
cat(sprintf("heat global ratio old %.3f new %.3f | cold old %.3f new %.3f\n", sum(z$oldh)/sum(z$g_heat), sum(z$newh)/sum(z$g_heat), sum(z$oldc)/sum(z$g_cold), sum(z$newc)/sum(z$g_cold)))
cat(sprintf("global sums 2022: old %.0f new %.0f paper %.0f gbd23 %.0f\n", sum(z$old), sum(z$new), sum(z$paper), sum(z$g_nonopt)))
# interval overlap with paper UI
z[, overlap := newlo <= phi & newhi >= plo]
cat("new draw interval overlaps paper UI:", sum(z$overlap), "of", nrow(z), "\n")
print(z[overlap == FALSE, .(location_name, new, newlo, newhi, paper, plo, phi)])
# which left / entered the paper UI
z[, `:=`(in_old = old >= plo & old <= phi, in_new = new >= plo & new <= phi)]
cat("entered paper UI:", z[in_new & !in_old, paste(location_name, collapse = ", "), ], "\n")
cat("left paper UI:", z[!in_new & in_old, paste(location_name, collapse = ", "), ], "\n")
cat("below UI new:", z[new < plo, .N], " above UI new:", z[new > phi, .N], " (old below", z[old < plo, .N], "above", z[old > phi, .N], ")\n")
# relative widths vs paper
z[, `:=`(rw_new = (newhi - newlo) / new, rw_paper = (phi - plo) / paper)]
cat("rel width new/paper quantiles:\n"); print(quantile(z[new > 10]$rw_new / z[new > 10]$rw_paper, c(.05, .25, .5, .75, .95)))
fwrite(z, file.path(SP, "gbd_scoring_round2.csv"))
# Venezuela: cause mortality means by year (access-cm2)
bc <- fread(file.path(R2, "summary/by_cause.csv"))[location_id %in% c(133, 69)]
v <- bc[model == "access-cm2-r1i1p1f1" & year %in% 2022:2030]
print(dcast(v[location_id == 133], acause ~ year, value.var = "deaths"), digits = 4)
print(dcast(v[location_id == 133], acause ~ year, value.var = "deaths_cold"), digits = 4)
