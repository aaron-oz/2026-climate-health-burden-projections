suppressPackageStartupMessages(library(data.table))
SP <- "output/review-ssp245/round2/work"
REV <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"  # untracked analysis CSVs from the round-1 review live in the main checkout
PIL <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-pilot-2026-09"
meta <- fread(file.path(REV, "location_meta.csv"))[, .(location_id, location_name)]
m <- readRDS(file.path(SP, "m.rds"))
stale <- c(50, 89, 101, 126)
# 1. pilot draws vs stage 2
pn <- fread(file.path(PIL, "pilot_draws_new.csv"))
pns <- pn[, .(p_mean = mean(deaths_nonopt), p_med = median(deaths_nonopt)), by = .(location_id, combo)]
pns[, model := sub("-ssp245$", "", combo)]
x <- merge(pns, m[year == 2022, .(location_id, model, deaths_nonopt_mean_2, deaths_nonopt_median)], by = c("location_id", "model"))
x[, d := deaths_nonopt_mean_2 / p_mean - 1]
cat("pilot vs stage2 (2022), combos:", nrow(x), " max |rel diff| mean:", max(abs(x$d)), " median:", max(abs(x$deaths_nonopt_median/x$p_med - 1)), "\n")
print(x[order(-abs(d))][1:5, .(location_id, model, p_mean, deaths_nonopt_mean_2, d)])
# 2. replica B all-204 (access-cm2 2022)
pb <- fread(file.path(REV, "pairing2x2_all204.csv"))
a <- merge(pb[, .(location_id, location_name, A, B, production, paper, lo, hi)], m[year == 2022 & model == "access-cm2-r1i1p1f1", .(location_id, new = deaths_nonopt_mean_2, newmed = deaths_nonopt_median, old = deaths_nonopt_mean_1)], by = "location_id")
a[, `:=`(rB = new / B, rA_old = old / A)]
a2 <- a[!location_id %in% stale]
cat("\nreplica B vs new production, access-cm2 2022, n =", nrow(a2), "(stale 4 excluded)\n")
print(quantile(a2$rB, c(0, .01, .05, .25, .5, .75, .95, .99, 1)))
cat("within 1%:", sum(abs(a2$rB - 1) < 0.01), " within 5%:", sum(abs(a2$rB - 1) < 0.05), "\n")
print(a2[abs(rB - 1) >= 0.01][order(-abs(rB - 1)), .(location_id, location_name, B, new, newmed, rB, A, old, rA_old)])
cat("stale four: new(=old) vs B\n"); print(a[location_id %in% stale, .(location_name, B, A, new, old, rB, rA_old)])
# in-UI vs paper for access-cm2 (compare to memory: B 143 in-UI of 184 scored)
a[, inui := new >= lo & new <= hi]
cat("access-cm2 2022 in paper UI:", sum(a$inui, na.rm=TRUE), "of", sum(!is.na(a$paper)), " B replica in-UI:", sum(a$B >= a$lo & a$B <= a$hi, na.rm=TRUE), "\n")
fwrite(a, file.path(SP, "replicaB_check.csv"))
