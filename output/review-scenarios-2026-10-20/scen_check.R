suppressMessages(library(data.table))
a <- commandArgs(TRUE); D <- a[1]; S <- a[3]
nat <- function(s) fread(file.path(D, s, s, "national_by_year.csv"))
n370 <- nat(S); n245 <- nat("ssp245")
cat("rows", nrow(n370), nrow(n245), "\n")
cat("locations", uniqueN(n370$location_id), " models", uniqueN(n370$model), "\n")

# 1. scenario-independent inputs must agree exactly: all-cause deaths (GBD forecast)
k <- c("location_id","model","year")
m <- merge(n370, n245, by = k, suffixes = c(".370",".245"))
cat("\n[1] common rows", nrow(m), " models", uniqueN(m$model), "\n")
cat("    max |deaths_mean diff|", max(abs(m$deaths_mean.370 - m$deaths_mean.245)), "\n")
# deaths_mean must also not depend on model
dm <- n370[, .(spread = diff(range(deaths_mean))), by = .(location_id, year)]
cat("    max across-model spread in deaths_mean (ssp370)", max(dm$spread), "\n")

# 2. NA / negative counts
num <- grep("_(mean|median|lower|upper)$", names(n370), value = TRUE)
cat("\n[2] NA cells", sum(is.na(n370[, ..num])), "\n")
for (v in c("deaths_heat_mean","deaths_cold_mean","deaths_nonopt_mean")) {
  cat(sprintf("    %s <0: ssp370 %d rows, ssp245 %d rows\n", v, sum(n370[[v]] < 0), sum(n245[[v]] < 0)))
}
cat("    ui ordering lower<=median<=upper violations (deaths_nonopt):",
    n370[, sum(deaths_nonopt_lower > deaths_nonopt_median + 1e-9 | deaths_nonopt_median > deaths_nonopt_upper + 1e-9)], "\n")

# 3. global totals, common models only, mean over models of the sum over locations
cm <- intersect(unique(n370$model), unique(n245$model))
g <- function(x) x[model %in% cm, .(heat = sum(deaths_heat_mean), cold = sum(deaths_cold_mean),
                                    nonopt = sum(deaths_nonopt_mean)), by = .(model, year)][
                   , .(heat = mean(heat), cold = mean(cold), nonopt = mean(nonopt),
                       heat_lo = min(heat), heat_hi = max(heat)), by = year]
G <- merge(g(n370), g(n245), by = "year", suffixes = c(".370", ".245"))
G[, `:=`(heat_ratio = heat.370 / heat.245, cold_ratio = cold.370 / cold.245, nonopt_ratio = nonopt.370 / nonopt.245)]
cat("\n[3] global, ", length(cm), " common models, model-mean of location sums (deaths)\n", sep = "")
print(G[year %in% c(2022, 2025, 2030, 2035, 2040, 2045, 2050),
        .(year, heat.245 = round(heat.245), heat.370 = round(heat.370), heat_ratio = round(heat_ratio, 3),
          cold.245 = round(cold.245), cold.370 = round(cold.370), cold_ratio = round(cold_ratio, 3),
          nonopt_ratio = round(nonopt_ratio, 3))])
dec <- function(x, y0, y1) x[year >= y0 & year <= y1]
cat("    decade means of heat_ratio: 2022-2030", round(mean(dec(G,2022,2030)$heat_ratio), 3),
    " 2031-2040", round(mean(dec(G,2031,2040)$heat_ratio), 3),
    " 2041-2050", round(mean(dec(G,2041,2050)$heat_ratio), 3), "\n")
cat("    decade means of cold_ratio: 2022-2030", round(mean(dec(G,2022,2030)$cold_ratio), 3),
    " 2031-2040", round(mean(dec(G,2031,2040)$cold_ratio), 3),
    " 2041-2050", round(mean(dec(G,2041,2050)$cold_ratio), 3), "\n")
cat("    ssp245 global heat 2022 -> 2050 growth", round(G[year==2050, heat.245]/G[year==2022, heat.245], 3),
    "; ssp370", round(G[year==2050, heat.370]/G[year==2022, heat.370], 3), "\n")

# 4. per model: 2041-2050 heat sum, ssp370 vs ssp245
pm <- merge(n370[model %in% cm & year >= 2041, .(h = sum(deaths_heat_mean), c = sum(deaths_cold_mean)), by = model],
            n245[model %in% cm & year >= 2041, .(h = sum(deaths_heat_mean), c = sum(deaths_cold_mean)), by = model],
            by = "model", suffixes = c(".370", ".245"))
pm[, `:=`(hr = h.370 / h.245, cr = c.370 / c.245)]
cat("\n[4] per model, global 2041-2050: heat ratio 370/245 range", round(range(pm$hr), 3),
    "; models with ratio>1:", sum(pm$hr > 1), "of", nrow(pm),
    "\n    cold ratio range", round(range(pm$cr), 3), "; models with ratio<1:", sum(pm$cr < 1), "of", nrow(pm), "\n")
print(pm[order(hr), .(model, heat_ratio = round(hr, 3), cold_ratio = round(cr, 3))])

# 5. per location (median over common models) 2041-2050 heat ratio
pl <- merge(n370[model %in% cm & year >= 2041, .(h = sum(deaths_heat_mean), c = sum(deaths_cold_mean)), by = .(location_id, model)],
            n245[model %in% cm & year >= 2041, .(h = sum(deaths_heat_mean), c = sum(deaths_cold_mean)), by = .(location_id, model)],
            by = c("location_id", "model"), suffixes = c(".370", ".245"))
plm <- pl[, .(hr = median(h.370 / h.245), cr = median(c.370 / c.245), frac_up = mean(h.370 > h.245),
              h245 = median(h.245) / 10), by = location_id]
cat("\n[5] per location, median over models of 2041-2050 heat ratio 370/245\n")
print(summary(plm$hr))
cat("    locations with median heat ratio < 1:", sum(plm$hr < 1), " (<0.97:", sum(plm$hr < 0.97), ")\n")
cat("    locations with median cold ratio > 1:", sum(plm$cr > 1), "\n")
print(plm[hr < 0.98 | hr > 1.5][order(hr)])
fwrite(plm, file.path(a[2], paste0("per_location_", S, "_vs_245.csv")))

# 6. 2022 (scenarios diverged only from 2015): ratio should be near 1
y22 <- G[year == 2022]
cat("\n[6] 2022 global heat ratio", round(y22$heat_ratio, 3), " cold ratio", round(y22$cold_ratio, 3), "\n")

# 7. YLL / deaths ratio consistent across scenarios
r <- function(x) x[year == 2050, sum(yll_nonopt_mean) / sum(deaths_nonopt_mean)]
cat("\n[7] YLL per non-optimal death, 2050: ssp370", round(r(n370), 2), " ssp245", round(r(n245), 2), "\n")

# 8. by_cause and by_age_sex sums vs national (means are linear)
bc <- fread(file.path(D, S, S, "by_cause.csv"), select = c(k, "acause", "deaths_heat", "deaths_cold", "deaths_nonopt"))
cat("\n[8] by_cause rows", nrow(bc), " causes", uniqueN(bc$acause), "\n")
s <- bc[, .(h = sum(deaths_heat), c = sum(deaths_cold), n = sum(deaths_nonopt)), by = k]
z <- merge(s, n370, by = k)
cat("    max rel diff by_cause sum vs national mean: heat", signif(z[, max(abs(h - deaths_heat_mean) / pmax(abs(deaths_heat_mean), 1))], 3),
    " cold", signif(z[, max(abs(c - deaths_cold_mean) / pmax(abs(deaths_cold_mean), 1))], 3),
    " nonopt", signif(z[, max(abs(n - deaths_nonopt_mean) / pmax(abs(deaths_nonopt_mean), 1))], 3), "\n")
gc_ <- bc[year >= 2041, .(h = sum(deaths_heat), c = sum(deaths_cold)), by = acause]
b2 <- fread(file.path(D, "ssp245", "ssp245", "by_cause.csv"), select = c(k, "acause", "deaths_heat", "deaths_cold"))
gc2 <- b2[model %in% cm & year >= 2041, .(h = sum(deaths_heat), c = sum(deaths_cold)), by = acause]
gc_ <- bc[model %in% cm & year >= 2041, .(h = sum(deaths_heat), c = sum(deaths_cold)), by = acause]
cc <- merge(gc_, gc2, by = "acause", suffixes = c(".370", ".245"))
cat("    by cause, 2041-2050 common models: heat ratio 370/245, cold ratio\n")
print(cc[, .(acause, heat_ratio = round(h.370 / h.245, 3), cold_ratio = round(c.370 / c.245, 3),
             heat_share_370 = round(h.370 / sum(h.370), 3))][order(heat_ratio)])
rm(b2); invisible(gc())
ba <- fread(file.path(D, S, S, "by_age_sex.csv"), select = c(k, "deaths_nonopt", "yll_nonopt"))
sa <- ba[, .(n = sum(deaths_nonopt), y = sum(yll_nonopt)), by = k]
z <- merge(sa, n370, by = k)
cat("\n    by_age_sex rows", nrow(ba), "; max rel diff vs national: deaths_nonopt",
    signif(z[, max(abs(n - deaths_nonopt_mean) / pmax(abs(deaths_nonopt_mean), 1))], 3),
    " yll_nonopt", signif(z[, max(abs(y - yll_nonopt_mean) / pmax(abs(yll_nonopt_mean), 1))], 3), "\n")

# 9. against an earlier test bundle for the same scenario, if given
if (length(a) >= 4) {
  t <- fread(file.path(a[4], "national_by_year.csv"))
  j <- merge(t, n370, by = k, suffixes = c(".test", ".full"))
  cat("\n[9] test-bundle rows", nrow(t), " matched in full run", nrow(j), "\n")
  for (v in c("deaths_heat_mean", "deaths_cold_mean", "deaths_nonopt_mean", "yll_nonopt_mean", "deaths_nonopt_median"))
    cat(sprintf("    %-22s max |diff| %.3g   rows differing >1e-6 rel: %d\n", v,
        max(abs(j[[paste0(v, ".test")]] - j[[paste0(v, ".full")]])),
        sum(abs(j[[paste0(v, ".test")]] - j[[paste0(v, ".full")]]) > 1e-6 * pmax(abs(j[[paste0(v, ".full")]]), 1))))
}
