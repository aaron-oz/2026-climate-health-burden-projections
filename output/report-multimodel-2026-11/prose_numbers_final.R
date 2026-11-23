# prose_numbers_final.R: every number quoted in the report's prose that can be computed
# from the final summary tables, written to a text digest so each one can be checked
# against the text. Numbers the final tables cannot give (single draws, single climate
# models, temperatures, IHME's population by single year) come from earlier analyses of
# the same runs and are not recomputed here.
# Usage (repo root, emacs-r distrobox):
#   Rscript output/report-multimodel-2026-11/prose_numbers_final.R <final_to_tidy out dir> <final tables dir> > numbers_for_prose_final.txt
suppressPackageStartupMessages(library(data.table))
options(width = 180, datatable.print.nrows = 400, scipen = 6)
a <- commandArgs(TRUE); POOL <- a[1]; FIN <- a[2]
x <- readRDS(file.path(POOL, "tidy_common.rds")); tot <- x$tot; cau <- x$cau; age <- x$age
zr <- function(f) fread(cmd = paste("zcat", shQuote(file.path(FIN, f))))
C17 <- c("ckd", "cvd_cmp", "cvd_htn", "cvd_ihd", "cvd_stroke", "diabetes", "inj_animal", "inj_disaster", "inj_drowning", "inj_homicide", "inj_mech",
         "inj_othunintent", "inj_suicide", "inj_trans_other", "inj_trans_road", "lri", "resp_copd")
INJ <- grep("^inj_", C17, value = TRUE)
W <- tot[unit == "World"]
w <- function(s, p, m) W[scenario == s & period == p & measure == m]
hd <- function(s) cat("\n=====", s, "\n")

hd("World, SSP2-4.5, counts and rates")
print(W[scenario == "ssp245" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt"), .(period, measure, mean, lower95, upper95, rate = round(rate_mean, 2), pop = N, ihme = ihme_deaths)][order(measure, period)])
for (m in c("deaths_heat", "deaths_cold")) cat(m, "change 2022-30 to 2041-50:", round(100 * (w("ssp245", "2041-2050", m)$mean / w("ssp245", "2022-2030", m)$mean - 1), 1), "%\n")
for (p in c("2022-2030", "2041-2050")) cat(p, "heat share of heat+cold:", round(100 * w("ssp245", p, "deaths_heat")$mean / w("ssp245", p, "deaths_nonopt")$mean, 1),
  "%; heat / IHME deaths:", round(100 * w("ssp245", p, "deaths_heat")$mean / w("ssp245", p, "deaths_heat")$ihme_deaths, 2),
  "%; cold / IHME:", round(100 * w("ssp245", p, "deaths_cold")$mean / w("ssp245", p, "deaths_cold")$ihme_deaths, 2), "%\n")
cat("heat share of heat+cold, every scenario, 2041-2050:", W[period == "2041-2050", .(s = round(100 * mean[measure == "deaths_heat"] / mean[measure == "deaths_nonopt"], 1)), by = scenario][, paste(scenario, s, collapse = "; ")], "\n")

hd("Differences from SSP2-4.5, World")
v <- zr("vetting.csv.gz")
print(merge(W[scenario != "ssp245" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt"), .(unit, scenario, period, measure, ratio = round(ratio, 3), diff_mean, diff_lower95, diff_upper95)],
            v[, .(unit, scenario, period, measure, models_above_ref)], by = c("unit", "scenario", "period", "measure"))[period != "2022-2050"][order(scenario, measure, period)])
cl <- tot[unit_type == "location" & period == "2041-2050" & measure == "deaths_heat" & scenario != "ssp245"]
cat("countries, heat diff 2041-2050, 95% interval above / below zero:\n"); print(cl[, .(above = sum(diff_lower95 > 0), below = sum(diff_upper95 < 0)), by = scenario])

hd("Between-model share of variance, World, decades")
print(v[unit == "World" & period != "2022-2050" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt"), .(lo = min(between_share), hi = max(between_share)), by = measure])
r <- w("ssp245", "2041-2050", "deaths_nonopt"); cat("heat+cold 2041-50 SSP2-4.5 interval relative to mean:", round(r$lower95 / r$mean, 3), round(r$upper95 / r$mean, 3), "\n")

hd("Income groups and regions, SSP2-4.5 2041-2050")
G <- tot[unit_type == "group" & period == "2041-2050"]
print(G[scenario == "ssp245" & measure %in% c("deaths_heat", "deaths_cold"), .(unit, measure, mean, rate = round(rate_mean, 1))][order(measure, -rate)])
for (m in c("deaths_heat", "deaths_cold")) cat(m, "LMIC share of World:", round(100 * G[unit == "Low and middle income" & scenario == "ssp245" & measure == m, mean] / G[unit == "World" & scenario == "ssp245" & measure == m, mean], 1), "%\n")
print(G[scenario == "ssp585" & measure == "deaths_heat" & unit %in% c("High income", "Low and middle income", "World"), .(unit, ratio = round(ratio, 3), diff_mean)])
print(G[scenario == "ssp585" & measure %in% c("deaths_heat", "deaths_cold"), .(unit, measure, ratio = round(ratio, 3), diff_mean)][order(measure, -ratio)])

hd("Countries")
t <- tot[unit_type == "location" & scenario == "ssp245" & period == "2041-2050" & measure %in% c("deaths_heat", "deaths_cold")]
t[, sh := mean / sum(mean), by = measure]
print(t[location_id %in% c(6, 163), .(name, measure, mean, lower95, upper95, share = round(100 * sh, 1))])
print(t[, .(top5 = round(100 * sum(sort(sh, decreasing = TRUE)[1:5]), 1), names = paste(name[order(-sh)][1:5], collapse = ", ")), by = measure])
print(t[measure == "deaths_heat"][order(-rate_mean)][1:6, .(name, rate = round(rate_mean, 1), lo = round(rate_lower95, 1), hi = round(rate_upper95, 1))])
an <- zr("annual.csv.gz")[scenario == "ssp245" & measure == "deaths_heat" & year %in% c(2022, 2050)]
ih <- dcast(an, unit ~ year, value.var = "ihme_deaths")[, ratio := round(`2050` / `2022`, 2)]
print(ih[unit %in% c("156", "151", "150", "140", "145", "152", "World")])

hd("Forces of nature: with and without")
ww <- fread(file.path(POOL, "extras", "with_without_inj_disaster_country.csv"))[scenario == "ssp245" & period == "2041-2050" & m == "n"]
wd <- dcast(ww, location_id ~ variant, value.var = "upper95")[, ch := abs(all_causes / without_inj_disaster - 1)]
cat("countries whose 95% upper bound moves by > 2%:", wd[ch > 0.02, .N], "; > 10%:", wd[ch > 0.10, .N], " (", paste(wd[ch > 0.10, location_id], collapse = ","), ")\n")
wg <- fread(file.path(POOL, "extras", "with_without_inj_disaster_group.csv"))[scenario == "ssp245" & period == "2041-2050" & m == "n" & group %in% c("World", "Lower middle income", "Latin America & Caribbean")]
print(wg[, .(group, variant, mean)])

hd("Causes, World, SSP2-4.5, 2041-2050")
cw <- cau[unit == "World" & scenario == "ssp245" & period == "2041-2050" & measure %in% c("deaths_heat", "deaths_cold")]
print(dcast(cw, slice ~ measure, value.var = "mean")[order(-deaths_heat)])
th <- w("ssp245", "2041-2050", "deaths_heat")$mean
cat("IHD + stroke share of heat:", round(100 * cw[measure == "deaths_heat" & slice %in% c("cvd_ihd", "cvd_stroke"), sum(mean)] / th, 1), "%\n")
cat("sum of nine injuries: heat", cw[measure == "deaths_heat" & slice %in% INJ, sum(mean)], " cold", cw[measure == "deaths_cold" & slice %in% INJ, sum(mean)], "; all nine cold negative:", cw[measure == "deaths_cold" & slice %in% INJ, all(mean < 0)], "\n")
cat("World causes whose 95% interval includes zero (of 17):\n"); print(cau[unit == "World" & scenario == "ssp245" & period == "2041-2050" & slice %in% C17 & measure %in% c("deaths_heat", "deaths_cold"), .(incl0 = sum(lower95 < 0 & upper95 > 0)), by = measure])
cc <- cau[unit_type == "location" & scenario == "ssp585" & period == "2041-2050" & slice %in% C17 & measure %in% c("deaths_heat", "deaths_cold") & mean != 0]
cat("country x cause, SSP5-8.5, 2041-2050, non-zero cells and share including zero:\n"); print(cc[, .(cells = .N, incl0 = round(100 * mean(lower95 < 0 & upper95 > 0), 1)), by = measure])
cat("country totals including zero, any scenario/decade, deaths:\n"); print(tot[unit_type == "location" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt") & period != "2022-2050", .(incl0 = sum(lower95 < 0 & upper95 > 0)), by = .(measure, scenario, period)][, .(max_incl0 = max(incl0)), by = measure])

hd("Ages and sexes, World, SSP2-4.5")
A <- age[unit == "World" & scenario == "ssp245" & period %in% c("2022-2030", "2041-2050")]
A[, ag := sub("_sex[12]$", "", slice)]
s <- A[measure %in% c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold"), .(mean = sum(mean)), by = .(period, ag, measure)]
s <- merge(s, W[scenario == "ssp245", .(period, measure, total = mean)], by = c("period", "measure"))[, share := round(100 * mean / total, 1)]
print(s[order(measure, ag, period)])
print(A[slice %in% c("a15_49_sex1", "a15_49_sex2") & measure %in% c("deaths_heat", "deaths_cold"), .(period, slice, measure, mean, lower95, upper95)])
yd <- dcast(A[measure %in% c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold")], period + slice ~ measure, value.var = "mean")
yd[, `:=`(yll_per_heat = round(yll_heat / deaths_heat, 1), yll_per_cold = round(yll_cold / deaths_cold, 1))]
print(yd)

hd("YLLs")
print(W[measure %in% c("yll_heat", "yll_cold", "yll_nonopt") & scenario %in% c("ssp245", "ssp585") & period %in% c("2022-2030", "2041-2050"), .(scenario, period, measure, mean, lower95, upper95, ratio = round(ratio, 3))][order(measure, scenario, period)])
for (p in c("2022-2030", "2041-2050")) cat(p, "heat share of YLLs:", round(100 * w("ssp245", p, "yll_heat")$mean / w("ssp245", p, "yll_nonopt")$mean, 1), "% vs of deaths", round(100 * w("ssp245", p, "deaths_heat")$mean / w("ssp245", p, "deaths_nonopt")$mean, 1), "%\n")
yl <- tot[unit_type == "location" & measure == "yll_cold" & scenario == "ssp245" & period != "2022-2050"]
print(yl[, .(negative = sum(mean < 0), incl0 = sum(lower95 < 0 & upper95 > 0)), by = period][order(period)])
bf <- tot[location_id == 201 & scenario == "ssp126" & period == "2041-2050" & measure %in% c("deaths_cold", "yll_cold"), .(measure, mean)]; print(bf)

hd("Age-standardized rates (GBD world standard)")
asr <- zr("asr.csv.gz")[unit_type == "group" & cause == "all" & sex == "both" & measure %in% c("deaths_heat", "deaths_cold") & scenario %in% c("ssp245", "ssp585")]
print(asr[unit == "World", .(scenario, period, measure, mean, lower95, upper95, diff_mean, diff_lower95, diff_upper95)][order(measure, scenario, period)])
print(dcast(asr[scenario == "ssp245" & period == "2041-2050"], unit ~ measure, value.var = "mean")[order(-deaths_cold)])

hd("Historical, GBD 2019 round")
h <- zr("historical.csv.gz")
print(h[unit == "World" & period == "2019", .(measure, metric, mean, lower95, upper95)])
a22 <- zr("annual.csv.gz")[scenario == "ssp245" & year == 2022]
wn <- a22[unit == "World" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt"), .(measure, ours_2022 = mean, rate = round(mean / pop * 1e5, 2))]
wn <- merge(wn, h[unit == "World" & period == "2019" & metric == "count", .(measure, gbd_2019 = mean)], by = "measure")[, ratio := round(ours_2022 / gbd_2019, 3)]
print(wn)
hc <- merge(a22[unit_type == "location" & measure == "deaths_nonopt", .(unit, ours = mean)], h[unit_type == "location" & period == "2019" & metric == "count" & measure == "deaths_nonopt", .(unit, lo = lower95, hi = upper95, gbd = mean)], by = "unit")
cat("countries whose 2022 heat+cold mean lies inside GBD's 2019 95% interval:", hc[ours >= lo & ours <= hi, .N], "of", nrow(hc), "\n")
cat("  above:", hc[ours > hi, .N], " below:", hc[ours < lo, .N], "\n")
cat("median ratio ours 2022 / GBD 2019 over countries:", round(hc[, median(ours / gbd)], 3), "\n")
print(hc[unit %in% c("133", "163", "6", "102")][, ratio := round(ours / gbd, 2)][])
