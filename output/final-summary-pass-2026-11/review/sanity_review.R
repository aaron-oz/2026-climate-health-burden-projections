suppressPackageStartupMessages(library(data.table))
options(width = 220)
TB <- commandArgs(TRUE)[1]
rd <- function(f) fread(cmd = paste("zcat", shQuote(file.path(TB, f))))
fs <- list.files(file.path(TB, "main"), "\\.csv\\.gz$", full.names = TRUE)
main <- rbindlist(lapply(fs, function(f) fread(cmd = paste("zcat", shQuote(f)))), fill = TRUE)
cat("main rows", nrow(main), " units", uniqueN(main$unit), "\n")
for (v in c("source","scenario","period","cause","age","sex","measure","metric")) cat(v, ":", paste(sort(unique(main[[v]])), collapse = " | "), "\n")
cat("\nNA means:", main[is.na(mean), .N], " NA bounds:", main[is.na(lower95) | is.na(upper95), .N], "\n")
cat("ordering violations (lower95<=lower80<=lower66<=mean? no: check lower95<=lower80<=lower66 and upper66<=upper80<=upper95, lower95<=mean<=upper95):",
    main[!(lower95 <= lower80 + 1e-9 & lower80 <= lower66 + 1e-9 & upper66 <= upper80 + 1e-9 & upper80 <= upper95 + 1e-9 & lower95 <= mean + 1e-9 & mean <= upper95 + 1e-9), .N], "\n")
cat("rows per unit: ", paste(range(main[, .N, by = unit]$N), collapse = "-"), "\n")
cat("negative means by measure/metric (count rows only):\n")
print(main[metric == "count" & mean < 0, .N, by = .(measure)])
cat("share of rows with negative mean, by cause and measure (all ages, both sexes, all units):\n")
print(dcast(main[age == "all" & sex == "both", .(neg = round(mean(mean < 0), 3)), by = .(cause, measure)], cause ~ measure, value.var = "neg"))
cat("negative mean, all causes all ages both sexes, by measure and unit type:\n")
print(main[cause == "all" & age == "all" & sex == "both" & mean < 0, .N, by = .(measure, unit_type)])
bad <- main[!(lower95 <= lower80 + 1e-9 & lower80 <= lower66 + 1e-9 & upper66 <= upper80 + 1e-9 & upper80 <= upper95 + 1e-9 & lower95 <= mean + 1e-9 & mean <= upper95 + 1e-9)]
bad[, which := fifelse(mean < lower95 - 1e-9, "mean<lower95", fifelse(mean > upper95 + 1e-9, "mean>upper95", "bounds"))]
print(bad[, .N, by = .(which, cause)][order(-N)][1:15])
print(head(bad[which == "bounds"], 3))
print(head(bad[order(-abs(mean))][, .(unit, scenario, period, cause, age, sex, measure, mean, lower95, upper95)], 5))

tot <- main[cause == "all" & age == "all" & sex == "both" & metric == "count"]
# group additivity of means
loc <- tot[unit_type == "location"]; grp <- tot[unit_type == "group"]
W <- loc[, .(sum_loc = sum(mean)), by = .(scenario, period, measure)]
chk <- merge(grp[unit == "World", .(scenario, period, measure, world = mean)], W)
cat("\nWorld mean vs sum of 204 location means, max rel diff:", chk[, max(abs(world - sum_loc) / abs(world))], "\n")
regions <- c("East Asia & Pacific","Europe & Central Asia","Latin America & Caribbean","Middle East, North Africa, Afghanistan & Pakistan","North America","South Asia","Sub-Saharan Africa")
inc <- c("Low income","Lower middle income","Upper middle income","High income")
for (set in list(regions = regions, income = inc, lmic_hi = c("Low and middle income","High income"), lmic_parts = c("Low income","Lower middle income","Upper middle income"))) {
  s <- grp[unit %in% set, .(s = sum(mean), n = .N), by = .(scenario, period, measure)]
  ref <- if (identical(set, c("Low income","Lower middle income","Upper middle income"))) grp[unit == "Low and middle income", .(scenario, period, measure, r = mean)] else grp[unit == "World", .(scenario, period, measure, r = mean)]
  x <- merge(s, ref); cat("sum of", paste(set, collapse = " + "), "vs reference: max rel diff", x[, max(abs(s - r) / abs(r))], " n per key", unique(x$n), "\n")
}
cat("pop of groups vs world: ", grp[measure == "deaths_heat" & period == "2022-2030" & scenario == "ssp245" & unit %in% regions, sum(pop)] / grp[measure == "deaths_heat" & period == "2022-2030" & scenario == "ssp245" & unit == "World", pop], "\n")

# cause and age additivity, World, ssp245
w <- main[unit == "World" & metric == "count" & sex == "both" & scenario == "ssp245" & period == "2022-2050"]
cat("\nWorld ssp245 2022-2050, all-ages causes:\n"); print(w[age == "all", .(cause, measure, mean)][measure %in% c("deaths_heat","deaths_cold")][order(measure, -mean)], nrows = 60)
cat("\nWorld ssp245 2022-2050, all-causes ages:\n"); print(w[cause == "all", .(age, measure, mean)][measure %in% c("deaths_heat","deaths_cold")][order(measure, age)], nrows = 60)
ws <- main[unit == "World" & metric == "count" & cause == "all" & age == "all" & scenario == "ssp245" & period == "2022-2050" & measure == "deaths_heat", .(sex, mean)]
cat("\nsex split heat:\n"); print(ws)

# World headline and scenario direction
cat("\nWorld all-cause all-age both-sex, mean annual deaths (and 95% UI), by scenario and period:\n")
print(dcast(grp[unit == "World" & measure %in% c("deaths_heat","deaths_cold","deaths_nonopt")], measure + period ~ scenario, value.var = "mean"))
cat("\nWorld diffs vs ssp245 (diff_mean, diff_lower95, diff_upper95), 2041-2050:\n")
print(grp[unit == "World" & period == "2041-2050" & scenario != "ssp245" & measure %in% c("deaths_heat","deaths_cold"), .(scenario, measure, mean, diff_mean, diff_lower95, diff_upper95)])

# rates
rt <- main[unit == "World" & cause == "all" & age == "all" & sex == "both" & scenario == "ssp245" & measure %in% c("deaths_heat","deaths_cold"), .(period, measure, metric, mean)]
cat("\nWorld rates in main (if any):\n"); print(dcast(rt, period + measure ~ metric, value.var = "mean"))
asr <- rd("asr.csv.gz")
cat("\nASR metrics:", paste(unique(asr$metric), collapse = " "), "\n")
a <- asr[unit == "World" & cause == "all" & sex == "both" & scenario == "ssp245" & measure %in% c("deaths_heat","deaths_cold"), .(period, measure, asr = mean)]
cr <- grp[unit == "World" & scenario == "ssp245" & measure %in% c("deaths_heat","deaths_cold"), .(period, measure, crude = mean / pop * 1e5 * 1)]
print(merge(a, cr))
cat("ASR vs crude across groups, 2022-2030 ssp245 (deaths_heat, deaths_cold):\n")
a2 <- merge(asr[unit_type == "group" & cause == "all" & sex == "both" & scenario == "ssp245" & period == "2022-2030" & measure %in% c("deaths_heat","deaths_cold"), .(unit, measure, asr = mean)],
            grp[scenario == "ssp245" & period == "2022-2030" & measure %in% c("deaths_heat","deaths_cold"), .(unit, measure, crude = mean / pop * 1e5)])
print(dcast(a2, unit ~ measure, value.var = c("crude","asr")))

# annual vs decade
an <- rd("annual.csv.gz")
cat("\nannual measures:", paste(unique(an$measure), collapse=" "), " metrics:", paste(unique(an$metric), collapse=" "), "\n")
aw <- an[unit == "World" & scenario == "ssp245" & metric == "count" & measure %in% c("deaths_heat","deaths_cold")]
aw[, period := fifelse(year <= 2030, "2022-2030", fifelse(year <= 2040, "2031-2040", "2041-2050"))]
x <- merge(aw[, .(annual_avg = mean(mean)), by = .(period, measure)], grp[unit == "World" & scenario == "ssp245", .(period, measure, decade = mean)])
cat("average of annual means vs decade mean, World ssp245:\n"); print(x)
cat("World annual heat and cold, ssp245, selected years:\n"); print(dcast(aw[year %in% c(2022, 2025, 2030, 2040, 2050)], year ~ measure, value.var = "mean"))

# historical vs first projection decade
h <- rd("historical.csv.gz")
cat("\nhistorical periods:", paste(unique(h$period), collapse = " "), " metrics:", paste(unique(h$metric), collapse = " "), "\n")
hw <- h[unit == "World" & cause == "all" & age == "all" & sex == "both" & period == max(period)]
print(hw[, .(period, measure, metric, mean)])
cr22 <- an[unit == "World" & scenario == "ssp245" & year == 2022 & metric == "count" & measure %in% c("deaths_heat","deaths_cold"), .(measure, count2022 = mean, rate2022 = mean / pop * 1e5)]
print(cr22)

v <- rd("vetting.csv.gz")
cat("\nvetting: World between-model share (share of variance between models):\n")
print(v[unit == "World" & scenario == "ssp245", .(period, measure, model_min, model_max, between_share, af_mean)])
cr <- rd("crossing.csv.gz"); cat("\ncrossing rows", nrow(cr), "; World:\n"); print(cr[unit == "World"])
tp <- rd("temperature.csv.gz"); cat("\ntemperature: models", uniqueN(tp$model), " locations", uniqueN(tp$location_id), " years", paste(unique(tp$year), collapse=" "), "\n")
print(tp[location_id == 163, .(t = round(mean(temp_popw), 2)), by = .(scenario, year)][order(scenario, year)])
