# build_historical_table.R: GBD 2019-round historical estimates as a long table in the
# shape of the projection tables, so both can be queried together (decided 2026-11-18,
# docs/meetings/summary-tables-request-2026-11-16.org section C).
#
# Inputs (all in <gbd dir> = ~/data/wb-temp-attr-projections/review/gbd-2019-results/):
#  - IHME_MORTALITY_TEMPERATURE_1990_2019_DATA_Y2021M09D27.CSV: GBD 2019-round deaths
#    attributable to heat, cold and both, as Rate (per PERSON in this file) and Percent
#    (a fraction of all deaths), all ages and age-standardized, 1990, 2010, 2019, 95% ranges.
#  - gbd2019_pop_allages_1990_2010_2019.csv: GBD 2019-round population, all ages, both
#    sexes, extracted from IHME_GBD_2019_POP_<year>_Y2020M10D15.CSV (GHDx, DOI
#    10.6069/7EGY-0354; downloaded 2026-11-19) by the extraction step below.
#
# Output rows (one per unit x year x measure x metric):
#  - countries and world: crude rate and age-standardized rate per 100,000 and percent of
#    all deaths, mean and 95% range as published; deaths = crude rate x GBD population
#    (mean only: the population's own uncertainty is not carried).
#  - World Bank income groups, low and middle income, regions: deaths = sum of country
#    deaths; crude rate = deaths / group population x 100,000. Means only (ranges cannot
#    be summed across countries). No age-standardized rate or percent for groups (they
#    need age-specific or all-cause numbers this file does not have).
# Columns follow the projection tables: source, scenario ("historical"), unit_type, unit,
# location_id, period (the year), measure (deaths_heat, deaths_cold, deaths_nonopt), metric
# (deaths, rate_crude, rate_asr, percent), mean, lower95, upper95, lower80, upper80, lower66,
# upper66 (80% and 66% are NA: GBD publishes 95% only).
#
# Usage (repo root, emacs-r distrobox):
#   Rscript output/historical-comparison-2026-11/build_historical_table.R <gbd dir> <out csv>
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); D <- a[1]; OUT <- a[2]
inc <- fread("output/report-multimodel-2026-11/income_groups_by_location.csv", encoding = "UTF-8")
stopifnot(nrow(inc) == 204)

# --- population (extract once from the GHDx files) --------------------------------------
pf <- file.path(D, "gbd2019_pop_allages_1990_2010_2019.csv")
if (!file.exists(pf)) {
  p <- rbindlist(lapply(c(1990, 2010, 2019), function(y)
    fread(sprintf("%s/pop/IHME_GBD_2019_POP_%d_Y2020M10D15.CSV", D, y))[age_group_id == 22 & sex_id == 3]))
  fwrite(p[, .(location_id, location_name, year = year_id, pop = val, pop_lower = lower, pop_upper = upper)], pf)
}
pop <- fread(pf)
stopifnot(all(inc$location_id %in% pop$location_id), 1 %in% pop$location_id)

# --- GBD 2019-round estimates ------------------------------------------------------------
P <- fread(file.path(D, "IHME_MORTALITY_TEMPERATURE_1990_2019_DATA_Y2021M09D27.CSV"))[measure_name == "Deaths"]
P[, measure := fcase(rei_id == 337, "deaths_heat", rei_id == 338, "deaths_cold", rei_id == 331, "deaths_nonopt")]
P[, metric := fcase(metric_name == "Rate" & age_group_id == 22, "rate_crude",
                    metric_name == "Rate" & age_group_id == 27, "rate_asr",
                    metric_name == "Percent" & age_group_id == 22, "percent")]
P <- P[!is.na(metric) & (location_id == 1 | location_id %in% inc$location_id)]
mult <- c(rate_crude = 1e5, rate_asr = 1e5, percent = 100)  # file: rate per person, percent as fraction
P[, `:=`(mean = val * mult[metric], lower95 = lower * mult[metric], upper95 = upper * mult[metric])]
P[, `:=`(unit_type = fifelse(location_id == 1, "world", "location"), year = year_id)]
P <- merge(P, inc[, .(location_id, name)], by = "location_id", all.x = TRUE)
P[location_id == 1, name := "World"]
pub <- P[, .(unit_type, unit = name, location_id, year, measure, metric, mean, lower95, upper95)]

# deaths for countries and world: crude rate x population (mean only)
cr <- merge(P[metric == "rate_crude", .(unit_type, unit = name, location_id, year, measure, rate = mean)],
            pop[, .(location_id, year, pop)], by = c("location_id", "year"))
dth <- cr[, .(unit_type, unit, location_id, year, measure, metric = "deaths", mean = rate * pop / 1e5,
              lower95 = NA_real_, upper95 = NA_real_)]

# groups from countries
gl <- rbind(inc[, .(location_id, group = income)], inc[, .(location_id, group = region)],
            inc[income %in% c("Low income", "Lower middle income", "Upper middle income"), .(location_id, group = "Low and middle income")])
gl <- gl[group != "" & !is.na(group)]
cd <- merge(cr[unit_type == "location"], gl, by = "location_id", allow.cartesian = TRUE)
grp <- cd[, .(deaths = sum(rate * pop / 1e5), pop = sum(pop)), by = .(group, year, measure)]
grows <- rbind(grp[, .(unit_type = "group", unit = group, location_id = NA_integer_, year, measure, metric = "deaths",
                       mean = deaths, lower95 = NA_real_, upper95 = NA_real_)],
               grp[, .(unit_type = "group", unit = group, location_id = NA_integer_, year, measure, metric = "rate_crude",
                       mean = 1e5 * deaths / pop, lower95 = NA_real_, upper95 = NA_real_)])

H <- rbind(pub, dth, grows)
H[, `:=`(source = "GBD 2019 round", scenario = "historical", period = as.character(year),
         lower80 = NA_real_, upper80 = NA_real_, lower66 = NA_real_, upper66 = NA_real_)]
setcolorder(H, c("source", "scenario", "unit_type", "unit", "location_id", "period", "measure", "metric",
                 "mean", "lower95", "upper95", "lower80", "upper80", "lower66", "upper66"))
H[, year := NULL]
setorder(H, unit_type, unit, period, measure, metric)
fwrite(H, OUT)

# --- checks -------------------------------------------------------------------------------
cat("rows:", nrow(H), "\n"); print(H[, .N, by = .(unit_type, metric)])
w <- merge(H[unit == "World" & metric == "deaths", .(period, measure, world = mean)],
           H[unit_type == "location" & metric == "deaths", .(sum204 = sum(mean)), by = .(period, measure)])
w[, ratio := sum204 / world]
cat("\nWorld deaths (published global rate x global population) against the sum of 204 countries:\n"); print(w)
chk <- H[unit_type == "group" & metric == "deaths" & unit %in% c("Sub-Saharan Africa", "East Asia & Pacific", "Europe & Central Asia",
          "Latin America & Caribbean", "Middle East, North Africa, Afghanistan & Pakistan", "North America", "South Asia"),
         .(regions = sum(mean)), by = .(period, measure)]
cat("\nSeven regions sum to the 204-country total:\n")
print(merge(chk, w[, .(period, measure, sum204)])[, d := regions / sum204 - 1][])
cat("\nGroup crude rates per 100,000, 2019:\n")
print(dcast(H[unit_type %in% c("group", "world") & metric == "rate_crude" & period == "2019"], unit ~ measure, value.var = "mean",
            fun.aggregate = function(v) round(v, 2)))
