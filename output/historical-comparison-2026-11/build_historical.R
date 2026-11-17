# build_historical.R: historical comparison of temperature-attributable deaths.
# Puts GBD's published estimates for past years next to our 2022-2050 results, as crude
# death rates per 100,000 (all ages, both sexes). No pipeline runs.
#
# Sources:
#  - GBD 2019 round, the round our method follows: GHDx "Mortality Burden Attributable to
#    Non-Optimal Temperature 1990-2019" (Burkart et al. 2021 companion data, DOI
#    10.6069/7QWW-PF74). Years 1990, 2010, 2019. Rate and Percent only, no counts. Its
#    Rate is per PERSON (global heat 2019 = 4.62e-05), so it is multiplied by 1e5 here.
#  - GBD 2023 round: GBD Results Tool download (data/gbd-comparison/gbd_A_anchor_2019_2022.csv,
#    untracked, in the main checkout). Years 2019, 2022. Number, Percent and Rate (per
#    100,000), countries only (no world row).
#  - Ours: the 2026-11 pooled results with WBG groups (24 common climate models x 500 draws),
#    decade tables, plus the per-draw world yearly series and IHME's population.
#
# Group rates for GBD (world in the 2023 round; income groups and regions in both rounds)
# are sums of country counts over sums of population: means only, no ranges, since ranges
# cannot be added up across countries. The GBD 2019-round counts need a population: the
# 2023 round's population for 2019 (Number / Rate) is used, so those group values exist
# for 2019 only and carry the difference between the two rounds' populations (not measured).
#
# Usage (repo root, emacs-r distrobox):
#   Rscript output/historical-comparison-2026-11/build_historical.R <gbd2019 csv> <gbd2023 anchor csv> \
#     <pooled dir (multimodel-2026-11-wbg)> <review dir (pop_by_loc.rds, world/world.rds)> <out dir>
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
a <- commandArgs(TRUE)
F19 <- a[1]; F23 <- a[2]; POOL <- a[3]; REV <- a[4]; OUT <- a[5]
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
options(width = 160, datatable.print.nrows = 300)
INC <- "output/report-multimodel-2026-11/income_groups_by_location.csv"
inc <- fread(INC, encoding = "UTF-8")[, .(location_id, name, income, region)]
stopifnot(nrow(inc) == 204)
rk <- function(x) fcase(x %like% "High", "heat", x %like% "Low", "cold", default = "total")

# ---- GBD 2019 round ---------------------------------------------------------------
P <- fread(F19)
P <- P[measure_name == "Deaths" & age_group_name == "All Ages" & metric_name == "Rate"]
P[, risk := rk(rei_name)]
g19w <- P[location_id == 1, .(source = "GBD 2019 round", year = year_id, risk,
                              rate = 1e5 * val, lower = 1e5 * lower, upper = 1e5 * upper)]
g19c <- P[location_id %in% inc$location_id,
          .(location_id, year = year_id, risk, rate = 1e5 * val, lower = 1e5 * lower, upper = 1e5 * upper)]
cat("GBD 2019 round: countries matched", uniqueN(g19c$location_id), "of 204; years",
    paste(sort(unique(g19c$year)), collapse = ", "), "\n")

# ---- GBD 2023 round ---------------------------------------------------------------
G <- fread(F23)[measure_name == "Deaths"]
G[, risk := rk(rei_name)]
g23 <- dcast(G, location_id + year + risk ~ metric_name, value.var = c("val", "lower", "upper"))
g23 <- g23[, .(location_id, year, risk, number = val_Number, rate = val_Rate,
               lower = lower_Rate, upper = upper_Rate, pop = 1e5 * val_Number / val_Rate)]
cat("GBD 2023 round: countries matched", uniqueN(g23[location_id %in% inc$location_id]$location_id),
    "of 204\n")
# population implied by heat, cold and total rows must agree
pchk <- g23[, .(rel = diff(range(pop)) / mean(pop)), by = .(location_id, year)]
cat("GBD 2023 implied population: largest relative disagreement across heat/cold/total rows",
    signif(max(pchk$rel), 3), "\n")
pop23 <- g23[risk == "total", .(location_id, year, pop)]

# ---- Ours: world by year (per draw) -----------------------------------------------
pop <- setDT(readRDS(file.path(REV, "pop_by_loc.rds")))
atomic <- c("Under 5", paste(seq(5, 75, 5), "to", seq(9, 79, 5)), "80 to 84", "85 to 89", "90 to 94", "95 plus")
popw <- pop[age_name %in% atomic, .(N = sum(pop)), by = year]
common <- readRDS(file.path(REV, "model_sets.rds"))$common
W <- setDT(readRDS(file.path(REV, "world", "world.rds"))$ann)[model %in% common]
cat("Ours: climate models", uniqueN(W$model), "; draws", uniqueN(W$draw), "\n")
stopifnot(uniqueN(W$model) == 24)
W[, deaths_total := deaths_heat + deaths_cold]
Wl <- melt(W, id.vars = c("model", "scenario", "year", "draw"),
           measure.vars = c("deaths_heat", "deaths_cold", "deaths_total"), variable.factor = FALSE)
Wl[, risk := sub("deaths_", "", variable)]
Wl <- merge(Wl, popw, by = "year")
q <- function(x, p) quantile(x, p, names = FALSE)
oy <- Wl[, .(deaths = mean(value), rate = 1e5 * mean(value) / N[1],
             lower = 1e5 * q(value, .025) / N[1], upper = 1e5 * q(value, .975) / N[1]),
         by = .(scenario, year, risk)]
fwrite(oy, file.path(OUT, "ours_world_yearly_rates.csv"))

# ---- Ours: decades, countries and groups (pooled tables) ---------------------------
tc <- fread(file.path(POOL, "tables", "country_totals.csv"))
tg <- fread(file.path(POOL, "tables", "world_and_income_groups_totals.csv"))
mk <- function(d) d[measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt"),
                    .(unit, period, scenario, risk = fcase(measure == "deaths_heat", "heat",
                      measure == "deaths_cold", "cold", default = "total"),
                      deaths = mean, rate = rate_mean, lower = rate_lower95, upper = rate_upper95)]
oc <- mk(tc[, unit := as.character(location_id)])
og <- mk(tg)

# ---- World table -------------------------------------------------------------------
g23w <- g23[location_id %in% inc$location_id, .(number = sum(number), pop = sum(pop)), by = .(year, risk)]
g23w[, `:=`(source = "GBD 2023 round", rate = 1e5 * number / pop)]
g19w <- merge(g19w, pop23[location_id %in% inc$location_id, .(pop = sum(pop)), by = year], by = "year", all.x = TRUE)
g19w[, number := rate * pop / 1e5]
world <- rbind(
  g19w[, .(source, period = as.character(year), scenario = "", risk, deaths = number, rate, lower, upper)],
  g23w[, .(source, period = as.character(year), scenario = "", risk, deaths = number, rate, lower = NA_real_, upper = NA_real_)],
  oy[year == 2022, .(source = "Ours", period = "2022", scenario, risk, deaths, rate, lower, upper)],
  og[unit == "World", .(source = "Ours", period, scenario, risk, deaths, rate, lower, upper)])
world[, risk := factor(risk, c("heat", "cold", "total"))]
setorder(world, risk, source, period, scenario)
fwrite(world, file.path(OUT, "world_rates.csv"))
cat("\n== World, deaths per 100,000 (crude); GBD 2023-round world = sum of 204 countries, no range;",
    "GBD 2019-round deaths = rate x GBD 2023-round 2019 population ==\n")
print(world[, .(risk, source, period, scenario, deaths = round(deaths), rate = round(rate, 2),
                lower = round(lower, 2), upper = round(upper, 2))])

# ---- Countries ---------------------------------------------------------------------
cw <- rbind(
  g19c[, .(location_id, col = paste0("gbd19_", year), risk, rate)],
  g23[location_id %in% inc$location_id, .(location_id, col = paste0("gbd23_", year), risk, rate)],
  oc[scenario == "ssp245", .(location_id = as.integer(unit), col = paste0("ours245_", period), risk, rate)])
cwide <- dcast(cw, location_id + risk ~ col, value.var = "rate")
cwide <- merge(inc, cwide, by = "location_id")
setcolorder(cwide, c("location_id", "name", "income", "region", "risk"))
fwrite(cwide, file.path(OUT, "country_rates.csv"))
cat("\nCountry table rows:", nrow(cwide), "(204 x 3 expected)\n")
cat("Countries missing from GBD 2019 round:", paste(setdiff(inc$location_id, g19c$location_id), collapse = ", "), "\n")

# country agreement: ours 2022-2030 (SSP2-4.5) / GBD 2019-round 2019
cwide[, ratio := `ours245_2022-2030` / gbd19_2019]
agree <- cwide[, .(n = sum(is.finite(ratio)),
                   median_ratio = median(ratio[is.finite(ratio)]),
                   within_1.5 = sum(ratio > 1 / 1.5 & ratio < 1.5, na.rm = TRUE),
                   within_2 = sum(ratio > 0.5 & ratio < 2, na.rm = TRUE),
                   sign_differs = sum(sign(`ours245_2022-2030`) != sign(gbd19_2019), na.rm = TRUE)), by = risk]
fwrite(agree, file.path(OUT, "country_agreement.csv"))
cat("\n== Countries: ours 2022-2030 (SSP2-4.5) / GBD 2019 round 2019, crude rates ==\n"); print(agree)
cat("\nLargest heat ratios (ours/GBD 2019 round):\n")
print(cwide[risk == "heat" & is.finite(ratio)][order(-abs(log(abs(ratio))))][1:12,
      .(name, income, gbd19_2019 = round(gbd19_2019, 2), gbd23_2022 = round(gbd23_2022, 2),
        ours = round(`ours245_2022-2030`, 2), ratio = round(ratio, 2))])

# ---- Groups ------------------------------------------------------------------------
grp_long <- rbind(inc[, .(location_id, group = income)], inc[, .(location_id, group = region)],
                  inc[income != "High income" & income != "", .(location_id, group = "Low and middle income")])
gsum <- function(d, lab) {
  x <- merge(d, grp_long, by = "location_id", allow.cartesian = TRUE)
  x[, .(source = lab, deaths = sum(number), rate = 1e5 * sum(number) / sum(pop)), by = .(group, period = as.character(year), risk)]
}
g19n <- merge(g19c[year == 2019], pop23[year == 2019, .(location_id, pop)], by = "location_id")
g19n[, number := rate * pop / 1e5]
grp <- rbind(gsum(g19n, "GBD 2019 round"), gsum(g23[location_id %in% inc$location_id], "GBD 2023 round"), fill = TRUE)
grp <- rbind(grp, og[unit != "World" & scenario == "ssp245", .(group = unit, period, risk, source = "Ours SSP2-4.5", deaths, rate, lower, upper)], fill = TRUE)
grp[, risk := factor(risk, c("heat", "cold", "total"))]
setorder(grp, group, risk, source, period)
fwrite(grp, file.path(OUT, "group_rates.csv"))
cat("\n== Groups, crude rates per 100,000 (GBD values: means only) ==\n")
print(dcast(grp, group + risk ~ source + period, value.var = "rate", fun.aggregate = function(v) round(v, 2)))
saveRDS(list(world = world, oy = oy, cwide = cwide, grp = grp, g19w = g19w, g23w = g23w), file.path(OUT, "historical.rds"))
cat("\ndone\n")
