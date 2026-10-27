# What drives the rise in heat deaths between two years (default 2022 and 2050)?
#
# Heat deaths in a location are a sum over age-sex cells c:
#   H = sum_c  P * s_c * r_c * f_c
#   P    total population of the location (all ages, both sexes)
#   s_c  share of the population in cell c (age structure)
#   r_c  deaths per person in c from the 17 temperature-related causes
#        (IHME forecast death rates)
#   f_c  share of those deaths attributable to heat (the climate, plus any
#        shift in cause mix within the cell)
# by_age_sex.csv only holds 2022, 2030, 2040 and 2050, so the comparison is
# between single years, not decades. The change in H between the two years
# is split into four additive parts, one per factor, with the
# Shapley (Das Gupta) method: each factor's part is its average effect over
# all 24 orders of switching the factors from the first period to the second.
# The parts sum exactly to the change. Done per location, model and scenario;
# world parts are sums of location parts.
#
# Inputs: the unscaled scenario summaries' by_age_sex.csv (pipeline burden;
# the Workflow B adjustment moves global totals by under 0.15%), and IHME's
# population forecast (gbd-forecasts/population.csv, mean of draws).
# Models: the 24 run for all four scenarios.
#
# Usage: Rscript decompose_heat.R <scenario bundles dir> <population.csv> <GBD hierarchy xlsx> <out dir> [from year] [to year]
suppressMessages({ library(data.table); library(readxl) })
a <- commandArgs(TRUE); SC <- a[1]; POPF <- a[2]; HIER <- a[3]; OUT <- a[4]
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
scens <- c("ssp126", "ssp245", "ssp370", "ssp585")
p1 <- if (length(a) >= 5) as.integer(a[5]) else 2022L
p2 <- if (length(a) >= 6) as.integer(a[6]) else 2050L
cat("comparing", p1, "with", p2, "\n")

# --- burden by age and sex --------------------------------------------------
b <- rbindlist(lapply(scens, function(s)
  fread(file.path(SC, s, s, "by_age_sex.csv"),
        select = c("location_id", "model", "scenario", "year", "age_group_id", "sex_id", "deaths", "deaths_heat"))))
common <- Reduce(intersect, split(b$model, b$scenario))
cat("models in all four scenarios:", length(common), "\n")
b <- b[model %in% common & (year %in% p1 | year %in% p2)]
b[, period := fifelse(year %in% p1, "p1", "p2")]
# deaths should not depend on the climate model
dv <- b[, .(sd = sd(deaths), m = mean(deaths)), by = .(location_id, scenario, year, age_group_id, sex_id)]
cat("worst relative spread of 17-cause deaths across models:", signif(max(dv$sd / pmax(dv$m, 1e-9), na.rm = TRUE), 3), "\n")
bp <- b[, .(D = sum(deaths) / uniqueN(year), H = sum(deaths_heat) / uniqueN(year)),
        by = .(location_id, model, scenario, period, age = age_group_id, sex = sex_id)]

# --- population -------------------------------------------------------------
pop <- fread(POPF)
setnames(pop, c("Location", "Age Group", "Sex", "Year"), c("loc_name", "age_name", "sex_name", "year"))
dcols <- grep("^draw_", names(pop), value = TRUE)
pop <- pop[sex_name %in% c("Male", "Female") & (year %in% p1 | year %in% p2)]
age_map <- data.table(age_name = c("Under 5", paste(seq(5, 75, 5), "to", seq(9, 79, 5)),
                                   "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                      age = c(seq(0, 75, 5), 80, 80, 80, 80))
pop <- merge(pop, age_map, by = "age_name")
pop[, pop := rowMeans(.SD), .SDcols = dcols]
pop[, sex := fifelse(sex_name == "Male", 1L, 2L)]
h <- as.data.table(read_excel(HIER))
h <- unique(h[Level == 3, .(location_id = as.integer(`Location ID`), loc_name = `Location Name`)])
# Names that differ between the population file and the hierarchy, matched by GBD id.
h <- rbind(h, data.table(location_id = c(205L, 8L, 155L),
                         loc_name = c("C\u00f4te d'Ivoire", "Taiwan (Province of China)", "Turkey")))
h <- unique(h, by = "loc_name")
pop <- merge(pop[, .(loc_name, age, sex, year, pop)], h, by = "loc_name", all.x = TRUE)
cat("population locations without a GBD id:", paste(unique(pop[is.na(location_id), loc_name]), collapse = "; "), "\n")
pop <- pop[!is.na(location_id)]
pop[, period := fifelse(year %in% p1, "p1", "p2")]
pp <- pop[, .(N = sum(pop) / uniqueN(year)), by = .(location_id, period, age, sex)]
miss <- setdiff(unique(bp$location_id), unique(pp$location_id))
cat("burden locations without population:", length(miss), if (length(miss)) paste(miss, collapse = ","), "\n")

# --- factors and Shapley decomposition ---------------------------------------
x <- merge(bp, pp, by = c("location_id", "period", "age", "sex"))
x[, Ptot := sum(N), by = .(location_id, model, scenario, period)]
x[, `:=`(s = N / Ptot, r = fifelse(N > 0, D / N, 0), f = fifelse(D > 0, H / D, 0))]
w <- dcast(x, location_id + model + scenario + age + sex ~ period, value.var = c("Ptot", "s", "r", "f", "H"), fill = 0)
fac <- c("P", "s", "r", "f")
val <- function(S) {   # heat deaths with the factors in S at period 2, the rest at period 1
  g <- function(k, col) if (k %in% S) w[[paste0(col, "_p2")]] else w[[paste0(col, "_p1")]]
  g("P", "Ptot") * g("s", "s") * g("r", "r") * g("f", "f")
}
subsets <- unlist(lapply(0:4, function(k) combn(fac, k, simplify = FALSE)), recursive = FALSE)
key <- function(S) if (length(S)) paste(sort(S), collapse = "+") else "none"
vals <- lapply(subsets, val); names(vals) <- vapply(subsets, key, "")
for (k in fac) {
  e <- 0
  for (S in subsets) if (!k %in% S) {
    wt <- factorial(length(S)) * factorial(3 - length(S)) / factorial(4)
    e <- e + wt * (vals[[key(c(S, k))]] - vals[[key(S)]])
  }
  w[, (paste0("eff_", k)) := e]
}
w[, `:=`(H1 = vals[[key(character(0))]], H2 = vals[[key(fac)]])]
cat("reconstruction check, worst |H rebuilt - H| (deaths):",
    signif(max(abs(w$H1 - w$H_p1), abs(w$H2 - w$H_p2)), 3), "\n")

# location x model x scenario, then world
lm <- w[, lapply(.SD, sum), by = .(location_id, model, scenario), .SDcols = c("H1", "H2", paste0("eff_", fac))]
fwrite(lm, file.path(OUT, "decomposition_location_model.csv"))
wm <- lm[, lapply(.SD, sum), by = .(model, scenario), .SDcols = c("H1", "H2", paste0("eff_", fac))]
pct <- function(d) d[, `:=`(change = H2 - H1)][, .(scenario, model,
  heat_from = H1, heat_to = H2, change_pct = 100 * change / H1,
  population_size_pct = 100 * eff_P / H1, age_structure_pct = 100 * eff_s / H1,
  death_rates_pct = 100 * eff_r / H1, heat_share_pct = 100 * eff_f / H1)]
wp <- pct(copy(wm))
fwrite(wp, file.path(OUT, "decomposition_world_by_model.csv"))
cols <- setdiff(names(wp), c("scenario", "model"))
world <- wp[, c(list(models = .N), lapply(.SD, mean)), by = scenario, .SDcols = cols]
rng <- wp[, .(heat_share_pct_min = min(heat_share_pct), heat_share_pct_max = max(heat_share_pct),
              change_pct_min = min(change_pct), change_pct_max = max(change_pct)), by = scenario]
world <- merge(world, rng, by = "scenario")
fwrite(world, file.path(OUT, "decomposition_world.csv"))
cat("\n== World: change in heat deaths,", p1, "to", p2, ", and its four parts (% of the", p1, "level; mean over models)\n")
print(world[, .(scenario, models, heat_from = round(heat_from), heat_to = round(heat_to),
                change_pct, population_size_pct, age_structure_pct, death_rates_pct, heat_share_pct,
                heat_share_pct_min, heat_share_pct_max)], digits = 3)

# location level, ensemble mean, for the largest contributors
lmean <- lm[, lapply(.SD, mean), by = .(location_id, scenario), .SDcols = c("H1", "H2", paste0("eff_", fac))]
fwrite(lmean, file.path(OUT, "decomposition_location.csv"))
lmean <- merge(lmean, h, by = "location_id", all.x = TRUE)
cat("\n== ssp245: the 10 locations with the largest rise in heat deaths (deaths per year)\n")
top <- lmean[scenario == "ssp245"][order(-(H2 - H1))][1:10]
print(top[, .(loc_name, heat_from = round(H1), change = round(H2 - H1), population_size = round(eff_P),
              age_structure = round(eff_s), death_rates = round(eff_r), heat_share = round(eff_f))])
