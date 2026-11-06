# build_tables.R: turns the pooling output into tidy tables with rates per 100,000,
# checks them against independent numbers, and prints what the report needs.
# Usage: Rscript build_tables.R <pool_dir> <review_dir> <repo report tables dir> <income_groups_by_location.csv>
suppressPackageStartupMessages(library(data.table))
setDTthreads(6)
a <- commandArgs(TRUE); POOL <- a[1]; REV <- a[2]; TAB <- a[3]; INC <- a[4]
dir.create(TAB, recursive = TRUE, showWarnings = FALSE)
options(width = 170, datatable.print.nrows = 400)
inc <- fread(INC, encoding = "UTF-8")
names_loc <- inc[, .(location_id, name, income)]

tidy <- function(f) {
  P <- fread(f)
  iv <- function(pre) paste0(pre, c("_mean", "_lower95", "_upper95", "_lower80", "_upper80", "_lower66", "_upper66", "_model_min", "_model_max"))
  nm <- c("mean", "lower95", "upper95", "lower80", "upper80", "lower66", "upper66", "model_min", "model_max")
  ky <- c("unit_type", "unit", "slice", "period", "measure")
  tt <- P[, c(ky, "target", iv("target"), "ratio", "ratio_model_min", "ratio_model_max", iv("diff")), with = FALSE]
  setnames(tt, iv("target"), nm); setnames(tt, "target", "scenario")
  rr <- unique(P[, c(ky, iv("ref")), with = FALSE])
  # the common set gives one reference value per cell; anything else would show up here
  chk <- rr[, .N, by = ky][N > 1]
  cat(basename(dirname(f)), ": rows", nrow(P), "; reference cells with more than one value:", nrow(chk), "\n")
  rr <- rr[, .SD[1], by = ky]
  setnames(rr, iv("ref"), nm); rr[, scenario := "ssp245"]
  rbind(tt, rr, fill = TRUE)
}
tot <- tidy(file.path(POOL, "totals_common", "pooled.csv"))
age <- tidy(file.path(POOL, "age", "pooled.csv"))
cau <- tidy(file.path(POOL, "cause", "pooled.csv"))

# --- population -------------------------------------------------------------
pop <- readRDS(file.path(REV, "pop_by_loc.rds")); setDT(pop)
atomic <- c("Under 5", paste(seq(5, 75, 5), "to", seq(9, 79, 5)), "80 to 84", "85 to 89", "90 to 94", "95 plus")
pop <- pop[age_name %in% atomic]
stopifnot(uniqueN(pop$location_id) == 204)
pop[, period := fifelse(year <= 2030, "2022-2030", fifelse(year <= 2040, "2031-2040", "2041-2050"))]
pop[, sex_id := fifelse(sex_name == "Male", 1L, 2L)]
amap <- rbind(data.table(age_name = "Under 5", ag = "under5"),
              data.table(age_name = paste(seq(15, 45, 5), "to", seq(19, 49, 5)), ag = "a15_49"),
              data.table(age_name = c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 plus"), ag = "a65plus"))
pa <- merge(pop, amap, by = "age_name")
Pall <- pop[, .(N = sum(pop)), by = .(location_id, period, year)][, .(N = mean(N)), by = .(location_id, period)]
Page <- pa[, .(N = sum(pop)), by = .(location_id, period, year, ag, sex_id)][, .(N = mean(N)), by = .(location_id, period, ag, sex_id)]
Page[, slice := paste0(ag, "_sex", sex_id)]
gmap <- rbind(names_loc[, .(location_id, group = income)],
              names_loc[income %in% c("Upper middle income", "Lower middle income", "Low income"), .(location_id, group = "Low and middle income")],
              names_loc[, .(location_id, group = "World")])
popunit <- function(P, extra = NULL) {
  b <- c("period", extra)
  lo <- P[, c("location_id", "N", b), with = FALSE][, `:=`(unit_type = "location", unit = as.character(location_id), location_id = NULL)]
  gr <- merge(P, gmap, by = "location_id", allow.cartesian = TRUE)[, .(N = sum(N)), by = c("group", b)]
  setnames(gr, "group", "unit")[, unit_type := "group"]
  rbind(lo, gr, use.names = TRUE)
}
Ua <- popunit(Pall); Us <- popunit(Page, "slice")
RC <- c("mean", "lower95", "upper95", "lower80", "upper80", "lower66", "upper66")
addrate <- function(x, U, by) {
  y <- merge(x, U, by = by, all.x = TRUE)
  stopifnot(!anyNA(y$N))
  for (v in RC) set(y, j = paste0("rate_", v), value = 1e5 * y[[v]] / y$N)
  y
}
tot <- addrate(tot, Ua, c("unit_type", "unit", "period"))
cau <- addrate(cau, Ua, c("unit_type", "unit", "period"))
age <- addrate(age, Us, c("unit_type", "unit", "period", "slice"))
labelled <- function(x) {
  x <- merge(x[, location_id := fifelse(unit_type == "location", suppressWarnings(as.integer(unit)), NA_integer_)], names_loc, by = "location_id", all.x = TRUE)
  x[unit_type == "group", name := unit]
  x[, asterisk := name %in% c("Haiti", "Honduras")]
  x
}
tot <- labelled(tot); cau <- labelled(cau); age <- labelled(age)
saveRDS(list(tot = tot, cau = cau, age = age), file.path(POOL, "tidy_common.rds"))

# Published tables carry 6 significant digits, the precision of the draw files.
fwrite6 <- function(x, f) { y <- copy(x); for (v in names(y)) if (is.double(y[[v]])) set(y, j = v, value = signif(y[[v]], 6)); fwrite(y, f) }
fwrite6(tot[unit_type == "group"], file.path(TAB, "world_and_income_groups_totals.csv"))
fwrite6(tot[unit_type == "location"], file.path(TAB, "country_totals.csv"))
fwrite6(age[unit_type == "group"], file.path(TAB, "world_and_income_groups_age_sex.csv"))
fwrite6(cau[unit_type == "group"], file.path(TAB, "world_and_income_groups_cause.csv"))
fwrite6(age[unit_type == "location"], file.path(POOL, "country_age_sex.csv.gz"))
fwrite6(cau[unit_type == "location"], file.path(POOL, "country_cause.csv.gz"))

# --- checks -----------------------------------------------------------------
cat("\n== checks ==\n")
rel <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-6))
w <- fread(file.path(REV, "world_common24_decades.csv"))
j <- merge(tot[unit == "World" & slice == "all"], w, by = c("scenario", "period", "measure"), suffixes = c("", ".i"))
cat("World totals vs independent code:", nrow(j), "rows; worst rel mean", signif(rel(j$mean, j$mean.i), 3), "l95", signif(rel(j$lower95, j$l95), 3), "u95", signif(rel(j$upper95, j$u95), 3), "\n")
wc <- fread(file.path(REV, "world_common24_cause.csv"))
j <- merge(cau[unit == "World" & measure == "deaths_heat"], wc, by = c("scenario", "period", "slice"))
cat("World cause heat vs independent:", nrow(j), "rows; worst rel mean", signif(rel(j$mean, j$h), 3), "l95", signif(rel(j$lower95, j$hl), 3), "u95", signif(rel(j$upper95, j$hu), 3), "\n")
j <- merge(cau[unit == "World" & measure == "deaths_cold"], wc, by = c("scenario", "period", "slice"))
cat("World cause cold vs independent:", nrow(j), "rows; worst rel mean", signif(rel(j$mean, j$c), 3), "l95", signif(rel(j$lower95, j$cl), 3), "u95", signif(rel(j$upper95, j$cu), 3), "\n")
wa <- fread(file.path(REV, "world_common24_age.csv"))
j <- merge(age[unit == "World" & measure == "deaths_heat"], wa, by = c("scenario", "period", "slice"))
cat("World age heat vs independent:", nrow(j), "rows; worst rel mean", signif(rel(j$mean, j$h), 3), "l95", signif(rel(j$lower95, j$hl), 3), "u95", signif(rel(j$upper95, j$hu), 3), "\n")
pl <- readRDS(file.path(REV, "loc_pooled_all.rds"))
j <- merge(cau[unit_type == "location" & scenario != "ssp245"], pl, by.x = c("location_id", "slice", "period", "scenario", "measure"), by.y = c("location_id", "slice", "period", "target", "measure"))
cat("Country x cause vs independent:", nrow(j), "rows; worst rel mean", signif(rel(j$mean, j$t_mean), 3), "l95", signif(rel(j$lower95, j$t_l95), 3), "u95", signif(rel(j$upper95, j$t_u95), 3), "\n")
j <- merge(age[unit_type == "location" & scenario != "ssp245"], pl, by.x = c("location_id", "slice", "period", "scenario", "measure"), by.y = c("location_id", "slice", "period", "target", "measure"))
cat("Country x age vs independent:", nrow(j), "rows; worst rel mean", signif(rel(j$mean, j$t_mean), 3), "l95", signif(rel(j$lower95, j$t_l95), 3), "u95", signif(rel(j$upper95, j$t_u95), 3), "\n")
ig <- tot[unit_type == "group" & unit %in% c("High income", "Upper middle income", "Lower middle income", "Low income", "Not classified"), .(s = sum(mean)), by = .(scenario, period, measure)]
j <- merge(ig, tot[unit == "World"], by = c("scenario", "period", "measure"))
cat("Income groups sum to World, worst rel:", signif(rel(j$s, j$mean), 3), "\n")
lm <- tot[unit_type == "group" & unit %in% c("Upper middle income", "Lower middle income", "Low income"), .(s = sum(mean)), by = .(scenario, period, measure)]
j <- merge(lm, tot[unit == "Low and middle income"], by = c("scenario", "period", "measure"))
cat("Three middle/low groups sum to Low and middle income, worst rel:", signif(rel(j$s, j$mean), 3), "\n")
cs <- cau[unit_type == "group", .(s = sum(mean)), by = .(unit, scenario, period, measure)]
j <- merge(cs, tot[unit_type == "group"], by = c("unit", "scenario", "period", "measure"))
cat("Causes sum to totals within groups, worst rel:", signif(rel(j$s, j$mean), 3), "\n")
cat("Population: World", tot[unit == "World" & measure == "deaths_heat" & scenario == "ssp245", paste(period, round(N / 1e9, 2), collapse = "; ")], "billion\n")

# --- what the report needs ----------------------------------------------------
fm <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
cat("\n== World and income groups, deaths and rate, all scenarios ==\n")
print(tot[unit_type == "group" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt")][order(unit, measure, scenario, period)][
  , .(unit, measure, scenario, period, mean = fm(mean), l95 = fm(lower95), u95 = fm(upper95), l66 = fm(lower66), u66 = fm(upper66), rate = round(rate_mean, 1), r_l95 = round(rate_lower95, 1), r_u95 = round(rate_upper95, 1), ratio = round(ratio, 3))])
cat("\n== World and income groups, YLLs (2041-2050) ==\n")
print(tot[unit_type == "group" & measure %in% c("yll_heat", "yll_cold", "yll_nonopt") & period == "2041-2050"][order(unit, measure, scenario)][
  , .(unit, measure, scenario, mean = fm(mean), l95 = fm(lower95), u95 = fm(upper95), rate = round(rate_mean), ratio = round(ratio, 3))])
cat("\n== ssp585 vs ssp245 differences, groups ==\n")
print(tot[unit_type == "group" & scenario != "ssp245" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt") & period == "2041-2050"][order(unit, measure, scenario)][
  , .(unit, measure, scenario, ratio = round(ratio, 3), r_min = round(ratio_model_min, 3), r_max = round(ratio_model_max, 3), d = fm(diff_mean), d_l95 = fm(diff_lower95), d_u95 = fm(diff_upper95), d_l66 = fm(diff_lower66), d_u66 = fm(diff_upper66))])
cat("\n== cause by income group, ssp245, 2041-2050, deaths ==\n")
print(dcast(cau[unit_type == "group" & scenario == "ssp245" & period == "2041-2050" & measure %in% c("deaths_heat", "deaths_cold"), .(unit, slice, measure, v = round(mean))], slice ~ unit + measure, value.var = "v"))
cat("\n== age sex by group, ssp245 2041-2050, deaths, rates ==\n")
print(age[unit_type == "group" & scenario == "ssp245" & period == "2041-2050" & measure %in% c("deaths_heat", "deaths_cold") & unit %in% c("World", "High income", "Low and middle income")][order(unit, measure, slice)][
  , .(unit, measure, slice, mean = fm(mean), l95 = fm(lower95), u95 = fm(upper95), rate = round(rate_mean, 1))])
cat("\n== model set sensitivity, World and two groups, ratio to ssp245, 2041-2050 ==\n")
sens <- rbindlist(lapply(c("common", "paired", "all"), function(s) fread(file.path(POOL, paste0("totals_", s), "pooled.csv"))[
  unit_type == "group" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt"), .(set = s, unit, target, period, measure, n_models, n_models_ref, target_mean, target_lower95, target_upper95, ref_mean, ratio)]))
fwrite(sens, file.path(TAB, "model_set_sensitivity.csv"))
print(dcast(sens[period == "2041-2050" & unit %in% c("World", "High income", "Low and middle income"), .(unit, measure, target, set, r = round(ratio, 3))], unit + measure + target ~ set, value.var = "r"))
print(unique(sens[, .(set, target, n_models, n_models_ref)]))
