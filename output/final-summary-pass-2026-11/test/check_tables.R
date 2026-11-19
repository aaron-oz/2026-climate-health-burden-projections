# check_tables.R: checks util_final_tables.R's output against numbers computed
# directly from the fixture's raw results files (wb_*.rds, burden_*.rds), the
# life tables and the population file, with independent, deliberately plain code.
# Usage (repo root): Rscript output/final-summary-pass-2026-11/test/check_tables.R <fixture root> <std_pop csv>
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); FX <- a[1]; STD <- a[2]
RES <- file.path(FX, "output/results"); TB <- file.path(FX, "tables")
common <- c("m1", "m2", "m3"); YEARS <- 2022:2050
pop <- fread(file.path(FX, "inputs/population.csv")); std <- fread(STD)
lt <- function(l) fread(sprintf("data/lifetables/%d_lifetable.csv", l))[, .(year = year_id, age_group_id, sex_id, ex)]
groups <- list(cardiovascular = c("cvd_ihd", "cvd_stroke", "cvd_htn", "cvd_cmp"), diabetes_kidney = c("diabetes", "ckd"))
raw <- function(l, m, s, y) {
  if (s == "ssp245") { b <- setDT(readRDS(sprintf("%s/cckp/%d/%s-%s/burden_%d.rds", RES, l, m, s, y)))
    b[, .(acause, age_group_id, sex_id, draw, heat = deaths_heat, cold = deaths_cold, ihme = deaths)] }
  else { w <- setDT(readRDS(sprintf("%s/workflow_b/%d/%s-%s/wb_%d.rds", RES, l, m, s, y)))
    w[, .(acause, age_group_id, sex_id, draw, heat = deaths_heat_attrib, cold = deaths_cold_attrib, ihme = ihme_deaths)] }
}
# per (model, draw) values of one cell, summed over locations and years, per year
cell <- function(ls, s, yrs, f) rbindlist(lapply(common, function(m) {
  v <- rbindlist(lapply(ls, function(l) rbindlist(lapply(yrs, function(y) {
    d <- raw(l, m, s, y); d[, `:=`(year = y, location_id = l)]; f(d) }))))
  v[, .(val = sum(val) / length(yrs)), by = draw][, model := m]
}))
st <- function(v) c(mean = mean(v), lower95 = quantile(v, .025, names = FALSE), upper95 = quantile(v, .975, names = FALSE),
                    lower80 = quantile(v, .1, names = FALSE), upper66 = quantile(v, .83, names = FALSE))
cmp <- function(label, got, want) {
  rel <- max(abs(got - want) / pmax(abs(want), 1e-9))
  ok <- rel < 2e-5
  cat(sprintf("%s  %-70s max rel diff %.1e\n", if (ok) "PASS" else "FAIL", label, rel))
  if (!ok) { print(rbind(got = got, want = want)) }
  invisible(ok)
}
main <- function(u) fread(cmd = paste("zcat", shQuote(file.path(TB, "main", paste0(u, ".csv.gz")))))
pick <- function(dt, ...) { x <- dt[...]; stopifnot(nrow(x) == 1); x }
res <- c()

# 1. one location, a cause group x age group x sex, target with difference from the reference
f1 <- function(d) d[acause %in% groups$cardiovascular & age_group_id >= 65 & sex_id == 2, .(val = sum(heat)), by = draw]
t1 <- cell(125L, "ssp585", 2022:2030, f1); r1 <- cell(125L, "ssp245", 2022:2030, f1)
x <- pick(main("location_125"), period == "2022-2030" & scenario == "ssp585" & cause == "cardiovascular" & age == "a65plus" & sex == "female" & measure == "deaths_heat")
res <- c(res, cmp("loc 125, 2022-30, SSP5-8.5, cardiovascular 65+ female heat deaths",
                  unlist(x[, .(mean, lower95, upper95, lower80, upper66)]), st(t1$val)))
dd <- merge(t1, r1, by = c("model", "draw"))[, val.x - val.y]
res <- c(res, cmp("   ... difference from SSP2-4.5, draw by draw", unlist(x[, .(diff_mean, diff_lower95, diff_upper95, diff_lower80, diff_upper66)]), st(dd)))
res <- c(res, cmp("   ... n_models = 3 (m4, m5 left out)", x$n_models, 3))

# 2. a group (World = all three), cold deaths, all ages, causes, sexes
f2 <- function(d) d[, .(val = sum(cold)), by = draw]
t2 <- cell(c(125L, 163L, 102L), "ssp126", 2041:2050, f2)
x <- pick(main("group_World"), period == "2041-2050" & scenario == "ssp126" & cause == "all" & age == "all" & sex == "both" & measure == "deaths_cold")
res <- c(res, cmp("World, 2041-50, SSP1-2.6, all cold deaths", unlist(x[, .(mean, lower95, upper95, lower80, upper66)]), st(t2$val)))

# 3. YLLs: deaths x life expectancy for the location, year, age and sex
ex163 <- lt(163L)
f3 <- function(d) { d <- merge(d[acause == "lri" & age_group_id == 80 & sex_id == 1], ex163, by.x = c("year", "age_group_id", "sex_id"), by.y = c("year", "age_group_id", "sex_id"))
  d[, .(val = sum(heat * ex)), by = draw] }
t3 <- cell(163L, "ssp370", 2031:2040, f3)
x <- pick(main("location_163"), period == "2031-2040" & scenario == "ssp370" & cause == "lri" & age == "80+" & sex == "male" & measure == "yll_heat")
res <- c(res, cmp("loc 163, 2031-40, SSP3-7.0, LRI 80+ male heat YLLs", unlist(x[, .(mean, lower95, upper95, lower80, upper66)]), st(t3$val)))

# 4. the whole period is the per-year average over 2022-2050
f4 <- function(d) { d <- merge(d, lt(125L), by = c("year", "age_group_id", "sex_id")); d[, .(val = sum(cold * ex)), by = draw] }
t4 <- cell(125L, "ssp245", YEARS, f4)
x <- pick(main("location_125"), period == "2022-2050" & scenario == "ssp245" & cause == "all" & age == "all" & sex == "both" & measure == "yll_cold")
res <- c(res, cmp("loc 125, 2022-50, SSP2-4.5, all cold YLLs (per year)", unlist(x[, .(mean, lower95, upper95, lower80, upper66)]), st(t4$val)))

# 5. annual table, with the difference
f5 <- function(d) d[, .(val = sum(heat + cold)), by = draw]
t5 <- cell(102L, "ssp370", 2035L, f5); r5 <- cell(102L, "ssp245", 2035L, f5)
an <- fread(cmd = paste("zcat", shQuote(file.path(TB, "annual.csv.gz"))))
x <- pick(an, unit == "102" & year == 2035 & scenario == "ssp370" & measure == "deaths_nonopt")
res <- c(res, cmp("annual: loc 102, 2035, SSP3-7.0, non-optimal deaths", unlist(x[, .(mean, lower95, upper95, lower80, upper66)]), st(t5$val)))
res <- c(res, cmp("   ... difference", unlist(x[, .(diff_mean, diff_lower95, diff_upper95, diff_lower80, diff_upper66)]),
                  st(merge(t5, r5, by = c("model", "draw"))[, val.x - val.y])))

# 6. age-standardized rate: sum over age of w_a x deaths_a / pop_a (both sexes, all causes), per 100,000
w <- std$weight / sum(std$weight); names(w) <- std$age_group_id
P <- pop[location_id == 102 & year %in% 2031:2040, .(p = sum(pop)), by = .(year, age_group_id)][, .(p = mean(p)), by = age_group_id]
f6 <- function(d) { a <- d[, .(dth = sum(heat)), by = .(draw, age_group_id)]; a <- merge(a, P, by = "age_group_id")
  a[, .(val = sum(w[as.character(age_group_id)] * dth / p) * 1e5), by = draw] }
t6 <- cell(102L, "ssp245", 2031:2040, f6)
as <- fread(cmd = paste("zcat", shQuote(file.path(TB, "asr.csv.gz"))))
x <- pick(as, unit == "102" & period == "2031-2040" & scenario == "ssp245" & sex == "both" & cause == "all" & measure == "deaths_heat")
res <- c(res, cmp("ASR: loc 102, 2031-40, SSP2-4.5, heat deaths, both sexes", unlist(x[, .(mean, lower95, upper95, lower80, upper66)]), st(t6$val)))

# 7. population and IHME deaths columns
x <- pick(main("location_125"), period == "2041-2050" & scenario == "ssp245" & cause == "all" & age == "a15_64" & sex == "female" & measure == "deaths_heat")
res <- c(res, cmp("pop column: loc 125, 2041-50, women 15-64", x$pop,
                  pop[location_id == 125 & year %in% 2041:2050 & sex_id == 2 & age_group_id %in% seq(15, 60, 5), sum(pop) / 10]))
x <- pick(main("location_125"), period == "2022-2030" & scenario == "ssp585" & cause == "diabetes_kidney" & age == "all" & sex == "both" & measure == "deaths_heat")
ihd <- rbindlist(lapply(2022:2030, function(y) raw(125L, "m1", "ssp245", y)[acause %in% groups$diabetes_kidney, .(v = sum(ihme)), by = draw]))[, sum(v) / 9 / uniqueN(draw)]
res <- c(res, cmp("ihme_deaths column: loc 125, 2022-30, diabetes and kidney", x$ihme_deaths, ihd))

# 8. attributable fraction in the vetting table: deaths / IHME deaths, per draw
f8 <- function(d) d[, .(val = sum(heat + cold), ih = sum(ihme)), by = draw]
t8 <- rbindlist(lapply(common, function(m) rbindlist(lapply(c(125L, 163L, 102L), function(l) rbindlist(lapply(2022:2030, function(y) f8(raw(l, m, "ssp245", y))))))[
  , .(val = sum(val), ih = sum(ih)), by = draw][, model := m]))
vt <- fread(cmd = paste("zcat", shQuote(file.path(TB, "vetting.csv.gz"))))
x <- pick(vt, unit == "World" & period == "2022-2030" & scenario == "ssp245" & measure == "deaths_nonopt")
res <- c(res, cmp("vetting: World, 2022-30, attributable fraction (non-optimal)", unlist(x[, .(af_mean, af_lower95, af_upper95, af_lower80, af_upper66)]), st(t8$val / t8$ih)))

# 9. the compact copy rebuilds a raw file at full detail
source(file.path(FX, "compact", "read_compact.R"))
xc <- read_compact(file.path(FX, "compact"), 163); fc <- read_factor(xc, "m2", "ssp126"); rb <- rebuild_combo(xc, fc, 2044)
w0 <- setDT(readRDS(sprintf("%s/workflow_b/163/m2-ssp126/wb_2044.rds", RES)))
got <- rb$heat[cbind(match(w0$draw, xc$draws), w0$sex_id, match(w0$age_group_id, xc$ages), match(w0$acause, xc$causes))]
res <- c(res, cmp("compact copy rebuilds wb_2044.rds (loc 163, m2, SSP1-2.6) cell by cell", sum(abs(got - w0$deaths_heat_attrib)) / sum(w0$deaths_heat_attrib) + 1, 1))

cat(sprintf("\n%d of %d checks pass\n", sum(res), length(res)))
if (!all(res)) quit(status = 1)
