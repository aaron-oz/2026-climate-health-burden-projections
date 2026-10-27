# Check the cause and age breakdown exports, and the reference-life-table YLL
# columns, against a recomputation straight from the fixture's input files.
# Usage: Rscript check_breakdown.R <fixture root>
suppressMessages(library(data.table))
R <- commandArgs(TRUE)[1]
O <- file.path(R, "output/summary_workflow_b")
rd <- function(d) rbindlist(lapply(list.files(file.path(O, d), full.names = TRUE), readRDS))
tot <- rd("draws"); cau <- rd("draws_cause"); age <- rd("draws_age")
ok <- function(lbl, cond) { cat(sprintf("  %-66s %s\n", lbl, if (isTRUE(cond)) "PASS" else "FAIL")); invisible(cond) }
rel <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-9))
periods <- list(`2022-2023` = 2022:2023, `2030` = 2030L)
slices <- list(under5 = 0, a15_49 = seq(15, 45, 5), a65plus = seq(65, 80, 5))
reflt <- fread(file.path(R, "ref_lt.csv"))[, .(age_group_id = as.integer(age_start), exr = ex)]

ok("cause file: 17 causes per (location, model, scenario, period, draw)", all(cau[, .N, by = .(location_id, model, scenario, period, draw)]$N == 17))
ok("age file: 3 age groups x 2 sexes per (location, model, scenario, period, draw)", all(age[, .N, by = .(location_id, model, scenario, period, draw)]$N == 6))
ok("same (location, model, scenario) runs in all three files",
   setequal(unique(tot[, paste(location_id, model, scenario)]), unique(cau[, paste(location_id, model, scenario)])) &&
   setequal(unique(tot[, paste(location_id, model, scenario)]), unique(age[, paste(location_id, model, scenario)])))
cs <- cau[, lapply(.SD, sum), by = .(location_id, model, scenario, period, draw), .SDcols = c("deaths_heat", "yll_cold", "yllref_heat")]
m <- merge(cs, tot, by = c("location_id", "model", "scenario", "period", "draw"))
ok(sprintf("causes sum to the total (worst rel. dev %.1e)", max(rel(m$deaths_heat.x, m$deaths_heat.y), rel(m$yll_cold.x, m$yll_cold.y), rel(m$yllref_heat.x, m$yllref_heat.y))),
   max(rel(m$deaths_heat.x, m$deaths_heat.y), rel(m$yll_cold.x, m$yll_cold.y), rel(m$yllref_heat.x, m$yllref_heat.y)) < 2e-4)

# recompute age groups and reference YLLs for one target and one reference run
for (case in list(list(loc = 125L, model = "m2", scen = "ssp585"), list(loc = 126L, model = "m3", scen = "ssp245"))) {
  e <- rbindlist(lapply(names(periods), function(p) rbindlist(lapply(periods[[p]], function(y) {
    x <- if (case$scen == "ssp245") {
      b <- setDT(readRDS(file.path(R, "output/results/cckp", case$loc, paste0(case$model, "-ssp245"), sprintf("burden_%d.rds", y))))
      b[, .(age_group_id, sex_id, draw, h = deaths_heat, c = deaths_cold)]
    } else {
      w <- setDT(readRDS(file.path(R, "output/results/workflow_b", case$loc, paste0(case$model, "-", case$scen), sprintf("wb_%d.rds", y))))
      w[, .(age_group_id, sex_id, draw, h = deaths_heat_attrib, c = deaths_cold_attrib)]
    }
    x <- merge(x, reflt, by = "age_group_id", all.x = TRUE)
    rbindlist(lapply(names(slices), function(a) x[age_group_id %in% slices[[a]],
      .(h = sum(h), rh = sum(h * exr)), by = .(sex_id, draw)][, age_group := a]))[, period := p]
  }))))[, .(h = sum(h), rh = sum(rh)), by = .(period, age_group, sex_id, draw)]
  g <- merge(age[location_id == case$loc & model == case$model & scenario == case$scen], e,
             by = c("period", "age_group", "sex_id", "draw"))
  ok(sprintf("loc %d %s %s: age groups and reference YLLs recompute (%d rows, worst %.1e)", case$loc, case$model, case$scen,
             nrow(g), max(rel(g$deaths_heat, g$h), rel(g$yllref_heat, g$rh))),
     nrow(g) == 2 * 6 * 500 && max(rel(g$deaths_heat, g$h), rel(g$yllref_heat, g$rh)) < 1e-4)
}
ok("values rounded to 6 significant digits", all(tot$deaths_heat == signif(tot$deaths_heat, 6)))
