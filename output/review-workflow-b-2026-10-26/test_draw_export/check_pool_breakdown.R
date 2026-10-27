# Check pooling of the cause and age breakdowns: slices present, pooled means
# of the causes add up to the pooled total, and one age cell recomputes.
# Usage: Rscript check_pool_breakdown.R <fixture root>
suppressMessages(library(data.table))
R <- commandArgs(TRUE)[1]
ok <- function(lbl, cond) { cat(sprintf("  %-66s %s\n", lbl, if (isTRUE(cond)) "PASS" else "FAIL")); invisible(cond) }
q <- function(x, p) unname(quantile(x, p, names = FALSE))
tot <- fread(file.path(R, "output/pool_common/pooled.csv"))
cau <- fread(file.path(R, "output/pool_cause/pooled.csv"))
age <- fread(file.path(R, "output/pool_age/pooled.csv"))
ok("totals pooled with slice 'all' only", identical(unique(tot$slice), "all"))
ok("17 cause slices", uniqueN(cau$slice) == 17)
ok("6 age-sex slices", setequal(unique(age$slice), paste0(rep(c("under5", "a15_49", "a65plus"), each = 2), "_sex", 1:2)))
cs <- cau[, .(s = sum(target_mean), r = sum(ref_mean)), by = .(unit, target, period, measure)]
m <- merge(cs, tot, by = c("unit", "target", "period", "measure"))
dev <- max(abs(m$s - m$target_mean) / pmax(abs(m$target_mean), 1e-9), abs(m$r - m$ref_mean) / pmax(abs(m$ref_mean), 1e-9))
ok(sprintf("pooled cause means add up to the pooled total (%d cells, worst %.1e)", nrow(m), dev), nrow(m) == nrow(tot) && dev < 2e-4)
# one age cell by hand: World, ssp585, 2030, women 65+, heat deaths, common models m1, m2
a <- rbindlist(lapply(list.files(file.path(R, "output/summary_workflow_b/draws_age"), full.names = TRUE), readRDS))
x <- a[age_group == "a65plus" & sex_id == 2 & period == "2030" & model %in% c("m1", "m2"),
       .(v = sum(deaths_heat)), by = .(model, scenario, draw)]
pr <- merge(x[scenario == "ssp585"], x[scenario == "ssp245"], by = c("model", "draw"))
row <- age[unit == "World" & slice == "a65plus_sex2" & target == "ssp585" & period == "2030" & measure == "deaths_heat"]
ok("World, ssp585, 2030, women 65+, heat deaths recomputes",
   nrow(row) == 1 && isTRUE(all.equal(row$target_mean, mean(pr$v.x), tolerance = 1e-12)) &&
   isTRUE(all.equal(row$diff_upper80, q(pr$v.x - pr$v.y, .9), tolerance = 1e-12)) && row$n_models == 2)
ok("reference-life-table YLLs pooled too", all(c("yllref_heat", "yllref_nonopt") %in% unique(age$measure)))
