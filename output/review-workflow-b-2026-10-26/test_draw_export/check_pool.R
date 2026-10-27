# Check util_pool_workflow_b_draws.R on the fixture: recompute a sample of
# pooled.csv cells directly from the draw files, for both model sets.
# Usage: Rscript check_pool.R <fixture root>
suppressMessages(library(data.table))
R <- commandArgs(TRUE)[1]
dr <- rbindlist(lapply(list.files(file.path(R, "output/summary_workflow_b/draws"), full.names = TRUE), readRDS))
dr[, deaths_nonopt := deaths_heat + deaths_cold]
ok <- function(lbl, cond) { cat(sprintf("  %-66s %s\n", lbl, if (isTRUE(cond)) "PASS" else "FAIL")); invisible(cond) }
q <- function(x, p) unname(quantile(x, p, names = FALSE))
close_ <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-12))

for (ms in c("paired", "common")) {
  cat(ms, "\n")
  P <- fread(file.path(R, paste0("output/pool_", ms), "pooled.csv"))
  S <- fread(file.path(R, paste0("output/pool_", ms), "model_sets.csv"))
  want <- if (ms == "paired") list(ssp126 = c("m1", "m2"), ssp585 = c("m1", "m2", "m3")) else list(ssp126 = c("m1", "m2"), ssp585 = c("m1", "m2"))
  ok("model sets as expected", all(sapply(names(want), function(t) setequal(S[target == t & note == "used", model], want[[t]]))))
  ok("units: 2 locations + World + groups A, B", setequal(unique(P$unit), c("125", "126", "World", "A", "B")))
  ok("rows: 5 units x 2 targets x 2 periods x 6 measures", nrow(P) == 120)
  # location 125, ssp585, 2022-2023, heat deaths
  for (cell in list(list(u = "125", locs = 125L, t = "ssp585", p = "2022-2023", m = "deaths_heat"),
                    list(u = "A", locs = c(125L, 126L), t = "ssp126", p = "2030", m = "deaths_nonopt"),
                    list(u = "World", locs = c(125L, 126L), t = "ssp585", p = "2030", m = "deaths_cold"))) {
    M <- want[[cell$t]]
    x <- dr[location_id %in% cell$locs & period == cell$p & model %in% M,
            .(v = sum(get(cell$m)), n_years = max(n_years)), by = .(model, scenario, draw)]
    tt <- x[scenario == cell$t]; rr <- x[scenario == "ssp245"]
    pr <- merge(tt, rr, by = c("model", "draw"))
    pr[, `:=`(vt = v.x / n_years.x, vr = v.y / n_years.y)]
    row <- P[unit == cell$u & target == cell$t & period == cell$p & measure == cell$m]
    pmn <- pr[, .(mt = mean(vt), mr = mean(vr)), by = model]
    good <- nrow(row) == 1 && row$n_models == length(M) &&
      close_(row$target_mean, mean(pr$vt)) && close_(row$target_lower, q(pr$vt, .025)) && close_(row$target_upper, q(pr$vt, .975)) &&
      close_(row$ref_mean, mean(pr$vr)) && close_(row$diff_lower, q(pr$vt - pr$vr, .025)) &&
      close_(row$diff_upper, q(pr$vt - pr$vr, .975)) && close_(row$ratio, mean(pr$vt) / mean(pr$vr)) &&
      close_(row$target_model_min, min(pmn$mt)) && close_(row$ratio_model_max, max(pmn$mt / pmn$mr))
    ok(sprintf("cell %s %s %s %s recomputes", cell$u, cell$t, cell$p, cell$m), good)
  }
  # group B is location 125 alone: must equal the location row
  a <- P[unit == "B"][order(target, period, measure)]; b <- P[unit == "125"][order(target, period, measure)]
  ok("group of one location equals the location", close_(as.matrix(a[, -(1:2)][, -(1:5)]), as.matrix(b[, -(1:2)][, -(1:5)])))
}
