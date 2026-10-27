# Check the summarizer's draw export on the fixture from make_fixture.R by
# recomputing every value straight from the fixture's input files.
# Usage: Rscript check_draws.R <fixture root>
suppressMessages(library(data.table))
R <- commandArgs(TRUE)[1]
O <- file.path(R, "output/summary_workflow_b")
dr <- rbindlist(lapply(list.files(file.path(O, "draws"), full.names = TRUE), readRDS))
periods <- list(`2022-2023` = 2022:2023, `2030` = 2030L)
ok <- function(lbl, cond) { cat(sprintf("  %-62s %s\n", lbl, if (isTRUE(cond)) "PASS" else "FAIL")); invisible(cond) }

cat("rows by location, scenario:\n"); print(dr[, .(rows = .N, models = paste(sort(unique(model)), collapse = ",")), by = .(location_id, scenario)])
ok("8000 rows per location (5 target + 3 reference model-runs x 2 periods x 500)", all(dr[, .N, by = location_id]$N == 8000))
ok("no m4 (cannot be scaled; no reference)", !"m4" %in% dr$model)
ok("n_years: 2 for 2022-2023, 1 for 2030", all(dr[period == "2022-2023", n_years] == 2) && all(dr[period == "2030", n_years] == 1))
ok("no duplicate keys", anyDuplicated(dr, by = c("location_id", "model", "scenario", "period", "draw")) == 0)

# independent recomputation
exp <- list()
for (loc in unique(dr$location_id)) {
  lt <- fread(file.path(R, "data/lifetables", paste0(loc, "_lifetable.csv")))
  lt <- unique(lt[, .(year = as.integer(year_id), age_group_id = as.integer(age_group_id), sex_id = as.integer(sex_id), ex)])
  for (k in seq_len(nrow(unique(dr[location_id == loc, .(model, scenario)])))) {
    ms <- unique(dr[location_id == loc, .(model, scenario)])[k]
    for (p in names(periods)) for (y in periods[[p]]) {
      if (ms$scenario == "ssp245") {
        x <- setDT(readRDS(file.path(R, "output/results/cckp", loc, paste0(ms$model, "-ssp245"), sprintf("burden_%d.rds", y))))
        setnames(x, c("deaths_heat", "deaths_cold"), c("h", "c"))
      } else {
        x <- setDT(readRDS(file.path(R, "output/results/workflow_b", loc, paste0(ms$model, "-", ms$scenario), sprintf("wb_%d.rds", y))))
        setnames(x, c("deaths_heat_attrib", "deaths_cold_attrib"), c("h", "c"))
      }
      x <- merge(x, lt, by = c("year", "age_group_id", "sex_id"), all.x = TRUE); x[is.na(ex), ex := 0]
      exp[[length(exp) + 1L]] <- x[, .(location_id = loc, model = ms$model, scenario = ms$scenario, period = p,
                                       h = sum(h), c = sum(c), yh = sum(h * ex), yc = sum(c * ex)), by = draw]
    }
  }
}
e <- rbindlist(exp)[, .(h = sum(h), c = sum(c), yh = sum(yh), yc = sum(yc)), by = .(location_id, model, scenario, period, draw)]
m <- merge(dr, e, by = c("location_id", "model", "scenario", "period", "draw"))
rel <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-9))
ok(sprintf("every row matched by the recomputation (%d of %d)", nrow(m), nrow(dr)), nrow(m) == nrow(dr) && nrow(e) == nrow(dr))
ok(sprintf("deaths match (worst rel. dev %.1e)", max(rel(m$deaths_heat, m$h), rel(m$deaths_cold, m$c))), max(rel(m$deaths_heat, m$h), rel(m$deaths_cold, m$c)) < 1e-12)
ok(sprintf("YLLs match (worst rel. dev %.1e)", max(rel(m$yll_heat, m$yh), rel(m$yll_cold, m$yc))), max(rel(m$yll_heat, m$yh), rel(m$yll_cold, m$yc)) < 1e-12)

# consistency with national_by_year.csv: mean over draws of a period sum equals the sum of the yearly draw means
n <- fread(file.path(O, "national_by_year.csv"))
n[, period := fifelse(year %in% 2022:2023, "2022-2023", "2030")]
nm <- n[, .(h = sum(deaths_heat_attrib_mean), yh = sum(yll_heat_mean), rh = sum(ref_deaths_heat_mean)), by = .(location_id, model, scenario, period)]
dm <- dr[, .(h = mean(deaths_heat), yh = mean(yll_heat)), by = .(location_id, model, scenario, period)]
x <- merge(dm[scenario != "ssp245"], nm, by = c("location_id", "model", "scenario", "period"))
ok("target draw means agree with national_by_year.csv", nrow(x) == 20 && rel(x$h.x, x$h.y) < 1e-12 && rel(x$yh.x, x$yh.y) < 1e-12)
r <- merge(dm[scenario == "ssp245"], unique(nm[scenario == "ssp585", .(location_id, model, period, rh)]), by = c("location_id", "model", "period"))
ok("reference draw means agree with ref_deaths_heat_mean", nrow(r) == 12 && rel(r$h, r$rh) < 1e-12)
