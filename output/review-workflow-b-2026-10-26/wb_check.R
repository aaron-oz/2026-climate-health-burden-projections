# Checks on Caspar's Workflow B summary (util_summarize_workflow_b.R output,
# written 2026-10-25, received 2026-10-26) for ssp126/ssp370/ssp585 scaled
# against ssp245.
# Usage: Rscript wb_check.R <workflow_b dir> <scenario bundles dir> <ssp585 test bundle-v2 summary_workflow_b dir>
suppressMessages(library(data.table))
a <- commandArgs(TRUE); WB <- a[1]; SC <- a[2]; T585 <- a[3]
ref <- "ssp245"; targets <- c("ssp126", "ssp370", "ssp585")
n <- fread(file.path(WB, "national_by_year.csv"))

cat("== A. rows per scenario (models x locations x years)\n")
print(n[, .(rows = .N, models = uniqueN(model), locs = uniqueN(location_id), years = uniqueN(year)), by = scenario])
cat("duplicate keys:", anyDuplicated(n, by = c("location_id", "model", "scenario", "year")), "\n")

cat("\n== B. unscaled columns vs the unscaled scenario bundles (draw means)\n")
for (s in c(targets, ref)) {
  u <- fread(file.path(SC, s, s, "national_by_year.csv"),
             select = c("location_id", "model", "scenario", "year", "deaths_heat_mean", "deaths_cold_mean", "deaths_nonopt_mean"))
  # target's own pipeline burden
  m <- merge(n[scenario == s], u, by = c("location_id", "model", "scenario", "year"))
  rel <- function(x, y) max(abs(x - y) / pmax(abs(y), 1e-9))
  cat(sprintf("  %s pipeline_* vs %s bundle: %d of %d rows matched; worst rel. dev heat %.2e cold %.2e nonopt %.2e\n",
              s, s, nrow(m), nrow(n[scenario == s]), rel(m$pipeline_deaths_heat_mean, m$deaths_heat_mean),
              rel(m$pipeline_deaths_cold_mean, m$deaths_cold_mean), rel(m$pipeline_deaths_nonopt_mean, m$deaths_nonopt_mean)))
  if (s == ref) {
    # reference columns of every target vs the ssp245 bundle
    r <- merge(n[scenario != ref, .(location_id, model, year, scenario, ref_deaths_heat_mean, ref_deaths_nonopt_mean)],
               u[, .(location_id, model, year, deaths_heat_mean, deaths_nonopt_mean)], by = c("location_id", "model", "year"))
    cat(sprintf("  ref_* (all targets) vs ssp245 bundle: %d of %d rows matched; worst rel. dev heat %.2e nonopt %.2e\n",
                nrow(r), nrow(n[scenario != ref]), rel(r$ref_deaths_heat_mean, r$deaths_heat_mean),
                rel(r$ref_deaths_nonopt_mean, r$deaths_nonopt_mean)))
    # scaled ssp245 vs unscaled ssp245
    cat(sprintf("  scaled ssp245 vs ssp245 bundle: worst rel. dev heat %.2e nonopt %.2e\n",
                rel(m$deaths_heat_attrib_mean, m$deaths_heat_mean), rel(m$deaths_nonopt_attrib_mean, m$deaths_nonopt_mean)))
  }
}

cat("\n== C. 12 ssp585 test locations vs the accepted test bundle v2 (summary_workflow_b)\n")
t <- fread(file.path(T585, "national_by_year.csv"))
cols <- intersect(setdiff(names(t), c("location_id", "model", "scenario", "year")), names(n))
m <- merge(t, n, by = c("location_id", "model", "scenario", "year"), suffixes = c(".t", ".f"))
dev <- sapply(cols, function(cn) max(abs(m[[paste0(cn, ".t")]] - m[[paste0(cn, ".f")]]), na.rm = TRUE))
cat(sprintf("  %d of %d test rows matched; columns compared %d; worst absolute difference %.3g (%s)\n",
            nrow(m), nrow(t), length(cols), max(dev), names(which.max(dev))))

cat("\n== D. by_cause sums to national (scaled heat, cold)\n")
bc <- fread(file.path(WB, "by_cause.csv"), select = c("location_id", "model", "scenario", "year", "deaths_heat_attrib", "deaths_cold_attrib"))
bs <- bc[, .(h = sum(deaths_heat_attrib), c = sum(deaths_cold_attrib), k = .N), by = .(location_id, model, scenario, year)]
m <- merge(bs, n, by = c("location_id", "model", "scenario", "year"))
cat(sprintf("  rows %d of %d; causes per row %s; worst rel. dev heat %.2e cold %.2e\n", nrow(m), nrow(n),
            paste(range(m$k), collapse = "-"), max(abs(m$h - m$deaths_heat_attrib_mean) / pmax(abs(m$deaths_heat_attrib_mean), 1e-9)),
            max(abs(m$c - m$deaths_cold_attrib_mean) / pmax(abs(m$deaths_cold_attrib_mean), 1e-9))))
rm(bc, bs); invisible(gc())

# Per-scenario plausibility. The summarizer's P1-P6 group by location only,
# so with three targets they pool the 26 + 24 + 27 = 77 (model, target) pairs.
tn <- n[scenario %in% targets]
tn[, decade := fcase(year <= 2030, "2022-2030", year <= 2040, "2031-2040", default = "2041-2050")]

cat("\n== E. global totals: mean over models of (sum over locations), scaled vs unscaled, target / ssp245\n")
g <- tn[, .(h = sum(deaths_heat_attrib_mean), c = sum(deaths_cold_attrib_mean),
            hu = sum(pipeline_deaths_heat_mean), cu = sum(pipeline_deaths_cold_mean),
            hr = sum(ref_deaths_heat_mean), cr = sum(ref_deaths_cold_mean)), by = .(scenario, model, decade)]
e <- g[, .(models = .N, heat_scaled = mean(h) / mean(hr), heat_unscaled = mean(hu) / mean(hr),
           cold_scaled = mean(c) / mean(cr), cold_unscaled = mean(cu) / mean(cr),
           models_heat_up_scaled = sum(h > hr)), by = .(scenario, decade)][order(scenario, decade)]
print(e, digits = 4)
cat("  global scaled / unscaled non-optimal, by scenario and decade\n")
print(tn[, .(scaled_over_unscaled = sum(deaths_nonopt_attrib_mean) / sum(pipeline_deaths_nonopt_mean)), by = .(scenario, decade)][order(scenario, decade)], digits = 4)

cat("\n== F. temperature: population-weighted mean, target minus ssp245 (C), per scenario\n")
tt <- tn[!is.na(temp_popw) & !is.na(ref_temp_popw)]
cat("  locations x models with temperatures:", nrow(tt), "years:", paste(sort(unique(tt$year)), collapse = ","), "\n")
# location-level ensemble mean, then the median over locations and share of locations warmer
loc <- tt[, .(d = mean(temp_popw - ref_temp_popw)), by = .(scenario, year, location_id)]
print(loc[, .(median_loc = median(d), p10 = quantile(d, .1), p90 = quantile(d, .9), share_warmer = mean(d > 0)), by = .(scenario, year)][order(scenario, year)], digits = 3)
# pooled over locations weighted by ssp245 non-optimal deaths, as a rough global figure
gw <- tt[, .(d = weighted.mean(temp_popw - ref_temp_popw, pmax(ref_deaths_nonopt_mean, 0))), by = .(scenario, year, model)][
  , .(death_weighted_mean = mean(d), models_warmer = sum(d > 0), models = .N), by = .(scenario, year)][order(scenario, year)]
print(gw, digits = 3)

cat("\n== G. per scenario, models warming more add more heat: correlation across models at 2050 (median over locations)\n")
cc <- tt[year == 2050, .(r = if (.N >= 3) suppressWarnings(cor(temp_popw - ref_temp_popw, deaths_heat_attrib_mean - ref_deaths_heat_mean)) else NA_real_),
         by = .(scenario, location_id)]
print(cc[, .(median_r = median(r, na.rm = TRUE), share_positive = mean(r > 0, na.rm = TRUE)), by = scenario], digits = 3)

cat("\n== H. Western/Central Europe, ssp370 vs ssp245 (Austria 75, Germany 81, Czechia 47, Netherlands 89)\n")
eu <- c(`Austria` = 75, `Germany` = 81, `Czechia` = 47, `Netherlands` = 89)
for (s in targets) {
  x <- tt[scenario == s & location_id %in% eu & year %in% c(2040, 2050),
          .(dT = mean(temp_popw - ref_temp_popw), models_warmer = sum(temp_popw > ref_temp_popw), models = .N), by = .(location_id, year)]
  h <- tn[scenario == s & location_id %in% eu & decade == "2041-2050",
          .(h = sum(deaths_heat_attrib_mean), hr = sum(ref_deaths_heat_mean)), by = .(location_id, model)][
    , .(heat_ratio_2041_2050 = sum(h) / sum(hr), models_heat_up = sum(h > hr)), by = location_id]
  x <- merge(dcast(x, location_id ~ year, value.var = c("dT", "models_warmer", "models")), h, by = "location_id")
  x[, name := names(eu)[match(location_id, eu)]]
  cat(" ", s, "\n"); print(x, digits = 3)
}
