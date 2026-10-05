# util_compare_scenarios.R: compare two scenarios' attributable burden, by
# location and decade (and optionally cause), from two pipeline summary tables
# (the national_by_year.csv or by_cause.csv that util_summarize_run.R writes).
#
# Written 2026-10-05 for the 12-location ssp585 test: ssp585 (v2 bundle,
# accepted 2026-10-01) against ssp245 (round-2 follow-up bundle of
# 2026-09-27). Both inputs are UNSCALED pipeline burden, not Workflow B output;
# the Workflow B summary carries ssp245 only for 2022 and 2050.
#
# Method. Rows are joined on (location, model, year), so only models present in
# both scenarios count (27 in the ssp585 test; hadgem3-gc31-mm and taiesm1 have
# no ssp245 run). Each input value is a model's draw mean. For each location
# and decade: average each model's annual values over the decade's years, then
# average over models (the ensemble mean). Ratios are scenario B / scenario A
# of those ensemble means. The *_ratio_model_* columns give the min, median and
# max over models of each model's own decade ratio, as a measure of how much the
# models disagree. Rows with location_name "All locations" sum the ensemble
# means over the locations in the table (ratios of the sums; no model spread).
#
# Negative values. Attributable deaths are negative wherever a cause's risk
# curve sits below 1 (in the ssp585 test, cold deaths for every injury cause,
# and heat deaths for cvd_cmp in India, Brazil, Nigeria and others), and 387 of
# the 612 location-decade-cause rows have a negative value somewhere. A ratio of
# negatives reads backwards, so every *_ratio column is NA unless both
# scenario values are positive, and the model spread uses only models where
# both are positive (counted in *_ratio_n_models). The *_diff columns are
# defined everywhere and are the measure to use for those rows. heat_share_* is
# likewise only meaningful when heat, cold and non-optimal are all positive.
#
# Usage:
#   Rscript global-scripts/util_compare_scenarios.R
#   Rscript global-scripts/util_compare_scenarios.R --by=cause
#   Rscript global-scripts/util_compare_scenarios.R --a=path/a.csv --b=path/b.csv
#
# Options:
#   --by=             national (default) or cause. cause reads by_cause.csv and
#                     adds acause to every key, so each output row is one
#                     (location, decade, cause); "All locations" rows are then
#                     per cause. by_cause.csv holds draw means only, and in the
#                     two default bundles its 17 causes sum to the national
#                     table to within 2e-14 (relative), so the cause rows of a
#                     location-decade add up to its row in the national output.
#   --a=, --b=        summary CSV for the reference (A) and comparison (B)
#                     scenario; defaults are the ssp245 round-2 follow-up and
#                     ssp585 v2 bundles under the data root
#                     (WB_DATA_ROOT, default /var/home/aoz/data/wb-temp-attr-projections)
#   --locations=      comma separated; default every location in --b
#   --years=          default 2022-2050
#   --out=            default output/review-ssp585/ssp585_vs_ssp245_by_decade.csv,
#                     or ..._by_decade_cause.csv with --by=cause
#
# Decades are 2022-2030 (9 years), 2031-2040 and 2041-2050. Deaths and YLLs in
# the output are per year (decade averages), not decade totals.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) default else sub(paste0("^--", name, "="), "", hit[1])
}

by_cause <- switch(get_arg("by", "national"), national = FALSE, cause = TRUE,
                   stop("--by must be national or cause"))
in_file <- if (by_cause) "by_cause.csv" else "national_by_year.csv"

data_root <- Sys.getenv("WB_DATA_ROOT", "/var/home/aoz/data/wb-temp-attr-projections")
path_a <- get_arg("a", file.path(data_root,
  "review/ssp245-round2/followup-review-bundle/review_bundle/summary", in_file))
path_b <- get_arg("b", file.path(data_root,
  "review/ssp585/bundle-v2/bundle/summary_pipeline", in_file))
years <- as.integer(strsplit(get_arg("years", "2022-2050"), "-")[[1]])
years <- seq(years[1], years[length(years)])
out <- get_arg("out", paste0("output/review-ssp585/ssp585_vs_ssp245_by_decade",
                          if (by_cause) "_cause" else "", ".csv"))

a <- fread(path_a)
b <- fread(path_b)
scen_a <- unique(a$scenario); scen_b <- unique(b$scenario)
stopifnot(length(scen_a) == 1, length(scen_b) == 1, scen_a != scen_b)

locs <- get_arg("locations", NA)
locs <- if (is.na(locs)) unique(b$location_id) else as.integer(strsplit(locs, ",")[[1]])

# by_cause.csv names its draw means without the _mean suffix
vals <- c(heat = "deaths_heat_mean", cold = "deaths_cold_mean",
          nonopt = "deaths_nonopt_mean", yll_nonopt = "yll_nonopt_mean")
if (by_cause) vals[] <- sub("_mean$", "", vals)
cause <- if (by_cause) "acause" else NULL
keys <- c("location_id", "model", "year", cause)
pick <- function(d) {
  d <- d[location_id %in% locs & year %in% years, c(keys, vals), with = FALSE]
  setnames(d, vals, names(vals))
  d
}
m <- merge(pick(a), pick(b), by = keys, suffixes = c("_a", "_b"))
if (nrow(m) == 0) stop("no (location, model, year) rows in common")

# every location-model must have every year, or the decade averages mix years
n_years <- m[, .N, by = c("location_id", "model", cause)]
if (any(n_years$N != length(years)))
  stop("some (location, model) pairs lack years: ",
       paste(unique(n_years[N != length(years), paste(location_id, model)]), collapse = "; "))

m[, decade := fcase(year <= 2030, "2022-2030", year <= 2040, "2031-2040",
                    default = "2041-2050")]
cols <- c(paste0(names(vals), "_a"), paste0(names(vals), "_b"))
per_model <- m[, lapply(.SD, mean), by = c("location_id", "decade", cause, "model"), .SDcols = cols]

ens <- per_model[, c(list(n_models = .N), lapply(.SD, mean)),
                 by = c("location_id", "decade", cause), .SDcols = cols]
# b / a, or NA unless both are positive (attributable deaths can be negative
# where the risk curve is below 1, and a ratio of negatives reads backwards)
pos_ratio <- function(b, a) fifelse(a > 0 & b > 0, b / a, NA_real_)
spread <- per_model[, {
  h <- pos_ratio(heat_b, heat_a); r <- pos_ratio(nonopt_b, nonopt_a)
  q <- function(x, f) if (all(is.na(x))) NA_real_ else f(x, na.rm = TRUE)
  list(heat_ratio_n_models = sum(!is.na(h)), heat_ratio_model_min = q(h, min),
       heat_ratio_model_median = q(h, median), heat_ratio_model_max = q(h, max),
       nonopt_ratio_n_models = sum(!is.na(r)), nonopt_ratio_model_min = q(r, min),
       nonopt_ratio_model_median = q(r, median), nonopt_ratio_model_max = q(r, max))
}, by = c("location_id", "decade", cause)]
ens <- merge(ens, spread, by = c("location_id", "decade", cause))

tot <- ens[, c(list(location_id = NA_integer_, n_models = NA_integer_),
               lapply(.SD, sum)), by = c("decade", cause), .SDcols = cols]
res <- rbind(ens, tot, fill = TRUE)

for (v in names(vals)) {
  res[, paste0(v, "_diff") := get(paste0(v, "_b")) - get(paste0(v, "_a"))]
  res[, paste0(v, "_ratio") := pos_ratio(get(paste0(v, "_b")), get(paste0(v, "_a")))]
}
res[, `:=`(heat_share_a = heat_a / nonopt_a, heat_share_b = heat_b / nonopt_b)]

# location names from the GBD hierarchy when it is readable; NA otherwise
names_dt <- tryCatch({
  h <- as.data.table(readxl::read_excel(
    "from-samuel/IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX"))
  unique(h[, .(location_id = `Location ID`, location_name = `Location Name`)])
}, error = function(e) data.table(location_id = integer(), location_name = character()))
res <- merge(res, names_dt, by = "location_id", all.x = TRUE)
res[is.na(location_id), location_name := "All locations"]

# rename _a / _b to the scenario names so the CSV reads on its own
setnames(res, names(res), sub("_a$", paste0("_", scen_a), sub("_b$", paste0("_", scen_b), names(res))))
res[, `:=`(scenario_a = scen_a, scenario_b = scen_b)]
setcolorder(res, c("location_id", "location_name", "decade", cause, "scenario_a", "scenario_b", "n_models"))
setorderv(res, c("location_id", "decade", cause), na.last = TRUE)

dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
fwrite(res, out)
cat(sprintf("%s vs %s: %d locations, %d models in common, years %d-%d -> %s\n",
            scen_b, scen_a, uniqueN(m$location_id), uniqueN(m$model),
            min(years), max(years), out))
