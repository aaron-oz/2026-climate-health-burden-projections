# util_compare_scenarios.R: compare two scenarios' attributable burden, by
# location and decade, from two pipeline summary tables (the
# national_by_year.csv that util_summarize_run.R writes).
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
# Usage:
#   Rscript global-scripts/util_compare_scenarios.R
#   Rscript global-scripts/util_compare_scenarios.R --a=path/a.csv --b=path/b.csv
#
# Options:
#   --a=, --b=        national_by_year.csv for the reference (A) and comparison
#                     (B) scenario; defaults are the ssp245 round-2 follow-up
#                     and ssp585 v2 bundles under the data root
#                     (WB_DATA_ROOT, default /var/home/aoz/data/wb-temp-attr-projections)
#   --locations=      comma separated; default every location in --b
#   --years=          default 2022-2050
#   --out=            default output/review-ssp585/ssp585_vs_ssp245_by_decade.csv
#
# Decades are 2022-2030 (9 years), 2031-2040 and 2041-2050. Deaths and YLLs in
# the output are per year (decade averages), not decade totals.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) default else sub(paste0("^--", name, "="), "", hit[1])
}

data_root <- Sys.getenv("WB_DATA_ROOT", "/var/home/aoz/data/wb-temp-attr-projections")
path_a <- get_arg("a", file.path(data_root,
  "review/ssp245-round2/followup-review-bundle/review_bundle/summary/national_by_year.csv"))
path_b <- get_arg("b", file.path(data_root,
  "review/ssp585/bundle-v2/bundle/summary_pipeline/national_by_year.csv"))
years <- as.integer(strsplit(get_arg("years", "2022-2050"), "-")[[1]])
years <- seq(years[1], years[length(years)])
out <- get_arg("out", "output/review-ssp585/ssp585_vs_ssp245_by_decade.csv")

a <- fread(path_a)
b <- fread(path_b)
scen_a <- unique(a$scenario); scen_b <- unique(b$scenario)
stopifnot(length(scen_a) == 1, length(scen_b) == 1, scen_a != scen_b)

locs <- get_arg("locations", NA)
locs <- if (is.na(locs)) unique(b$location_id) else as.integer(strsplit(locs, ",")[[1]])

vals <- c(heat = "deaths_heat_mean", cold = "deaths_cold_mean",
          nonopt = "deaths_nonopt_mean", yll_nonopt = "yll_nonopt_mean")
keys <- c("location_id", "model", "year")
pick <- function(d) {
  d <- d[location_id %in% locs & year %in% years, c(keys, vals), with = FALSE]
  setnames(d, vals, names(vals))
  d
}
m <- merge(pick(a), pick(b), by = keys, suffixes = c("_a", "_b"))
if (nrow(m) == 0) stop("no (location, model, year) rows in common")

# every location-model must have every year, or the decade averages mix years
n_years <- m[, .N, by = .(location_id, model)]
if (any(n_years$N != length(years)))
  stop("some (location, model) pairs lack years: ",
       paste(n_years[N != length(years), paste(location_id, model)], collapse = "; "))

m[, decade := fcase(year <= 2030, "2022-2030", year <= 2040, "2031-2040",
                    default = "2041-2050")]
cols <- c(paste0(names(vals), "_a"), paste0(names(vals), "_b"))
per_model <- m[, lapply(.SD, mean), by = .(location_id, decade, model), .SDcols = cols]

ens <- per_model[, c(list(n_models = .N), lapply(.SD, mean)),
                 by = .(location_id, decade), .SDcols = cols]
spread <- per_model[, {
  r <- nonopt_b / nonopt_a; h <- heat_b / heat_a
  list(heat_ratio_model_min = min(h), heat_ratio_model_median = median(h),
       heat_ratio_model_max = max(h), nonopt_ratio_model_min = min(r),
       nonopt_ratio_model_median = median(r), nonopt_ratio_model_max = max(r))
}, by = .(location_id, decade)]
ens <- merge(ens, spread, by = c("location_id", "decade"))

tot <- ens[, c(list(location_id = NA_integer_, n_models = NA_integer_),
               lapply(.SD, sum)), by = decade, .SDcols = cols]
res <- rbind(ens, tot, fill = TRUE)

for (v in names(vals)) {
  res[, paste0(v, "_diff") := get(paste0(v, "_b")) - get(paste0(v, "_a"))]
  res[, paste0(v, "_ratio") := get(paste0(v, "_b")) / get(paste0(v, "_a"))]
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
setcolorder(res, c("location_id", "location_name", "decade", "scenario_a", "scenario_b", "n_models"))
setorder(res, location_id, decade, na.last = TRUE)

dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
fwrite(res, out)
cat(sprintf("%s vs %s: %d locations, %d models in common, years %d-%d -> %s\n",
            scen_b, scen_a, uniqueN(m$location_id), uniqueN(m$model),
            min(years), max(years), out))
