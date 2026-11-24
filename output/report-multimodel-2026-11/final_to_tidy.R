# final_to_tidy.R: put the final summary tables (Caspar's final pass of 2026-11-22,
# global-scripts/util_final_tables.R) into the shape that make_report_tables.R and
# make_figures.R read (tidy_common.rds with tot, cau, age), so the report's tables and
# figures come from the final tables.
#
# Where each piece comes from:
#  - means, 95/80/66% intervals, differences from SSP2-4.5, population: the final main
#    tables (all ages, both sexes for tot and cau; under 5, 15-49, 15-64 and 65+ by sex
#    for age).
#  - lowest and highest climate-model mean, models above SSP2-4.5: the final vetting
#    table (totals only; cause and age slices get NA, which no report table uses).
#  - single-year world series for the yearly figure: the final annual table.
#  - with and without "exposure to forces of nature": the final main tables (cause
#    "all" and "all_but_disaster").
#  - crossing year: the 2026-11-13 pooled output (extras/crossover_country.csv), because
#    the final crossing table keeps the shares but not the full distribution of the year;
#    the shares are compared with the final table below.
# Usage (repo root, emacs-r distrobox):
#   Rscript output/report-multimodel-2026-11/final_to_tidy.R <final_tables dir> <2026-11-13 pooled dir> <out dir>
suppressPackageStartupMessages(library(data.table))
setDTthreads(4)
a <- commandArgs(TRUE); TB <- a[1]; OLD <- a[2]; OUT <- a[3]
dir.create(file.path(OUT, "extras"), recursive = TRUE, showWarnings = FALSE)
inc <- fread("output/report-multimodel-2026-11/income_groups_by_location.csv", encoding = "UTF-8")
zr <- function(f) fread(cmd = paste("zcat", shQuote(f)))

fs <- list.files(file.path(TB, "main"), "\\.csv\\.gz$", full.names = TRUE)
AGES <- c("under5", "a15_49", "a15_64", "a65plus")
main <- rbindlist(lapply(fs, function(f) {
  d <- zr(f)
  d[(age == "all" & sex == "both") | (cause == "all" & age %in% AGES & sex %in% c("male", "female"))]
}))
main[, location_id := fifelse(unit_type == "location", suppressWarnings(as.integer(unit)), NA_integer_)]
main <- merge(main, inc[, .(location_id, name_l = name, income_l = income)], by = "location_id", all.x = TRUE)
main[, `:=`(name = fifelse(unit_type == "location", name_l, unit), income = income_l, asterisk = location_id %in% c(114L, 129L))]
main[, c("name_l", "income_l") := NULL]
stopifnot(main[unit_type == "location", !anyNA(name)])

# ratio of means to SSP2-4.5 and rates per 100,000 (population is fixed within a cell)
ref <- main[scenario == "ssp245", .(unit, period, cause, age, sex, measure, ref = mean)]
main <- merge(main, ref, by = c("unit", "period", "cause", "age", "sex", "measure"))
main[, ratio := mean / ref][, ref := NULL]
for (v in c("mean", "lower95", "upper95", "lower80", "upper80", "lower66", "upper66")) set(main, j = paste0("rate_", v), value = main[[v]] / main$pop * 1e5)
main[, N := pop]

v <- zr(file.path(TB, "vetting.csv.gz"))
main <- merge(main, v[, .(unit, scenario, period, measure, model_min, model_max, models_above_ref)], by = c("unit", "scenario", "period", "measure"), all.x = TRUE)
main[!(cause == "all" & age == "all" & sex == "both"), c("model_min", "model_max", "models_above_ref") := NA]

keep <- c("location_id", "unit_type", "unit", "period", "slice", "measure", "scenario", "mean", "lower95", "upper95", "lower80", "upper80", "lower66", "upper66",
          "model_min", "model_max", "models_above_ref", "ratio", "diff_mean", "diff_lower95", "diff_upper95", "diff_lower80", "diff_upper80", "diff_lower66", "diff_upper66",
          "N", "ihme_deaths", "rate_mean", "rate_lower95", "rate_upper95", "rate_lower80", "rate_upper80", "rate_lower66", "rate_upper66", "name", "income", "asterisk")
tot <- main[cause == "all" & age == "all" & sex == "both"][, slice := "all"][, ..keep]
CAUSES <- c("ckd", "cvd_cmp", "cvd_htn", "cvd_ihd", "cvd_stroke", "diabetes", "inj_animal", "inj_disaster", "inj_drowning", "inj_homicide", "inj_mech",
            "inj_othunintent", "inj_suicide", "inj_trans_other", "inj_trans_road", "lri", "resp_copd", "cardiovascular", "diabetes_kidney", "injuries", "all_but_disaster")
cau <- main[cause %in% CAUSES & age == "all" & sex == "both"][, slice := cause][, ..keep]
age <- main[cause == "all" & age %in% AGES & sex %in% c("male", "female")][, slice := paste0(age, fifelse(sex == "male", "_sex1", "_sex2"))][, ..keep]
saveRDS(list(tot = tot, cau = cau, age = age), file.path(OUT, "tidy_common.rds"))
cat("tot", nrow(tot), " cau", nrow(cau), " age", nrow(age), "\n")

# with and without forces of nature, in the shape of extras/with_without_inj_disaster_*.csv
M <- c(deaths_heat = "h", deaths_cold = "c", deaths_nonopt = "n")
ww <- main[cause %in% c("all", "all_but_disaster") & age == "all" & sex == "both" & measure %in% names(M),
           .(variant = fifelse(cause == "all", "all_causes", "without_inj_disaster"), scenario, period, m = M[measure], mean, lower95, upper95, max = NA_real_, location_id, unit_type, unit)]
fwrite(ww[unit_type == "location", !c("unit_type", "unit")], file.path(OUT, "extras", "with_without_inj_disaster_country.csv"))
fwrite(ww[unit_type == "group", !c("unit_type", "location_id")][, group := unit][, unit := NULL], file.path(OUT, "extras", "with_without_inj_disaster_group.csv"))

# yearly world series from the annual table
an <- zr(file.path(TB, "annual.csv.gz"))
ys <- an[unit == "World" & measure %in% c("deaths_heat", "deaths_cold"), .(scenario, year, variable = measure, mean, lower95, upper95)]
fwrite(ys, file.path(OUT, "world_yearly_final.csv"))

# crossing year from the 2026-11-13 output, shares compared with the final table
co <- fread(file.path(OLD, "extras", "crossover_country.csv"))
fwrite(co, file.path(OUT, "extras", "crossover_country.csv"))
cn <- zr(file.path(TB, "crossing.csv.gz"))[unit_type == "location"][, location_id := as.integer(unit)]
cc <- merge(co, cn, by = c("location_id", "scenario"))
cat("crossing shares, 2026-11-13 output against the final table (", nrow(cc), "rows ): max abs difference before 2022",
    cc[, max(abs(share_before_2022 - share_above_at_start))], ", within", cc[, max(abs(share_within - share_crossing_within))],
    ", after 2050", cc[, max(abs(share_after_2050 - share_below_at_end))], "\n")
cat("ok\n")
