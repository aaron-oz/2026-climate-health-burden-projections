# util_compare_to_benchmarks.R: put our numbers next to published ones.
#
# Everything else we run checks that the pipeline is internally consistent.
# This is the only script that asks whether the answer is right, by comparing
# the run against estimates published by other people:
#
#   GBD 2023        attributable deaths and YLLs for all 204 countries, 2019
#                   and 2022, split into high, low and non-optimal temperature.
#                   data/gbd-comparison/
#   Burkart 2021    Table 1, nine countries, 1990 and 2019, with uncertainty.
#                   data/benchmarks/burkart2021_table1.csv
#
# Usage:
#   Rscript global-scripts/util_compare_to_benchmarks.R
#   Rscript global-scripts/util_compare_to_benchmarks.R --summary=output/summary --year=2022
#
# Options:
#   --summary=  directory holding national_by_year.csv; default output/summary
#   --year=     our projection year to compare;         default 2022
#   --out=      where to write the comparison;          default <summary>/benchmarks
#   --flag=     ratio outside [1/flag, flag] is flagged; default 2
#
# READ THIS BEFORE READING A DIFFERENCE AS AN ERROR. Our numbers and theirs are
# not the same quantity, and three differences are structural:
#
#   1. Ours is CMIP6 model temperature with no baseline shift to ERA5. IHME
#      applies one (gbd/gbd-appendix.txt, Section 2.1.6.7). Theirs for 2022 is
#      observation-driven; ours is a 27-model ensemble. So the right question is
#      whether their value sits inside our model spread, not whether any single
#      model matches.
#   2. Ours uses IHME forecast mortality; theirs uses estimated mortality.
#   3. Burkart is 2019, ours starts in 2022. That matters more for the
#      heat-to-cold split than for totals: GBD's own global cold share is stable
#      across those years (72.4% in 2019, 73.2% in 2022) but individual
#      countries move a lot, Brazil from 66.9% to 87.0% for instance. So compare
#      the split against GBD for the SAME year, and treat the Burkart split as
#      corroboration of magnitude rather than a target.
#
# A factor of two would be explainable. A factor of ten, or a sign flip in the
# heat-to-cold balance, would not.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(SUMMARY = file.path(OUTPUT_DIR, "summary"), YEAR = 2022,
                 OUT = "", FLAG = 2)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}
if (!nzchar(as.character(OUT))) OUT <- file.path(SUMMARY, "benchmarks")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
YEAR <- as.integer(YEAR)

need <- function(p) if (!file.exists(p)) stop("Missing input: ", p) else p
nat <- fread(need(file.path(SUMMARY, "national_by_year.csv")))
gbd <- fread(need(file.path(DATA_DIR, "gbd-comparison", "gbd_A_anchor_2019_2022.csv")))
den <- fread(need(file.path(DATA_DIR, "gbd-comparison", "gbd_F_denominator_2019_2022.csv")))
bk  <- fread(need(file.path(PROJECT_ROOT, "data/benchmarks/burkart2021_table1.csv")))

hdr <- function(s) cat("\n", strrep("=", 72), "\n", s, "\n", strrep("=", 72), "\n", sep = "")

# -----------------------------------------------------------------------------
# Our side: collapse 27 models to one number per location
#
# Each model carries its own draw-based interval. Across models we have a
# spread. The comparison uses the median of the per-model means, and reports the
# model minimum and maximum, because the question a benchmark answers is whether
# the published value falls inside the ensemble, not whether it equals its
# centre.
# -----------------------------------------------------------------------------
ours <- nat[year == YEAR, .(
  n_models   = .N,
  deaths_med = median(deaths_nonopt_mean, na.rm = TRUE),
  deaths_min = min(deaths_nonopt_mean,    na.rm = TRUE),
  deaths_max = max(deaths_nonopt_mean,    na.rm = TRUE),
  heat_med   = median(deaths_heat_mean,   na.rm = TRUE),
  cold_med   = median(deaths_cold_mean,   na.rm = TRUE),
  denom_med  = median(deaths_mean,        na.rm = TRUE),
  yll_med    = if ("yll_nonopt_mean" %in% names(nat))
                 median(yll_nonopt_mean, na.rm = TRUE) else NA_real_
), by = location_id]
ours[, paf17_ours := 100 * deaths_med / denom_med]
ours[, pct_cold_ours := 100 * cold_med / (heat_med + cold_med)]

# -----------------------------------------------------------------------------
# Their side: GBD 2023, same year, plus the denominator-consistent PAF
# -----------------------------------------------------------------------------
g_num <- function(rei, meas = "Deaths", metric = "Number") {
  gbd[rei_name == rei & measure_name == meas & metric_name == metric & year == YEAR,
      .(location_id, location_name, val)]
}
gd <- merge(g_num("Non-optimal temperature")[, .(location_id, location_name, gbd_deaths = val)],
            g_num("High temperature")[, .(location_id, gbd_heat = val)], by = "location_id")
gd <- merge(gd, g_num("Low temperature")[, .(location_id, gbd_cold = val)], by = "location_id")
gd <- merge(gd, gbd[rei_name == "Non-optimal temperature" & measure_name == "Deaths" &
                    metric_name == "Percent" & year == YEAR,
                    .(location_id, gbd_paf_allcause = 100 * val)], by = "location_id")
gden <- den[year == YEAR, .(gbd_deaths17 = sum(val)), by = location_id]
gd <- merge(gd, gden, by = "location_id")
gd[, gbd_paf17 := 100 * gbd_deaths / gbd_deaths17]
gd[, pct_cold_gbd := 100 * gbd_cold / (gbd_heat + gbd_cold)]

cmp <- merge(ours, gd, by = "location_id")
cmp[, ratio_deaths := deaths_med / gbd_deaths]
cmp[, ratio_paf17  := paf17_ours / gbd_paf17]
cmp[, gbd_in_model_range := gbd_deaths >= deaths_min & gbd_deaths <= deaths_max]
cmp[, flagged := is.na(ratio_deaths) | ratio_deaths > FLAG | ratio_deaths < 1 / FLAG]
setorder(cmp, -gbd_deaths)

# -----------------------------------------------------------------------------
# Report
# -----------------------------------------------------------------------------
sink(file.path(OUT, "benchmark_report.txt"), split = TRUE)
cat("Benchmark comparison, our ", YEAR, " against GBD 2023 ", YEAR, "\n", sep = "")
cat("generated ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), "\n", sep = "")
cat("locations compared: ", nrow(cmp), " of ", uniqueN(gd$location_id), " in GBD\n", sep = "")
miss <- setdiff(gd$location_id, cmp$location_id)
if (length(miss)) cat("absent from our run: ", paste(miss, collapse = ", "), "\n", sep = "")

hdr("Global totals")
gt <- data.table(
  quantity = c("attributable deaths", "of which heat", "of which cold",
               "deaths from the 17 causes", "PAF on 17-cause denominator (%)",
               "cold share of attributable (%)"),
  ours = c(sum(cmp$deaths_med), sum(cmp$heat_med), sum(cmp$cold_med), sum(cmp$denom_med),
           100 * sum(cmp$deaths_med) / sum(cmp$denom_med),
           100 * sum(cmp$cold_med) / (sum(cmp$heat_med) + sum(cmp$cold_med))),
  gbd  = c(sum(cmp$gbd_deaths), sum(cmp$gbd_heat), sum(cmp$gbd_cold), sum(cmp$gbd_deaths17),
           100 * sum(cmp$gbd_deaths) / sum(cmp$gbd_deaths17),
           100 * sum(cmp$gbd_cold) / (sum(cmp$gbd_heat) + sum(cmp$gbd_cold))))
gt[, ratio := round(ours / gbd, 3)]
gt[, `:=`(ours = round(ours, 2), gbd = round(gbd, 2))]
print(gt, row.names = FALSE)
# A global total is a sum, so one large disagreeing location can carry it while
# every other location agrees. Report the total again without the flagged ones.
if (any(cmp$flagged, na.rm = TRUE)) {
  ok <- cmp[flagged == FALSE]
  cat(sprintf("\n  excluding the %d flagged location(s): ours %.0f vs GBD %.0f, ratio %.2f\n",
              sum(cmp$flagged, na.rm = TRUE), sum(ok$deaths_med), sum(ok$gbd_deaths),
              sum(ok$deaths_med) / sum(ok$gbd_deaths)))
  cat("  (the global ratio above is dominated by them; this one is not)\n")
}

hdr("Agreement across locations")
cat(sprintf("  median ratio (ours / GBD)      : %.2f\n", median(cmp$ratio_deaths, na.rm = TRUE)))
cat(sprintf("  interquartile range            : %.2f to %.2f\n",
            quantile(cmp$ratio_deaths, .25, na.rm = TRUE),
            quantile(cmp$ratio_deaths, .75, na.rm = TRUE)))
cat(sprintf("  GBD inside our 27-model range  : %d of %d locations (%.0f%%)\n",
            sum(cmp$gbd_in_model_range, na.rm = TRUE), nrow(cmp),
            100 * mean(cmp$gbd_in_model_range, na.rm = TRUE)))
cat(sprintf("  flagged (outside %gx either way): %d\n", FLAG, sum(cmp$flagged, na.rm = TRUE)))
cat(sprintf("  cold share, ours vs GBD        : median %.1f%% vs %.1f%%\n",
            median(cmp$pct_cold_ours, na.rm = TRUE), median(cmp$pct_cold_gbd, na.rm = TRUE)))

hdr(sprintf("Worst %d disagreements", 20L))
print(head(cmp[order(-abs(log(ratio_deaths)))][, .(
  location_id, location_name, ours = round(deaths_med), gbd = round(gbd_deaths),
  ratio = round(ratio_deaths, 2), in_range = gbd_in_model_range)], 20), row.names = FALSE)
cat("\n  (per-model minimum and maximum are in comparison_by_location.csv)\n")

hdr("The nine Burkart countries")
b19 <- dcast(bk[year == 2019], location_id + location_name ~ risk, value.var = "deaths")
setnames(b19, c("high", "low", "nonopt"), c("bk_heat", "bk_cold", "bk_nonopt"))
b <- merge(cmp[, .(location_id, ours = deaths_med, model_min = deaths_min,
                   model_max = deaths_max, gbd = gbd_deaths,
                   pct_cold_ours, pct_cold_gbd)],
           b19, by = "location_id")
b[, `:=`(pct_cold_bk = 100 * bk_cold / (bk_heat + bk_cold),
         ratio_vs_bk = ours / bk_nonopt)]
cat("Burkart is 2019 and ours is ", YEAR, ", so a modest gap is expected.\n\n", sep = "")
print(b[order(-bk_nonopt), .(location_name, ours = round(ours), gbd = round(gbd),
        burkart_2019 = bk_nonopt, ratio_vs_bk = round(ratio_vs_bk, 2),
        cold_pct_ours = round(pct_cold_ours, 1), cold_pct_bk = round(pct_cold_bk, 1))],
      row.names = FALSE)

hdr("Verdict")
med <- median(cmp$ratio_deaths, na.rm = TRUE)
inr <- mean(cmp$gbd_in_model_range, na.rm = TRUE)
cat(if (med > 0.5 && med < 2 && inr > 0.5)
      "  Consistent with the published estimates at the level this comparison can resolve.\n"
    else
      "  NOT consistent. The median ratio or the ensemble coverage is outside what the\n  structural differences alone would explain. Investigate before using these numbers.\n")
cat(sprintf("  median ratio %.2f, GBD inside our model range for %.0f%% of locations.\n", med, 100 * inr))
sink()

fwrite(cmp, file.path(OUT, "comparison_by_location.csv"))
cat("\nWrote:\n  ", file.path(OUT, "comparison_by_location.csv"), " (", nrow(cmp), " rows)\n",
    "  ", file.path(OUT, "benchmark_report.txt"), "\n", sep = "")
