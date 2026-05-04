# util_compare_to_samuel.R — Compare pipeline output to Samuel's reported Colombia numbers
#
# Targets are hard-coded from the World Bank report:
#   from-samuel/results/WB_climatechange_Colombia_executive summary.pdf (p9-15)
#   from-samuel/results/WB_climatechange_Colombia.pdf (Annex 1, Tables A1.13/A1.14/A1.22)
#
# Period: Colombia (LOCATION_ID = 125), 2010-2019
#
# Comparison layers:
#   Layer 1 — magnitude agreement at +-5% per metric
#   Layer 2 — rank agreement: Spearman rho on per-cause attributable deaths,
#             plus top-3 cause exact-match.
#
# Run AFTER the pipeline finishes (08_outputs.R).
# Usage: Rscript util_compare_to_samuel.R

source("config.R")

library(data.table)

TOLERANCE <- 0.05  # +-5% magnitude tolerance

# =============================================================================
# Samuel's reported targets
# =============================================================================

# --- National totals (10-year sum, 2010-2019) ---
samuel_national <- list(
  deaths_total      = 9472,
  deaths_men        = 5510,
  deaths_women      = 3962,
  deaths_cold_pct   = 0.757,    # 75.7% of 9472 = 7170
  deaths_heat_pct   = 0.243,    # 24.3% of 9472 = 2302
  rate_per_million_total = 20.41,
  rate_per_million_cold  = 15.41,
  rate_per_million_heat  = 5.00,
  yll_total         = 172870,
  yll_cold          = 109313,
  yll_heat          = 63556,
  share_of_all_cause_mortality = 0.0043,  # 0.43%
  share_of_17_cause_mortality  = 0.0105,  # 1.05%
  total_deaths_17_causes_denom = 884628   # Table A1.11
)

# --- Top causes by PAF, mentioned in exec summary p13 ---
samuel_top_paf_heat <- c("inj_homicide", "inj_trans_road", "diabetes")
samuel_top_paf_cold <- c("inj_drowning", "resp_copd", "lri")

# --- Top causes by YLL, mentioned in exec summary p13 ---
samuel_top_yll_heat <- c("inj_homicide", "inj_trans_road", "cvd_ihd")
samuel_top_yll_cold <- c("cvd_ihd", "resp_copd", "cvd_stroke")

# --- Per-cause numeric YLL targets that are explicitly stated in the report ---
# (only those given as numbers in the prose)
samuel_yll_by_cause_heat <- c(
  inj_homicide   = 30045,
  inj_trans_road = 10937,
  cvd_ihd        = 9408
)
samuel_yll_by_cause_cold <- c(
  cvd_ihd     = 48907,
  cvd_stroke  = 10372
  # COPD mentioned as #2 cold but no number given
)

# --- Per-cause PAF targets (10-year average, percent) ---
# Exec summary p13: heat top-3 PAFs and cold top-3 PAFs (national-level percentages)
# These are death-weighted averages of the per-cause PAF.
samuel_paf_by_cause_heat <- c(
  inj_homicide   = 0.007,
  inj_trans_road = 0.006,
  diabetes       = 0.005
)
samuel_paf_by_cause_cold <- c(
  inj_drowning = 0.0185,
  resp_copd    = 0.0111,
  lri          = 0.0104
)

# =============================================================================
# Helpers
# =============================================================================

pass_5pct <- function(actual, target) {
  if (is.na(actual) || is.na(target) || target == 0) return(NA)
  abs(actual - target) / abs(target) <= TOLERANCE
}

fmt_row <- function(label, actual, target) {
  if (is.na(actual) || is.na(target) || target == 0) {
    pct <- NA_real_; verdict <- "SKIP"
  } else {
    pct <- (actual - target) / target * 100
    verdict <- if (abs(pct) <= TOLERANCE * 100) "PASS" else "FAIL"
  }
  data.table(metric = label,
             actual = round(actual, 4),
             target = round(target, 4),
             pct_diff = round(pct, 2),
             verdict = verdict)
}

# =============================================================================
# Load pipeline output
# =============================================================================

burden_file <- file.path(RESULTS_DIR, paste0("burden_", LOCATION_ID, ".rds"))
ylls_file   <- file.path(RESULTS_DIR, paste0("ylls_", LOCATION_ID, ".rds"))
ylls_detail_file <- file.path(RESULTS_DIR, paste0("ylls_detail_", LOCATION_ID, ".rds"))
pafs_file   <- file.path(RESULTS_DIR, paste0("pafs_", LOCATION_ID, ".rds"))

for (f in c(burden_file, ylls_file, pafs_file)) {
  if (!file.exists(f)) stop("Missing pipeline output: ", f)
}

burden <- setDT(readRDS(burden_file))
ylls   <- setDT(readRDS(ylls_file))
pafs   <- setDT(readRDS(pafs_file))

ylls_detail <- if (file.exists(ylls_detail_file)) {
  setDT(readRDS(ylls_detail_file))
} else {
  warning("ylls_detail_", LOCATION_ID, ".rds not found — sex/age splits will be SKIPPED")
  NULL
}

# Mid-decade Colombia population (rough denominator for per-million rates).
# Samuel reports rate_total = 9472 / pop / 10 years * 1,000,000 = 20.41
# => implied pop = 9472 / 20.41 * 1e6 / 10 = ~46.4 M (consistent with Colombia 2010-2019)
implied_pop <- samuel_national$deaths_total /
               samuel_national$rate_per_million_total / 10 * 1e6

# =============================================================================
# Layer 1 — magnitude checks
# =============================================================================

results <- list()

# --- National totals (10-year sums) ---
total_deaths_heat   <- sum(burden$deaths_heat,   na.rm = TRUE)
total_deaths_cold   <- sum(burden$deaths_cold,   na.rm = TRUE)
total_deaths_nonopt <- sum(burden$deaths_nonopt, na.rm = TRUE)
total_deaths_17causes <- sum(burden$deaths,      na.rm = TRUE)

results$nat <- rbindlist(list(
  fmt_row("Total attributable deaths (non-optimal)",
          total_deaths_nonopt, samuel_national$deaths_total),
  fmt_row("Heat-attributable deaths",
          total_deaths_heat,
          samuel_national$deaths_total * samuel_national$deaths_heat_pct),
  fmt_row("Cold-attributable deaths",
          total_deaths_cold,
          samuel_national$deaths_total * samuel_national$deaths_cold_pct),
  fmt_row("Cold share of attributable deaths",
          total_deaths_cold / total_deaths_nonopt,
          samuel_national$deaths_cold_pct),
  fmt_row("17-cause mortality denominator",
          total_deaths_17causes,
          samuel_national$total_deaths_17_causes_denom),
  fmt_row("Share of 17-cause mortality (PAF)",
          total_deaths_nonopt / total_deaths_17causes,
          samuel_national$share_of_17_cause_mortality)
))

# --- Per-million rates (uses Samuel's implied population) ---
results$rates <- rbindlist(list(
  fmt_row("Rate per 1M (non-optimal)",
          total_deaths_nonopt / implied_pop * 1e6 / 10,
          samuel_national$rate_per_million_total),
  fmt_row("Rate per 1M (heat)",
          total_deaths_heat / implied_pop * 1e6 / 10,
          samuel_national$rate_per_million_heat),
  fmt_row("Rate per 1M (cold)",
          total_deaths_cold / implied_pop * 1e6 / 10,
          samuel_national$rate_per_million_cold)
))

# --- YLLs (10-year sums) ---
total_yll_heat   <- sum(ylls$yll_heat,   na.rm = TRUE)
total_yll_cold   <- sum(ylls$yll_cold,   na.rm = TRUE)
total_yll_nonopt <- sum(ylls$yll_nonopt, na.rm = TRUE)

results$ylls <- rbindlist(list(
  fmt_row("Total YLL (non-optimal)",
          total_yll_nonopt, samuel_national$yll_total),
  fmt_row("Heat YLL",
          total_yll_heat,   samuel_national$yll_heat),
  fmt_row("Cold YLL",
          total_yll_cold,   samuel_national$yll_cold)
))

# --- Sex split (requires ylls_detail with sex_id) ---
if (!is.null(ylls_detail) && "sex_id" %in% names(ylls_detail)) {
  sex_split <- ylls_detail[, .(deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                           by = sex_id]
  d_men   <- sex_split[sex_id == 1L, deaths_nonopt]
  d_women <- sex_split[sex_id == 2L, deaths_nonopt]
  results$sex <- rbindlist(list(
    fmt_row("Attributable deaths — men",
            if (length(d_men))   d_men   else NA_real_, samuel_national$deaths_men),
    fmt_row("Attributable deaths — women",
            if (length(d_women)) d_women else NA_real_, samuel_national$deaths_women)
  ))
} else {
  results$sex <- NULL
}

# --- Per-cause attributable deaths (Layer 1 magnitude per cause) ---
cause_deaths <- burden[, .(deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
                           deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
                           deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE)),
                       by = acause]

# --- Per-cause YLL (only for the few causes Samuel reports numerically) ---
cause_ylls <- ylls[, .(yll_heat = sum(yll_heat, na.rm = TRUE),
                       yll_cold = sum(yll_cold, na.rm = TRUE)),
                   by = acause]

results$yll_cause <- rbindlist(c(
  lapply(names(samuel_yll_by_cause_heat), function(c) {
    a <- cause_ylls[acause == c, yll_heat]
    fmt_row(paste0("YLL heat — ", c),
            if (length(a)) a else NA_real_,
            samuel_yll_by_cause_heat[[c]])
  }),
  lapply(names(samuel_yll_by_cause_cold), function(c) {
    a <- cause_ylls[acause == c, yll_cold]
    fmt_row(paste0("YLL cold — ", c),
            if (length(a)) a else NA_real_,
            samuel_yll_by_cause_cold[[c]])
  })
))

# --- Per-cause PAF (death-weighted average across years) ---
# Samuel reports a single PAF % per cause for the whole period.
# Reproduce by: total_attrib_deaths / total_cause_deaths
cause_paf <- burden[, .(paf_heat_avg = sum(deaths_heat, na.rm = TRUE) /
                                       sum(deaths,      na.rm = TRUE),
                        paf_cold_avg = sum(deaths_cold, na.rm = TRUE) /
                                       sum(deaths,      na.rm = TRUE)),
                    by = acause]

results$paf_cause <- rbindlist(c(
  lapply(names(samuel_paf_by_cause_heat), function(c) {
    a <- cause_paf[acause == c, paf_heat_avg]
    fmt_row(paste0("PAF heat — ", c),
            if (length(a)) a else NA_real_,
            samuel_paf_by_cause_heat[[c]])
  }),
  lapply(names(samuel_paf_by_cause_cold), function(c) {
    a <- cause_paf[acause == c, paf_cold_avg]
    fmt_row(paste0("PAF cold — ", c),
            if (length(a)) a else NA_real_,
            samuel_paf_by_cause_cold[[c]])
  })
))

# =============================================================================
# Layer 2 — rank checks
# =============================================================================

# Top-3 causes by attributable deaths (heat / cold)
top3_pipeline_heat <- cause_deaths[order(-deaths_heat)][1:3, acause]
top3_pipeline_cold <- cause_deaths[order(-deaths_cold)][1:3, acause]

# Top-3 YLLs (heat / cold)
top3_pipeline_yll_heat <- cause_ylls[order(-yll_heat)][1:3, acause]
top3_pipeline_yll_cold <- cause_ylls[order(-yll_cold)][1:3, acause]

# Top-3 PAFs (heat / cold) — note: Samuel reports PAF rankings, may differ
# from death-count rankings since PAFs and death magnitudes are different metrics
top3_pipeline_paf_heat <- cause_paf[order(-paf_heat_avg)][1:3, acause]
top3_pipeline_paf_cold <- cause_paf[order(-paf_cold_avg)][1:3, acause]

set_overlap <- function(a, b) length(intersect(a, b))

# =============================================================================
# Print report
# =============================================================================

print_block <- function(title, dt) {
  if (is.null(dt) || nrow(dt) == 0) return(invisible())
  cat("\n--- ", title, " ---\n", sep = "")
  print(dt, row.names = FALSE)
}

cat("\n=========================================\n")
cat("PIPELINE vs SAMUEL — COLOMBIA 2010-2019\n")
cat("Run date: ", as.character(Sys.Date()), "\n")
cat("Tolerance: +-", TOLERANCE * 100, "%\n", sep = "")
cat("=========================================\n")

print_block("Layer 1: National totals", results$nat)
print_block("Layer 1: Per-million rates (uses implied pop ~46.4M)", results$rates)
print_block("Layer 1: YLL totals", results$ylls)
if (!is.null(results$sex)) print_block("Layer 1: Sex split", results$sex)
print_block("Layer 1: Per-cause YLL (causes Samuel reports numerically)",
            results$yll_cause)
print_block("Layer 1: Per-cause PAF (causes Samuel reports numerically)",
            results$paf_cause)

cat("\n--- Layer 2: Top-3 cause exact match ---\n")
top3_dt <- data.table(
  ranking = c("Heat — top-3 attributable deaths",
              "Cold — top-3 attributable deaths",
              "Heat — top-3 YLL",
              "Cold — top-3 YLL",
              "Heat — top-3 PAF",
              "Cold — top-3 PAF"),
  pipeline = c(paste(top3_pipeline_heat,     collapse = ", "),
               paste(top3_pipeline_cold,     collapse = ", "),
               paste(top3_pipeline_yll_heat, collapse = ", "),
               paste(top3_pipeline_yll_cold, collapse = ", "),
               paste(top3_pipeline_paf_heat, collapse = ", "),
               paste(top3_pipeline_paf_cold, collapse = ", ")),
  samuel = c("(rank by deaths not stated)",
             "(rank by deaths not stated)",
             paste(samuel_top_yll_heat, collapse = ", "),
             paste(samuel_top_yll_cold, collapse = ", "),
             paste(samuel_top_paf_heat, collapse = ", "),
             paste(samuel_top_paf_cold, collapse = ", ")),
  overlap = c(NA, NA,
              set_overlap(top3_pipeline_yll_heat, samuel_top_yll_heat),
              set_overlap(top3_pipeline_yll_cold, samuel_top_yll_cold),
              set_overlap(top3_pipeline_paf_heat, samuel_top_paf_heat),
              set_overlap(top3_pipeline_paf_cold, samuel_top_paf_cold))
)
print(top3_dt, row.names = FALSE)

# Spearman rho on per-cause attributable deaths — only computable if Samuel
# reported numbers for all 17 causes, which he didn't. We compare relative
# ordering only on the subset of causes with numeric YLL targets.
samuel_yll_subset <- c(samuel_yll_by_cause_heat,
                       setNames(samuel_yll_by_cause_cold,
                                paste0("cold_", names(samuel_yll_by_cause_cold))))
# Skipping rho since N=5 is too small to be meaningful.

# =============================================================================
# Overall verdict
# =============================================================================

all_layer1 <- rbindlist(Filter(Negate(is.null), results), fill = TRUE)
n_pass <- sum(all_layer1$verdict == "PASS", na.rm = TRUE)
n_fail <- sum(all_layer1$verdict == "FAIL", na.rm = TRUE)
n_skip <- sum(all_layer1$verdict == "SKIP", na.rm = TRUE)

cat("\n=========================================\n")
cat("LAYER 1 SUMMARY: ", n_pass, " PASS / ", n_fail, " FAIL / ",
    n_skip, " SKIP (of ", nrow(all_layer1), ")\n", sep = "")
cat("=========================================\n")

# Save the diff table to disk for sharing
fwrite(all_layer1, file.path(RESULTS_DIR,
                             paste0("comparison_to_samuel_", LOCATION_ID, ".csv")))
cat("Saved: ", file.path(RESULTS_DIR,
                         paste0("comparison_to_samuel_", LOCATION_ID, ".csv")),
    "\n", sep = "")

if (n_fail > 0) quit(status = 1, save = "no")
