# util_vet_benchmark.R — Substantive vetting of benchmark burden outputs. This
# checks that the NUMBERS are sensible, not merely that files exist. Two tiers:
#
#   Tier 1 (hard, PASS/FAIL): arithmetic + internal consistency that must hold
#     regardless of climate/scenario. A FAIL here is a bug.
#   Tier 2 (directional, for human judgment): physically-expected patterns. These
#     are printed with context; a research/epi eye decides if they're right.
#
# Reads per-combo burden RDS under output/results/cckp/<loc>/<model>-<scenario>/
# burden_<year>.rds (model + scenario columns must be stamped -- run the pipeline
# post-2026-07-27, or util_backfill_scenario_columns.R on older output).
#
# Usage:
#   Rscript global-scripts/util_vet_benchmark.R --locations=305,131,163
#   Rscript global-scripts/util_vet_benchmark.R --locations=163 --tol=1e-6

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(LOCATIONS = "", TOL = 1e-6)
for (k in names(defaults)) if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())

# SSP radiative-forcing order = warmth order (low -> high). Used to test that
# heat-attributable burden rises, and cold falls, as the scenario warms.
SCEN_WARMTH <- c(ssp126 = 1L, ssp245 = 2L, ssp370 = 3L, ssp585 = 4L)

load_benchmark_burden <- function(locs) {
  files <- unlist(lapply(locs, function(l)
    Sys.glob(file.path(RESULTS_DIR, "cckp", l, "*", "burden_*.rds"))), use.names = FALSE)
  if (length(files) == 0) stop("No burden files found for locations: ", paste(locs, collapse = ","))
  b <- rbindlist(lapply(files, function(f) setDT(readRDS(f))), use.names = TRUE, fill = TRUE)
  if (!all(c("model", "scenario") %in% names(b)))
    stop("burden files lack model/scenario columns; run util_backfill_scenario_columns.R first")
  b
}

hr <- function() cat(strrep("-", 78), "\n")

# ---- Tier 1: hard checks -----------------------------------------------------
tier1 <- function(b, tol) {
  cat("\n=== TIER 1: hard consistency checks (must all PASS) ===\n")
  chk <- function(name, ok, detail = "") {
    cat(sprintf("  [%s] %s%s\n", if (ok) "PASS" else "**FAIL**", name,
                if (nzchar(detail)) paste0("  -- ", detail) else ""))
    ok
  }
  all_ok <- TRUE
  # 1. paf_nonopt = paf_heat + paf_cold
  d1 <- b[, max(abs(paf_nonopt - (paf_heat + paf_cold)), na.rm = TRUE)]
  all_ok <- chk("paf_nonopt == paf_heat + paf_cold", d1 < tol, sprintf("max dev %.2e", d1)) && all_ok
  # 2. deaths_nonopt = deaths_heat + deaths_cold
  d2 <- b[, max(abs(deaths_nonopt - (deaths_heat + deaths_cold)), na.rm = TRUE)]
  all_ok <- chk("deaths_nonopt == deaths_heat + deaths_cold", d2 < 1e-3, sprintf("max dev %.2e", d2)) && all_ok
  # 3. attributable never exceeds total deaths (|paf| <= 1)
  pmax_abs <- b[, max(abs(paf_nonopt), na.rm = TRUE)]
  all_ok <- chk("|paf_nonopt| <= 1 (attributable <= total)", pmax_abs <= 1 + tol,
                sprintf("max |paf| %.4f", pmax_abs)) && all_ok
  # 4. no NA / non-finite in key fields
  keyf <- c("paf_heat","paf_cold","deaths","deaths_nonopt")
  nbad <- sum(sapply(keyf, function(c) sum(!is.finite(b[[c]]))))
  all_ok <- chk("no NA/Inf in paf/deaths", nbad == 0, sprintf("%d non-finite", nbad)) && all_ok
  # 5. total deaths non-negative
  nneg <- b[deaths < 0, .N]
  all_ok <- chk("total deaths >= 0", nneg == 0, sprintf("%d negative rows", nneg)) && all_ok
  # 6. draws present and mean within its own 95% interval (per loc/scenario/cause)
  if ("draw" %in% names(b)) {
    nd <- b[, uniqueN(draw)]
    agg <- b[, .(m = mean(deaths_nonopt), lo = quantile(deaths_nonopt, .025),
                 hi = quantile(deaths_nonopt, .975)),
             by = .(location_id, scenario, acause)]
    inb <- agg[, mean(m >= lo & m <= hi)]
    all_ok <- chk(sprintf("draws present (%d) and mean within 95%% interval", nd),
                  nd > 1 && inb > 0.99, sprintf("%.1f%% within", 100*inb)) && all_ok
  } else {
    cat("  [note] summary mode (no draw column) -- draw checks skipped\n")
  }
  cat(sprintf("\n  Tier 1 overall: %s\n", if (all_ok) "PASS" else "**FAIL -- investigate before trusting outputs**"))
  invisible(all_ok)
}

# ---- Tier 2: directional plausibility ---------------------------------------
tier2 <- function(b) {
  cat("\n=== TIER 2: directional plausibility (human judgment) ===\n")
  # Collapse to (loc, model, scenario, year, cause): PAF is broadcast across
  # age/sex, so take the draw-mean of the per-combo paf; deaths summed.
  pafm <- b[, .(paf_heat = mean(paf_heat), paf_cold = mean(paf_cold),
                paf_nonopt = mean(paf_nonopt),
                deaths_nonopt = sum(deaths_nonopt) / uniqueN(draw)),
            by = .(location_id, model, scenario, year, acause)]
  pafm[, warmth := SCEN_WARMTH[scenario]]

  cat("\n[2a] Scenario ordering: does heat-attributable PAF rise, and cold fall,",
      "\n     as the scenario warms (ssp126<245<370<585)? Near-term years are noisy",
      "\n     because scenarios barely differ; the late-horizon years are the test.\n")
  ord <- pafm[!is.na(warmth), {
    o <- order(warmth)
    list(heat_rises = if (.N >= 2) cor(warmth, paf_heat, method = "spearman") else NA_real_,
         cold_falls = if (.N >= 2) cor(warmth, paf_cold, method = "spearman") else NA_real_,
         n_scen = .N)
  }, by = .(location_id, year, acause)]
  by_year <- ord[n_scen >= 3, .(
    frac_heat_rises = mean(heat_rises > 0, na.rm = TRUE),
    frac_cold_falls = mean(cold_falls < 0, na.rm = TRUE),
    n = .N), by = year][order(year)]
  cat("     fraction of (loc,cause) with heat-PAF rising / cold-PAF falling vs warmth, by year:\n")
  print(by_year)

  cat("\n[2b] Scenario divergence in total non-optimal PAF (ssp585 - ssp245),",
      "\n     mean over causes, by location & year. Should be ~0 near 2030 and grow with year.\n")
  wide <- dcast(pafm, location_id + year + acause ~ scenario, value.var = "paf_nonopt")
  if (all(c("ssp245","ssp585") %in% names(wide))) {
    div <- wide[, .(mean_diff_585_245 = mean(ssp585 - ssp245, na.rm = TRUE)),
                by = .(location_id, year)][order(location_id, year)]
    print(div)
  } else cat("     (need both ssp245 and ssp585 present)\n")

  cat("\n[2c] Heat vs cold composition by location (share of |non-optimal| that is heat).",
      "\n     Tropical/hot locations should be heat-leaning; cool/high-altitude cold-leaning.\n")
  comp <- pafm[scenario == "ssp245", .(
    heat_share = sum(abs(paf_heat)) / sum(abs(paf_heat) + abs(paf_cold))),
    by = location_id]
  print(comp)

  cat("\n[2d] Top-3 causes by |non-optimal attributable deaths| per location (ssp245).",
      "\n     Expect temperature-sensitive CVD/respiratory causes near the top.\n")
  tops <- pafm[scenario == "ssp245", .(deaths = sum(abs(deaths_nonopt))),
               by = .(location_id, acause)][order(location_id, -deaths)]
  print(tops[, head(.SD, 3), by = location_id])

  cat("\n[2e] PAF magnitude summary (ssp245): believable range is roughly single-",
      "\n     to low-double-digit percent for the main causes (cf. Colombia ~4% cvd_ihd).\n")
  mag <- pafm[scenario == "ssp245", .(
    paf_nonopt_pct = round(100 * mean(paf_nonopt), 2),
    min_pct = round(100 * min(paf_nonopt), 2),
    max_pct = round(100 * max(paf_nonopt), 2)), by = location_id]
  print(mag)
  invisible(NULL)
}

vet <- function() {
  locs <- strsplit(as.character(LOCATIONS), ",", fixed = TRUE)[[1]]
  locs <- trimws(locs[nzchar(locs)])
  if (length(locs) == 0) stop("Pass --locations=305,131,163")
  cat("Vetting benchmark burden for locations:", paste(locs, collapse = ", "), "\n")
  b <- load_benchmark_burden(locs)
  cat(sprintf("Loaded %s rows | locations %s | scenarios %s | years %s | %d causes\n",
              format(nrow(b), big.mark = ","),
              paste(sort(unique(b$location_id)), collapse = ","),
              paste(sort(unique(b$scenario)), collapse = ","),
              paste(range(b$year), collapse = "-"), uniqueN(b$acause)))
  hr()
  t1 <- tier1(b, as.numeric(TOL))
  hr()
  tier2(b)
  hr()
  cat("\nDone. Tier 1 must be PASS. Review Tier 2 against expectation (loop in Samuel",
      "\nfor the epi read on magnitudes and heat/cold composition).\n")
  invisible(t1)
}

if (sys.nframe() == 0L) vet()
