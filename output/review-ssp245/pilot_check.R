# Self-serve gate checker for the ssp245 rerun pilot (stage 1).
#
# Run from the repo root AFTER pilot_rollup.R has produced both CSVs:
#   Rscript output/review-ssp245/pilot_check.R pilot_draws_old.csv pilot_draws_new.csv
# Optional third argument: path to the derived-TMREL cache directory
# (default data/tmrel/derived_cache).
#
# Prints one PASS/WARN/FAIL line per gate and a final verdict:
#   ALL PASS            -> proceed directly to stage 2 (the full grid)
#   WARN, no FAIL       -> proceed to stage 2, but send Aaron the outputs
#                          and warnings in parallel
#   any FAIL            -> STOP; send Aaron pilot_draws_*.csv, the
#                          derived_cache tarball, and a couple of run logs
#
# The value gate is self-normalizing: it compares (new / predicted-fixed)
# against (old / predicted-production) per country, so each country's known
# replica-emulation residual cancels and only an implementation divergence
# would trip it.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: Rscript pilot_check.R old.csv new.csv [cache_dir]")
old <- fread(args[1]); new <- fread(args[2])
cache_dir <- if (length(args) >= 3) args[3] else file.path("data", "tmrel", "derived_cache")
EXP_B <- fread("output/review-ssp245/pilot_expectations_burden.csv")
EXP_T <- fread("output/review-ssp245/pilot_expectations_tmrel.csv")
ACCESS <- "access-cm2-r1i1p1f1-ssp245"

status <- character(0)
gate <- function(name, level, detail) {
  status <<- c(status, level)
  cat(sprintf("[%s] %-28s %s\n", level, name, detail))
}
lvl <- function(x, pass, warn) if (x <= pass) "PASS" else if (x <= warn) "WARN" else "FAIL"

summ <- function(d) d[, .(
  mean_nonopt = mean(deaths_nonopt),
  lo = quantile(deaths_nonopt, 0.025), hi = quantile(deaths_nonopt, 0.975),
  width = quantile(deaths_nonopt, 0.975) - quantile(deaths_nonopt, 0.025),
  neg_frac = mean(deaths_nonopt < 0)), by = .(location_id, combo)]
SO <- summ(old); SN <- summ(new)
S <- merge(SO, SN, by = c("location_id", "combo"), suffixes = c("_old", "_new"))
if (nrow(S) == 0) stop("no (location, combo) present in both CSVs")

# --- Gate 1: value match on access-cm2, self-normalized ------------------
a <- merge(S[combo == ACCESS], EXP_B, by = "location_id")
a[, factor := (mean_nonopt_new / repB) / (mean_nonopt_old / repA)]
worst <- a[which.max(abs(factor - 1))]
g1 <- max(abs(a$factor - 1))
gate("value match (access-cm2)", lvl(g1, 0.015, 0.03),
     sprintf("max |factor-1| = %.3f (%s); tolerance PASS<=0.015 WARN<=0.03 over %d locations",
             g1, worst$location_name, nrow(a)))

# --- Gate 2: derived TMRELs vs expectations ------------------------------
tm <- rbindlist(lapply(unique(EXP_T$location_id), function(loc) {
  f <- file.path(cache_dir, sprintf("%d_2022_N500.rds", loc))
  if (!file.exists(f)) return(data.table(location_id = loc, mad = NA_real_))
  d <- readRDS(f)$tmrel[, .(got = mean(tmrel) / 10), by = zone]
  m <- merge(d, EXP_T[location_id == loc], by = "zone")
  data.table(location_id = loc, mad = m[, mean(abs(got - expected_tmrel_c))])
}))
if (anyNA(tm$mad)) {
  gate("TMREL vs expectations", "FAIL",
       paste("missing cache files for locations:",
             paste(tm[is.na(mad), location_id], collapse = ",")))
} else {
  g2 <- max(tm$mad)
  gate("TMREL vs expectations", lvl(g2, 0.2, 0.4),
       sprintf("max per-location mean |diff| = %.2f C; tolerance PASS<=0.2 WARN<=0.4", g2))
}

# --- Gate 3: one-signed direction of the fix -----------------------------
S[, up := mean_nonopt_new > mean_nonopt_old]
g3 <- 1 - S[, mean(up)]
gate("burden moves up", lvl(g3, 0.05, 0.10),
     sprintf("%.1f%% of %d (location, model) combos did NOT increase; PASS<=5%% WARN<=10%%",
             100 * g3, nrow(S)))

# --- Gate 4: uncertainty-interval widths ---------------------------------
S[, wratio := width_new / width_old]
g4 <- 1 - S[, mean(wratio >= 0.5 & wratio <= 2.0)]
gate("interval widths sane", lvl(g4, 0.05, 0.10),
     sprintf("%.1f%% of combos outside width-ratio [0.5, 2.0]; PASS<=5%% WARN<=10%%",
             100 * g4))

# --- Gate 5: negative-draw fraction does not grow ------------------------
g5 <- 1 - S[, mean(neg_frac_new <= neg_frac_old + 0.02)]
gate("negative draws shrink", lvl(g5, 0.05, 0.10),
     sprintf("%.1f%% of combos grew their negative-draw fraction by >2pp; mean %.3f -> %.3f",
             100 * g5, S[, mean(neg_frac_old)], S[, mean(neg_frac_new)]))

# --- Gate 6: GBD 2019 centrals inside our access-cm2 interval ------------
a[, `:=`(in_old = gbd_val >= lo_old & gbd_val <= hi_old,
         in_new = gbd_val >= lo_new & gbd_val <= hi_new)]
g6 <- a[, sum(in_old) - sum(in_new)]
gate("GBD-in-our-interval count", if (g6 <= 1) "PASS" else if (g6 <= 3) "WARN" else "FAIL",
     sprintf("GBD 2019 centrals inside our 95%% interval: %d old -> %d new (of %d)",
             a[, sum(in_old)], a[, sum(in_new)], nrow(a)))

cat("\n")
if (any(status == "FAIL")) {
  cat("VERDICT: FAIL — STOP. Do not start stage 2. Send Aaron the two CSVs,\n")
  cat("a tarball of", cache_dir, ", and a couple of run_2022.log files.\n")
} else if (any(status == "WARN")) {
  cat("VERDICT: WARN — proceed to stage 2, but send Aaron the outputs and\n")
  cat("the warning lines above in parallel.\n")
} else {
  cat("VERDICT: ALL PASS — proceed directly to stage 2 (the full grid).\n")
  cat("Please still send Aaron the two CSVs and the derived_cache tarball\n")
  cat("for the record, but no need to wait.\n")
}
