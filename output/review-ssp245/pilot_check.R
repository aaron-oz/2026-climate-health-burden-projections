# Self-serve gate checker for the ssp245 rerun pilot (stage 1).
#
# Run from the repo root AFTER pilot_rollup.R has produced both CSVs:
#   Rscript output/review-ssp245/pilot_check.R pilot_draws_old.csv pilot_draws_new.csv
# Optional third argument: path to the derived-TMREL cache directory
# (default data/tmrel/derived_cache). Optional --exempt=<ids>: comma-separated
# location ids left out of the value, TMREL, floor and width gates and reported on
# their own EXEMPT line; used for Haiti (114), whose inj_disaster shock tail
# the team decided on 2026-09-14 to keep (all 17 causes in the TMREL weights,
# as Burkart/IHME) and to asterisk in the outputs rather than gate on.
#
# Prints one PASS/WARN/FAIL line per gate and a final verdict:
#   ALL PASS            -> proceed directly to stage 2 (the full grid)
#   WARN, no FAIL       -> proceed to stage 2, but send Aaron the outputs
#                          and warnings in parallel
#   any FAIL            -> STOP; send Aaron pilot_draws_*.csv, the
#                          derived_cache tarball, and a couple of run logs
#
# Revised 2026-09-14 after the first pilot (see
# docs/reviews/ssp245-pilot-review-2026-09-14.org):
#   - the value gate compares the NEW run's burden over the causes outside
#     TMREL_WEIGHT_EXCLUDE (deaths_nonopt_exdis from pilot_rollup.R; equal to
#     deaths_nonopt when nothing is excluded, the default) against the
#     replica prediction repB_exdis, without normalizing by the old run;
#   - a floor gate: share of derived TMREL zone-draws sitting at the 6.6 C
#     search floor (the signature of a shock cause dominating the weights);
#   - the width gate compares each combo's relative interval width against
#     GBD 2019's relative width for the location, not against the old run
#     (whose widths the pairing bug had inflated several-fold in the tropics);
#   - the cache file name must carry the weight-exclusion tag; a cache from
#     the pre-exclusion code is reported as a FAIL, not silently accepted.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
exempt_arg <- grep("^--exempt=", args, value = TRUE)
EXEMPT <- if (length(exempt_arg)) as.integer(strsplit(sub("^--exempt=", "", exempt_arg[1]), ",")[[1]]) else integer(0)
args <- grep("^--", args, value = TRUE, invert = TRUE)
if (length(args) < 2) stop("usage: Rscript pilot_check.R old.csv new.csv [cache_dir] [--exempt=114]")
old <- fread(args[1]); new <- fread(args[2])
cache_dir <- if (length(args) >= 3) args[3] else file.path("data", "tmrel", "derived_cache")
EXP_B <- fread("output/review-ssp245/pilot_expectations_burden.csv")
EXP_T <- fread("output/review-ssp245/pilot_expectations_tmrel.csv")
ACCESS <- "access-cm2-r1i1p1f1-ssp245"
cfg <- new.env(); sys.source(file.path("global-scripts", "config.R"), envir = cfg)
EXCL <- trimws(unlist(strsplit(as.character(cfg$TMREL_WEIGHT_EXCLUDE), ",")))
EXCL <- sort(EXCL[nzchar(EXCL) & !tolower(EXCL) %in% c("none", "false")])
cache_tag <- if (length(EXCL)) paste0("_ex-", paste(EXCL, collapse = "+")) else ""
N_DRAWS <- cfg$N_DRAWS

if (!"deaths_nonopt_exdis" %in% names(new)) {
  if (length(EXCL) > 0)
    stop("new CSV lacks deaths_nonopt_exdis but TMREL_WEIGHT_EXCLUDE is set: ",
         "re-run pilot_rollup.R from the current main to produce it")
  new[, deaths_nonopt_exdis := deaths_nonopt]   # nothing excluded: same total
}
if (length(EXEMPT) > 0)
  cat(sprintf("[EXEMPT] %-28s location(s) %s left out of the value, TMREL, floor and width gates (known inj_disaster shock tail; asterisked in outputs)\n",
              "shock-tail locations", paste(EXEMPT, collapse = ",")))
if (!"repB_exdis" %in% names(EXP_B))
  stop("pilot_expectations_burden.csv is the pre-2026-09-14 version; git pull")

status <- character(0)
gate <- function(name, level, detail) {
  status <<- c(status, level)
  cat(sprintf("[%s] %-28s %s\n", level, name, detail))
}
lvl <- function(x, pass, warn) if (x <= pass) "PASS" else if (x <= warn) "WARN" else "FAIL"

q <- function(v, p) unname(quantile(v, p))
summ <- function(d, col = "deaths_nonopt") d[, .(
  mean = mean(get(col)),
  lo = q(get(col), 0.025), hi = q(get(col), 0.975),
  width = q(get(col), 0.975) - q(get(col), 0.025),
  neg_frac = mean(get(col) < 0)), by = .(location_id, combo)]
SO <- summ(old); SN <- summ(new); SNx <- summ(new, "deaths_nonopt_exdis")
S <- merge(SO, SN, by = c("location_id", "combo"), suffixes = c("_old", "_new"))
if (nrow(S) == 0) stop("no (location, combo) present in both CSVs")

# --- Gate 1: value match on access-cm2, non-disaster burden vs replica -----
a <- merge(SNx[combo == ACCESS & !location_id %in% EXEMPT], EXP_B, by = "location_id")
a[, ratio := mean / repB_exdis]
worst <- a[which.max(abs(ratio - 1))]
g1 <- max(abs(a$ratio - 1))
gate("value match (access-cm2)", lvl(g1, 0.02, 0.04),
     sprintf("max |new_exdis/repB_exdis - 1| = %.3f (%s); tolerance PASS<=0.02 WARN<=0.04 over %d locations",
             g1, worst$location_name, nrow(a)))

# --- Gate 2: derived TMRELs vs expectations, and Gate 3: floor share --------
cache_file <- function(loc) file.path(cache_dir, sprintf("%d_2022_N%d%s.rds", loc, N_DRAWS, cache_tag))
EXP_T <- EXP_T[!location_id %in% EXEMPT]
stale <- sapply(unique(EXP_T$location_id), function(loc)
  !file.exists(cache_file(loc)) && file.exists(file.path(cache_dir, sprintf("%d_2022_N%d.rds", loc, N_DRAWS))))
if (any(stale)) {
  gate("TMREL vs expectations", "FAIL",
       sprintf("%d location(s) have only a pre-exclusion cache file (%%d_2022_N%d.rds, no '%s' tag): the run used code from before 2026-09-14; git pull and re-run the pilot",
               sum(stale), N_DRAWS, cache_tag))
  gate("TMREL floor share", "FAIL", "not evaluated (stale cache)")
} else {
  tm <- rbindlist(lapply(unique(EXP_T$location_id), function(loc) {
    f <- cache_file(loc)
    if (!file.exists(f)) return(data.table(location_id = loc, mad = NA_real_, floor = NA_real_))
    obj <- readRDS(f)
    d <- obj$tmrel[, .(got = mean(tmrel) / 10), by = zone]
    m <- merge(d, EXP_T[location_id == loc], by = "zone")
    data.table(location_id = loc, mad = m[, mean(abs(got - expected_tmrel_c))],
               floor = mean(obj$tmrel$tmrel == 66L))
  }))
  if (all(is.na(tm$mad))) {
    gate("TMREL vs expectations", "FAIL", sprintf("no %s files found in %s", cache_tag, cache_dir))
    gate("TMREL floor share", "FAIL", "not evaluated (no cache)")
  } else {
    g2 <- max(tm$mad, na.rm = TRUE)
    gate("TMREL vs expectations", lvl(g2, 0.2, 0.4),
         sprintf("max per-location mean |diff| = %.2f C (loc %d); tolerance PASS<=0.2 WARN<=0.4; %d of %d caches found",
                 g2, tm[which.max(mad), location_id], sum(!is.na(tm$mad)), nrow(tm)))
    g3 <- max(tm$floor, na.rm = TRUE)
    gate("TMREL floor share", lvl(g3, 0.001, 0.01),
         sprintf("max share of zone-draws at the 6.6 C search floor = %.2f%% (loc %d); PASS<=0.1%% WARN<=1%%",
                 100 * g3, tm[which.max(floor), location_id]))
  }
}

# --- Gate 4: burden moves up (the pairing fix removes a one-signed bias) ---
S[, up := mean_new > mean_old]
g4 <- S[, mean(!up)]
gate("burden moves up", lvl(g4, 0.05, 0.10),
     sprintf("%.1f%% of %d (location, model) combos did NOT increase; PASS<=5%% WARN<=10%%",
             100 * g4, nrow(S)))

# --- Gate 5: interval widths vs GBD 2019 (non-disaster burden) -------------
wx <- merge(SNx[!location_id %in% EXEMPT], EXP_B[, .(location_id, rw_gbd = (gbd_hi - gbd_lo) / gbd_val)], by = "location_id")
wx[, rw_ratio := (width / mean) / rw_gbd]
g5 <- wx[, mean(rw_ratio < 0.4 | rw_ratio > 3.0)]
gate("interval widths sane", lvl(g5, 0.05, 0.10),
     sprintf("%.1f%% of %d combos have relative width outside [0.4, 3.0] x GBD 2019's; PASS<=5%% WARN<=10%% (median ratio %.2f)",
             100 * g5, nrow(wx), median(wx$rw_ratio)))

# --- Gate 6: negative draws shrink -----------------------------------------
S[, neg_up := neg_frac_new - neg_frac_old > 0.02]
g6 <- S[, mean(neg_up)]
gate("negative draws shrink", lvl(g6, 0.05, 0.10),
     sprintf("%.1f%% of combos grew their negative-draw fraction by >2pp; mean %.3f -> %.3f",
             100 * g6, mean(S$neg_frac_old), mean(S$neg_frac_new)))

# --- Gate 7: GBD 2019 centrals inside our 95 % intervals -------------------
u <- merge(S[combo == ACCESS], EXP_B, by = "location_id")
in_old <- u[, sum(gbd_val >= lo_old & gbd_val <= hi_old)]
in_new <- u[, sum(gbd_val >= lo_new & gbd_val <= hi_new)]
gate("GBD-in-our-interval count", if (in_new >= in_old - 1) "PASS" else if (in_new >= in_old - 3) "WARN" else "FAIL",
     sprintf("GBD 2019 centrals inside our 95%% interval: %d old -> %d new (of %d)", in_old, in_new, nrow(u)))

cat("\n")
if (any(status == "FAIL")) {
  cat("VERDICT: FAIL: STOP. Do not start stage 2. Send Aaron the two CSVs,\n")
  cat("a tarball of", cache_dir, ", and a couple of run_2022.log files.\n")
} else if (any(status == "WARN")) {
  cat("VERDICT: WARN: proceed to stage 2, but send Aaron the outputs and\n")
  cat("the warning lines above in parallel.\n")
} else {
  cat("VERDICT: ALL PASS: proceed directly to stage 2 (the full grid).\n")
  cat("Please still send Aaron the two CSVs and the derived_cache tarball\n")
  cat("for the record, but no need to wait.\n")
}
