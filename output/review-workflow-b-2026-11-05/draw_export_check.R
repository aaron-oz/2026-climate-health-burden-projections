# draw_export_check.R: full pass over Caspar's per-draw exports (2026-10-29 run,
# downloaded 2026-11-04). For every location: structure of the three draw files,
# agreement with the summary CSVs, internal consistency between the files, and
# independent pooled numbers (24 common models) to compare with
# global-scripts/util_pool_workflow_b_draws.R.
#
# Usage: Rscript draw_export_check.R <summary_dir> <bundle_root> <lifetable_dir> <out_dir> [locations]
# Limits itself to 8 workers (half the cores on Aaron's machine).

suppressPackageStartupMessages(library(data.table))
setDTthreads(1)
a <- commandArgs(TRUE)
SUM <- a[1]; BUNDLE <- a[2]; LTDIR <- a[3]; OUT <- a[4]
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
for (s in c("loc_checks", "loc_pooled", "world")) dir.create(file.path(OUT, s), showWarnings = FALSE)

TARGETS <- c("ssp126", "ssp370", "ssp585"); REF <- "ssp245"
YEARS <- 2022:2050
BLOCK <- function(y) c("2022-2025", "2026-2030", "2031-2035", "2036-2040", "2041-2045", "2046-2050")[
  findInterval(y, c(2022, 2026, 2031, 2036, 2041, 2046))]
DEC_OF_BLOCK <- c("2022-2025" = "2022-2030", "2026-2030" = "2022-2030", "2031-2035" = "2031-2040",
                  "2036-2040" = "2031-2040", "2041-2045" = "2041-2050", "2046-2050" = "2041-2050")
DEC_YEARS <- c("2022-2030" = 9, "2031-2040" = 10, "2041-2050" = 10)
BLOCK_YEARS <- c("2022-2025" = 4, "2026-2030" = 5, "2031-2035" = 5, "2036-2040" = 5, "2041-2045" = 5, "2046-2050" = 5)
AGE_IDS <- list(under5 = 0, a15_49 = seq(15, 45, 5), a65plus = seq(65, 80, 5))
VALS <- c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold")
TAPER <- c(0.025, 0.1, 0.17, 0.5, 0.83, 0.9, 0.975)

log_msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
log_msg("Reading summary CSVs")
nat <- fread(file.path(SUM, "national_by_year.csv"))
nat <- nat[, .(location_id, model, scenario, year,
               deaths_heat_attrib_mean, deaths_heat_attrib_median, deaths_heat_attrib_lower, deaths_heat_attrib_upper,
               deaths_cold_attrib_mean, deaths_cold_attrib_median, deaths_cold_attrib_lower, deaths_cold_attrib_upper,
               yll_heat_mean, yll_heat_median, yll_heat_lower, yll_heat_upper,
               yll_cold_mean, yll_cold_median, yll_cold_lower, yll_cold_upper,
               ref_deaths_heat_mean, ref_deaths_cold_mean)]
cau <- fread(file.path(SUM, "by_cause.csv"),
             select = c("location_id", "model", "scenario", "year", "acause",
                        "deaths_heat_attrib", "deaths_cold_attrib", "yll_heat", "yll_cold"))
cau <- cau[scenario != REF]
cau[, block := BLOCK(year)]
cau <- cau[, lapply(.SD, sum), by = .(location_id, model, scenario, block, acause),
           .SDcols = c("deaths_heat_attrib", "deaths_cold_attrib", "yll_heat", "yll_cold")]
rnat <- fread(file.path(BUNDLE, REF, "national_by_year.csv"),
              select = c("location_id", "model", "year",
                         paste0("deaths_heat_", c("mean", "median", "lower", "upper")),
                         paste0("deaths_cold_", c("mean", "median", "lower", "upper")),
                         paste0("yll_heat_", c("mean", "median", "lower", "upper")),
                         paste0("yll_cold_", c("mean", "median", "lower", "upper"))))
rcau <- fread(file.path(BUNDLE, REF, "by_cause.csv"),
              select = c("location_id", "model", "year", "acause", "deaths_heat", "deaths_cold", "yll_heat", "yll_cold"))
rcau[, block := BLOCK(year)]
rcau <- rcau[, lapply(.SD, sum), by = .(location_id, model, block, acause),
             .SDcols = c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold")]
setkey(nat, location_id); setkey(cau, location_id); setkey(rnat, location_id); setkey(rcau, location_id)
log_msg("CSVs ready")

files <- list.files(file.path(SUM, "draws"), "^\\d+\\.rds$")
locs <- sort(as.integer(sub("\\.rds$", "", files)))
if (length(a) >= 5) locs <- as.integer(strsplit(a[5], ",")[[1]])

# Expected model sets come from coverage.csv of the same delivery: models with
# all 29 Workflow B years per target.
cov <- fread(file.path(SUM, "coverage.csv"))
tset <- cov[scenario != REF & years_wb == 29, .(n = .N), by = .(scenario, model)][n == 204]
exp_models <- split(tset$model, tset$scenario)
exp_models[[REF]] <- sort(unique(unlist(exp_models)))
common <- Reduce(intersect, exp_models)
stopifnot(length(common) == 24)
saveRDS(list(exp_models = exp_models, common = common), file.path(OUT, "model_sets.rds"))
log_msg("Expected models:", paste(names(exp_models), lengths(exp_models), collapse = " "), "; common", length(common))

qs <- function(x) {  # type-7 quantiles from one sort, faster than quantile()
  s <- sort(x); n <- length(s); h <- (n - 1) * TAPER + 1; lo <- floor(h)
  s[lo] + (h - lo) * (s[pmin(lo + 1, n)] - s[lo])
}
rel <- function(a, b) max(abs(a - b) / pmax(abs(b), 1), na.rm = TRUE)

# Pool one (period, slice) x (target, ref) table. d: scenario, model, period, slice, draw, values, per PERIOD total.
pool_one <- function(d, loc) {
  d[, deaths_nonopt := deaths_heat + deaths_cold]
  d[, yll_nonopt := yll_heat + yll_cold]
  ms <- c("deaths_heat", "deaths_cold", "deaths_nonopt", "yll_heat", "yll_cold", "yll_nonopt")
  d[, ny := DEC_YEARS[period]]
  for (m in ms) set(d, j = m, value = d[[m]] / d$ny)
  setkey(d, slice, period, model, draw)
  r <- d[scenario == REF]
  out <- list()
  for (t in TARGETS) {
    x <- d[scenario == t]
    stopifnot(identical(x$draw, r$draw), identical(x$model, r$model), identical(x$period, r$period), identical(x$slice, r$slice))
    gid <- x[, .GRP, by = .(slice, period)]
    idx <- x[, .(i = list(.I)), by = .(slice, period)]
    for (g in seq_len(nrow(idx))) {
      ii <- idx$i[[g]]
      for (m in ms) {
        vt <- x[[m]][ii]; vr <- r[[m]][ii]; dd <- vt - vr; mod <- x$model[ii]
        mt <- vapply(split(vt, mod), mean, 0); mr <- vapply(split(vr, mod), mean, 0)
        q_t <- qs(vt); q_r <- qs(vr); q_d <- qs(dd)
        out[[length(out) + 1L]] <- data.table(
          location_id = loc, slice = idx$slice[g], period = idx$period[g], target = t, measure = m,
          t_mean = mean(vt), t_l95 = q_t[1], t_l80 = q_t[2], t_l66 = q_t[3], t_med = q_t[4], t_u66 = q_t[5], t_u80 = q_t[6], t_u95 = q_t[7],
          t_min = min(vt), t_max = max(vt), t_sd = sd(vt), t_sd_models = sd(mt), t_mod_min = min(mt), t_mod_max = max(mt),
          r_mean = mean(vr), r_l95 = q_r[1], r_u95 = q_r[7], r_min = min(vr), r_max = max(vr), r_sd = sd(vr), r_sd_models = sd(mr),
          d_mean = mean(dd), d_l95 = q_d[1], d_l66 = q_d[3], d_u66 = q_d[5], d_u95 = q_d[7],
          d_share_pos = mean(dd > 0), share_t_neg = mean(vt < 0),
          cor_tr = suppressWarnings(cor(vt - mt[mod], vr - mr[mod])),
          ratio_mod_min = min(mt / mr), ratio_mod_max = max(mt / mr))
      }
    }
  }
  rbindlist(out)
}

check_loc <- function(l) {
  L <- as.character(l); res <- list()
  add <- function(name, value) res[[length(res) + 1L]] <<- data.table(location_id = l, check = name, value = as.numeric(value))
  D <- readRDS(file.path(SUM, "draws", paste0(L, ".rds"))); setDT(D)
  C <- readRDS(file.path(SUM, "draws_cause", paste0(L, ".rds"))); setDT(C)
  A <- readRDS(file.path(SUM, "draws_age", paste0(L, ".rds"))); setDT(A)

  # --- structure ------------------------------------------------------------
  add("D_rows_minus_expected", nrow(D) - 104 * 29 * 500)
  add("C_rows_minus_expected", nrow(C) - 104 * 6 * 17 * 500)
  add("A_rows_minus_expected", nrow(A) - 104 * 6 * 6 * 500)
  add("D_dup_keys", nrow(D) - uniqueN(D, by = c("model", "scenario", "period", "draw")))
  add("C_dup_keys", nrow(C) - uniqueN(C, by = c("model", "scenario", "period", "acause", "draw")))
  add("A_dup_keys", nrow(A) - uniqueN(A, by = c("model", "scenario", "period", "age_group", "sex_id", "draw")))
  for (nm in c("D", "C", "A")) {
    X <- get(nm)
    add(paste0(nm, "_nonfinite"), sum(vapply(VALS, function(v) sum(!is.finite(X[[v]])), 0L)))
    add(paste0(nm, "_wrong_location"), sum(X$location_id != l))
    add(paste0(nm, "_draws_not_0_499"), !setequal(unique(X$draw), 0:499))
    ms <- X[, .(m = list(sort(unique(model)))), by = scenario]
    add(paste0(nm, "_wrong_model_set"), sum(vapply(seq_len(nrow(ms)), function(i) !identical(ms$m[[i]], sort(exp_models[[ms$scenario[i]]])), TRUE)))
    add(paste0(nm, "_scenarios"), uniqueN(X$scenario))
    add(paste0(nm, "_round_dev_gt6"), max(vapply(VALS, function(v) max(abs(X[[v]] - signif(X[[v]], 6)) / pmax(abs(X[[v]]), 1e-300)), 0)))
    add(paste0(nm, "_all_zero_runs"), X[, .(z = all(deaths_heat == 0 & deaths_cold == 0)), by = .(model, scenario, period)][, sum(z)])
  }
  add("D_nyears_not_1", sum(D$n_years != 1))
  add("C_nyears_wrong", sum(C$n_years != BLOCK_YEARS[C$period]))
  add("A_nyears_wrong", sum(A$n_years != BLOCK_YEARS[A$period]))
  add("D_periods", uniqueN(D$period)); add("C_periods", uniqueN(C$period)); add("A_periods", uniqueN(A$period))
  add("C_causes", uniqueN(C$acause)); add("A_slices", uniqueN(paste(A$age_group, A$sex_id)))

  # --- totals file vs national_by_year.csv (targets) ------------------------
  D[, year := as.integer(period)]
  S <- D[, .(hm = mean(deaths_heat), hq = median(deaths_heat), hl = quantile(deaths_heat, .025, names = FALSE), hu = quantile(deaths_heat, .975, names = FALSE),
             cm = mean(deaths_cold), cq = median(deaths_cold), cl = quantile(deaths_cold, .025, names = FALSE), cu = quantile(deaths_cold, .975, names = FALSE),
             ym = mean(yll_heat), yq = median(yll_heat), yl = quantile(yll_heat, .025, names = FALSE), yu = quantile(yll_heat, .975, names = FALSE),
             zm = mean(yll_cold), zq = median(yll_cold), zl = quantile(yll_cold, .025, names = FALSE), zu = quantile(yll_cold, .975, names = FALSE)),
         by = .(model, scenario, year)]
  N <- nat[.(l)]
  j <- merge(S[scenario != REF], N[scenario != REF], by = c("model", "scenario", "year"))
  add("nat_target_rows_matched_minus_expected", nrow(j) - 77 * 29)
  add("nat_target_max_rel_mean", with(j, max(rel(hm, deaths_heat_attrib_mean), rel(cm, deaths_cold_attrib_mean), rel(ym, yll_heat_mean), rel(zm, yll_cold_mean))))
  add("nat_target_max_rel_median", with(j, max(rel(hq, deaths_heat_attrib_median), rel(cq, deaths_cold_attrib_median), rel(yq, yll_heat_median), rel(zq, yll_cold_median))))
  add("nat_target_max_rel_lower", with(j, max(rel(hl, deaths_heat_attrib_lower), rel(cl, deaths_cold_attrib_lower), rel(yl, yll_heat_lower), rel(zl, yll_cold_lower))))
  add("nat_target_max_rel_upper", with(j, max(rel(hu, deaths_heat_attrib_upper), rel(cu, deaths_cold_attrib_upper), rel(yu, yll_heat_upper), rel(zu, yll_cold_upper))))
  # reference rows vs the ref columns of nat, and vs the unscaled ssp245 bundle
  jr <- merge(S[scenario == REF], unique(N[scenario != REF, .(model, year, rh = ref_deaths_heat_mean, rc = ref_deaths_cold_mean)]), by = c("model", "year"))
  add("nat_ref_rows_matched_minus_expected", nrow(jr) - 27 * 29)
  add("nat_ref_max_rel_mean", with(jr, max(rel(hm, rh), rel(cm, rc))))
  rb <- merge(S[scenario == REF], rnat[.(l)], by = c("model", "year"))
  add("bundle_ref_rows_matched_minus_expected", nrow(rb) - 27 * 29)
  add("bundle_ref_max_rel_mean", with(rb, max(rel(hm, deaths_heat_mean), rel(cm, deaths_cold_mean), rel(ym, yll_heat_mean), rel(zm, yll_cold_mean))))
  add("bundle_ref_max_rel_median", with(rb, max(rel(hq, deaths_heat_median), rel(cq, deaths_cold_median), rel(yq, yll_heat_median), rel(zq, yll_cold_median))))
  add("bundle_ref_max_rel_lower", with(rb, max(rel(hl, deaths_heat_lower), rel(cl, deaths_cold_lower), rel(yl, yll_heat_lower), rel(zl, yll_cold_lower))))
  add("bundle_ref_max_rel_upper", with(rb, max(rel(hu, deaths_heat_upper), rel(cu, deaths_cold_upper), rel(yu, yll_heat_upper), rel(zu, yll_cold_upper))))
  # scaled ssp245 self-check rows (2022, 2050) vs the exported reference rows
  js <- merge(S[scenario == REF], N[scenario == REF], by = c("model", "scenario", "year"))
  add("selfscale_rows", nrow(js))
  add("selfscale_max_rel_mean", with(js, max(rel(hm, deaths_heat_attrib_mean), rel(cm, deaths_cold_attrib_mean), rel(ym, yll_heat_mean), rel(zm, yll_cold_mean))))
  # exact duplicates: two runs with identical draws
  sig <- D[, .(s = sum(deaths_heat * (draw + 1)) + 1e3 * sum(deaths_cold * (draw + 1))), by = .(model, scenario)]
  add("duplicate_run_signatures", nrow(sig) - uniqueN(sig$s))

  # --- cause file vs by_cause.csv -------------------------------------------
  CM <- C[, .(h = mean(deaths_heat), c = mean(deaths_cold), y = mean(yll_heat), z = mean(yll_cold)), by = .(model, scenario, period, acause)]
  jc <- merge(CM[scenario != REF], cau[.(l)], by.x = c("model", "scenario", "period", "acause"), by.y = c("model", "scenario", "block", "acause"))
  add("cause_target_rows_matched_minus_expected", nrow(jc) - 77 * 6 * 17)
  add("cause_target_max_rel", with(jc, max(rel(h, deaths_heat_attrib), rel(c, deaths_cold_attrib), rel(y, yll_heat), rel(z, yll_cold))))
  jrc <- merge(CM[scenario == REF], rcau[.(l)], by.x = c("model", "period", "acause"), by.y = c("model", "block", "acause"))
  add("cause_ref_rows_matched_minus_expected", nrow(jrc) - 27 * 6 * 17)
  add("cause_ref_max_rel", with(jrc, max(rel(h, deaths_heat), rel(c, deaths_cold), rel(y, yll_heat), rel(z, yll_cold))))

  # --- draw-level: annual totals summed to blocks vs causes summed vs ages ---
  D[, block := BLOCK(year)]
  DB <- D[, lapply(.SD, sum), by = .(model, scenario, block, draw), .SDcols = VALS]
  CB <- C[, c(lapply(.SD, sum), list(ah = sum(abs(deaths_heat)), ac = sum(abs(deaths_cold)), ay = sum(abs(yll_heat)), az = sum(abs(yll_cold)))),
          by = .(model, scenario, block = period, draw), .SDcols = VALS]
  jd <- merge(DB, CB, by = c("model", "scenario", "block", "draw"), suffixes = c("", "_c"))
  add("draw_D_vs_C_rows_minus_expected", nrow(jd) - 104 * 6 * 500)
  add("draw_D_vs_C_maxdev_rel_to_gross", with(jd, max(
    abs(deaths_heat - deaths_heat_c) / pmax(ah, 1e-9), abs(deaths_cold - deaths_cold_c) / pmax(ac, 1e-9),
    abs(yll_heat - yll_heat_c) / pmax(ay, 1e-9), abs(yll_cold - yll_cold_c) / pmax(az, 1e-9))))
  # age slices: share of the total (means), and YLL/death ratio per slice against the life table
  AM <- A[, .(h = mean(deaths_heat), c = mean(deaths_cold), y = mean(yll_heat), z = mean(yll_cold)), by = .(model, scenario, period, age_group, sex_id)]
  lt <- fread(file.path(LTDIR, paste0(L, "_lifetable.csv")))
  lt <- lt[year_id %in% YEARS]
  lt[, block := BLOCK(year_id)]
  rng <- rbindlist(lapply(names(AGE_IDS), function(ag)
    lt[age_group_id %in% AGE_IDS[[ag]], .(ex_lo = min(ex), ex_hi = max(ex)), by = .(block, sex_id)][, age_group := ag]))
  ar <- merge(AM, rng, by.x = c("period", "age_group", "sex_id"), by.y = c("block", "age_group", "sex_id"))
  add("age_ratio_rows_minus_expected", nrow(ar) - 104 * 6 * 6)
  ar[, `:=`(rh = y / h, rc = z / c)]
  add("age_ratio_out_of_range_heat_gt10", ar[abs(h) > 10 & h > 0, sum(rh < ex_lo * 0.999 | rh > ex_hi * 1.001)])
  add("age_ratio_n_heat_gt10", ar[abs(h) > 10 & h > 0, .N])
  add("age_ratio_out_of_range_cold_gt10", ar[abs(c) > 10 & c > 0, sum(rc < ex_lo * 0.999 | rc > ex_hi * 1.001)])
  add("age_ratio_n_cold_gt10", ar[abs(c) > 10 & c > 0, .N])
  # slices must not exceed the total in gross terms for 65+ etc; store shares for the sanity section
  DBm <- DB[, .(H = mean(deaths_heat), Cd = mean(deaths_cold)), by = .(model, scenario, block)]
  sl <- A[, .(h = mean(deaths_heat), c = mean(deaths_cold), y = mean(yll_heat), z = mean(yll_cold)), by = .(scenario, period, age_group, sex_id, model)]
  sl[, location_id := l]

  # --- temporal and cross-scenario coherence of the draw index --------------
  cm <- common[1:min(3, length(common))]
  yr <- dcast(D[model %in% cm & scenario == "ssp585" & period %in% c("2030", "2031", "2050")], model + draw ~ period, value.var = "deaths_cold")
  add("cor_draw_2030_2031_cold", yr[, cor(`2030`, `2031`), by = model][, median(V1, na.rm = TRUE)])
  add("cor_draw_2030_2050_cold", yr[, cor(`2030`, `2050`), by = model][, median(V1, na.rm = TRUE)])

  # --- pooled per-location numbers on the common 24 --------------------------
  Dd <- D[model %in% common]
  Dd[, period := fifelse(year <= 2030, "2022-2030", fifelse(year <= 2040, "2031-2040", "2041-2050"))]
  Dd <- Dd[, lapply(.SD, sum), by = .(model, scenario, period, draw), .SDcols = VALS][, slice := "all"]
  Cd <- C[model %in% common][, period := DEC_OF_BLOCK[period]][
    , lapply(.SD, sum), by = .(model, scenario, period, slice = acause, draw), .SDcols = VALS]
  Ad <- A[model %in% common][, `:=`(period = DEC_OF_BLOCK[period], slice = paste0(age_group, "_sex", sex_id))][
    , lapply(.SD, sum), by = .(model, scenario, period, slice, draw), .SDcols = VALS]
  pooled <- rbind(pool_one(copy(Dd), l), pool_one(copy(Cd), l), pool_one(copy(Ad), l))
  saveRDS(pooled, file.path(OUT, "loc_pooled", paste0(L, ".rds")))
  fwrite(rbindlist(res), file.path(OUT, "loc_checks", paste0(L, ".csv")))
  saveRDS(sl, file.path(OUT, "loc_pooled", paste0("agemeans_", L, ".rds")))

  # World accumulators, returned to the chunk fold.
  ann <- D[, .(model, scenario, year, draw, deaths_heat, deaths_cold, yll_heat, yll_cold)]
  setkey(ann, model, scenario, year, draw); setkey(Cd, model, scenario, period, slice, draw); setkey(Ad, model, scenario, period, slice, draw)
  list(ann = ann, Cd = Cd, Ad = Ad, Dd = Dd)
}

KEYS <- list(ann = c("model", "scenario", "year", "draw"), Cd = c("model", "scenario", "period", "slice", "draw"),
             Ad = c("model", "scenario", "period", "slice", "draw"))
fold <- function(acc, x) {
  for (k in names(KEYS)) {
    if (is.null(acc[[k]])) { acc[[k]] <- x[[k]]; next }
    for (kc in KEYS[[k]]) stopifnot(identical(acc[[k]][[kc]], x[[k]][[kc]]))
    for (v in VALS) set(acc[[k]], j = v, value = acc[[k]][[v]] + x[[k]][[v]])
  }
  acc
}
SAMPLE <- c(6, 163, 102, 135, 214)  # China, India, USA, Brazil, Nigeria
run_chunk <- function(ls, id) {
  acc <- list()
  for (l in ls) {
    t0 <- Sys.time()
    x <- tryCatch(check_loc(l), error = function(e) { log_msg("ERROR loc", l, conditionMessage(e)); NULL })
    if (is.null(x)) next
    if (l %in% SAMPLE) saveRDS(x$Dd, file.path(OUT, "world", paste0("sample_", l, ".rds")))
    acc <- fold(acc, x); rm(x); invisible(gc(FALSE))
    log_msg("chunk", id, "loc", l, round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s")
  }
  saveRDS(acc, file.path(OUT, "world", paste0("chunk_", id, ".rds")))
  TRUE
}
chunks <- split(locs, rep_len(1:8, length(locs)))
r <- parallel::mclapply(seq_along(chunks), function(i) run_chunk(chunks[[i]], i), mc.cores = 8, mc.preschedule = FALSE)
print(r)
log_msg("Summing chunks")
acc <- list()
for (i in seq_along(chunks)) acc <- fold(acc, readRDS(file.path(OUT, "world", paste0("chunk_", i, ".rds"))))
saveRDS(acc, file.path(OUT, "world", "world.rds"))
log_msg("Done")
