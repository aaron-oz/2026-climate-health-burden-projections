# util_final_tables.R: the summary tables of the final pass (stage B), computed
# from the compact copy (util_export_compact.R). Runs wherever the compact copy
# is; nothing here reads the 10 TB scenario output.
#
# What is computed (docs/meetings/summary-tables-request-2026-11-16.org as
# synced to the 2026-11-18 meeting, docs/meetings/team-meeting-2026-11-18-notes.org):
#   - Units: every location, plus the groups in --groups (World is added: every
#     location in the compact copy). A group's value in a draw is the sum of its
#     locations' values in that draw, so its interval is valid.
#   - Scenarios: every scenario in the compact copy; the reference (--ref_scenario)
#     and, for every other scenario, the difference from it taken draw by draw
#     within a climate model.
#   - Climate models: the "common" set, the models present for every scenario
#     (the same set for every scenario and location; checked). Pooled with the
#     draws: each number is a summary of n_models x n_draws values.
#   - Periods: --periods (default the three decades) and --whole (2022-2050).
#     Values are per year: the period total divided by its number of years.
#   - Measures: deaths and YLLs, heat, cold and non-optimal (heat + cold, summed
#     draw by draw). YLLs = deaths x remaining life expectancy at the start of the
#     5-year age group, from the location's life table (UN WPP, national, by year,
#     age and sex: LIFETABLE_DIR/{LOC}_lifetable.csv), the same as the pipeline.
#   - Statistics: mean and central intervals (--intervals, default 95,80,66) of
#     the pooled values; diff_* the same for the difference from the reference.
#
# Tables, under --out. Every table starts with the same keys, so the projection
# tables and the historical table can be stacked and queried together: source
# ("projection", or "GBD 2019 round" in historical.csv.gz), scenario, unit_type
# (location or group), unit (location_id, or the group name), period, then
# cause, age, sex where present, measure and metric (count = deaths or YLLs per
# year; rate_asr = per 100,000; the historical table also has rate_crude and percent).
#   main/{unit}.csv.gz   every (period, scenario, cause slice, age slice, sex,
#                        measure): n_years, n_models, n_draws, mean, lower/upper
#                        per interval, diff_mean and diff intervals (empty for the
#                        reference), pop (mean population per year of the age-sex
#                        slice), ihme_deaths (mean over draws of IHME's forecast
#                        deaths for the slice, per year).
#                        Cause slices: all; the 17 causes; cardiovascular
#                        (cvd_ihd, cvd_stroke, cvd_htn, cvd_cmp); diabetes_kidney
#                        (diabetes, ckd); injuries (the nine inj_*);
#                        all_but_disaster (all except inj_disaster). Age slices:
#                        all; the 17 five-year groups ("0-4" ... "80+"); under5,
#                        a15_49, a15_64, a65plus. Sex: both, male, female.
#   annual.csv.gz        every unit, scenario and year: all causes, ages and
#                        sexes, the same statistics, pop and ihme_deaths.
#   asr.csv.gz           age-standardized rates per 100,000 (--std_pop weights
#                        over the 17 age groups), every unit, period, scenario,
#                        sex and cause slice: mean, intervals, diff_*. Population
#                        = mean per year over the period, per age and sex.
#   vetting.csv.gz       for our own checks, every unit, period and scenario,
#                        totals only: lowest and highest climate-model mean,
#                        models above the reference, between-model share of the
#                        pooled variance, attributable fraction (attributable /
#                        IHME deaths, per draw) mean and intervals, share of
#                        (model, draw) pairs with heat deaths above cold.
#   crossing.csv.gz      every unit and scenario: a straight line through yearly
#                        heat minus cold deaths per (model, draw); shares of pairs
#                        whose line is already above zero in the first year, crosses
#                        within the years, or is still below in the last year, and
#                        the median crossing year among pairs that cross within.
#   temperature.csv.gz   population-weighted annual mean temperature per location,
#                        model, scenario and year, as the compact export recorded it.
#   historical.csv.gz    a copy of --historical (GBD 2019-round table), if given.
#   units.csv, run_info.txt
#
# Usage:
#   Rscript global-scripts/util_final_tables.R --compact=output/compact --out=output/final_tables \
#     --pop=population.csv --std_pop=std_pop.csv --groups=groups.csv [--names=names.csv] \
#     [--historical=historical.csv] [--jobs=8] [--units=125,World] [--overwrite=FALSE]
#   --pop       CSV: location_id, year, age_group_id (0, 5, ..., 80; 80 = 80+), sex_id (1, 2), pop
#   --std_pop   CSV: age_group_id (0, 5, ..., 80), weight (normalized here to sum to 1)
#   --groups    CSV: location_id, group (a location may be in several groups)
#   --names     CSV: location_id, name (optional, carried into units.csv)
# A unit whose main/{unit}.csv.gz exists is skipped unless --overwrite=TRUE.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
source(file.path(SCRIPTS_DIR, "util_compact_io.R"))
suppressPackageStartupMessages(library(data.table))
setDTthreads(1L)

defaults <- list(COMPACT = file.path(OUTPUT_DIR, "compact"), OUT = file.path(OUTPUT_DIR, "final_tables"),
                 POP = "", STD_POP = "", GROUPS = "", NAMES = "", HISTORICAL = "", LIFETABLES = LIFETABLE_DIR,
                 REF_SCENARIO = "ssp245", PERIODS = "2022-2030,2031-2040,2041-2050", WHOLE = "2022-2050",
                 INTERVALS = "95,80,66", JOBS = max(1L, floor(parallel::detectCores() / 2)), UNITS = "",
                 OVERWRITE = "FALSE")
for (k in names(defaults)) if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
split_csv <- function(x) { x <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]]); x[nzchar(x)] }
range_years <- function(p) { ab <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]]); if (length(ab) == 2) seq(ab[1], ab[2]) else ab }
for (k in c("POP", "STD_POP")) if (!nzchar(get(k))) stop("--", tolower(k), " is required")
ref_scen <- as.character(REF_SCENARIO); overwrite <- isTRUE(as.logical(OVERWRITE))
levels_pct <- as.numeric(split_csv(INTERVALS))
dir.create(file.path(OUT, "main"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT, "parts"), recursive = TRUE, showWarnings = FALSE)
has_ms <- requireNamespace("matrixStats", quietly = TRUE)

# ---- the compact copy: locations, scenarios, the common model set ------------
locs <- sort(as.integer(basename(list.dirs(COMPACT, recursive = FALSE))))
locs <- locs[file.exists(file.path(COMPACT, locs, "meta.rds"))]
if (!length(locs)) stop("no locations with meta.rds in ", COMPACT)
metas <- setNames(lapply(locs, function(l) readRDS(file.path(COMPACT, l, "meta.rds"))), locs)
m1 <- metas[[1]]; YEARS <- m1$years; D <- length(m1$draws); CAUSES <- m1$causes; AGES <- m1$ages
cmb <- rbindlist(lapply(metas, function(m) m$combos[, location_id := m$location_id]))
scenarios <- sort(unique(cmb$scenario))
if (!ref_scen %in% scenarios) stop("reference scenario ", ref_scen, " not in the compact copy")
targets <- setdiff(scenarios, ref_scen)
# common: models present in every scenario at every location
common <- Reduce(intersect, lapply(split(cmb, by = c("location_id", "scenario")), function(x) x$model))
common <- sort(common); M <- length(common)
if (!M) stop("no climate model is present in every scenario at every location")
incomplete <- cmb[model %in% common & n_years != length(YEARS)]
if (nrow(incomplete)) stop("combos in the common set missing years: ",
                           paste(head(incomplete[, paste(location_id, model, scenario, n_years)], 20), collapse = "; "))
stopifnot(all(vapply(metas, function(m) identical(m$years, YEARS) && length(m$draws) == D &&
                       identical(m$causes, CAUSES) && identical(m$ages, AGES), TRUE)))
periods <- c(setNames(lapply(split_csv(PERIODS), range_years), split_csv(PERIODS)))
whole <- as.character(WHOLE); whole_years <- range_years(whole)
for (p in names(periods)) if (!all(periods[[p]] %in% YEARS)) stop("period ", p, " has years outside the compact copy")
if (!setequal(unlist(periods), whole_years)) stop("--whole must be the union of --periods")
log_msg("Final tables: ", length(locs), " locations, scenarios ", paste(scenarios, collapse = ","), ", ", M,
        " common models x ", D, " draws, periods ", paste(c(names(periods), whole), collapse = ","))

# ---- slices -------------------------------------------------------------------
age_lab <- c(paste0(AGES[-17], "-", AGES[-17] + 4), "80+")
cause_sl <- c(list(all = CAUSES), setNames(as.list(CAUSES), CAUSES),
              list(cardiovascular = c("cvd_ihd", "cvd_stroke", "cvd_htn", "cvd_cmp"),
                   diabetes_kidney = c("diabetes", "ckd"), injuries = grep("^inj_", CAUSES, value = TRUE),
                   all_but_disaster = setdiff(CAUSES, "inj_disaster")))
age_sl <- c(list(all = AGES), setNames(as.list(AGES), age_lab),
            list(under5 = 0L, a15_49 = seq(15L, 45L, 5L), a15_64 = seq(15L, 60L, 5L), a65plus = seq(65L, 80L, 5L)))
sex_sl <- list(both = 1:2, male = 1L, female = 2L)
agg_mat <- function(sl, atoms) { m <- matrix(0, length(atoms), length(sl)); for (j in seq_along(sl)) m[match(sl[[j]], atoms), j] <- 1; m }
Cm <- agg_mat(cause_sl, CAUSES); Am <- agg_mat(age_sl, AGES); Sm <- agg_mat(sex_sl, 1:2)
NC <- ncol(Cm); NA_ <- ncol(Am); NS <- ncol(Sm)

# ---- population, standard population, life tables ----------------------------
pop <- fread(POP)[, .(location_id = as.integer(location_id), year = as.integer(year),
                      age_group_id = as.integer(age_group_id), sex_id = as.integer(sex_id), pop = as.numeric(pop))]
std <- fread(STD_POP)[, .(age_group_id = as.integer(age_group_id), weight = as.numeric(weight))]
stopifnot(setequal(std$age_group_id, AGES)); std <- std[match(AGES, age_group_id)]; w_std <- std$weight / sum(std$weight)
# pop array per location: [year, age, sex]
pop_arr <- function(l) {
  x <- pop[location_id == l & year %in% YEARS]
  a <- array(NA_real_, c(length(YEARS), 17L, 2L))
  a[cbind(match(x$year, YEARS), match(x$age_group_id, AGES), x$sex_id)] <- x$pop
  if (anyNA(a)) stop("population missing for location ", l)
  a
}
ex_arr <- function(l) {
  f <- file.path(LIFETABLES, paste0(l, "_lifetable.csv"))
  if (!file.exists(f)) stop("no life table for location ", l, " at ", f)
  x <- fread(f); if (!"ex" %in% names(x) && "ev" %in% names(x)) setnames(x, "ev", "ex")
  x <- x[year_id %in% YEARS & age_group_id %in% AGES & sex_id %in% 1:2]
  a <- array(NA_real_, c(length(YEARS), 2L, 17L))   # [year, sex, age], the ihme order
  a[cbind(match(x$year_id, YEARS), x$sex_id, match(x$age_group_id, AGES))] <- x$ex
  if (anyNA(a)) stop("life table incomplete for location ", l)
  a
}

# ---- groups -------------------------------------------------------------------
gmap <- data.table(location_id = locs, group = "World")
if (nzchar(GROUPS)) {
  g <- unique(fread(GROUPS)[, .(location_id = as.integer(location_id), group = as.character(group))])
  g <- g[!is.na(group) & nzchar(group)]
  miss <- setdiff(g$location_id, locs)
  if (length(miss)) stop("groups file lists locations not in the compact copy: ", paste(head(miss, 30), collapse = ","),
                         " (a group without all its locations would be wrong; remove them from the groups file deliberately)")
  gmap <- rbind(gmap, g)
}
nm <- if (nzchar(NAMES)) fread(NAMES)[, .(location_id = as.integer(location_id), name = as.character(name))] else data.table(location_id = locs, name = as.character(locs))
units <- rbind(data.table(unit_type = "location", unit = as.character(locs), n_locations = 1L),
               gmap[, .(unit_type = "group", n_locations = .N), by = .(unit = group)])
units <- merge(units, nm[, .(unit = as.character(location_id), name)], by = "unit", all.x = TRUE, sort = FALSE)
units[is.na(name), name := unit]
if (nzchar(UNITS)) units <- units[unit %in% split_csv(UNITS)]
fwrite(units, file.path(OUT, "units.csv"))
members <- function(u, type) if (type == "location") as.integer(u) else gmap[group == u, location_id]

# ---- accumulation for one location and one scenario ---------------------------
# Cells are in the ihme order: sex fastest, then age, then cause (578 = 2 x 17 x 17).
# Returns, for the scenario:
#   A[[period]][[q]]: list of M matrices [D, 578], q = deaths heat, deaths cold, YLL heat,
#     YLL cold, each summed over the period's years (one matrix per climate model)
#   ann: [D, M, Y, 4] totals per year
# and, when want_ihme: IH[[period]] [D, 578] IHME deaths summed over the period,
#   ih_ann [D, Y]; always pop [Y, 17 age, 2 sex].
NCELL <- 2L * 17L * 17L
cause_of_cell <- rep(seq_len(17L), each = 34L)
location_arrays <- function(l, scen, want_ihme) {
  x <- read_compact(COMPACT, l); ex <- ex_arr(l)
  sc <- which(x$combos$scenario == scen)
  ks <- sc[match(common, x$combos$model[sc])]
  if (anyNA(ks)) stop("location ", l, " lacks a common model for ", scen)
  yper <- vapply(YEARS, function(y) { h <- names(periods)[vapply(periods, function(v) y %in% v, TRUE)]; if (length(h)) h else NA_character_ }, "")
  A <- setNames(lapply(names(periods), function(p) lapply(1:4, function(q) vector("list", M))), names(periods))
  ann <- array(0, c(D, M, length(YEARS), 4L))
  ihm <- lapply(seq_along(YEARS), function(yi) matrix(x$ihme[, , , , yi], D, NCELL))
  exv <- lapply(seq_along(YEARS), function(yi) rep(rep(as.vector(ex[yi, , ]), times = 17L), each = D))   # [D x cell]
  for (j in seq_len(M)) {
    Fk <- read_factor(x, common[j], scen)                    # only this combo's block
    acc <- setNames(lapply(names(periods), function(p) lapply(1:4, function(q) matrix(0, D, NCELL))), names(periods))
    for (yi in which(!is.na(yper))) {
      p <- yper[yi]; ih <- ihm[[yi]]
      fh <- Fk[, , 1, yi] * Fk[, , 2, yi]; fc <- Fk[, , 1, yi] * Fk[, , 3, yi]   # [D, cause]
      h <- ih * fh[, cause_of_cell]; cc <- ih * fc[, cause_of_cell]
      yh <- h * exv[[yi]]; yc <- cc * exv[[yi]]
      acc[[p]][[1]] <- acc[[p]][[1]] + h; acc[[p]][[2]] <- acc[[p]][[2]] + cc
      acc[[p]][[3]] <- acc[[p]][[3]] + yh; acc[[p]][[4]] <- acc[[p]][[4]] + yc
      ann[, j, yi, ] <- c(rowSums(h), rowSums(cc), rowSums(yh), rowSums(yc))
    }
    for (p in names(periods)) for (q in 1:4) A[[p]][[q]][[j]] <- acc[[p]][[q]]
    rm(acc)
  }
  out <- list(A = A, ann = ann, pop = pop_arr(l))
  if (want_ihme) {
    out$IH <- setNames(lapply(names(periods), function(p) Reduce(`+`, ihm[which(yper == p)])), names(periods))
    out$ih_ann <- vapply(ihm, rowSums, numeric(D))
  }
  out
}
add_into <- function(acc, x) {
  if (is.null(acc)) return(x)
  for (p in names(acc$A)) for (q in 1:4) acc$A[[p]][[q]] <- Map(`+`, acc$A[[p]][[q]], x$A[[p]][[q]])
  acc$ann <- acc$ann + x$ann; acc$pop <- acc$pop + x$pop
  if (!is.null(acc$IH)) { acc$IH <- Map(`+`, acc$IH, x$IH); acc$ih_ann <- acc$ih_ann + x$ih_ann }
  acc
}
unit_arrays <- function(ls, scen, want_ihme) {
  acc <- NULL
  for (l in ls) { acc <- add_into(acc, location_arrays(l, scen, want_ihme)); invisible(gc(FALSE)) }
  acc
}
# raw per-model matrices for one period and quantity; the whole period sums the decades
raw <- function(z, p, q) if (p == whole) Reduce(function(a, b) Map(`+`, a, b), lapply(names(periods), function(pp) z$A[[pp]][[q]])) else z$A[[p]][[q]]

# ---- slicing and statistics ---------------------------------------------------
# list of m matrices [D, 578] -> list(sl = [D*m, NC*NA*NS], asr = [D*m, 17 age, NC, NS])
slice_measure <- function(lst) {
  mm <- length(lst)
  X <- do.call(rbind, lst)                                   # [(d, model), cell]
  dim(X) <- c(D * mm * 34L, 17L)                             # rows (dm, sex, age), cols cause
  X <- X %*% Cm
  dim(X) <- c(D * mm, 2L, 17L, NC)
  X <- aperm(X, c(1, 3, 4, 2)); dim(X) <- c(D * mm * 17L * NC, 2L)
  X <- X %*% Sm
  dim(X) <- c(D * mm, 17L, NC, NS)
  asr_in <- X
  X <- aperm(X, c(1, 3, 4, 2)); dim(X) <- c(D * mm * NC * NS, 17L)
  X <- X %*% Am
  dim(X) <- c(D * mm, NC, NS, NA_)
  X <- aperm(X, c(1, 2, 4, 3)); dim(X) <- c(D * mm, NC * NA_ * NS)   # columns: cause fastest, then age, then sex
  list(sl = X, asr = asr_in)
}
slice_keys <- data.table(cause = rep(names(cause_sl), times = NA_ * NS),
                         age = rep(rep(names(age_sl), each = NC), times = NS),
                         sex = rep(names(sex_sl), each = NC * NA_))
qcols <- function(X, probs) {
  if (has_ms) return(matrixStats::colQuantiles(X, probs = probs, type = 7L, drop = FALSE))
  t(apply(X, 2L, function(v) quantile(v, probs, names = FALSE, type = 7L)))
}
iv_probs <- unlist(lapply(levels_pct, function(L) { a <- (1 - L / 100) / 2; c(a, 1 - a) }))
iv_names <- unlist(lapply(levels_pct, function(L) paste0(c("lower", "upper"), L)))
stats_cols <- function(X, prefix = "") {
  X <- as.matrix(X); q <- qcols(X, iv_probs)
  out <- data.table(colMeans(X)); setnames(out, paste0(prefix, "mean"))
  for (i in seq_along(iv_names)) set(out, j = paste0(prefix, iv_names[i]), value = q[, i])
  out
}
# ASR per 100,000 from asr input [DM, 17 age, NC, NS]: one matrix [DM, NC] per sex
asr_mats <- function(a, P, n) lapply(seq_len(NS), function(s) {
  wv <- 1e5 * w_std / as.vector(P %*% Sm[, s]) / n
  X <- aperm(a[, , , s, drop = FALSE], c(1, 3, 2, 4)); dim(X) <- c(dim(a)[1] * NC, 17L)
  matrix(X %*% wv, dim(a)[1], NC)
})
# 6 significant digits (relative rounding at most 5e-6), as the earlier draw exports
round6 <- function(dt) { for (cl in names(dt)) if (is.double(dt[[cl]])) set(dt, j = cl, value = signif(dt[[cl]], 6)); dt }
MEAS <- c("deaths_heat", "deaths_cold", "deaths_nonopt", "yll_heat", "yll_cold", "yll_nonopt")

# ---- one unit -----------------------------------------------------------------
do_unit <- function(u, type) {
  key <- gsub("[^A-Za-z0-9_.-]+", "_", u)
  fmain <- file.path(OUT, "main", paste0(type, "_", key, ".csv.gz"))
  if (!overwrite && file.exists(fmain)) return("skipped")
  ls <- members(u, type); t0 <- Sys.time()
  pers <- c(names(periods), whole)
  pyears <- c(periods, setNames(list(whole_years), whole))
  nyr <- vapply(pyears, length, 1L)
  ref <- unit_arrays(ls, ref_scen, TRUE)
  ref$IH[[whole]] <- Reduce(`+`, ref$IH[names(periods)])
  popP <- lapply(pyears, function(v) apply(ref$pop[match(v, YEARS), , , drop = FALSE], c(2, 3), mean))   # [17 age, 2 sex]
  pop_slice <- lapply(popP, function(P) rep(as.vector(t(Am) %*% P %*% Sm), each = NC))
  ih_slice <- lapply(pers, function(p) colMeans(slice_measure(list(ref$IH[[p]]))$sl) / nyr[[p]]); names(ih_slice) <- pers
  ih_draw <- lapply(pers, function(p) rowSums(ref$IH[[p]]) / nyr[[p]]); names(ih_draw) <- pers
  pop_tot_y <- apply(ref$pop, 1, sum); ih_tot_y <- colMeans(ref$ih_ann)
  main <- list(); asr <- list(); vet <- list(); annual <- list(); cross <- list()
  base_row <- function(scen, p) data.table(source = "projection", scenario = scen, unit_type = type, unit = u, period = p)

  emit <- function(scen, z, zr) {
    for (p in pers) {
      n <- nyr[[p]]
      for (pair in list(c(1L, 2L), c(3L, 4L))) {
        nmz <- if (pair[1] == 1L) "deaths" else "yll"
        st <- lapply(pair, function(q) slice_measure(raw(z, p, q)))
        sr <- if (!is.null(zr)) lapply(pair, function(q) slice_measure(raw(zr, p, q)))
        mt <- list(st[[1]]$sl / n, st[[2]]$sl / n); mt[[3]] <- mt[[1]] + mt[[2]]
        mr <- if (!is.null(sr)) { r <- list(sr[[1]]$sl / n, sr[[2]]$sl / n); r[[3]] <- r[[1]] + r[[2]]; r }
        for (i in 1:3) {
          m <- paste0(nmz, "_", c("heat", "cold", "nonopt")[i])
          row <- cbind(base_row(scen, p), slice_keys, data.table(measure = m, metric = "count", n_years = n, n_models = M, n_draws = D), stats_cols(mt[[i]]))
          if (!is.null(mr)) row <- cbind(row, stats_cols(mt[[i]] - mr[[i]], "diff_"))
          row[, `:=`(pop = pop_slice[[p]], ihme_deaths = ih_slice[[p]])]
          main[[length(main) + 1L]] <<- row
          # vetting, totals only (column 1 = all causes, all ages, both sexes)
          v <- mt[[i]][, 1]; mm <- colMeans(matrix(v, D, M)); g <- mean(v)
          vr <- cbind(base_row(scen, p), data.table(measure = m, model_min = min(mm), model_max = max(mm),
                       between_share = mean((mm - g)^2) / mean((v - g)^2)))
          if (!is.null(mr)) vr[, models_above_ref := sum(mm > colMeans(matrix(mr[[i]][, 1], D, M)))]
          if (nmz == "deaths") vr <- cbind(vr, stats_cols(v / rep(ih_draw[[p]], times = M), "af_"))
          if (m == "deaths_heat") vr[, heat_above_cold_share := mean(mt[[1]][, 1] > mt[[2]][, 1])]
          vet[[length(vet) + 1L]] <<- vr
        }
        # age-standardized rates; non-optimal = heat + cold draw by draw
        at <- lapply(st, function(s) asr_mats(s$asr, popP[[p]], n))
        ar <- if (!is.null(sr)) lapply(sr, function(s) asr_mats(s$asr, popP[[p]], n))
        for (s in seq_len(NS)) {
          tt <- list(at[[1]][[s]], at[[2]][[s]]); tt[[3]] <- tt[[1]] + tt[[2]]
          if (!is.null(ar)) { rr <- list(ar[[1]][[s]], ar[[2]][[s]]); rr[[3]] <- rr[[1]] + rr[[2]] }
          for (i in 1:3) {
            r <- cbind(base_row(scen, p), data.table(sex = names(sex_sl)[s], cause = names(cause_sl),
                       measure = paste0(nmz, "_", c("heat", "cold", "nonopt")[i]), metric = "rate_asr"), stats_cols(tt[[i]]))
            if (!is.null(ar)) r <- cbind(r, stats_cols(tt[[i]] - rr[[i]], "diff_"))
            asr[[length(asr) + 1L]] <<- r
          }
        }
        rm(st, sr, mt, mr, at, ar); invisible(gc(FALSE))
      }
    }
    # annual totals
    for (yi in seq_along(YEARS)) {
      vt <- matrix(z$ann[, , yi, ], D * M, 4L)
      vv <- list(vt[, 1], vt[, 2], vt[, 1] + vt[, 2], vt[, 3], vt[, 4], vt[, 3] + vt[, 4])
      if (!is.null(zr)) { w0 <- matrix(zr$ann[, , yi, ], D * M, 4L)
        rv <- list(w0[, 1], w0[, 2], w0[, 1] + w0[, 2], w0[, 3], w0[, 4], w0[, 3] + w0[, 4]) }
      for (i in 1:6) {
        r <- cbind(data.table(source = "projection", scenario = scen, unit_type = type, unit = u, year = YEARS[yi], measure = MEAS[i], metric = "count",
                              n_models = M, n_draws = D), stats_cols(vv[[i]]))
        if (!is.null(zr)) r <- cbind(r, stats_cols(vv[[i]] - rv[[i]], "diff_"))
        r[, `:=`(pop = pop_tot_y[yi], ihme_deaths = ih_tot_y[yi])]
        annual[[length(annual) + 1L]] <<- r
      }
    }
    # crossing: a straight line through yearly heat minus cold deaths per (model, draw)
    dd <- matrix(z$ann[, , , 1] - z$ann[, , , 2], D * M, length(YEARS))
    yr <- YEARS - mean(YEARS); b <- as.vector(dd %*% yr) / sum(yr^2); a0 <- rowMeans(dd)
    f_first <- a0 + b * (min(YEARS) - mean(YEARS)); f_last <- a0 + b * (max(YEARS) - mean(YEARS))
    within <- f_first < 0 & f_last >= 0
    cross[[length(cross) + 1L]] <<- data.table(unit_type = type, unit = u, scenario = scen,
      first_year = min(YEARS), last_year = max(YEARS), share_above_at_start = mean(f_first >= 0),
      share_crossing_within = mean(within), share_below_at_end = mean(f_last < 0),
      median_crossing_year = if (any(within)) median((mean(YEARS) - a0 / b)[within]) else NA_real_)
  }
  emit(ref_scen, ref, NULL)
  for (t in targets) { tg <- unit_arrays(ls, t, FALSE); emit(t, tg, ref); rm(tg); invisible(gc(FALSE)) }
  fwrite(round6(rbindlist(main, fill = TRUE)), fmain)
  out_name <- c(asr = "asr", vet = "vetting", annual = "annual", cross = "crossing")
  for (tb in names(out_name)) fwrite(round6(rbindlist(get(tb), fill = TRUE)),
    file.path(OUT, "parts", paste0(out_name[[tb]], "_", type, "_", key, ".csv")))
  sprintf("ok %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
}

# groups first (they take longest), then locations
units <- units[order(unit_type != "group", -n_locations)]
res <- parallel::mclapply(seq_len(nrow(units)), function(i)
  tryCatch(do_unit(units$unit[i], units$unit_type[i]), error = function(e) paste("ERROR:", conditionMessage(e))),
  mc.cores = as.integer(JOBS), mc.preschedule = FALSE)
units[, status := vapply(res, function(r) if (is.character(r)) r[1] else paste("ERROR:", as.character(r)), "")]
fwrite(units, file.path(OUT, "units.csv"))
for (tb in c("asr", "vetting", "annual", "crossing")) {
  fs <- list.files(file.path(OUT, "parts"), paste0("^", tb, "_.*\\.csv$"), full.names = TRUE)
  if (length(fs)) fwrite(rbindlist(lapply(fs, fread), fill = TRUE), file.path(OUT, paste0(tb, ".csv.gz")))
}
tp <- rbindlist(lapply(metas, function(m) if (nrow(m$temp)) m$temp[, location_id := m$location_id]), fill = TRUE)
if (nrow(tp)) fwrite(tp, file.path(OUT, "temperature.csv.gz"))
if (nzchar(HISTORICAL)) fwrite(fread(HISTORICAL), file.path(OUT, "historical.csv.gz"))
info <- c(sprintf("created %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
          sprintf("compact copy: %s (%d locations)", normalizePath(COMPACT), length(locs)),
          sprintf("reference: %s; targets: %s", ref_scen, paste(targets, collapse = ",")),
          sprintf("common climate models (%d): %s", M, paste(common, collapse = ",")),
          sprintf("draws per model: %d; periods: %s", D, paste(c(names(periods), whole), collapse = ",")),
          sprintf("population: %s; standard population: %s; life tables: %s", POP, STD_POP, LIFETABLES),
          sprintf("intervals: %s; quantiles via %s", INTERVALS, if (has_ms) "matrixStats::colQuantiles (type 7)" else "stats::quantile (type 7)"),
          sprintf("units: %d ok, %d skipped, %d errors", sum(startsWith(units$status, "ok")), sum(units$status == "skipped"),
                  sum(startsWith(units$status, "ERROR"))),
          units[startsWith(status, "ERROR"), paste(" ", unit_type, unit, status)])
writeLines(info, file.path(OUT, "run_info.txt")); cat(info, sep = "\n")
