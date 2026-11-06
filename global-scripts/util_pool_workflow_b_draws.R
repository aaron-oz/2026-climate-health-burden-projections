# util_pool_workflow_b_draws.R: the multi-model summary. Pools the per-draw
# period totals that util_summarize_workflow_b.R writes with --draw_periods
# (draws/{LOC}.rds) across climate models, per location and for groups of
# locations, and sets each target scenario beside the reference.
#
# Method (decided 2026-09-29, docs/reviews/ssp585-test-review-2026-09-29.org):
#   - Intervals pool every (model, draw) value of the chosen model set and take
#     the 2.5% and 97.5% quantiles of that mixture, so disagreement between
#     climate models is part of the interval. Each model has the same number
#     of draws, so each model has equal weight (checked below).
#   - Alongside, the range (min, max) of the per-model means.
#   - Groups of locations (regions, world) are summed draw by draw within a
#     model first, and only then pooled across models.
#   - Target minus reference is taken draw by draw within a model (draw d of
#     the target against draw d of the reference), then pooled the same way.
#     The ratio is target mean / reference mean over the pooled set.
#   - A period of several years is summed draw by draw over its years first.
#
# Model sets (--model_set):
#   paired  for each target, the models that have both the target and the
#           reference. The reference columns are recomputed on that same set,
#           so they differ slightly between targets. (default)
#   common  the models present in every scenario in the draw files, the same
#           set for every target.
#   all     every model each scenario has: the target over its own models, the
#           reference over all of its models. The sets differ, so the ratio
#           compares different models; diff_* and ratio_model_* use only the
#           models the two share. For sensitivity analysis.
#
# Usage:
#   Rscript global-scripts/util_pool_workflow_b_draws.R \
#     --draws_dir=output/summary_workflow_b/draws [--model_set=paired] \
#     [--periods=2022-2030,2031-2040,2041-2050] [--ref_scenario=ssp245] \
#     [--groups=groups.csv] [--out=output/summary_multimodel] [--jobs=1]
#
#   --periods=       periods to report, as year ranges or single years. Each is
#                    formed by summing the draw files' periods whose years make
#                    it up exactly (yearly files make any period; the 5-year
#                    breakdown blocks make 2022-2030, 2031-2040, 2041-2050). A
#                    period that cuts across a file period stops with an error.
#                    Default empty: report the draw files' own periods as they are.
#   --period_batch=  how many reported periods to build per pass over the draw
#                    files; lower it if memory runs short. Default 4.
#   --jobs=          parallel workers over locations. Default 1. Locations are
#                    pooled one at a time and groups from running totals, so
#                    memory scales with the workers, not with the locations
#                    (the 17-cause files, 204 locations: see
#                    docs/reviews/workflow-b-draw-export-review-2026-11-05.org
#                    for why this changed).
#   --intervals=     central uncertainty intervals to report, in percent.
#                    Default 95,80,66 (team decision 2026-10-27): GBD's 95%, 80%, and the
#                    IPCC "likely" 66% (17th-83rd percentile).
#   --groups=        optional CSV with columns location_id, group: one row per
#                    location per group (a location may be in several
#                    groups). A "World" group of every location in the draw
#                    files is always added.
#
# Outputs, under --out:
#   pooled.csv      one row per (unit, slice, target scenario, period, measure).
#                   unit_type is "location" or "group". slice is "all" for the
#                   national totals (draws/), the cause for draws_cause/, and
#                   e.g. "a65plus_sex2" (age group, sex) for draws_age/. Every value is per
#                   year (period total / n_years). Columns: n_years, n_models
#                   (target's models), n_models_ref;
#                   target_* and ref_* each with mean, lowerXX and upperXX
#                   for each interval XX in --intervals (e.g. lower95,
#                   upper95), model_min, model_max (range of per-model
#                   means); diff_* (target minus reference) the same; ratio
#                   (target_mean / ref_mean) and ratio_model_min/max (range of
#                   per-model mean ratios). Measures: deaths and YLLs for heat,
#                   cold and non-optimal (heat + cold), and yllref_* (YLLs on
#                   the reference life table) when the draw files carry them.
#   model_sets.csv  the models used for each target, and any model left out
#                   of a group because it lacks some of the group's locations.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(DRAWS_DIR = file.path(OUTPUT_DIR, "summary_workflow_b", "draws"),
                 MODEL_SET = "paired", REF_SCENARIO = "ssp245", GROUPS = "",
                 PERIODS = "", PERIOD_BATCH = 4, INTERVALS = "95,80,66", JOBS = 1,
                 OUT = file.path(OUTPUT_DIR, "summary_multimodel"))
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}
split_csv <- function(x) {
  x <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
  x[nzchar(x)]
}
range_years <- function(p) {
  ab <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]])
  if (length(ab) == 1) ab else seq(ab[1], ab[2])
}
ref_scen  <- as.character(REF_SCENARIO)
model_set <- as.character(MODEL_SET)
if (!model_set %in% c("paired", "common", "all")) stop("--model_set must be paired, common or all, not ", model_set)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
jobs <- as.integer(JOBS)

files <- list.files(DRAWS_DIR, pattern = "^\\d+\\.rds$", full.names = TRUE)
if (length(files) == 0) stop("No draw files in ", DRAWS_DIR)
first <- readRDS(files[1])
# Value columns present (yllref_* only when the summarizer had --ref_lifetable),
# and the breakdown: draws_cause files carry acause, draws_age files carry
# age_group and sex_id. Each breakdown cell is pooled separately; the "slice"
# column names it ("all" for the national totals).
src_vals <- intersect(c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold", "yllref_heat", "yllref_cold"), names(first))
measures <- c("deaths_heat", "deaths_cold", "deaths_nonopt", "yll_heat", "yll_cold", "yll_nonopt",
              if ("yllref_heat" %in% src_vals) c("yllref_heat", "yllref_cold", "yllref_nonopt"))
slice_of <- function(d) {
  if ("acause" %in% names(d)) d$acause
  else if ("age_group" %in% names(d)) paste0(d$age_group, "_sex", d$sex_id)
  else rep("all", nrow(d))
}
# Reported periods -> the draw-file periods each one sums. A reported period must
# be an exact union of file periods, or the sum would silently cover other years.
src_periods <- unique(first$period)
rm(first)
want <- split_csv(PERIODS)
period_map <- if (length(want) == 0) setNames(as.list(src_periods), src_periods) else {
  src_years <- lapply(setNames(src_periods, src_periods), range_years)
  setNames(lapply(want, function(p) {
    y <- range_years(p)
    inside <- vapply(src_years, function(s) all(s %in% y), TRUE)
    crossing <- vapply(src_years, function(s) any(s %in% y) && !all(s %in% y), TRUE)
    if (any(crossing) || !setequal(unlist(src_years[inside]), y))
      stop("--periods: ", p, " is not an exact union of the draw files' periods (",
           paste(src_periods, collapse = ","), ")")
    names(src_years)[inside]
  }), want)
}
batches <- split(names(period_map), ceiling(seq_along(period_map) / as.integer(PERIOD_BATCH)))
loc_of <- function(f) as.integer(sub("\\.rds$", "", basename(f)))

# --- groups -----------------------------------------------------------------
all_locs <- sort(vapply(files, loc_of, 1L, USE.NAMES = FALSE))
gmap <- data.table(location_id = all_locs, group = "World")
if (nzchar(as.character(GROUPS))) {
  g <- fread(as.character(GROUPS))
  stopifnot(all(c("location_id", "group") %in% names(g)))
  miss <- setdiff(g$location_id, all_locs)
  if (length(miss)) log_msg("Groups file: ", length(miss), " locations not in the draw files (left out of their groups): ",
                            paste(head(miss, 20), collapse = ","))
  gmap <- rbind(gmap, unique(g[location_id %in% all_locs, .(location_id = as.integer(location_id), group = as.character(group))]))
}
gsize <- gmap[, .(size = .N), by = group]

levels_pct <- as.numeric(split_csv(INTERVALS))
if (!length(levels_pct) || any(is.na(levels_pct) | levels_pct <= 0 | levels_pct >= 100))
  stop("--intervals must be percentages between 0 and 100, e.g. 95,66")
# Central interval bounds for one vector, named lowerXX / upperXX (type-7 quantiles
# from one sort, the same values as quantile()).
qint <- function(x, prefix) {
  s <- sort(x); n <- length(s); out <- list()
  q <- function(p) { h <- (n - 1) * p + 1; lo <- floor(h + 4 * .Machine$double.eps); hi <- min(lo + 1, n); s[lo] + (h - lo) * (s[hi] - s[lo]) }
  for (L in levels_pct) {
    a <- (1 - L / 100) / 2
    out[[paste0(prefix, "_lower", L)]] <- q(a)
    out[[paste0(prefix, "_upper", L)]] <- q(1 - a)
  }
  out
}

# Which models each target uses, from a first pass over every file.
log_msg("Scanning ", length(files), " draw files for models")
has <- unique(rbindlist(parallel::mclapply(files, function(f) unique(setDT(readRDS(f))[, .(model, scenario)]),
                                           mc.cores = jobs, mc.preschedule = FALSE)))
if (!ref_scen %in% has$scenario) stop("No reference (", ref_scen, ") rows in the draw files")
targets <- setdiff(sort(unique(has$scenario)), ref_scen)
sets <- if (model_set == "paired") {
  setNames(lapply(targets, function(t) { m <- intersect(has[scenario == t, model], has[scenario == ref_scen, model]); list(t = m, r = m) }), targets)
} else if (model_set == "common") {
  common <- Reduce(intersect, split(has$model, has$scenario))
  setNames(rep(list(list(t = common, r = common)), length(targets)), targets)
} else {
  setNames(lapply(targets, function(t) list(t = has[scenario == t, model], r = has[scenario == ref_scen, model])), targets)
}
for (t in targets) log_msg("  ", t, ": ", length(sets[[t]]$t), " models; ", ref_scen, ": ", length(sets[[t]]$r))

# One location's rows for these reported periods, summed over each period's file
# periods, draw by draw.
read_location <- function(f, pers) {
  need <- unique(unlist(period_map[pers]))
  d <- setDT(readRDS(f))[period %in% need]
  d[, slice := slice_of(d)]
  rbindlist(lapply(pers, function(p)
    d[period %in% period_map[[p]], c(list(n_years = sum(n_years)), lapply(.SD, sum)),
      by = .(location_id, model, scenario, slice, draw), .SDcols = src_vals][, period := p]))
}
add_derived <- function(dr) {
  dr[, `:=`(deaths_nonopt = deaths_heat + deaths_cold, yll_nonopt = yll_heat + yll_cold)]
  if ("yllref_heat" %in% names(dr)) dr[, yllref_nonopt := yllref_heat + yllref_cold]
  dr
}

# Pool one measure over units. u: unit_type, unit, model, scenario, slice, period,
# n_years, draw, v (period totals).
pool_measure <- function(u, m) {
  keys <- c("unit_type", "unit", "slice", "period")
  rbindlist(lapply(targets, function(t) {
    S <- sets[[t]]
    a <- u[scenario == t & model %in% S$t]; a[, vt := v / n_years]
    b <- u[scenario == ref_scen & model %in% S$r]; b[, vr := v / n_years]
    if (!nrow(a) || !nrow(b)) return(NULL)
    p <- merge(a[, .(unit_type, unit, slice, model, period, draw, vt)], b[, .(unit_type, unit, slice, model, period, draw, vr)],
               by = c("unit_type", "unit", "slice", "model", "period", "draw"))
    p[, d := vt - vr]
    st <- a[, c(list(n_years = max(n_years), n_models = uniqueN(model), target_mean = mean(vt)), qint(vt, "target")), by = keys]
    sr <- b[, c(list(n_models_ref = uniqueN(model), ref_mean = mean(vr)), qint(vr, "ref")), by = keys]
    sd <- p[, c(list(diff_mean = mean(d)), qint(d, "diff")), by = keys]
    mt <- a[, .(mt = mean(vt)), by = c(keys, "model")]; mr <- b[, .(mr = mean(vr)), by = c(keys, "model")]
    md <- p[, .(md = mean(d)), by = c(keys, "model")]
    mp <- merge(mt, mr, by = c(keys, "model")); mp[, rat := mt / mr]
    r <- Reduce(function(x, y) merge(x, y, by = keys), list(
      mt[, .(target_model_min = min(mt), target_model_max = max(mt)), by = keys],
      mr[, .(ref_model_min = min(mr), ref_model_max = max(mr)), by = keys],
      md[, .(diff_model_min = min(md), diff_model_max = max(md)), by = keys],
      mp[, .(ratio_model_min = min(rat), ratio_model_max = max(rat)), by = keys]))
    s <- Reduce(function(x, y) merge(x, y, by = keys), list(st, sr, sd, r))
    s[, `:=`(ratio = target_mean / ref_mean, target = t, measure = m)]
    s
  }), use.names = TRUE)
}

# Running totals per group, one worker's share of the locations at a time.
GKEYS <- c("model", "scenario", "slice", "period", "draw")
add_to_group <- function(acc, x) {
  if (is.null(acc)) return(x)
  if (nrow(acc) == nrow(x) && all(vapply(GKEYS, function(k) identical(acc[[k]], x[[k]]), TRUE))) {
    for (v in c(src_vals, "nloc")) set(acc, j = v, value = acc[[v]] + x[[v]])
    set(acc, j = "n_years", value = pmax(acc$n_years, x$n_years))
    return(acc)
  }
  z <- rbind(acc, x)
  z <- z[, c(list(n_years = max(n_years)), lapply(.SD, sum)), by = GKEYS, .SDcols = c(src_vals, "nloc")]
  setkeyv(z, GKEYS); z
}
run_share <- function(fs, pers, tag) {
  gacc <- list(); out <- list(); bad <- character(0); short <- character(0)
  exp_n <- vapply(pers, function(p) length(unlist(lapply(period_map[[p]], range_years))), integer(1))
  for (f in fs) {
    l <- loc_of(f)
    dr <- read_location(f, pers)
    # Equal weight per model needs the same number of draws in every model.
    nd <- dr[, .(n = uniqueN(draw)), by = .(model, scenario, slice, period)]
    if (uniqueN(nd$n) != 1) bad <- c(bad, paste0(l, ":", paste(sort(unique(nd$n)), collapse = "/")))
    short <- c(short, dr[n_years < exp_n[period], unique(period)])
    dd <- add_derived(copy(dr))
    u <- dd[, .(unit_type = "location", unit = as.character(location_id), model, scenario, slice, period, n_years, draw)]
    out[[length(out) + 1L]] <- rbindlist(lapply(measures, function(m) { u[, v := dd[[m]]]; pool_measure(copy(u), m) }), use.names = TRUE)
    x <- dr[, c(GKEYS, "n_years", src_vals), with = FALSE][, nloc := 1L]
    setkeyv(x, GKEYS)
    for (g in gmap[location_id == l, group]) gacc[[g]] <- add_to_group(gacc[[g]], if (is.null(gacc[[g]])) copy(x) else x)
    rm(dr, dd, u, x); invisible(gc(FALSE))
  }
  tf <- file.path(OUT, paste0(".tmp_", tag, ".rds"))
  saveRDS(list(out = rbindlist(out, use.names = TRUE), gacc = gacc, bad = bad, short = unique(short)), tf, compress = FALSE)
  tf
}

log_msg("Pooling ", length(files), " locations, periods ", paste(names(period_map), collapse = ","),
        " in ", length(batches), " pass(es), model set ", model_set, ", ", jobs, " worker(s)")
out <- list(); excl <- list()
for (bi in seq_along(batches)) {
  b <- batches[[bi]]
  shares <- split(files, rep_len(seq_len(jobs), length(files)))
  tfs <- parallel::mclapply(seq_along(shares), function(i) run_share(shares[[i]], b, paste0(bi, "_", i)),
                            mc.cores = jobs, mc.preschedule = FALSE)
  errs <- vapply(tfs, function(z) inherits(z, "try-error") || !is.character(z), TRUE)
  if (any(errs)) stop("Worker failed: ", paste(vapply(tfs[errs], function(z) as.character(z)[1], ""), collapse = " | "))
  gacc <- list()
  for (tf in unlist(tfs)) {
    r <- readRDS(tf); unlink(tf)
    if (length(r$bad)) stop("Draw counts differ across models in locations: ", paste(r$bad, collapse = ", "))
    if (length(r$short)) warning("Periods missing some years in some rows: ", paste(r$short, collapse = ","))
    out[[length(out) + 1L]] <- r$out
    for (g in names(r$gacc)) gacc[[g]] <- add_to_group(gacc[[g]], r$gacc[[g]])
    rm(r); invisible(gc(FALSE))
  }
  for (g in names(gacc)) {
    gv <- gacc[[g]]; size <- gsize[group == g, size]
    inc <- unique(gv[nloc < size, .(model, scenario, period)])
    if (nrow(inc)) excl[[length(excl) + 1L]] <- inc[, group := g]
    gv <- add_derived(gv[nloc == size])
    u <- gv[, .(unit_type = "group", unit = g, model, scenario, slice, period, n_years, draw)]
    out[[length(out) + 1L]] <- rbindlist(lapply(measures, function(m) { u[, v := gv[[m]]]; pool_measure(copy(u), m) }), use.names = TRUE)
  }
  rm(gacc); invisible(gc(FALSE))
  log_msg("  done: ", paste(b, collapse = ","))
}
out <- rbindlist(out, use.names = TRUE)
setcolorder(out, c("unit_type", "unit", "slice", "target", "period", "measure", "n_years", "n_models", "n_models_ref"))
setorder(out, unit_type, unit, slice, target, period, measure)
fwrite(out, file.path(OUT, "pooled.csv"))

ms <- rbindlist(lapply(names(sets), function(t) rbind(
  data.table(target = t, model_set = model_set, model = sets[[t]]$t, note = "used"),
  if (!setequal(sets[[t]]$t, sets[[t]]$r)) data.table(target = t, model_set = model_set, model = sets[[t]]$r, note = paste0("used for the reference (", ref_scen, ")")))))
if (length(excl)) {
  ex <- rbindlist(excl)
  ms <- rbind(ms, ex[, .(target = scenario, model_set = model_set, model,
                         note = paste0("left out of group ", group, " for period ", period, ": missing locations"))], fill = TRUE)
}
fwrite(ms, file.path(OUT, "model_sets.csv"))
log_msg("Wrote ", nrow(out), " rows to ", file.path(OUT, "pooled.csv"), " and model_sets.csv")
