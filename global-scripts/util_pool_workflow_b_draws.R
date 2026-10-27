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
#
# Usage:
#   Rscript global-scripts/util_pool_workflow_b_draws.R \
#     --draws_dir=output/summary_workflow_b/draws [--model_set=paired] \
#     [--periods=2022-2030,2031-2040,2041-2050] [--ref_scenario=ssp245] \
#     [--groups=groups.csv] [--out=output/summary_multimodel]
#
#   --periods=       periods to report, as year ranges or single years. Each is
#                    formed by summing the draw files' single-year periods
#                    (export them with --draw_periods=annual). Default empty:
#                    report the draw files' own periods as they are.
#   --period_batch=  how many reported periods to build per pass over the draw
#                    files; lower it if memory runs short. Default 4.
#   --groups=        optional CSV with columns location_id, group: one row per
#                    location per group (a location may be in several
#                    groups). A "World" group of every location in the draw
#                    files is always added.
#
# Outputs, under --out:
#   pooled.csv      one row per (unit, target scenario, period, measure).
#                   unit_type is "location" or "group". Every value is per
#                   year (period total / n_years). Columns: n_years, n_models;
#                   target_* and ref_* each with mean, lower, upper (pooled
#                   95% interval), model_min, model_max (range of per-model
#                   means); diff_* (target minus reference) the same; ratio
#                   (target_mean / ref_mean) and ratio_model_min/max (range of
#                   per-model mean ratios). Measures: deaths and YLLs for heat,
#                   cold and non-optimal (heat + cold).
#   model_sets.csv  the models used for each target, and any model left out
#                   of a group because it lacks some of the group's locations.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(DRAWS_DIR = file.path(OUTPUT_DIR, "summary_workflow_b", "draws"),
                 MODEL_SET = "paired", REF_SCENARIO = "ssp245", GROUPS = "",
                 PERIODS = "", PERIOD_BATCH = 4,
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
if (!model_set %in% c("paired", "common")) stop("--model_set must be paired or common, not ", model_set)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

files <- list.files(DRAWS_DIR, pattern = "^\\d+\\.rds$", full.names = TRUE)
if (length(files) == 0) stop("No draw files in ", DRAWS_DIR)
measures <- c("deaths_heat", "deaths_cold", "deaths_nonopt", "yll_heat", "yll_cold", "yll_nonopt")
src_vals <- c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold")

# Reported periods -> the draw-file periods each one sums.
first <- readRDS(files[1])
src_periods <- unique(first$period)
want <- split_csv(PERIODS)
period_map <- if (length(want) == 0) setNames(as.list(src_periods), src_periods) else {
  if (!all(grepl("^\\d{4}$", src_periods)))
    stop("--periods needs single-year draw files (--draw_periods=annual); these have ", paste(src_periods, collapse = ","))
  setNames(lapply(want, function(p) as.character(range_years(p))), want)
}
rm(first)
batches <- split(names(period_map), ceiling(seq_along(period_map) / as.integer(PERIOD_BATCH)))

# One pass over the draw files: the rows for this batch's periods, summed over
# each period's years, draw by draw.
read_batch <- function(pers) {
  need <- unique(unlist(period_map[pers]))
  rbindlist(lapply(files, function(f) {
    d <- setDT(readRDS(f))[period %in% need]
    rbindlist(lapply(pers, function(p)
      d[period %in% period_map[[p]], c(list(n_years = sum(n_years)), lapply(.SD, sum)),
        by = .(location_id, model, scenario, draw), .SDcols = src_vals][, period := p]))
  }))
}

# --- groups -----------------------------------------------------------------
all_locs <- sort(as.integer(sub("\\.rds$", "", basename(files))))
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

q <- function(x, p) unname(quantile(x, p, names = FALSE))

pool_measure <- function(dr, m, sets, targets, excluded_env) {
  v <- dr[, .(location_id, model, scenario, period, n_years, draw, v = get(m))]
  loc <- v[, .(unit_type = "location", unit = as.character(location_id), model, scenario, period, n_years, draw, v)]
  gv <- merge(v, gmap, by = "location_id", allow.cartesian = TRUE)[
    , .(v = sum(v), nloc = .N, n_years = max(n_years)), by = .(group, model, scenario, period, draw)]
  gv <- merge(gv, gsize, by = "group")
  inc <- unique(gv[nloc < size, .(group, model, scenario, period)])
  if (nrow(inc) && m == measures[1]) excluded_env$x[[length(excluded_env$x) + 1L]] <- inc
  gv <- gv[nloc == size, .(unit_type = "group", unit = group, model, scenario, period, n_years, draw, v)]
  u <- rbind(loc, gv)
  rbindlist(lapply(targets, function(t) {
    S <- sets[[t]]
    p <- merge(u[scenario == t & model %in% S], u[scenario == ref_scen & model %in% S],
               by = c("unit_type", "unit", "model", "period", "draw"), suffixes = c("_t", "_r"))
    p[, `:=`(vt = v_t / n_years_t, vr = v_r / n_years_r)]
    p[, d := vt - vr]
    pm <- p[, .(mt = mean(vt), mr = mean(vr), md = mean(d)), by = .(unit_type, unit, period, model)]
    pm[, rat := mt / mr]
    s <- p[, .(n_years = max(n_years_t), n_models = uniqueN(model),
               target_mean = mean(vt), target_lower = q(vt, 0.025), target_upper = q(vt, 0.975),
               ref_mean = mean(vr), ref_lower = q(vr, 0.025), ref_upper = q(vr, 0.975),
               diff_mean = mean(d), diff_lower = q(d, 0.025), diff_upper = q(d, 0.975)),
           by = .(unit_type, unit, period)]
    r <- pm[, .(target_model_min = min(mt), target_model_max = max(mt),
                ref_model_min = min(mr), ref_model_max = max(mr),
                diff_model_min = min(md), diff_model_max = max(md),
                ratio_model_min = min(rat), ratio_model_max = max(rat)), by = .(unit_type, unit, period)]
    s <- merge(s, r, by = c("unit_type", "unit", "period"))
    s[, `:=`(ratio = target_mean / ref_mean, target = t, measure = m)]
    s
  }))
}

log_msg("Pooling ", length(files), " locations, periods ", paste(names(period_map), collapse = ","),
        " in ", length(batches), " pass(es), model set ", model_set)
out <- list(); sets0 <- NULL; excluded_env <- new.env(); excluded_env$x <- list()
for (b in batches) {
  dr <- read_batch(b)
  dr[, `:=`(deaths_nonopt = deaths_heat + deaths_cold, yll_nonopt = yll_heat + yll_cold)]
  if (!ref_scen %in% dr$scenario) stop("No reference (", ref_scen, ") rows in the draw files")
  targets <- setdiff(sort(unique(dr$scenario)), ref_scen)
  # Equal weight per model needs the same number of draws in every model.
  nd <- dr[, .(n = uniqueN(draw)), by = .(location_id, model, scenario, period)]
  if (uniqueN(nd$n) != 1) stop("Draw counts differ across models: ", paste(sort(unique(nd$n)), collapse = ","))
  # A reported period should have all its years in every model.
  exp_n <- vapply(b, function(p) length(range_years(p)), integer(1))
  short <- dr[n_years < exp_n[period], unique(period)]
  if (length(short)) warning("Periods missing some years in some rows: ", paste(short, collapse = ","))
  has <- unique(dr[, .(model, scenario)])
  sets <- if (model_set == "paired") {
    setNames(lapply(targets, function(t) intersect(has[scenario == t, model], has[scenario == ref_scen, model])), targets)
  } else {
    common <- Reduce(intersect, split(has$model, has$scenario))
    setNames(rep(list(common), length(targets)), targets)
  }
  if (is.null(sets0)) {
    sets0 <- sets
    for (t in targets) log_msg("  ", t, ": ", length(sets[[t]]), " models")
  } else if (!identical(sets, sets0)) warning("Model sets differ between period batches")
  out[[length(out) + 1L]] <- rbindlist(lapply(measures, function(m) pool_measure(dr, m, sets, targets, excluded_env)))
  log_msg("  done: ", paste(b, collapse = ","))
  rm(dr); invisible(gc())
}
out <- rbindlist(out)
setcolorder(out, c("unit_type", "unit", "target", "period", "measure", "n_years", "n_models"))
setorder(out, unit_type, unit, target, period, measure)
fwrite(out, file.path(OUT, "pooled.csv"))

ms <- rbindlist(lapply(names(sets0), function(t) data.table(target = t, model_set = model_set, model = sets0[[t]], note = "used")))
if (length(excluded_env$x)) {
  ex <- rbindlist(excluded_env$x)
  ms <- rbind(ms, ex[, .(target = scenario, model_set = model_set, model,
                         note = paste0("left out of group ", group, " for period ", period, ": missing locations"))], fill = TRUE)
}
fwrite(ms, file.path(OUT, "model_sets.csv"))
log_msg("Wrote ", nrow(out), " rows to ", file.path(OUT, "pooled.csv"), " and model_sets.csv")
