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
#     [--ref_scenario=ssp245] [--groups=groups.csv] [--out=output/summary_multimodel]
#
#   --groups=  optional CSV with columns location_id, group: one row per
#              location per group (a location may be in several groups). A
#              "World" group of every location in the draw files is always
#              added.
#
# Outputs, under --out:
#   pooled.csv      one row per (unit, target scenario, period, measure).
#                   unit_type is "location" or "group". Every value is per
#                   year (period total / n_years). Columns: n_models; target_*
#                   and ref_* each with mean, lower, upper (pooled 95%
#                   interval), model_min, model_max (range of per-model
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
                 OUT = file.path(OUTPUT_DIR, "summary_multimodel"))
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}
ref_scen  <- as.character(REF_SCENARIO)
model_set <- as.character(MODEL_SET)
if (!model_set %in% c("paired", "common")) stop("--model_set must be paired or common, not ", model_set)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

files <- list.files(DRAWS_DIR, pattern = "^\\d+\\.rds$", full.names = TRUE)
if (length(files) == 0) stop("No draw files in ", DRAWS_DIR)
dr <- rbindlist(lapply(files, readRDS))
dr[, `:=`(deaths_nonopt = deaths_heat + deaths_cold, yll_nonopt = yll_heat + yll_cold)]
measures <- c("deaths_heat", "deaths_cold", "deaths_nonopt", "yll_heat", "yll_cold", "yll_nonopt")
targets  <- setdiff(sort(unique(dr$scenario)), ref_scen)
if (!ref_scen %in% dr$scenario) stop("No reference (", ref_scen, ") rows in the draw files")
log_msg("Pooling ", nrow(dr), " draw rows: ", uniqueN(dr$location_id), " locations, targets ",
        paste(targets, collapse = ","), ", periods ", paste(unique(dr$period), collapse = ","),
        ", model set ", model_set)

# Equal weight per model needs the same number of draws in every model.
nd <- dr[, .(n = uniqueN(draw)), by = .(location_id, model, scenario, period)]
if (uniqueN(nd$n) != 1) stop("Draw counts differ across models: ", paste(sort(unique(nd$n)), collapse = ","))
# Per-year values need one n_years per (location, scenario, period) across models.
ny <- dr[, .(k = uniqueN(n_years)), by = .(location_id, scenario, period)]
if (any(ny$k > 1)) warning("Some (location, scenario, period) have different n_years across models")

# --- model sets -------------------------------------------------------------
has <- unique(dr[, .(model, scenario)])
sets <- if (model_set == "paired") {
  setNames(lapply(targets, function(t) intersect(has[scenario == t, model], has[scenario == ref_scen, model])), targets)
} else {
  common <- Reduce(intersect, split(has$model, has$scenario))
  setNames(rep(list(common), length(targets)), targets)
}
for (t in targets) log_msg("  ", t, ": ", length(sets[[t]]), " models")

# --- groups -----------------------------------------------------------------
all_locs <- sort(unique(dr$location_id))
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

# --- pooling ----------------------------------------------------------------
q <- function(x, p) unname(quantile(x, p, names = FALSE))
excluded <- list()

pool_measure <- function(m) {
  v <- dr[, .(location_id, model, scenario, period, n_years, draw, v = get(m))]
  loc <- v[, .(unit_type = "location", unit = as.character(location_id), model, scenario, period, n_years, draw, v)]
  gv <- merge(v, gmap, by = "location_id", allow.cartesian = TRUE)[
    , .(v = sum(v), nloc = .N, n_years = max(n_years)), by = .(group, model, scenario, period, draw)]
  gv <- merge(gv, gsize, by = "group")
  inc <- gv[nloc < size, unique(.SD), .SDcols = c("group", "model", "scenario", "period")]
  if (nrow(inc) && m == measures[1]) excluded[[length(excluded) + 1L]] <<- inc
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

out <- rbindlist(lapply(measures, pool_measure))
setcolorder(out, c("unit_type", "unit", "target", "period", "measure", "n_years", "n_models"))
setorder(out, unit_type, unit, target, period, measure)
fwrite(out, file.path(OUT, "pooled.csv"))

ms <- rbindlist(lapply(targets, function(t) data.table(target = t, model_set = model_set, model = sets[[t]], note = "used")))
if (length(excluded)) {
  ex <- rbindlist(excluded)
  ms <- rbind(ms, ex[, .(target = scenario, model_set = model_set, model,
                         note = paste0("left out of group ", group, " for period ", period, ": missing locations"))], fill = TRUE)
}
fwrite(ms, file.path(OUT, "model_sets.csv"))
log_msg("Wrote ", nrow(out), " rows to ", file.path(OUT, "pooled.csv"), " and model_sets.csv")
