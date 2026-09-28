# util_summarize_workflow_b.R: collapse Workflow B (scaled) output into
# review-sized tables, and check the scaling arithmetic on the way past.
#
# util_apply_workflow_b_batch.R writes one file per (location, model, target
# scenario, year) at output/results/workflow_b/{LOC}/{model}-{target}/wb_{year}.rds,
# holding IHME's deaths, the per-draw scale factor S_target / S_ref, the target
# scenario's PAFs, and the scaled attributable deaths, over the full
# (cause x age x sex x draw) grid. This reads those files, adds YLLs from the
# location's life table (Workflow B output carries no YLLs), sets each combo
# beside the unscaled pipeline burden for the same combo, and writes a few MB of
# CSV plus a QA report.
#
# Usage:
#   Rscript global-scripts/util_summarize_workflow_b.R --scenarios=ssp245,ssp585
#   Rscript global-scripts/util_summarize_workflow_b.R --locations=125,163 --jobs=8
#
# Options:
#   --scenarios=     target scenarios to summarize; default ssp245,ssp585
#   --ref_scenario=  the Workflow B reference; default ssp245. A target equal to
#                    the reference must reproduce the unscaled burden exactly
#                    (scale factor 1 in every draw), which the QA checks.
#   --locations=     comma separated; default every location with Workflow B output
#   --years=         default 2022-2050, used to report missing years
#   --jobs=          parallel workers over locations; default half the cores
#   --out=           output directory; default output/summary_workflow_b
#
# Outputs, under --out:
#   national_by_year.csv  (location, model, scenario, year): draw mean, median
#                         and 95% interval of the national scaled attributable
#                         deaths and YLLs (heat, cold, non-optimal), of IHME's
#                         17-cause deaths before (ihme_deaths) and after scaling
#                         (m_scaled), and the draw mean of the UNSCALED pipeline
#                         burden for the same combo (pipeline_deaths_*_mean).
#   by_cause.csv          the same keys plus acause: draw means, and the scale
#                         factor's draw mean, min and max.
#   coverage.csv          one row per (location, model, scenario): years with
#                         Workflow B output, years with a target burden file but
#                         no Workflow B output, and whether the reference
#                         scenario exists for that model at all.
#   qa_report.txt         the checks, worst first.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(SCENARIOS = "ssp245,ssp585", REF_SCENARIO = "ssp245",
                 LOCATIONS = "", YEARS = "2022-2050",
                 JOBS = max(1L, floor(parallel::detectCores() / 2)),
                 OUT = file.path(OUTPUT_DIR, "summary_workflow_b"))
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

split_csv <- function(x) {
  x <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
  x[nzchar(x)]
}
parse_years <- function(s) {
  parts <- strsplit(as.character(s), ",", fixed = TRUE)[[1]]
  out <- integer(0)
  for (p in parts) {
    if (grepl("-", p, fixed = TRUE)) {
      ab <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]])
      out <- c(out, seq(ab[1], ab[2]))
    } else out <- c(out, as.integer(p))
  }
  unique(sort(out))
}

scenarios  <- split_csv(SCENARIOS)
ref_scen   <- as.character(REF_SCENARIO)
want_years <- parse_years(YEARS)
wb_root    <- file.path(RESULTS_ROOT, "workflow_b")
cckp_root  <- file.path(RESULTS_ROOT, "cckp")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(wb_root)) stop("No Workflow B output at ", wb_root)

locs <- if (nzchar(as.character(LOCATIONS))) split_csv(LOCATIONS) else {
  l <- basename(list.dirs(wb_root, recursive = FALSE))
  sort(suppressWarnings(l[!is.na(as.integer(l))]))
}
if (length(locs) == 0) stop("No locations with output under ", wb_root)
log_msg("Summarizing Workflow B for ", length(locs), " locations x ",
        paste(scenarios, collapse = ","), " over ", JOBS, " workers")

WB_VALS <- c("ihme_deaths", "m_scaled", "deaths_heat_attrib", "deaths_cold_attrib",
             "deaths_nonopt_attrib", "yll_heat", "yll_cold", "yll_nonopt")

load_lifetable <- function(loc) {
  f <- file.path(LIFETABLE_DIR, paste0(loc, "_lifetable.csv"))
  if (!file.exists(f)) return(NULL)
  lt <- fread(f)
  if (!"ex" %in% names(lt) && "ev" %in% names(lt)) setnames(lt, "ev", "ex")
  unique(lt[, .(year = as.integer(year_id), age_group_id = as.integer(age_group_id),
                sex_id = as.integer(sex_id), ex)])
}

summ <- function(x) {
  q <- unname(quantile(x, c(0.025, 0.975), na.rm = TRUE, names = FALSE))
  c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE),
    lower = q[1], upper = q[2])
}

summarize_combo <- function(loc, model, scen, year, lt) {
  f <- file.path(wb_root, loc, paste0(model, "-", scen), sprintf("wb_%d.rds", year))
  d <- tryCatch(setDT(readRDS(f)), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  if (!"draw" %in% names(d)) d[, draw := 0L]

  # YLLs: same life-table merge as 07_compute_ylls.R (national, no age remap).
  n_no_ex <- NA_integer_
  if (!is.null(lt)) {
    d <- merge(d, lt, by = c("year", "age_group_id", "sex_id"), all.x = TRUE)
    n_no_ex <- sum(is.na(d$ex))
    d[is.na(ex), ex := 0]
    d[, `:=`(yll_heat   = deaths_heat_attrib * ex,
             yll_cold   = deaths_cold_attrib * ex,
             yll_nonopt = deaths_nonopt_attrib * ex)]
  }
  vals <- intersect(WB_VALS, names(d))

  # --- per-draw scale factors, one per (cause, draw) ---------------------------
  sf <- unique(d[, .(acause, draw, scale_factor)])
  temp_sf  <- sf[acause %in% CAUSES_IHME_TEMP_SCALED]
  ident_sf <- sf[!acause %in% CAUSES_IHME_TEMP_SCALED]

  qa <- data.table(
    location_id = loc, model = model, scenario = scen, year = year,
    rows = nrow(d), draws = uniqueN(d$draw), causes = uniqueN(d$acause),
    n_na_scale = sum(is.na(sf$scale_factor)),
    n_nonfinite = sum(vapply(vals, function(cl) sum(!is.finite(d[[cl]])), integer(1))),
    dev_heat_cold = d[, max(abs(deaths_nonopt_attrib -
                                (deaths_heat_attrib + deaths_cold_attrib)), na.rm = TRUE)],
    dev_identity_causes = if (nrow(ident_sf)) max(abs(ident_sf$scale_factor - 1), na.rm = TRUE) else 0,
    scale_min = if (nrow(temp_sf)) min(temp_sf$scale_factor, na.rm = TRUE) else NA_real_,
    scale_max = if (nrow(temp_sf)) max(temp_sf$scale_factor, na.rm = TRUE) else NA_real_,
    n_no_ex = n_no_ex)

  # --- national totals: sum within a draw, then summarize across draws -------
  by_draw <- d[, lapply(.SD, sum, na.rm = TRUE), by = draw, .SDcols = vals]
  nat <- data.table(location_id = loc, model = model, scenario = scen, year = year,
                    n_draws = nrow(by_draw))
  for (v in vals) {
    s <- summ(by_draw[[v]])
    for (nm in names(s)) set(nat, j = paste0(v, "_", nm), value = s[[nm]])
  }

  # --- the unscaled pipeline burden for the same combo -----------------------
  bf <- file.path(cckp_root, loc, paste0(model, "-", scen), sprintf("burden_%d.rds", year))
  b <- tryCatch(setDT(readRDS(bf)), error = function(e) NULL)
  if (!is.null(b)) {
    if (!"draw" %in% names(b)) b[, draw := 0L]
    bd <- b[, .(p_heat = sum(deaths_heat), p_cold = sum(deaths_cold),
                p_nonopt = sum(deaths_nonopt)), by = draw]
    set(nat, j = "pipeline_deaths_heat_mean",   value = mean(bd$p_heat))
    set(nat, j = "pipeline_deaths_cold_mean",   value = mean(bd$p_cold))
    set(nat, j = "pipeline_deaths_nonopt_mean", value = mean(bd$p_nonopt))
    # Identity check: when the target IS the reference, every scale factor is
    # 1 and the scaled burden must equal the pipeline burden draw for draw.
    if (scen == ref_scen) {
      m <- merge(by_draw[, .(draw, w = deaths_nonopt_attrib)], bd[, .(draw, p = p_nonopt)], by = "draw")
      qa[, ref_identity_rel_dev := max(abs(m$w - m$p) / pmax(abs(m$p), 1))]
      qa[, ref_identity_scale_dev := max(abs(sf$scale_factor - 1), na.rm = TRUE)]
    }
  }

  # --- per cause --------------------------------------------------------------
  cvals <- setdiff(vals, character(0))
  cd <- d[, lapply(.SD, sum, na.rm = TRUE), by = .(acause, draw), .SDcols = cvals][
    , lapply(.SD, mean, na.rm = TRUE), by = acause, .SDcols = cvals]
  cs <- sf[, .(scale_mean = mean(scale_factor, na.rm = TRUE),
               scale_min = min(scale_factor, na.rm = TRUE),
               scale_max = max(scale_factor, na.rm = TRUE)), by = acause]
  cause <- cbind(data.table(location_id = loc, model = model, scenario = scen, year = year),
                 merge(cd, cs, by = "acause"))

  list(nat = nat, cause = cause, qa = qa)
}

summarize_location <- function(loc) {
  lt <- load_lifetable(loc)
  out_nat <- list(); out_cause <- list(); out_qa <- list(); out_cov <- list()
  for (scen in scenarios) {
    wb_dirs  <- list.dirs(file.path(wb_root, loc), recursive = FALSE)
    tgt_dirs <- list.dirs(file.path(cckp_root, loc), recursive = FALSE)
    suffix <- paste0("-", scen, "$")
    models <- sort(unique(c(sub(suffix, "", basename(grep(suffix, wb_dirs, value = TRUE))),
                            sub(suffix, "", basename(grep(suffix, tgt_dirs, value = TRUE))))))
    for (model in models) {
      wd <- file.path(wb_root, loc, paste0(model, "-", scen))
      td <- file.path(cckp_root, loc, paste0(model, "-", scen))
      rd <- file.path(cckp_root, loc, paste0(model, "-", ref_scen))
      wb_years  <- as.integer(sub("^wb_(\\d+)\\.rds$", "\\1",
                                  grep("^wb_\\d+\\.rds$", list.files(wd), value = TRUE)))
      tgt_years <- as.integer(sub("^burden_(\\d+)\\.rds$", "\\1",
                                  grep("^burden_\\d+\\.rds$", list.files(td), value = TRUE)))
      tgt_years <- intersect(tgt_years, want_years)
      for (y in intersect(sort(wb_years), want_years)) {
        r <- summarize_combo(loc, model, scen, y, lt)
        if (is.null(r)) next
        out_nat[[length(out_nat) + 1L]]     <- r$nat
        out_cause[[length(out_cause) + 1L]] <- r$cause
        out_qa[[length(out_qa) + 1L]]       <- r$qa
      }
      out_cov[[length(out_cov) + 1L]] <- data.table(
        location_id = as.integer(loc), model = model, scenario = scen,
        years_wb = length(intersect(wb_years, want_years)),
        years_target_burden = length(tgt_years),
        years_target_without_wb = length(setdiff(tgt_years, wb_years)),
        ref_scenario_exists = dir.exists(rd))
    }
  }
  list(nat = rbindlist(out_nat, fill = TRUE), cause = rbindlist(out_cause, fill = TRUE),
       qa = rbindlist(out_qa, fill = TRUE), cov = rbindlist(out_cov, fill = TRUE))
}

results <- parallel::mclapply(locs, function(l)
  tryCatch(summarize_location(as.integer(l)), error = function(e) {
    log_msg("ERROR loc=", l, ": ", conditionMessage(e)); NULL }),
  mc.cores = as.integer(JOBS), mc.preschedule = FALSE)
bad <- locs[vapply(results, function(r) is.null(r) || inherits(r, "try-error"), logical(1))]
pull <- function(field) rbindlist(lapply(results, function(r)
  if (is.list(r)) r[[field]] else NULL), fill = TRUE)
nat <- pull("nat"); cause <- pull("cause"); qa <- pull("qa"); cov <- pull("cov")
if (nrow(nat) == 0) stop("No Workflow B combos read.")

setorder(nat, location_id, scenario, model, year)
setorder(cause, location_id, scenario, model, year, acause)
setorder(cov, location_id, scenario, model)
fwrite(nat, file.path(OUT, "national_by_year.csv"))
fwrite(cause, file.path(OUT, "by_cause.csv"))
fwrite(cov, file.path(OUT, "coverage.csv"))

# -----------------------------------------------------------------------------
# QA report
# -----------------------------------------------------------------------------
con <- file(file.path(OUT, "qa_report.txt"), "w")
w <- function(...) cat(..., "\n", sep = "", file = con)
pf <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"
fmt <- function(x) formatC(x, digits = 3, format = "g")

w("Workflow B summary QA, ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
w("reference: ", ref_scen, "   targets: ", paste(scenarios, collapse = ","),
  "   locations: ", length(locs), "   combos read: ", nrow(qa))
if (length(bad)) w("LOCATIONS THAT FAILED TO SUMMARIZE: ", paste(bad, collapse = ","))
w("")
w("--- Hard checks (a failure here is a bug, not a modelling choice) ---")
w("  heat + cold == non-optimal (scaled)          : ", pf(max(qa$dev_heat_cold) < 1e-6),
  "  (worst deviation ", fmt(max(qa$dev_heat_cold)), ")")
w("  all values finite                            : ", pf(sum(qa$n_nonfinite) == 0),
  "  (", sum(qa$n_nonfinite), " non-finite values)")
w("  no NA scale factors                          : ", pf(sum(qa$n_na_scale) == 0),
  "  (", sum(qa$n_na_scale), " cause-draws; NA means PAF >= 1 or a missing scenario row)")
w("  scale factor 1 for the 4 non-temp-scaled causes: ", pf(max(qa$dev_identity_causes) < 1e-12),
  "  (worst |sf - 1| ", fmt(max(qa$dev_identity_causes)), ")")
if ("ref_identity_rel_dev" %in% names(qa) && any(!is.na(qa$ref_identity_rel_dev))) {
  r <- qa[!is.na(ref_identity_rel_dev)]
  w("  target = reference reproduces pipeline burden: ",
    pf(max(r$ref_identity_rel_dev) < 1e-9 && max(r$ref_identity_scale_dev) < 1e-12),
    "  (", nrow(r), " combos; worst relative deviation ", fmt(max(r$ref_identity_rel_dev)),
    ", worst |sf - 1| ", fmt(max(r$ref_identity_scale_dev)), ")")
}
if (all(is.na(qa$n_no_ex))) {
  w("  life tables                                  : FAIL  (no life table found; no YLLs)")
} else {
  w("  every row matched a life-table ex            : ", pf(sum(qa$n_no_ex, na.rm = TRUE) == 0),
    "  (", sum(qa$n_no_ex, na.rm = TRUE), " rows without ex, set to 0)")
}
w("")
w("--- Shape ---")
w("  draws per combo  : ", paste(sort(unique(qa$draws)), collapse = ", "))
w("  causes per combo : ", paste(sort(unique(qa$causes)), collapse = ", "))
w("")
w("--- Scale factor S_target / S_ref for the 13 temp-scaled causes (per cause-draw) ---")
w("  S = 1 / (1 - PAF_nonopt). Expect values near 1; the spread grows with the")
w("  gap between the scenarios' PAFs, so later years should spread more.")
tq <- qa[scenario != ref_scen]
if (nrow(tq)) {
  w("  over all target combos: min ", fmt(min(tq$scale_min, na.rm = TRUE)),
    ", max ", fmt(max(tq$scale_max, na.rm = TRUE)))
  cs <- cause[scenario != ref_scen & acause %in% CAUSES_IHME_TEMP_SCALED,
              .(mean_sf = mean(scale_mean), min_sf = min(scale_min), max_sf = max(scale_max)),
              by = .(scenario, year)][order(scenario, year)]
  w("  by year (draw-mean over locations, models and causes; min and max over all):")
  for (i in seq_len(nrow(cs))) w(sprintf("    %s %d  mean %.4f  min %.4f  max %.4f",
                                         cs$scenario[i], cs$year[i], cs$mean_sf[i],
                                         cs$min_sf[i], cs$max_sf[i]))
}
w("")
w("--- Scaled vs unscaled, national non-optimal deaths (draw means) ---")
w("  ratio = Workflow B / unscaled pipeline burden for the same combo. For the")
w("  reference it must be 1; for a target it is the scaling's net effect.")
if ("pipeline_deaths_nonopt_mean" %in% names(nat)) {
  rr <- nat[, .(ratio = deaths_nonopt_attrib_mean / pipeline_deaths_nonopt_mean),
            by = .(location_id, scenario, model, year)]
  rs <- rr[, .(median = median(ratio, na.rm = TRUE), min = min(ratio, na.rm = TRUE),
               max = max(ratio, na.rm = TRUE)), by = .(location_id, scenario)][order(scenario, location_id)]
  for (i in seq_len(nrow(rs))) w(sprintf("    loc %-4d %s  median %.4f  range %.4f to %.4f",
                                         rs$location_id[i], rs$scenario[i], rs$median[i],
                                         rs$min[i], rs$max[i]))
}
w("")
w("--- Coverage ---")
w("  expected years per (location, model): ", length(want_years), " (",
  min(want_years), " to ", max(want_years), ")")
for (sc in scenarios) {
  cv <- cov[scenario == sc]
  w("  ", sc, ": ", nrow(cv), " (location, model) pairs; ",
    sum(cv$years_wb == length(want_years)), " complete; ",
    sum(cv$years_target_without_wb > 0), " with target burden files but no Workflow B output")
  noref <- cv[ref_scenario_exists == FALSE, unique(model)]
  if (length(noref)) w("    models with no ", ref_scen, " run (cannot be scaled, expected): ",
                       paste(noref, collapse = ", "))
  short <- cv[ref_scenario_exists == TRUE & years_wb < length(want_years)]
  if (nrow(short)) {
    w("    pairs missing years although the reference exists:")
    for (i in seq_len(nrow(short))) w(sprintf("      loc %d %s: %d of %d years",
                                              short$location_id[i], short$model[i],
                                              short$years_wb[i], length(want_years)))
  }
}
close(con)
cat(readLines(file.path(OUT, "qa_report.txt")), sep = "\n")
log_msg("Wrote ", OUT, "/{national_by_year,by_cause,coverage}.csv and qa_report.txt")
