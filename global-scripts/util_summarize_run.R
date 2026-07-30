# util_summarize_run.R — collapse a finished production run into review-sized
# tables, and check the arithmetic of every combo on the way past.
#
# A finished scenario is one output file per (location, model, year): on the
# order of 160,000 files for 204 locations x 27 models x 29 years, each holding
# the full (cause x age x sex x draw) grid. That is not something anyone can
# review, and it is too large to move. This reads it once, in parallel, and
# writes a few MB of tidy CSV that can be mailed, plotted, and argued about.
#
# Usage:
#   Rscript global-scripts/util_summarize_run.R --scenarios=ssp245
#   Rscript global-scripts/util_summarize_run.R --scenarios=ssp245 --jobs=16
#   Rscript global-scripts/util_summarize_run.R --locations=125,135 --out=output/summary_spotcheck
#
# Options:
#   --scenarios=  comma separated; default ssp245
#   --locations=  comma separated; default every location with output
#   --years=      default 2022-2050, used to report missing years
#   --jobs=       parallel workers over locations; default half the cores
#   --out=        output directory; default output/summary
#   --force       recompute locations already in the cache
#
# Outputs, under --out:
#   national_by_year.csv  (location, model, scenario, year) x draw mean and 95%
#                         interval for attributable deaths and YLLs. The review
#                         table.
#   by_cause.csv          the same keys plus acause, draw means only.
#   coverage.csv          one row per (location, model, scenario): years found,
#                         years missing, whether YLLs were present.
#   qa_report.txt         the arithmetic checks, worst deviations first.
#
# Restartable: each location's summary is cached under <out>/_cache/, so an
# interrupted run picks up where it stopped. Delete the cache or pass --force to
# recompute.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(SCENARIOS = "ssp245", LOCATIONS = "", YEARS = "2022-2050",
                 JOBS = max(1L, floor(parallel::detectCores() / 2)),
                 OUT = file.path(OUTPUT_DIR, "summary"), FORCE = FALSE)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

split_csv <- function(x) {
  x <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
  x[nzchar(x)]
}

# Same "2022-2050" / "2022,2030" grammar the runners accept. Defined here rather
# than imported because each runner carries its own copy; config.R does not.
parse_years <- function(s) {
  parts <- strsplit(as.character(s), ",", fixed = TRUE)[[1]]
  out <- integer(0)
  for (p in parts) {
    if (grepl("-", p, fixed = TRUE)) {
      ab <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]])
      out <- c(out, seq(ab[1], ab[2]))
    } else {
      out <- c(out, as.integer(p))
    }
  }
  unique(sort(out))
}

scenarios <- split_csv(SCENARIOS)
want_years <- parse_years(YEARS)
cckp_root <- file.path(RESULTS_ROOT, "cckp")
cache_dir <- file.path(OUT, "_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(cckp_root)) stop("No run output at ", cckp_root)

locs <- if (nzchar(as.character(LOCATIONS))) split_csv(LOCATIONS) else {
  l <- basename(list.dirs(cckp_root, recursive = FALSE))
  sort(suppressWarnings(l[!is.na(as.integer(l))]))
}
if (length(locs) == 0) stop("No locations with output under ", cckp_root)

log_msg("Summarizing ", length(locs), " locations x ",
        paste(scenarios, collapse = ","), " over ", JOBS, " workers")

# -----------------------------------------------------------------------------
# One combo file -> (national per-draw totals, per-cause means, QA row)
#
# The YLL file carries every burden column plus ex and the yll_* columns, so
# when it exists it is the only file worth opening. Falling back to burden means
# deaths without YLLs, which is still worth summarizing: a location missing its
# life table should show up as a gap in the review table, not as a missing row.
# -----------------------------------------------------------------------------
read_combo <- function(dir, year) {
  yf <- file.path(dir, sprintf("ylls_%d.rds", year))
  bf <- file.path(dir, sprintf("burden_%d.rds", year))
  src <- if (file.exists(yf)) "ylls" else if (file.exists(bf)) "burden" else return(NULL)
  d <- tryCatch(setDT(readRDS(if (src == "ylls") yf else bf)),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  # 07 emits year_id where 05 emits year; normalise so the rest is one path.
  if (!"year" %in% names(d) && "year_id" %in% names(d)) setnames(d, "year_id", "year")
  list(d = d, src = src)
}

VALUE_COLS <- c("deaths", "deaths_heat", "deaths_cold", "deaths_nonopt",
                "yll_heat", "yll_cold", "yll_nonopt")

summarize_combo <- function(dir, year, loc, model, scen) {
  got <- read_combo(dir, year)
  if (is.null(got)) return(NULL)
  d <- got$d
  vals <- intersect(VALUE_COLS, names(d))

  # --- arithmetic that must hold regardless of climate or scenario -----------
  dev_paf <- if (all(c("paf_nonopt", "paf_heat", "paf_cold") %in% names(d)))
    d[, max(abs(paf_nonopt - (paf_heat + paf_cold)), na.rm = TRUE)] else NA_real_
  dev_deaths <- if (all(c("deaths_nonopt", "deaths_heat", "deaths_cold") %in% names(d)))
    d[, max(abs(deaths_nonopt - (deaths_heat + deaths_cold)), na.rm = TRUE)] else NA_real_
  max_abs_paf <- if ("paf_nonopt" %in% names(d))
    d[, max(abs(paf_nonopt), na.rm = TRUE)] else NA_real_
  n_nonfinite <- sum(vapply(vals, function(cl) sum(!is.finite(d[[cl]])), integer(1)))
  # A combo whose attributable deaths are all zero is the signature of the
  # silent-zero failure mode (a PAF that matched nothing on the merge), so it is
  # worth counting rather than discovering later in a plot.
  all_zero <- if ("deaths_nonopt" %in% names(d))
    isTRUE(all(d$deaths_nonopt == 0, na.rm = TRUE)) else NA

  qa <- data.table(location_id = loc, model = model, scenario = scen, year = year,
                   source = got$src, rows = nrow(d),
                   draws = if ("draw" %in% names(d)) uniqueN(d$draw) else 1L,
                   dev_paf_identity = dev_paf, dev_deaths_identity = dev_deaths,
                   max_abs_paf = max_abs_paf, n_nonfinite = n_nonfinite,
                   all_zero_burden = all_zero)

  # --- national totals, summing within a draw before summarizing across ------
  # Order matters: the uncertainty interval of a national total is the spread of
  # the summed draws, not the sum of per-stratum intervals.
  by_draw <- if ("draw" %in% names(d)) {
    d[, lapply(.SD, sum, na.rm = TRUE), by = draw, .SDcols = vals]
  } else {
    cbind(data.table(draw = 0L), d[, lapply(.SD, sum, na.rm = TRUE), .SDcols = vals])
  }
  nat <- data.table(location_id = loc, model = model, scenario = scen, year = year,
                    n_draws = nrow(by_draw))
  for (v in vals) {
    x <- by_draw[[v]]
    q <- unname(quantile(x, c(0.025, 0.975), na.rm = TRUE, names = FALSE))
    nat[[paste0(v, "_mean")]]  <- mean(x, na.rm = TRUE)
    nat[[paste0(v, "_lower")]] <- q[1]
    nat[[paste0(v, "_upper")]] <- q[2]
  }

  # --- per-cause means (draw mean of the cause total) ------------------------
  cause <- if ("acause" %in% names(d)) {
    cd <- if ("draw" %in% names(d)) {
      d[, lapply(.SD, sum, na.rm = TRUE), by = .(acause, draw), .SDcols = vals][
        , lapply(.SD, mean, na.rm = TRUE), by = acause, .SDcols = vals]
    } else {
      d[, lapply(.SD, sum, na.rm = TRUE), by = acause, .SDcols = vals]
    }
    cbind(data.table(location_id = loc, model = model, scenario = scen, year = year),
          cd)
  } else NULL

  list(nat = nat, cause = cause, qa = qa)
}

# -----------------------------------------------------------------------------
# One location: every combo dir for the requested scenarios
# -----------------------------------------------------------------------------
summarize_location <- function(loc) {
  cache <- file.path(cache_dir, paste0(loc, ".rds"))
  if (!isTRUE(FORCE) && file.exists(cache)) {
    r <- tryCatch(readRDS(cache), error = function(e) NULL)
    if (!is.null(r)) return(r)
  }
  ldir <- file.path(cckp_root, loc)
  dirs <- list.dirs(ldir, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "status"]
  out_nat <- list(); out_cause <- list(); out_qa <- list(); out_cov <- list()
  for (dd in dirs) {
    nm <- basename(dd)
    # "<model>-<scenario>": the scenario never contains a dash, the model
    # usually does (access-cm2-r1i1p1f1), so split on the last one.
    scen  <- sub("^.*-", "", nm)
    model <- sub("-[^-]+$", "", nm)
    if (!scen %in% scenarios) next
    have <- as.integer(sub("^(ylls|burden)_(\\d+)\\.rds$", "\\2",
                           grep("^(ylls|burden)_\\d+\\.rds$",
                                list.files(dd), value = TRUE)))
    have <- sort(unique(have[!is.na(have)]))
    for (y in have) {
      r <- summarize_combo(dd, y, as.integer(loc), model, scen)
      if (is.null(r)) next
      out_nat[[length(out_nat) + 1L]]     <- r$nat
      if (!is.null(r$cause)) out_cause[[length(out_cause) + 1L]] <- r$cause
      out_qa[[length(out_qa) + 1L]]       <- r$qa
    }
    missing <- setdiff(want_years, have)
    out_cov[[length(out_cov) + 1L]] <- data.table(
      location_id = as.integer(loc), model = model, scenario = scen,
      years_found = length(have), years_missing = length(missing),
      missing_years = paste(missing, collapse = " "),
      has_ylls = any(grepl("^ylls_\\d+\\.rds$", list.files(dd))))
  }
  res <- list(
    nat   = if (length(out_nat))   rbindlist(out_nat, fill = TRUE)   else NULL,
    cause = if (length(out_cause)) rbindlist(out_cause, fill = TRUE) else NULL,
    qa    = if (length(out_qa))    rbindlist(out_qa, fill = TRUE)    else NULL,
    cov   = if (length(out_cov))   rbindlist(out_cov, fill = TRUE)   else NULL)
  saveRDS(res, cache)
  res
}

t0 <- Sys.time()
results <- if (JOBS > 1L) {
  parallel::mclapply(locs, function(l)
    tryCatch(summarize_location(l), error = function(e) {
      log_msg("loc ", l, " FAILED: ", conditionMessage(e)); NULL
    }), mc.cores = JOBS, mc.preschedule = FALSE)
} else {
  lapply(locs, summarize_location)
}
bad <- vapply(results, function(r) is.null(r) || inherits(r, "try-error"), logical(1))
if (any(bad)) log_msg("WARN ", sum(bad), " location(s) produced nothing; see messages above")

pull <- function(field) {
  parts <- lapply(results[!bad], `[[`, field)
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) NULL else rbindlist(parts, fill = TRUE)
}
nat <- pull("nat"); cause <- pull("cause"); qa <- pull("qa"); cov <- pull("cov")
if (is.null(nat)) stop("No combos summarized. Wrong --scenarios, or the run has no output yet.")

setorder(nat, location_id, scenario, model, year)
fwrite(nat, file.path(OUT, "national_by_year.csv"))
if (!is.null(cause)) {
  setorder(cause, location_id, scenario, model, year, acause)
  fwrite(cause, file.path(OUT, "by_cause.csv"))
}
if (!is.null(cov)) {
  setorder(cov, location_id, scenario, model)
  fwrite(cov, file.path(OUT, "coverage.csv"))
}

# -----------------------------------------------------------------------------
# QA report
# -----------------------------------------------------------------------------
qa_path <- file.path(OUT, "qa_report.txt")
con <- file(qa_path, "w")
w <- function(...) cat(..., "\n", sep = "", file = con)
w("Run summary QA — ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
w("scenarios: ", paste(scenarios, collapse = ","), "   locations: ", uniqueN(qa$location_id),
  "   combos read: ", nrow(qa))
w("")
w("--- Hard checks (a failure here is a bug, not a modelling choice) ---")
tol <- 1e-6
n_bad_paf    <- sum(qa$dev_paf_identity    > tol,  na.rm = TRUE)
n_bad_deaths <- sum(qa$dev_deaths_identity > 1e-3, na.rm = TRUE)
n_bad_range  <- sum(qa$max_abs_paf > 1 + tol, na.rm = TRUE)
n_bad_finite <- sum(qa$n_nonfinite > 0, na.rm = TRUE)
w(sprintf("  paf_nonopt == paf_heat + paf_cold        : %s  (worst deviation %.3g)",
          if (n_bad_paf == 0) "PASS" else sprintf("**FAIL** in %d combos", n_bad_paf),
          max(qa$dev_paf_identity, na.rm = TRUE)))
w(sprintf("  deaths_nonopt == heat + cold             : %s  (worst deviation %.3g)",
          if (n_bad_deaths == 0) "PASS" else sprintf("**FAIL** in %d combos", n_bad_deaths),
          max(qa$dev_deaths_identity, na.rm = TRUE)))
w(sprintf("  |paf_nonopt| <= 1                        : %s  (max %.4f)",
          if (n_bad_range == 0) "PASS" else sprintf("**FAIL** in %d combos", n_bad_range),
          max(qa$max_abs_paf, na.rm = TRUE)))
w(sprintf("  all values finite                        : %s",
          if (n_bad_finite == 0) "PASS" else sprintf("**FAIL** in %d combos", n_bad_finite)))
w("")
w("--- Shape ---")
w("  draws per combo   : ", paste(sort(unique(qa$draws)), collapse = ", "))
w("  rows per combo    : ", paste(range(qa$rows), collapse = " to "))
w("  read from ylls    : ", sum(qa$source == "ylls"), " of ", nrow(qa),
  " combos (the rest had no YLL file, so no YLL columns)")
n_zero <- sum(qa$all_zero_burden, na.rm = TRUE)
w("  all-zero burden   : ", n_zero, " combos",
  if (n_zero > 0) "  <-- worth investigating; this is the silent-zero signature" else "")
if (n_zero > 0) {
  z <- qa[all_zero_burden == TRUE, .N, by = .(location_id)][order(-N)]
  w("      by location: ", paste(sprintf("%d (%d)", z$location_id, z$N),
                                 collapse = ", "))
}
w("")
if (!is.null(cov)) {
  w("--- Coverage ---")
  w("  expected years per (location, model): ", length(want_years),
    "  (", min(want_years), " to ", max(want_years), ")")
  mm <- cov[, .(models = .N), by = .(location_id, scenario)]
  w("  models per location: median ", median(mm$models),
    ", range ", min(mm$models), " to ", max(mm$models))
  short <- cov[years_missing > 0]
  w("  (location, model) pairs missing at least one year: ", nrow(short))
  if (nrow(short) > 0) {
    for (i in seq_len(min(30L, nrow(short)))) {
      w(sprintf("      loc %d  %s  missing %d: %s", short$location_id[i],
                short$model[i], short$years_missing[i],
                substr(short$missing_years[i], 1, 60)))
    }
    if (nrow(short) > 30) w("      ... and ", nrow(short) - 30, " more (see coverage.csv)")
  }
  no_yll <- cov[has_ylls == FALSE]
  w("  (location, model) pairs with no YLL output: ", nrow(no_yll))
  if (nrow(no_yll) > 0)
    w("      locations: ", paste(sort(unique(no_yll$location_id)), collapse = ", "))
}
close(con)

el <- as.numeric(Sys.time() - t0, units = "mins")
log_msg(sprintf("Summarized %d combos from %d locations in %.1f min",
                nrow(qa), uniqueN(qa$location_id), el))
cat("\nWrote:\n",
    "  ", file.path(OUT, "national_by_year.csv"), "  (", nrow(nat), " rows)\n",
    if (!is.null(cause)) paste0("  ", file.path(OUT, "by_cause.csv"), "  (", nrow(cause), " rows)\n") else "",
    if (!is.null(cov))   paste0("  ", file.path(OUT, "coverage.csv"), "  (", nrow(cov), " rows)\n") else "",
    "  ", qa_path, "\n\n", sep = "")
cat(readLines(qa_path), sep = "\n")
cat("\n")
