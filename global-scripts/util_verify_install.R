# util_verify_install.R — One command that checks this machine is set up
# correctly and that the parallel-safety fixes actually hold here.
#
#   Rscript global-scripts/util_verify_install.R
#
# Runs a series of independent checks and prints PASS / FAIL for each, then a
# single verdict line. Exits 0 only if every check passed, so it can gate a
# production launch. Nothing here touches the real run: the parallel-safety
# checks use a synthetic mirror in a temporary directory, and no combo output,
# marker, or manifest belonging to a real location is written or removed.
#
# Checks:
#   1  the R package library matches the RENV_ACTIVATE_PROJECT setting and the
#      required packages load
#   2  the same is true when launched from a different working directory
#   3  the shapefile resolves and contains the micro-nations
#   4  the CCKP local mirror is configured and readable
#   5  a grid with an absent model x scenario runs to completion (the bug that
#      silently abandoned the rest of the grid at the first gap)
#   6  per-combo status files and derived progress are written
#   7  a parallel grid produces the same statuses as a serial one
#   8  concurrent combos of one location get private scratch directories
#      (the race that could stamp one combo's burden with another's model)

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
SCRIPTS_DIR <- normalizePath(SCRIPTS_DIR)
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

results <- list()
check <- function(name, fn, detail_on_pass = "") {
  # fn is a FUNCTION, not an expression. An expression argument is a promise
  # evaluated in this frame, so a return() inside it would unwind check()'s
  # caller instead of the check. That is the same mistake that produced the
  # convert-grid abort this script exists to catch.
  out <- tryCatch(fn(), error = function(e) structure(FALSE, detail = conditionMessage(e)))
  passed <- isTRUE(out)
  detail <- if (!is.null(attr(out, "detail"))) attr(out, "detail")
            else if (passed) detail_on_pass else "see above"
  results[[length(results) + 1]] <<- data.table(check = name, pass = passed,
                                                detail = detail)
  cat(sprintf("[%s] %-52s %s\n", if (passed) "PASS" else "FAIL", name, detail))
  invisible(passed)
}
ok_with <- function(detail) structure(TRUE, detail = detail)
no_with <- function(detail) structure(FALSE, detail = detail)

cat("\n=== Verifying install at ", PROJECT_ROOT, " ===\n\n", sep = "")

# --- 1. package library + packages -------------------------------------------
# renv is opt-in (see config.R): without RENV_ACTIVATE_PROJECT=TRUE the
# machine's own library is the intended one, so "renv not active" is the
# expected PASS state, not a failure.
check("R package library matches renv setting", function() {
  optin <- tolower(Sys.getenv("RENV_ACTIVATE_PROJECT", "FALSE")) %in%
             c("true", "t", "1")
  lib <- normalizePath(file.path(PROJECT_ROOT, "renv", "library"), mustWork = FALSE)
  active <- any(startsWith(normalizePath(.libPaths(), mustWork = FALSE), lib))
  if (optin && active)
    ok_with(paste("renv:", basename(.libPaths()[1])))
  else if (optin)
    no_with("RENV_ACTIVATE_PROJECT=TRUE but renv library not on .libPaths(); run renv::restore() from the repo root")
  else if (active)
    no_with("renv is active although RENV_ACTIVATE_PROJECT is not TRUE")
  else
    ok_with("system library (renv opt-in not set)")
})

check("required packages load", function() {
  need <- c("data.table", "sf", "ncdf4")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0)
    return(no_with(paste("missing:", paste(miss, collapse = ", "))))
  # Compare the loaded data.table against the lockfile pin. Informational
  # only: in system-library mode a different version is allowed, but the
  # operator should know about the drift.
  have <- as.character(packageVersion("data.table"))
  # The lockfile comparison is a courtesy, so it must never be able to fail the
  # check. Reading it unguarded made a missing or unreadable renv.lock report
  # "cannot open the connection" as a package failure, which is both wrong and
  # alarming: it says the packages did not load when they loaded fine.
  pin <- tryCatch({
    f <- file.path(PROJECT_ROOT, "renv.lock")
    if (!file.exists(f)) NA_character_ else {
      lock <- readLines(f, warn = FALSE)
      i <- grep('"Package": "data.table"', lock, fixed = TRUE)
      if (length(i) == 1) {
        v <- grep('"Version"', lock[i + seq_len(5)], value = TRUE)[1]
        sub('.*"Version": "([^"]+)".*', "\\1", v)
      } else NA_character_
    }
  }, error = function(e) NA_character_, warning = function(w) NA_character_)
  note <- if (!is.na(pin) && !identical(have, pin))
    paste0(" (lockfile pins ", pin, ")") else ""
  ok_with(paste0("data.table ", have, note))
})

# --- 2. works from another working directory ---------------------------------
check("runs from any working directory", function() {
  probe <- tempfile(fileext = ".R")
  writeLines(c(
    'SCRIPTS_DIR <- commandArgs(TRUE)[1]',
    'source(file.path(SCRIPTS_DIR, "config.R"))',
    'suppressPackageStartupMessages(library(data.table))',
    'cat("OK", as.character(packageVersion("data.table")), "\n")'), probe)
  # Rscript inherits this process's cwd, so run the probe from a directory that
  # is certainly not the project root. That is the case that used to fail:
  # path resolution and (when opted in) renv activation both depended on the
  # working directory, so launches from elsewhere broke silently.
  old <- setwd(tempdir()); on.exit(setwd(old), add = TRUE)
  out <- suppressWarnings(system2("Rscript", c(probe, SCRIPTS_DIR),
                                  stdout = TRUE, stderr = FALSE))
  setwd(old)
  unlink(probe)
  hit <- grep("^OK ", out, value = TRUE)
  if (length(hit) == 1)
    ok_with(paste("from", basename(tempdir()), "| data.table",
                  sub("^OK ", "", hit[1])))
  else no_with("config.R did not load cleanly from a foreign directory")
})

# --- 3. shapefile ------------------------------------------------------------
check("shapefile resolves and covers all locations", function() {
  suppressPackageStartupMessages(library(sf))
  sf::sf_use_s2(FALSE)
  if (!file.exists(DEFAULT_SHAPEFILE)) return(no_with(paste("missing:", DEFAULT_SHAPEFILE)))
  shp <- sf::st_read(DEFAULT_SHAPEFILE, quiet = TRUE)
  l3 <- shp[!is.na(shp$level) & shp$level == 3, ]
  micro <- c(14, 24, 367, 369, 413, 416)  # Maldives, Marshall Is, Monaco, Nauru, Tokelau, Tuvalu
  have <- sum(micro %in% l3$loc_id)
  msg <- sprintf("%s | %d level-3 | %d/6 micro-nations",
                 basename(DEFAULT_SHAPEFILE), nrow(l3), have)
  if (nrow(l3) >= 204 && have == 6) ok_with(msg)
  else no_with(paste(msg, "- run util_augment_shapefile.R"))
})

# --- 4. CCKP mirror ----------------------------------------------------------
check("CCKP local mirror is readable", function() {
  if (!nzchar(CCKP_LOCAL_ROOT)) return(no_with("CCKP_LOCAL_ROOT is empty"))
  if (!dir.exists(CCKP_LOCAL_ROOT))
    return(no_with(paste("not a directory:", CCKP_LOCAL_ROOT)))
  n <- length(list.files(file.path(CCKP_LOCAL_ROOT, "cmip6-daily-x0.25", "tas"),
                         recursive = FALSE))
  if (n > 0) ok_with(sprintf("%s (%d model-scenario dirs)", CCKP_LOCAL_ROOT, n))
  else no_with(paste("no cmip6-daily-x0.25/tas content under", CCKP_LOCAL_ROOT))
})

# --- synthetic mirror shared by the behavioural checks -----------------------
# A gap model with no files at all, and a present model whose files are not real
# NetCDFs. The conversion of the present model is EXPECTED to fail; what is
# under test is that the grid keeps going and records every combo either way.
tmp_root <- file.path(tempdir(), "verify_cckp")
unlink(tmp_root, recursive = TRUE)
pm <- "verify-present-r1i1p1f1"
dir.create(file.path(tmp_root, "mirror", "cmip6-daily-x0.25", "tas",
                     paste0(pm, "-ssp245")), recursive = TRUE)
dir.create(file.path(tmp_root, "mirror", "pop-x0.25", "popcount",
                     "gpw-v4-rev11-ssp245"), recursive = TRUE)
for (y in 2022:2024) {
  writeLines("not-a-netcdf",
             file.path(tmp_root, "mirror", "cmip6-daily-x0.25", "tas",
                       paste0(pm, "-ssp245"),
                       sprintf(paste0("timeseries-tas-daily-mean_cmip6-daily-x0.25_",
                                      pm, "-ssp245_timeseries_mean_%d.nc"), y)))
}
writeLines("not-a-netcdf",
           file.path(tmp_root, "mirror", "pop-x0.25", "popcount",
                     "gpw-v4-rev11-ssp245",
                     paste0("climatology-popcount-annual-mean_pop-x0.25_",
                            "gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc")))

# A throwaway location id so nothing here can collide with a real location's
# markers, outputs or manifest.
VLOC <- 999999
run_convert <- function(n_cores, models) {
  manifest <- file.path(tmp_root, sprintf("manifest_%d.csv", n_cores))
  unlink(manifest)
  args <- c(file.path(SCRIPTS_DIR, "util_run_cckp_pipeline.R"),
            paste0("--location_id=", VLOC),
            paste0("--models=", paste(models, collapse = ",")),
            "--scenarios=ssp245", "--years=2022-2024",
            paste0("--cckp_cache=", file.path(tmp_root, "cache", n_cores)),
            paste0("--output_root=", file.path(tmp_root, "out", n_cores)),
            paste0("--manifest=", manifest),
            paste0("--n_cores=", n_cores))
  env <- c(paste0("CCKP_LOCAL_ROOT=", file.path(tmp_root, "mirror")),
           "CCKP_REQUIRE_LOCAL=TRUE")
  suppressWarnings(system2("Rscript", args, env = env,
                           stdout = file.path(tmp_root, sprintf("log_%d.txt", n_cores)),
                           stderr = file.path(tmp_root, sprintf("log_%d.txt", n_cores))))
  if (file.exists(manifest)) fread(manifest) else data.table()
}

models <- c("verify-gap-r1i1p1f1", pm)   # gap FIRST: this is what used to abort
m1 <- run_convert(1, models)

# --- 5. grid survives an absent model x scenario -----------------------------
check("grid completes past an absent model x scenario", function() {
  if (nrow(m1) == 0) return(no_with("no manifest written; the grid exited early"))
  if (nrow(m1) != 6) return(no_with(sprintf("manifest has %d rows, expected 6", nrow(m1))))
  n_gap <- sum(m1$status == "missing-on-s3")
  if (n_gap != 3) return(no_with(sprintf("%d gap combos recorded, expected 3", n_gap)))
  ok_with("6/6 combos recorded, 3 gaps skipped not fatal")
})

# --- 6. per-combo status + derived progress ----------------------------------
check("per-combo status files and progress are written", function() {
  st <- read_combo_statuses(VLOC, "convert")
  prog <- file.path(cckp_marker_root(), VLOC, "_progress.tsv")
  if (nrow(st) != 6) return(no_with(sprintf("%d status files, expected 6", nrow(st))))
  if (!file.exists(prog)) return(no_with("no _progress.tsv derived"))
  ok_with(sprintf("6 status files + progress at %s", dirname(prog)))
})

# --- 7. parallel matches serial ----------------------------------------------
check("parallel grid matches serial grid", function() {
  s1 <- read_combo_statuses(VLOC, "convert")[, .(model, scenario, year, status)]
  m4 <- run_convert(4, models)
  s4 <- read_combo_statuses(VLOC, "convert")[, .(model, scenario, year, status)]
  setorder(s1, model, scenario, year); setorder(s4, model, scenario, year)
  if (nrow(m4) != 6) return(no_with(sprintf("parallel manifest has %d rows, expected 6", nrow(m4))))
  if (!isTRUE(all.equal(s1, s4))) return(no_with("statuses differ between serial and parallel"))
  ok_with("4 workers: identical statuses, 6/6 combos")
})

# --- 8. per-combo scratch isolation ------------------------------------------
# The failure this guards against is silent: two combos of one location sharing
# INTERMEDIATE_DIR and the RESULTS_DIR staging files, so one combo's burden gets
# stamped with another's model. Assert the tag actually redirects both.
check("concurrent combos get private scratch dirs", function() {
  probe <- tempfile(fileext = ".R")
  writeLines(c(
    'SCRIPTS_DIR <- commandArgs(TRUE)[1]',
    'LOCATION_ID <- 999999',
    'source(file.path(SCRIPTS_DIR, "config.R"))',
    'cat("INTERMEDIATE", INTERMEDIATE_DIR, "\n")',
    'cat("RESULTS", RESULTS_DIR, "\n")',
    'cat("ROOT", RESULTS_ROOT, "\n")'), probe)
  get <- function(tag) {
    out <- suppressWarnings(system2("Rscript", c(probe, SCRIPTS_DIR),
                                    env = if (nzchar(tag)) paste0("PIPELINE_RUN_TAG=", tag) else character(),
                                    stdout = TRUE, stderr = FALSE))
    setNames(lapply(c("INTERMEDIATE", "RESULTS", "ROOT"), function(k) {
      v <- grep(paste0("^", k, " "), out, value = TRUE)
      if (length(v)) sub(paste0("^", k, " "), "", trimws(v[1])) else NA_character_
    }), c("INTERMEDIATE", "RESULTS", "ROOT"))
  }
  a <- get("modelA__ssp245__2030")
  b <- get("modelB__ssp245__2030")
  none <- get("")
  unlink(probe)
  if (anyNA(unlist(a)) || anyNA(unlist(b))) return(no_with("probe did not report paths"))
  if (identical(a$INTERMEDIATE, b$INTERMEDIATE))
    return(no_with("two combos share INTERMEDIATE_DIR"))
  if (identical(a$RESULTS, b$RESULTS))
    return(no_with("two combos share the RESULTS_DIR staging path"))
  if (!identical(a$ROOT, b$ROOT))
    return(no_with("RESULTS_ROOT moved with the tag; markers would scatter"))
  if (grepl("_staging", none$RESULTS, fixed = TRUE))
    return(no_with("untagged run was redirected; single-combo layout changed"))
  ok_with("distinct scratch per combo, shared canonical root")
})

# --- 9. real-data equivalence: parallel burden == serial burden --------------
# The strongest check available, and the one that would catch the scratch-dir
# race in the form that actually matters: run the SAME combos serially and in
# parallel on real inputs and require identical burden numbers, with each
# combo's stamped model matching the directory it landed in.
#
# Needs a location that already has converted temperature for 2+ models plus
# mortality. Skipped (not failed) when no such location exists yet, since on a
# fresh machine there is nothing converted to compare.
check("parallel burden matches serial on real data", function() {
  temp_root <- file.path(TEMP_DIR, "cckp")
  if (!dir.exists(temp_root)) return(ok_with("SKIPPED: nothing converted yet"))

  cand <- NULL
  for (loc in basename(list.dirs(temp_root, recursive = FALSE))) {
    mort <- Sys.glob(file.path(MORTALITY_DIR,
                               sprintf("%s_mortality_ihme_*_draws.rds", loc)))
    if (length(mort) == 0) next
    combos <- list.dirs(file.path(temp_root, loc), recursive = FALSE)
    yrs <- lapply(combos, function(cd) {
      f <- list.files(cd, pattern = "^daily_temp_\\d+\\.rds$")
      as.integer(sub("^daily_temp_(\\d+)\\.rds$", "\\1", f))
    })
    common <- Reduce(intersect, yrs)
    common <- common[common >= 2022]           # IHME forecasts start at 2022
    if (length(combos) >= 2 && length(common) >= 1) {
      cand <- list(loc = loc, year = min(common), mort = mort,
                   models = sub("-[^-]+$", "", basename(combos))[1:2])
      break
    }
  }
  if (is.null(cand))
    return(ok_with("SKIPPED: no location has 2+ converted models for a 2022+ year"))

  # Run in a throwaway project root so real outputs are never touched. data/ and
  # renv/ are symlinked in; output/ is fresh.
  tr <- file.path(tempdir(), "verify_equiv")
  unlink(tr, recursive = TRUE); dir.create(tr, recursive = TRUE)
  file.symlink(DATA_DIR, file.path(tr, "data"))
  if (dir.exists(file.path(PROJECT_ROOT, "renv")))
    file.symlink(file.path(PROJECT_ROOT, "renv"), file.path(tr, "renv"))
  on.exit(unlink(tr, recursive = TRUE), add = TRUE)

  run <- function(n_cores) {
    unlink(file.path(tr, "output", "results"), recursive = TRUE)
    unlink(file.path(tr, "output", "intermediate"), recursive = TRUE)
    suppressWarnings(system2("Rscript", c(
      file.path(SCRIPTS_DIR, "util_run_cckp_burden.R"),
      paste0("--location_id=", cand$loc),
      paste0("--models=", paste(cand$models, collapse = ",")),
      "--scenarios=ssp245", paste0("--years=", cand$year),
      "--use_draws_run=FALSE", paste0("--n_cores=", n_cores),
      paste0("--mortality_file=", paste(cand$mort, collapse = ","))),
      env = paste0("PROJECT_ROOT=", tr), stdout = FALSE, stderr = FALSE))
    fs <- list.files(file.path(tr, "output", "results", "cckp"),
                     pattern = sprintf("^burden_%d\\.rds$", cand$year),
                     recursive = TRUE, full.names = TRUE)
    if (length(fs) == 0) return(NULL)
    d <- data.table::rbindlist(lapply(fs, function(f) {
      x <- data.table::as.data.table(readRDS(f))
      x[, dir := basename(dirname(f))][]
    }), fill = TRUE)
    data.table::setorderv(d, names(d))
    d
  }

  a <- run(1); b <- run(2)
  if (is.null(a) || is.null(b))
    return(no_with("burden produced no output for the sampled location"))
  if (nrow(a) == 0)
    return(no_with("burden produced 0 rows; cannot compare"))
  if (!isTRUE(all.equal(a, b)))
    return(no_with("PARALLEL OUTPUT DIFFERS FROM SERIAL - do not run in parallel"))
  # Each combo's stamped model must match the directory it was written into.
  mis <- b[, .(bad = any(model != sub("-ssp245$", "", dir))), by = dir][bad == TRUE]
  if (nrow(mis) > 0)
    return(no_with("a combo was stamped with the wrong model"))
  ok_with(sprintf("loc %s, %d models, %d: identical (%d rows)",
                  cand$loc, length(cand$models), cand$year, nrow(a)))
})

# The conversion refactor is no longer checked here. It was a regression test
# comparing the new implementation against its predecessor read out of git, which
# made sense while the change was in review. Now that it is merged, "main" IS the
# new implementation, so the comparison compares merged code against itself and
# proves nothing, while still costing two full NetCDF reads and being able to
# fail for reasons that have nothing to do with this machine. The test itself
# lives on in util_test_convert_mask.R for anyone revisiting that code; it takes
# --ref=<pre-merge commit> to name the version to compare against.

unlink(tmp_root, recursive = TRUE)
unlink(file.path(cckp_marker_root(), as.character(VLOC)), recursive = TRUE)
unlink(file.path(OUTPUT_DIR, "intermediate", as.character(VLOC)), recursive = TRUE)

res <- rbindlist(results)
cat("\n=== ", sum(res$pass), " of ", nrow(res), " checks passed ===\n", sep = "")
if (all(res$pass)) {
  cat("\nThis machine is ready. Launch with:  ./run_production.sh\n\n")
  quit(status = 0)
} else {
  cat("\nFAILED CHECKS:\n")
  for (i in which(!res$pass)) cat("  - ", res$check[i], ": ", res$detail[i], "\n", sep = "")
  cat("\nSend this output to Aaron; do not start the production run yet.\n\n")
  quit(status = 1)
}
