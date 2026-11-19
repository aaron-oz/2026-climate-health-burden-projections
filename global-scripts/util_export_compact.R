# util_export_compact.R: write the compact copy of the scaled scenario output
# (stage A of the final summary pass), and check on every file read that the
# full output rebuilds exactly from it.
#
# Why this works. In Workflow B (util_workflow_b_ratio.R) the attributable
# deaths for one (location, model, scenario, year) are
#     deaths_heat = ihme_deaths(cause, age, sex, draw) x S(cause, draw) x paf_heat(cause, draw)
#     deaths_cold = ihme_deaths(cause, age, sex, draw) x S(cause, draw) x paf_cold(cause, draw)
# where S is the scale factor (1 for the reference scenario and for the four
# causes IHME does not temperature-scale) and the PAFs are broadcast across age
# and sex. IHME's deaths do not depend on the climate model or the scenario. So
# the full output (about 52 GB per country) is the product of two small arrays:
#     ihme   [draw, sex, age, cause, year]                 one per location
#     factor [draw, cause, (S, paf_heat, paf_cold), year]  one per (model, scenario)
# Both are stored as 4-byte floats (relative rounding error at most 6e-8).
#
# Sources read, per location (RESULTS_ROOT = output/results):
#   - target scenarios: workflow_b/{LOC}/{model}-{scen}/wb_{year}.rds
#     (ihme_deaths, scale_factor, paf_heat, paf_cold, deaths_*_attrib)
#   - the reference scenario: cckp/{LOC}/{model}-{ref}/burden_{year}.rds, the
#     pipeline burden (deaths = IHME deaths, paf_heat, paf_cold, deaths_heat,
#     deaths_cold); its scale factor is 1 by definition, and the existing
#     summarizer's identity check shows its scaled burden equals this.
#
# Checks, on every file (written to checks.csv and summarized in report.txt):
#   - grid complete: every (cause, age, sex, draw) cell present exactly once;
#   - one subnational unit (national runs; a location with several sublocs
#     stops, since its PAFs differ by subloc and the factorization fails);
#   - broadcast: S and the PAFs constant across age and sex within (cause, draw);
#   - IHME deaths identical across every model, scenario and the reference
#     (max relative difference against the first file read for that year);
#   - rebuild: deaths_heat and deaths_cold rebuilt from the STORED 4-byte
#     values against the file's own values (max relative error over cells with
#     |deaths| > 1e-6, and max absolute error).
#   - heat + cold = non-optimal in the file.
#
# Usage:
#   Rscript global-scripts/util_export_compact.R [--out=output/compact] [--jobs=8]
#     [--scenarios=ssp126,ssp245,ssp370,ssp585] [--ref_scenario=ssp245]
#     [--locations=125,163] [--years=2022-2050] [--temp_years=2022,2030,2040,2050]
#     [--overwrite=FALSE]
#
# Output, under --out:
#   {LOC}/ihme.f32      float32 array [draw, sex, age, cause, year]
#   {LOC}/factor.f32    float32, one block per combo in meta$combos order, each
#                       [draw, cause, quantity (S, paf_heat, paf_cold), year];
#                       NaN where a combo lacks a year
#   {LOC}/meta.rds      list: location_id, dims and labels (draws, sexes, ages,
#                       causes, years, quantities), combos (model, scenario,
#                       years present), temp (population-weighted annual mean
#                       temperature per combo and --temp_years year), format note
#   {LOC}/checks.csv    one row per file read, the checks above
#   manifest.csv        one row per location: combos, files, worst check values, status
#   report.txt          pass/fail summary
#   read_compact.R      a copy of the reader (global-scripts/util_compact_io.R)
# A location whose {LOC}/meta.rds exists is skipped unless --overwrite=TRUE, so
# an interrupted run can be restarted.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
source(file.path(SCRIPTS_DIR, "util_compact_io.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(OUT = file.path(OUTPUT_DIR, "compact"), JOBS = max(1L, floor(parallel::detectCores() / 2)),
                 SCENARIOS = "ssp126,ssp245,ssp370,ssp585", REF_SCENARIO = "ssp245",
                 LOCATIONS = "", YEARS = "2022-2050", TEMP_YEARS = "2022,2030,2040,2050",
                 OVERWRITE = "FALSE")
for (k in names(defaults)) if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())

split_csv <- function(x) { x <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]]); x[nzchar(x)] }
parse_years <- function(s) sort(unique(unlist(lapply(split_csv(s), function(p) {
  ab <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]]); if (length(ab) == 2) seq(ab[1], ab[2]) else ab }))))

scenarios <- split_csv(SCENARIOS); ref_scen <- as.character(REF_SCENARIO)
years <- parse_years(YEARS); temp_years <- parse_years(TEMP_YEARS)
overwrite <- isTRUE(as.logical(OVERWRITE))
wb_root <- file.path(RESULTS_ROOT, "workflow_b"); cckp_root <- file.path(RESULTS_ROOT, "cckp")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
CAUSES <- GBD_CAUSES; AGES <- seq(0L, 80L, 5L); SEXES <- c(1L, 2L)

locs <- if (nzchar(as.character(LOCATIONS))) as.integer(split_csv(LOCATIONS)) else {
  l <- basename(list.dirs(cckp_root, recursive = FALSE)); sort(suppressWarnings(as.integer(l[!is.na(suppressWarnings(as.integer(l)))])))
}
log_msg("Compact export: ", length(locs), " locations, scenarios ", paste(scenarios, collapse = ","),
        " (reference ", ref_scen, "), years ", min(years), "-", max(years), ", ", JOBS, " workers -> ", OUT)

popw_temp <- function(loc, model, scen, year) {
  f <- file.path(TEMP_DIR, "cckp", loc, paste0(model, "-", scen), sprintf("daily_temp_%d.rds", year))
  t <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(t)) return(NA_real_)
  setDT(t); t[is.finite(daily_temp) & is.finite(pop), sum(daily_temp * pop) / sum(pop)]
}

# Read one source file into flat vectors on the fixed grid. Returns NULL if absent.
read_one <- function(loc, model, scen, year) {
  is_ref <- scen == ref_scen
  f <- if (is_ref) file.path(cckp_root, loc, paste0(model, "-", scen), sprintf("burden_%d.rds", year))
       else file.path(wb_root, loc, paste0(model, "-", scen), sprintf("wb_%d.rds", year))
  if (!file.exists(f)) return(NULL)
  d <- setDT(readRDS(f))
  if (!"draw" %in% names(d)) d[, draw := 0L]
  if (is_ref) d[, `:=`(ihme_deaths = deaths, scale_factor = 1, deaths_heat_attrib = deaths_heat,
                       deaths_cold_attrib = deaths_cold, deaths_nonopt_attrib = deaths_nonopt)]
  list(d = d, file = f)
}

export_location <- function(loc) {
  ldir <- file.path(OUT, loc)
  if (!overwrite && file.exists(file.path(ldir, "meta.rds"))) return(list(status = "skipped (exists)", loc = loc))
  dir.create(ldir, recursive = TRUE, showWarnings = FALSE)
  # combos: every (model, scenario) directory that exists for this location
  combos <- rbindlist(lapply(scenarios, function(s) {
    root <- if (s == ref_scen) file.path(cckp_root, loc) else file.path(wb_root, loc)
    dd <- basename(list.dirs(root, recursive = FALSE)); dd <- grep(paste0("-", s, "$"), dd, value = TRUE)
    if (!length(dd)) return(NULL)
    data.table(model = sub(paste0("-", s, "$"), "", dd), scenario = s)
  }))
  if (!nrow(combos)) stop("no output for location ", loc)
  setorder(combos, scenario, model)
  D <- NULL; draws <- NULL
  ihme <- NULL; ihme_have <- logical(length(years))
  ffac <- file.path(ldir, "factor.f32.tmp"); con <- file(ffac, "wb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  checks <- list(); present <- vector("list", nrow(combos)); temps <- list()
  for (k in seq_len(nrow(combos))) {
    m <- combos$model[k]; s <- combos$scenario[k]
    fac <- NULL; yrs <- integer(0)
    for (yi in seq_along(years)) {
      y <- years[yi]
      r <- read_one(loc, m, s, y)
      if (is.null(r)) next
      d <- r$d
      if (is.null(D)) {
        draws <- sort(unique(d$draw)); D <- length(draws)
        ihme <- array(NA_real_, c(D, 2L, 17L, 17L, length(years)))
      }
      if (is.null(fac)) fac <- array(NaN, c(D, 17L, 3L, length(years)))
      n_sub <- uniqueN(d$subloc_id)
      if (n_sub != 1) stop(sprintf("loc %s %s-%s %d: %d sublocs; the compact form assumes national runs", loc, m, s, y, n_sub))
      di <- match(d$draw, draws); ci <- match(d$acause, CAUSES); ai <- match(as.integer(d$age_group_id), AGES); si <- match(as.integer(d$sex_id), SEXES)
      grid_ok <- !anyNA(di) && !anyNA(ci) && !anyNA(ai) && !anyNA(si) && nrow(d) == D * 2L * 17L * 17L
      cell <- di + D * (si - 1L) + D * 2L * (ai - 1L) + D * 2L * 17L * (ci - 1L)
      grid_ok <- grid_ok && !anyDuplicated(cell)
      if (!grid_ok) stop(sprintf("loc %s %s-%s %d: incomplete or duplicated (cause, age, sex, draw) grid (%d rows)", loc, m, s, y, nrow(d)))
      # IHME deaths: stored once per year, compared on every later file
      iv <- numeric(D * 2L * 17L * 17L); iv[cell] <- d$ihme_deaths
      iv32 <- f32(iv)
      if (!ihme_have[yi]) { ihme[, , , , yi] <- iv32; ihme_have[yi] <- TRUE; ihme_dev <- 0 }
      else { ref <- as.vector(ihme[, , , , yi]); ihme_dev <- max(abs(iv32 - ref) / pmax(abs(ref), 1)) }
      # factors per (cause, draw), with the broadcast check
      fi <- di + D * (ci - 1L)
      bc <- 0
      for (q in 1:3) {
        v <- switch(q, d$scale_factor, d$paf_heat, d$paf_cold)
        x <- numeric(D * 17L); x[fi] <- v
        bc <- max(bc, max(abs(v - x[fi])))
        fac[, , q, yi] <- f32(x)
      }
      # rebuild from the stored values
      S <- as.vector(fac[, , 1, yi]); ph <- as.vector(fac[, , 2, yi]); pc <- as.vector(fac[, , 3, yi])
      ib <- ihme[, , , , yi][cell]
      rh <- ib * S[fi] * ph[fi]; rc <- ib * S[fi] * pc[fi]
      big_h <- abs(d$deaths_heat_attrib) > 1e-6; big_c <- abs(d$deaths_cold_attrib) > 1e-6
      checks[[length(checks) + 1L]] <- data.table(
        location_id = loc, model = m, scenario = s, year = y, rows = nrow(d), draws = D,
        ihme_rel_dev = ihme_dev, broadcast_dev = bc,
        rebuild_rel_heat = if (any(big_h)) max(abs(rh[big_h] - d$deaths_heat_attrib[big_h]) / abs(d$deaths_heat_attrib[big_h])) else 0,
        rebuild_rel_cold = if (any(big_c)) max(abs(rc[big_c] - d$deaths_cold_attrib[big_c]) / abs(d$deaths_cold_attrib[big_c])) else 0,
        rebuild_abs = max(abs(rh - d$deaths_heat_attrib), abs(rc - d$deaths_cold_attrib)),
        nonopt_dev = max(abs(d$deaths_nonopt_attrib - d$deaths_heat_attrib - d$deaths_cold_attrib)),
        total_heat = sum(d$deaths_heat_attrib), total_cold = sum(d$deaths_cold_attrib))
      yrs <- c(yrs, y)
      if (y %in% temp_years) temps[[length(temps) + 1L]] <- data.table(model = m, scenario = s, year = y, temp_popw = popw_temp(loc, m, s, y))
      rm(d, r); invisible(gc(FALSE))
    }
    if (is.null(fac)) fac <- array(NaN, c(if (is.null(D)) 1L else D, 17L, 3L, length(years)))
    writeBin(as.vector(fac), con, size = 4L)
    present[[k]] <- yrs
  }
  close(con)
  if (is.null(D)) stop("no files read for location ", loc)
  con2 <- file(file.path(ldir, "ihme.f32.tmp"), "wb"); writeBin(as.vector(ihme), con2, size = 4L); close(con2)
  combos[, years_present := vapply(present, function(v) paste(v, collapse = ","), "")]
  combos[, n_years := lengths(present)]
  meta <- list(location_id = loc, draws = draws, sexes = SEXES, ages = AGES, causes = CAUSES, years = years,
               quantities = c("S", "paf_heat", "paf_cold"), combos = combos, ref_scenario = ref_scen,
               ihme_years_present = years[ihme_have], temp = rbindlist(temps),
               format = paste("float32 little-endian via writeBin(size = 4). ihme.f32: [draw, sex, age, cause, year].",
                              "factor.f32: one block per combo (meta$combos order), each [draw, cause, quantity, year].",
                              "Read with read_compact() in read_compact.R."),
               created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  file.rename(file.path(ldir, "ihme.f32.tmp"), file.path(ldir, "ihme.f32"))
  file.rename(ffac, file.path(ldir, "factor.f32"))
  ck <- rbindlist(checks); fwrite(ck, file.path(ldir, "checks.csv"))
  saveRDS(meta, file.path(ldir, "meta.rds"))   # written last: its presence marks the location done
  list(status = "ok", loc = loc, combos = nrow(combos), files = nrow(ck),
       worst = ck[, .(ihme_rel_dev = max(ihme_rel_dev), broadcast_dev = max(broadcast_dev),
                      rebuild_rel = max(rebuild_rel_heat, rebuild_rel_cold), rebuild_abs = max(rebuild_abs),
                      nonopt_dev = max(nonopt_dev))])
}

res <- parallel::mclapply(locs, function(l) tryCatch(export_location(l), error = function(e) list(status = paste("ERROR:", conditionMessage(e)), loc = l)),
                          mc.cores = as.integer(JOBS), mc.preschedule = FALSE)
man <- rbindlist(lapply(res, function(r) {
  if (!is.list(r)) return(data.table(location_id = NA_integer_, status = paste("ERROR:", as.character(r))))
  x <- data.table(location_id = r$loc, status = r$status, combos = r$combos %||% NA_integer_, files = r$files %||% NA_integer_)
  if (!is.null(r$worst)) x <- cbind(x, r$worst)
  x
}), fill = TRUE)
fwrite(man, file.path(OUT, "manifest.csv"))
invisible(file.copy(file.path(SCRIPTS_DIR, "util_compact_io.R"), file.path(OUT, "read_compact.R"), overwrite = TRUE))

# Thresholds: 4-byte floats carry about 7 significant digits, so a rebuild from
# them differs from the double-precision file by a relative 1e-6 at most
# (product of three rounded values); IHME deaths should agree to that level.
ok <- man[status == "ok"]
lines <- c(sprintf("Compact export report, %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  sprintf("locations: %d requested, %d written, %d skipped (already done), %d errors",
          length(locs), nrow(ok), sum(grepl("^skipped", man$status)), sum(grepl("^ERROR", man$status))),
  if (nrow(ok)) c(
    sprintf("files read: %d", sum(ok$files)),
    sprintf("IHME deaths identical across models and scenarios (max rel dev < 1e-6): %s (worst %.2g)",
            if (max(ok$ihme_rel_dev) < 1e-6) "PASS" else "FAIL", max(ok$ihme_rel_dev)),
    sprintf("S and PAFs constant across age and sex (max dev < 1e-12): %s (worst %.2g)",
            if (max(ok$broadcast_dev) < 1e-12) "PASS" else "FAIL", max(ok$broadcast_dev)),
    sprintf("rebuild from stored values, relative (< 1e-6): %s (worst %.2g)",
            if (max(ok$rebuild_rel) < 1e-6) "PASS" else "FAIL", max(ok$rebuild_rel)),
    sprintf("rebuild, largest absolute difference in any cell (reported, not tested; scales with cell size): %.2g deaths",
            max(ok$rebuild_abs)),
    sprintf("heat + cold = non-optimal in the files (< 1e-6): %s (worst %.2g)",
            if (max(ok$nonopt_dev) < 1e-6) "PASS" else "FAIL", max(ok$nonopt_dev))),
  if (any(grepl("^ERROR", man$status))) c("errors:", paste(" ", man[grepl("^ERROR", status), paste(location_id, status)])),
  sprintf("size on disk: %.2f GB", sum(file.size(list.files(OUT, recursive = TRUE, full.names = TRUE)), na.rm = TRUE) / 1e9))
writeLines(lines, file.path(OUT, "report.txt")); cat(lines, sep = "\n")
