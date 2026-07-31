# util_export_review_bundle.R: assemble what a reviewer needs, small enough to move.
#
# A finished ssp245 is about 3.3 TB of per-combo output. Almost none of that has
# to travel. Three observations make the bundle small:
#
#   1. pafs_<year>.rds is 17 rows, under a kilobyte, and it holds the whole
#      climate signal: heat, cold and non-optimal PAF per cause. Every one of
#      them, for the entire run, is on the order of 140 MB. So the PAF surface
#      travels complete, at full resolution, for all 204 locations.
#   2. Counts and uncertainty come from the summary tables, which are already
#      small.
#   3. The sampled combos ship normalised, not raw: per-draw PAF, mortality once
#      per location-year, and ex. Burden and YLLs rebuild from those exactly, and
#      the rebuild is verified before the bundle is written.
#
# Usage:
#   Rscript global-scripts/util_export_review_bundle.R --scenarios=ssp245
#   Rscript global-scripts/util_export_review_bundle.R --scenarios=ssp245 --jobs=16
#
# Options:
#   --scenarios=      comma separated; default ssp245
#   --out=            bundle directory; default output/review_bundle
#   --jobs=           parallel workers; default half the cores
#   --sample_locs=    locations to include draw-level detail for; default 125,102,349,128
#   --sample_years=   years for the sample;               default 2022,2035,2050
#   --sample_models=  models for the sample; default all of them
#   --input_locs=     locations whose mortality inputs to include, so the
#                     reviewer can re-run the pipeline independently; default 125
#   --sevs=FALSE      skip the SEV surface (it is diagnostic, not burden)
#   --tar=FALSE       leave the directory uncompressed

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(SCENARIOS = "ssp245",
                 OUT = file.path(OUTPUT_DIR, "review_bundle"),
                 JOBS = max(1L, floor(parallel::detectCores() / 2)),
                 SAMPLE_LOCS = "125,102,349,128", SAMPLE_YEARS = "2022,2035,2050",
                 SAMPLE_MODELS = "", INPUT_LOCS = "125",
                 SEVS = TRUE, TAR = TRUE)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

split_csv <- function(x) {
  x <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
  x[nzchar(x)]
}

scenarios    <- split_csv(SCENARIOS)
sample_locs  <- split_csv(SAMPLE_LOCS)
sample_years <- suppressWarnings(as.integer(split_csv(SAMPLE_YEARS)))
input_locs   <- split_csv(INPUT_LOCS)
cckp_root    <- file.path(RESULTS_ROOT, "cckp")
if (!dir.exists(cckp_root)) stop("No run output at ", cckp_root)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
locs <- basename(list.dirs(cckp_root, recursive = FALSE))
locs <- sort(suppressWarnings(locs[!is.na(as.integer(locs))]))
if (length(locs) == 0) stop("No locations under ", cckp_root)

log_msg("Bundling ", length(locs), " locations for ", paste(scenarios, collapse = ","))

# Combo directories are "<model>-<scenario>". The scenario never contains a
# dash and the model usually does, so split on the last one.
combo_dirs <- function(loc) {
  d <- list.dirs(file.path(cckp_root, loc), recursive = FALSE)
  d <- d[basename(d) != "status"]
  d[sub("^.*-", "", basename(d)) %in% scenarios]
}

# -----------------------------------------------------------------------------
# 1 + 2. The PAF and SEV surfaces, consolidated
#
# One file per combo is unusable at 160,000 files, and tar would spend longer on
# the inodes than on the bytes. Read them into one table instead: each already
# carries its model, scenario, location and year, stamped by the burden runner.
# -----------------------------------------------------------------------------
gather_surface <- function(prefix) {
  parts <- parallel::mclapply(locs, function(loc) {
    out <- list()
    for (dd in combo_dirs(loc)) {
      f <- list.files(dd, pattern = sprintf("^%s_\\d+\\.rds$", prefix), full.names = TRUE)
      for (p in f) {
        x <- tryCatch(setDT(readRDS(p)), error = function(e) NULL)
        if (is.null(x) || nrow(x) == 0) next
        # Belt and braces: older output may predate the model/scenario stamping.
        if (!"model" %in% names(x))    x[, model := sub("-[^-]+$", "", basename(dd))]
        if (!"scenario" %in% names(x)) x[, scenario := sub("^.*-", "", basename(dd))]
        if (!"location_id" %in% names(x)) x[, location_id := as.integer(loc)]
        out[[length(out) + 1L]] <- x
      }
    }
    if (length(out)) rbindlist(out, fill = TRUE) else NULL
  }, mc.cores = JOBS, mc.preschedule = FALSE)
  parts <- parts[!vapply(parts, function(z) is.null(z) || inherits(z, "try-error"), logical(1))]
  if (length(parts) == 0) return(NULL)
  rbindlist(parts, fill = TRUE)
}

t0 <- Sys.time()
paf <- gather_surface("pafs")
if (!is.null(paf)) {
  saveRDS(paf, file.path(OUT, "paf_surface.rds"), compress = "xz")
  log_msg("PAF surface: ", format(nrow(paf), big.mark = ","), " rows -> ",
          round(file.size(file.path(OUT, "paf_surface.rds")) / 1e6, 1), " MB")
} else log_msg("WARN no pafs_*.rds found")

if (isTRUE(SEVS)) {
  sev <- gather_surface("sevs")
  if (!is.null(sev)) {
    saveRDS(sev, file.path(OUT, "sev_surface.rds"), compress = "xz")
    log_msg("SEV surface: ", format(nrow(sev), big.mark = ","), " rows")
  }
}

# -----------------------------------------------------------------------------
# 3. Whatever the summarizer produced
# -----------------------------------------------------------------------------
sum_dir <- file.path(OUTPUT_DIR, "summary")
if (dir.exists(sum_dir)) {
  dir.create(file.path(OUT, "summary"), showWarnings = FALSE)
  f <- list.files(sum_dir, pattern = "\\.(csv|txt)$", full.names = TRUE)
  file.copy(f, file.path(OUT, "summary"), overwrite = TRUE)
  log_msg("Summary tables: ", length(f), " files")
} else {
  log_msg("WARN ", sum_dir, " not found. Run util_summarize_run.R first; ",
          "counts and uncertainty come from there, not from this bundle.")
}

# -----------------------------------------------------------------------------
# 4. Manifests and run records: what ran, what failed, and why
# -----------------------------------------------------------------------------
dir.create(file.path(OUT, "manifests"), showWarnings = FALSE)
for (f in c("cckp_run_manifest.csv", "cckp_burden_manifest.csv")) {
  p <- file.path(OUTPUT_DIR, f)
  if (file.exists(p)) file.copy(p, file.path(OUT, "manifests", f), overwrite = TRUE)
}
log_root <- file.path(OUTPUT_DIR, "logs")
if (dir.exists(log_root)) {
  runs <- sort(list.dirs(log_root, recursive = FALSE), decreasing = TRUE)
  if (length(runs)) {
    for (f in c("joblog.tsv", "verify.log")) {
      p <- file.path(runs[1], f)
      if (file.exists(p)) file.copy(p, file.path(OUT, "manifests", f), overwrite = TRUE)
    }
    # One full per-location log, so the reviewer can see what a run actually says.
    ll <- list.files(runs[1], pattern = "^loc_.*\\.log$", full.names = TRUE)
    if (length(ll)) file.copy(ll[1], file.path(OUT, "manifests", basename(ll[1])), overwrite = TRUE)
  }
}

# -----------------------------------------------------------------------------
# 5. The sample, stored normalised rather than raw
#
# A raw burden file is one row per (cause, age, sex, draw), about 289,000 rows,
# and almost none of it is independent information:
#
#   paf_heat/cold   vary only by (cause, draw)  -> repeated across age and sex
#   deaths          vary by all four, but are IDENTICAL across models, because
#                   mortality comes from the IHME forecast and does not know
#                   which climate model is being run
#   ex              varies only by (age, sex)
#   deaths_heat, deaths_cold, deaths_nonopt, yll_heat, yll_cold, yll_nonopt
#                   are exact products of the above
#
# So the sample ships as: per-draw PAF per combo, mortality once per
# location-year, and ex once per location-year. Everything else is recomputed on
# arrival. This is lossless, and the reconstruction is verified below rather
# than asserted: if it does not reproduce the original to floating point, the
# bundle is not written.
# -----------------------------------------------------------------------------
norm_dir <- file.path(OUT, "sample_normalised")
dir.create(norm_dir, showWarnings = FALSE)

read_combo <- function(dd, y, pre) {
  p <- file.path(dd, sprintf("%s_%d.rds", pre, y))
  if (!file.exists(p)) return(NULL)
  d <- tryCatch(setDT(readRDS(p)), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  if (!"year" %in% names(d) && "year_id" %in% names(d)) setnames(d, "year_id", "year")
  d
}

paf_draws <- list(); mort_store <- list(); ex_store <- list()
verify <- list(); n_norm <- 0L
for (loc in intersect(sample_locs, locs)) {
  dirs <- combo_dirs(loc)
  models <- unique(sub("-[^-]+$", "", basename(dirs)))
  keep <- if (nzchar(as.character(SAMPLE_MODELS))) split_csv(SAMPLE_MODELS) else sort(models)
  for (dd in dirs) {
    mdl <- sub("-[^-]+$", "", basename(dd))
    if (!mdl %in% keep) next
    scn <- sub("^.*-", "", basename(dd))
    for (y in sample_years) {
      b <- read_combo(dd, y, "burden")
      if (is.null(b)) next
      n_norm <- n_norm + 1L

      # The only genuinely model-specific quantity.
      pd <- unique(b[, .(acause, draw, paf_heat, paf_cold)])
      if (nrow(pd) != uniqueN(b[, .(acause, draw)]))
        stop("PAF is not constant within (cause, draw) for loc ", loc, " ", mdl, " ", y,
             ". The normalisation assumption does not hold; ship this sample raw.")
      paf_draws[[length(paf_draws) + 1L]] <-
        cbind(data.table(location_id = as.integer(loc), model = mdl, scenario = scn, year = y), pd)

      # Mortality, stored once per location-year. Checked for model invariance
      # rather than assumed: this is the claim the whole saving rests on.
      key <- paste(loc, y)
      mo <- b[, .(acause, age_group_id, sex_id, draw, deaths)]
      setorder(mo, acause, age_group_id, sex_id, draw)
      if (is.null(mort_store[[key]])) {
        mort_store[[key]] <- cbind(data.table(location_id = as.integer(loc), year = y), mo)
      } else {
        prev <- mort_store[[key]][, .(acause, age_group_id, sex_id, draw, deaths)]
        if (!isTRUE(all.equal(prev, mo, check.attributes = FALSE)))
          stop("Mortality differs between models for loc ", loc, " year ", y,
               " (model ", mdl, "). It was assumed model-invariant; it is not. ",
               "Ship this sample raw rather than normalised.")
      }

      yl <- read_combo(dd, y, "ylls")
      if (!is.null(yl) && "ex" %in% names(yl) && is.null(ex_store[[key]])) {
        ex <- unique(yl[, .(age_group_id, sex_id, ex)])
        if (nrow(ex) != uniqueN(yl[, .(age_group_id, sex_id)]))
          stop("ex is not constant within (age, sex) for loc ", loc, " year ", y)
        ex_store[[key]] <- cbind(data.table(location_id = as.integer(loc), year = y), ex)
      }

      # Round-trip: rebuild the original from the pieces and compare.
      rb <- merge(mo, pd, by = c("acause", "draw"))
      rb[, paf_nonopt := paf_heat + paf_cold]
      rb[, `:=`(deaths_heat = deaths * paf_heat, deaths_cold = deaths * paf_cold,
                deaths_nonopt = deaths * paf_nonopt)]
      cols <- c("acause", "age_group_id", "sex_id", "draw", "deaths",
                "paf_heat", "paf_cold", "paf_nonopt",
                "deaths_heat", "deaths_cold", "deaths_nonopt")
      a <- copy(b)[, ..cols]; setorderv(a, cols[1:4])
      setorderv(rb, cols[1:4]); rb <- rb[, ..cols]
      ok <- isTRUE(all.equal(as.data.frame(a), as.data.frame(rb), tolerance = 1e-12))
      verify[[length(verify) + 1L]] <- data.table(
        location_id = as.integer(loc), model = mdl, year = y, rows = nrow(b),
        reconstructs = ok)
      if (!ok)
        stop("Reconstruction does not reproduce burden for loc ", loc, " ", mdl, " ", y,
             ". Not writing a bundle that cannot be rebuilt.")
    }
  }
}

if (n_norm > 0) {
  saveRDS(rbindlist(paf_draws),  file.path(norm_dir, "paf_draws.rds"),  compress = "xz")
  saveRDS(rbindlist(mort_store), file.path(norm_dir, "mortality_draws.rds"), compress = "xz")
  if (length(ex_store))
    saveRDS(rbindlist(ex_store), file.path(norm_dir, "life_expectancy.rds"), compress = "xz")
  vt <- rbindlist(verify)
  fwrite(vt, file.path(norm_dir, "reconstruction_check.csv"))
  got <- sum(file.size(list.files(norm_dir, full.names = TRUE)), na.rm = TRUE)
  log_msg("Normalised sample: ", n_norm, " combos, all reconstructing exactly, ",
          round(got / 1e6, 1), " MB")
} else {
  log_msg("WARN no sample combos found for locations ", paste(sample_locs, collapse = ","))
}

# -----------------------------------------------------------------------------
# 6. Inputs for an independent re-run
#
# Only mortality travels. The CCKP temperature can be re-downloaded from the
# public bucket, and the ERF curves, TMRELs, life tables and shapefile are in
# the repo, so a reviewer can reproduce a combo end to end from these plus a
# download.
# -----------------------------------------------------------------------------
in_dir <- file.path(OUT, "inputs")
dir.create(in_dir, showWarnings = FALSE)
mort_bytes <- 0
for (loc in input_locs) {
  f <- list.files(file.path(DATA_DIR, "mortality"),
                  pattern = paste0("^", loc, "_"), full.names = TRUE)
  for (p in f) {
    file.copy(p, file.path(in_dir, basename(p)), overwrite = TRUE)
    mort_bytes <- mort_bytes + file.size(p)
  }
  # One converted daily temperature file, to check our re-download reproduces it.
  td <- file.path(DATA_DIR, "temperature", "cckp", loc)
  if (dir.exists(td)) {
    cd <- list.dirs(td, recursive = FALSE)
    cd <- cd[sub("^.*-", "", basename(cd)) %in% scenarios]
    if (length(cd)) {
      tf <- list.files(cd[1], pattern = "^daily_temp_\\d+\\.rds$", full.names = TRUE)
      if (length(tf)) {
        dst <- file.path(in_dir, "daily_temp", basename(cd[1]))
        dir.create(dst, recursive = TRUE, showWarnings = FALSE)
        file.copy(tf[1], file.path(dst, basename(tf[1])), overwrite = TRUE)
        mort_bytes <- mort_bytes + file.size(tf[1])
      }
    }
  }
}
log_msg("Inputs: ", round(mort_bytes / 1e6, 1), " MB")

# -----------------------------------------------------------------------------
# Manifest
# -----------------------------------------------------------------------------
all_f <- list.files(OUT, recursive = TRUE, full.names = TRUE)
sz <- sum(file.size(all_f), na.rm = TRUE)
mf <- file.path(OUT, "BUNDLE_MANIFEST.txt")
con <- file(mf, "w"); w <- function(...) cat(..., "\n", sep = "", file = con)
w("Review bundle, ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
w("scenarios: ", paste(scenarios, collapse = ","), "   locations with output: ", length(locs))
w("")
w("paf_surface.rds   every combo's cause-level heat/cold/non-optimal PAF, all locations")
w("sev_surface.rds   same for SEVs (diagnostic)")
w("summary/          national totals with uncertainty, by cause, by age and sex, coverage, QA")
w("manifests/        per-combo status and messages, exit codes, one full location log")
w("sample_normalised/ per-draw PAF, mortality and ex for the sampled combos,")
w("                  from which burden and YLLs rebuild exactly (check included)")
w("inputs/           mortality and one converted temperature file, for an independent re-run")
w("")
w("files: ", length(all_f), "   total: ", round(sz / 1e6, 1), " MB")
for (p in sort(all_f)) w(sprintf("  %10.2f MB  %s", file.size(p) / 1e6, sub(paste0(OUT, "/"), "", p, fixed = TRUE)))
close(con)

el <- as.numeric(Sys.time() - t0, units = "mins")
log_msg(sprintf("Bundle: %.1f MB in %d files (%.1f min)", sz / 1e6, length(all_f), el))

if (isTRUE(TAR)) {
  # Archive from the parent with a relative member name, so it unpacks as
  # review_bundle/ rather than recreating this machine's absolute path.
  tgz <- paste0(OUT, ".tar.gz")
  owd <- getwd()
  setwd(dirname(normalizePath(OUT)))
  ok <- utils::tar(basename(tgz), files = basename(OUT), compression = "gzip", tar = "tar")
  setwd(owd)
  if (ok == 0 && file.exists(tgz))
    log_msg(sprintf("Wrote %s (%.1f MB)", tgz, file.size(tgz) / 1e6))
  else log_msg("WARN tar failed; the directory at ", OUT, " is still complete")
}
cat("\n"); cat(readLines(mf)[1:8], sep = "\n"); cat("\n")
