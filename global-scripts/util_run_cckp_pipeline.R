# util_run_cckp_pipeline.R — Drive the CCKP adapter over a (location, model,
# scenario, year) grid, producing per-combo RDS files for downstream pipeline.
#
# Behavior:
# - Iterates (model x scenario x year) for a single location
# - Years <= 2014 use {model}-historical; years >= 2015 use {model}-{scenario}
# - Downloads temp + pop NetCDFs to CCKP_CACHE (skip if present)
# - Calls convert_cckp() to write
#     TEMP_DIR/cckp/{LOCATION_ID}/{model}-{scenario}/daily_temp_{year}.rds
# - Skips combos whose output RDS already exists (idempotent / resumable)
# - Logs each combo's outcome (ok/skip/fail) to a CSV manifest
#
# Usage:
#   Rscript util_run_cckp_pipeline.R \
#     --location_id=125 \
#     --models=access-cm2-r1i1p1f1,bcc-csm2-mr-r1i1p1f1 \
#     --scenarios=ssp245 \
#     --years=2010-2015 \
#     --shapefile=../data/shapefiles/colombia_divipola_admin1.gpkg \
#     --shapefile_subloc_field=divipola_code
#
# All arguments default below. Add --keep_netcdfs=FALSE to delete cached
# NetCDFs after each successful conversion (saves disk; recompute requires
# re-download).

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
source(file.path(SCRIPTS_DIR, "util_convert_cckp_temperature.R"))

suppressPackageStartupMessages(library(data.table))

# =============================================================================
# Defaults (overridable via --flag=val)
# =============================================================================
defaults <- list(
  MODELS    = "access-cm2-r1i1p1f1",
  SCENARIOS = "ssp245",
  YEARS     = "2010-2010",
  POP_DATASET = "gpw-v4-rev11",
  SHAPEFILE = file.path(SHAPEFILE_DIR, "GBD2023_mapping_final.shp"),
  SHAPEFILE_SUBLOC_FIELD = NULL,
  CCKP_CACHE = file.path(DATA_DIR, "cmip6-scratch", "cckp"),
  OUTPUT_ROOT = file.path(TEMP_DIR, "cckp"),
  MANIFEST    = file.path(OUTPUT_DIR, "cckp_run_manifest.csv"),
  KEEP_NETCDFS = TRUE
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) {
    assign(k, defaults[[k]], envir = globalenv())
  }
}

# =============================================================================
# URL builders
# =============================================================================
CCKP_BASE <- "https://wbg-cckp.s3.amazonaws.com/data"

cckp_temp_url <- function(model, model_scen_key, year) {
  ms <- paste0(model, "-", model_scen_key)
  fn <- sprintf("timeseries-tas-daily-mean_cmip6-daily-x0.25_%s_timeseries_mean_%d.nc",
                ms, year)
  file.path(CCKP_BASE, "cmip6-daily-x0.25", "tas", ms, fn)
}

# Year -> 20-year climatology bucket. CCKP buckets:
#   historical: 1995-2014   (yearbase 1995)
#   future:     2020-2039, 2040-2059, 2060-2079, 2080-2099
year_to_pop_bucket <- function(year, scenario) {
  if (year <= 2014) {
    list(scen_key = "historical", ystart = 1995, yend = 2014)
  } else if (year <= 2039) {
    list(scen_key = scenario, ystart = 2020, yend = 2039)
  } else if (year <= 2059) {
    list(scen_key = scenario, ystart = 2040, yend = 2059)
  } else if (year <= 2079) {
    list(scen_key = scenario, ystart = 2060, yend = 2079)
  } else {
    list(scen_key = scenario, ystart = 2080, yend = 2099)
  }
}

cckp_pop_url <- function(pop_dataset, year, scenario) {
  b <- year_to_pop_bucket(year, scenario)
  ds <- paste0(pop_dataset, "-", b$scen_key)
  fn <- sprintf("climatology-popcount-annual-mean_pop-x0.25_%s_climatology_mean_%d-%d.nc",
                ds, b$ystart, b$yend)
  file.path(CCKP_BASE, "pop-x0.25", "popcount", ds, fn)
}

# =============================================================================
# Cached download
# =============================================================================
ensure_download <- function(url, cache_dir, local_root = "") {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(cache_dir, basename(url))
  if (file.exists(dest) && file.info(dest)$size > 0) {
    log_msg("Cached (skip download): ", basename(url),
            " [", format(file.info(dest)$size / 1e6, digits = 4), " MB]")
    return(dest)
  }
  # Local CCKP mirror: if CCKP_LOCAL_ROOT is set and the file exists there at
  # the same bucket-relative path, symlink it into the cache (no GB-scale copy,
  # no download). We symlink rather than return the source path directly so the
  # KEEP_NETCDFS=FALSE cleanup removes only the link, never the mirror. Falls
  # through to the public download on a miss, so a partial mirror is fine.
  if (nzchar(local_root)) {
    rel <- sub("^/", "", sub(CCKP_BASE, "", url, fixed = TRUE))
    local_path <- file.path(local_root, rel)
    if (file.exists(local_path) && file.info(local_path)$size > 0) {
      log_msg("Local mirror hit: ", local_path,
              " [", format(file.info(local_path)$size / 1e6, digits = 4), " MB]")
      file.symlink(normalizePath(local_path), dest)
      return(dest)
    }
    log_msg("Local mirror miss (falling back to public download): ", local_path)
  }
  log_msg("Downloading ", basename(url))
  t0 <- Sys.time()
  # --fail makes curl exit 22 on HTTP 4xx/5xx (e.g., 404 for a missing
  # (model, scenario) combo on S3) without writing a body. We retry on
  # transient failures but not on 404 -- 404 means the file genuinely
  # doesn't exist; retrying won't change that.
  status <- system2("curl",
                    c("-sS", "--fail",
                      "--retry", "5", "--retry-delay", "10",
                      "--continue-at", "-", "--max-time", "7200",
                      "-o", shQuote(dest), shQuote(url)))
  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  if (status != 0 || !file.exists(dest) || file.info(dest)$size == 0) {
    # Soft failure: log a missing-on-s3 message and return NA. The caller
    # checks for NA and records a missing-on-s3 status to the manifest,
    # rather than aborting the whole grid loop on the first 404. Known
    # gaps (see MODELS_ALL comments in config.R): gfdl-cm4 lacks ssp126/
    # ssp370; hadgem3-gc31-mm lacks ssp245/ssp370; nesm3 lacks ssp370;
    # taiesm1 lacks ssp245.
    log_msg(sprintf("MISSING-ON-S3: %s (curl exit %d, %.1fs)",
                    basename(url), status, elapsed))
    if (file.exists(dest)) try(file.remove(dest), silent = TRUE)
    return(NA_character_)
  }
  size_mb <- file.info(dest)$size / 1e6
  throughput <- if (elapsed > 0) size_mb / elapsed else NA_real_
  log_msg(sprintf("Downloaded %s [%.1f MB] in %.1fs (%.1f MB/s)",
                  basename(url), size_mb, elapsed,
                  if (is.na(throughput)) 0 else throughput))
  dest
}

# =============================================================================
# Year-range parser: "2010-2015,2030" -> c(2010,2011,...,2015,2030)
# =============================================================================
parse_years <- function(s) {
  # config.R::parse_args auto-coerces flag values to numeric when parseable,
  # so a single-year flag like --years=2010 arrives as numeric.
  s <- as.character(s)
  parts <- strsplit(s, ",", fixed = TRUE)[[1]]
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

# =============================================================================
# Main driver
# =============================================================================
run_cckp_grid <- function() {
  models    <- strsplit(MODELS,    ",", fixed = TRUE)[[1]]
  scenarios <- strsplit(SCENARIOS, ",", fixed = TRUE)[[1]]
  years     <- parse_years(YEARS)

  grid <- CJ(model = models, scenario = scenarios, year = years, sorted = FALSE)
  log_msg("Grid: ", nrow(grid), " combos (",
          length(models), " models x ", length(scenarios),
          " scenarios x ", length(years), " years)")

  dir.create(OUTPUT_ROOT, showWarnings = FALSE, recursive = TRUE)
  results <- vector("list", nrow(grid))

  for (i in seq_len(nrow(grid))) {
    g <- grid[i]
    model_scen_key <- if (g$year <= 2014) "historical" else g$scenario
    out_dir <- file.path(OUTPUT_ROOT, LOCATION_ID,
                         paste0(g$model, "-", g$scenario))
    out_path <- file.path(out_dir, sprintf("daily_temp_%d.rds", g$year))

    log_msg(sprintf("[%d/%d] loc=%d %s/%s/%d",
                    i, nrow(grid), LOCATION_ID, g$model, g$scenario, g$year))

    if (file.exists(out_path)) {
      log_msg("  -> SKIP (output exists)")
      results[[i]] <- data.table(grid[i], status = "skip", out_path = out_path,
                                 rows = NA_integer_, message = "")
      next
    }

    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    res <- tryCatch({
      temp_url <- cckp_temp_url(g$model, model_scen_key, g$year)
      pop_url  <- cckp_pop_url(POP_DATASET, g$year, g$scenario)
      # Pop may sit under a different mirror root than temp; fall back to the
      # temp root when no pop-specific root is set.
      pop_root <- if (nzchar(CCKP_POP_LOCAL_ROOT)) CCKP_POP_LOCAL_ROOT else CCKP_LOCAL_ROOT
      temp_nc <- ensure_download(temp_url, CCKP_CACHE, CCKP_LOCAL_ROOT)
      pop_nc  <- ensure_download(pop_url,  CCKP_CACHE, pop_root)
      if (is.na(temp_nc) || is.na(pop_nc)) {
        # ensure_download returned NA on 404 / size-0 -- the combo isn't on
        # S3 (known gaps for some model x scenario pairs; see MODELS_ALL in
        # config.R). Record and move on rather than crashing the grid.
        log_msg("  -> MISSING-ON-S3 (skipping)")
        return(data.table(grid[i], status = "missing-on-s3", out_path = out_path,
                          rows = NA_integer_,
                          message = if (is.na(temp_nc)) basename(temp_url)
                                    else basename(pop_url)))
      }
      dt <- convert_cckp(temp_nc_path = temp_nc,
                         pop_nc_path  = pop_nc,
                         location_id  = LOCATION_ID,
                         shapefile_path = SHAPEFILE,
                         shapefile_subloc_field = SHAPEFILE_SUBLOC_FIELD,
                         output_path  = out_path,
                         subnational  = SUBNATIONAL)
      if (!KEEP_NETCDFS) {
        try(file.remove(temp_nc), silent = TRUE)
        # Pop NetCDFs are shared across years; only delete if not used elsewhere
        # in this grid (cheap heuristic: keep them all in the cache).
      }
      data.table(grid[i], status = "ok", out_path = out_path,
                 rows = nrow(dt), message = "")
    }, error = function(e) {
      log_msg("  -> FAIL: ", conditionMessage(e))
      data.table(grid[i], status = "fail", out_path = out_path,
                 rows = NA_integer_, message = conditionMessage(e))
    })
    results[[i]] <- res
  }

  manifest <- rbindlist(results, fill = TRUE)
  manifest[, run_ts := format(Sys.time(), "%Y-%m-%dT%H:%M:%S")]
  fwrite(manifest, MANIFEST, append = file.exists(MANIFEST))
  log_msg("Manifest: ", nrow(manifest), " combos | ",
          sum(manifest$status == "ok"),            " ok | ",
          sum(manifest$status == "skip"),          " skip | ",
          sum(manifest$status == "missing-on-s3"), " missing-on-s3 | ",
          sum(manifest$status == "fail"),          " fail -> ", MANIFEST)
  invisible(manifest)
}

# =============================================================================
# CLI entrypoint
# =============================================================================
if (sys.nframe() == 0L) {
  run_cckp_grid()
}
