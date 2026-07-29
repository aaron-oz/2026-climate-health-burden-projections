# util_test_convert_mask.R — Prove the mask-first convert_cckp() returns exactly
# what the bbox-wide version returned, and measure what it saves.
#
#   Rscript global-scripts/util_test_convert_mask.R
#   Rscript global-scripts/util_test_convert_mask.R --locs=62,125
#
# Runs both implementations over the same real NetCDF and compares the output
# tables with all.equal (values, ordering, column types and the pixel_id key),
# reporting peak allocation and wall time for each.
#
# The reference implementation is read out of git (the version at the merge base
# with main) rather than kept as a copy in this file, so it cannot silently drift
# away from what is actually being replaced.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
SCRIPTS_DIR <- normalizePath(SCRIPTS_DIR)
source(file.path(SCRIPTS_DIR, "config.R"))
source(file.path(SCRIPTS_DIR, "util_convert_cckp_temperature.R"))
suppressPackageStartupMessages({library(data.table); library(sf)})

defaults <- list(
  LOCS    = "22,62,102,125,135,14",
  TEMP_NC = file.path(DATA_DIR, "cmip6-scratch", "tas_daily_access-cm2_hist_2010.nc"),
  POP_NC  = file.path(DATA_DIR, "cmip6-scratch", "popcount_ssp245_2020-2039.nc"),
  REF     = "main"
)
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}

# Fall back to any daily temperature + population pair in the local CCKP mirror,
# so this runs on a production machine that has the mirror but not the ad-hoc
# scratch files this was first written against.
if (!file.exists(TEMP_NC) && nzchar(CCKP_LOCAL_ROOT)) {
  cand <- Sys.glob(file.path(CCKP_LOCAL_ROOT, "cmip6-daily-x0.25", "tas", "*", "*.nc"))
  if (length(cand)) TEMP_NC <- cand[1]
}
if (!file.exists(POP_NC)) {
  pop_root <- if (nzchar(CCKP_POP_LOCAL_ROOT)) CCKP_POP_LOCAL_ROOT else CCKP_LOCAL_ROOT
  cand <- Sys.glob(file.path(pop_root, "pop-x0.25", "popcount", "*", "*.nc"))
  if (length(cand)) POP_NC <- cand[1]
}
for (f in c(TEMP_NC, POP_NC)) {
  if (!file.exists(f)) stop("Missing test input: ", f,
                            "\nPass --temp_nc= and --pop_nc= to point elsewhere.")
}
log_msg("Test inputs: ", basename(TEMP_NC), " + ", basename(POP_NC))

# --- Load the reference implementation under a different name ----------------
ref_src <- system2("git", c("-C", shQuote(PROJECT_ROOT), "show",
                            paste0(REF, ":global-scripts/util_convert_cckp_temperature.R")),
                   stdout = TRUE, stderr = FALSE)
if (length(ref_src) == 0) stop("Could not read the reference implementation from git ref '", REF, "'")
ref_env <- new.env(parent = globalenv())
eval(parse(text = ref_src), envir = ref_env)
convert_ref <- get("convert_cckp", envir = ref_env)

timed <- function(fn, ...) {
  gc(reset = TRUE, full = TRUE)
  t0 <- Sys.time()
  out <- fn(...)
  secs <- as.numeric(Sys.time() - t0, units = "secs")
  # gc() returns Ncells/Vcells rows; column 6 is "max used" in MB. Sum both
  # rows for a single peak-allocation figure.
  g <- gc(full = TRUE)
  list(dt = out, secs = secs, peak_mb = sum(g[, 6]))
}

locs <- as.integer(strsplit(as.character(LOCS), ",", fixed = TRUE)[[1]])
rows <- list()

for (loc in locs) {
  cat("\n=== location ", loc, " ===\n", sep = "")
  out_ref <- tempfile(fileext = ".rds"); out_new <- tempfile(fileext = ".rds")
  args <- list(temp_nc_path = TEMP_NC, pop_nc_path = POP_NC, location_id = loc,
               shapefile_path = DEFAULT_SHAPEFILE, subnational = FALSE)

  r <- tryCatch(do.call(timed, c(list(convert_ref), args, list(output_path = out_ref))),
                error = function(e) list(err = conditionMessage(e)))
  n <- tryCatch(do.call(timed, c(list(convert_cckp), args, list(output_path = out_new))),
                error = function(e) list(err = conditionMessage(e)))

  if (!is.null(r$err) || !is.null(n$err)) {
    cat("  ERROR ref:", r$err, " new:", n$err, "\n")
    rows[[length(rows) + 1]] <- data.table(
      loc = loc, identical = NA, rows = NA_integer_,
      ref_s = NA_real_, new_s = NA_real_, ref_peak_mb = NA_real_,
      new_peak_mb = NA_real_, note = paste0(r$err, n$err))
    next
  }

  cmp <- all.equal(r$dt, n$dt)
  same <- isTRUE(cmp)
  cat(sprintf("  identical: %s | rows %s | time %.1fs -> %.1fs | peak %.0f MB -> %.0f MB\n",
              same, format(nrow(n$dt), big.mark = ","), r$secs, n$secs,
              r$peak_mb, n$peak_mb))
  if (!same) cat("  DIFF: ", paste(utils::head(cmp, 5), collapse = " | "), "\n")

  rows[[length(rows) + 1]] <- data.table(
    loc = loc, identical = same, rows = nrow(n$dt),
    ref_s = round(r$secs, 1), new_s = round(n$secs, 1),
    ref_peak_mb = round(r$peak_mb), new_peak_mb = round(n$peak_mb),
    note = if (same) "" else paste(utils::head(cmp, 3), collapse = " | "))
  unlink(c(out_ref, out_new))
}

res <- rbindlist(rows)
cat("\n\n=== summary ===\n")
print(res[, .(loc, identical, rows, ref_s, new_s, ref_peak_mb, new_peak_mb)])
res[, speedup := round(ref_s / new_s, 2)]
res[, mem_ratio := round(ref_peak_mb / new_peak_mb, 2)]
cat("\npeak-memory reduction (ref/new):\n")
print(res[, .(loc, mem_ratio, speedup)])

bad <- res[identical %in% c(FALSE, NA)]
if (nrow(bad) > 0) {
  cat("\nFAILED for ", nrow(bad), " location(s): ",
      paste(bad$loc, collapse = ", "), "\n", sep = "")
  quit(status = 1)
}
cat("\nAll ", nrow(res), " locations produce identical output.\n", sep = "")
quit(status = 0)
