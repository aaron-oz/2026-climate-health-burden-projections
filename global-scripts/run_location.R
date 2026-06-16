# run_location.R — Run the full pipeline for a single location
#
# Usage:
#   Rscript run_location.R --location_id=125
#   Rscript run_location.R --location_id=125 --use_draws=TRUE
#   Rscript run_location.R --location_id=125 --run_diagnostics=FALSE

# Resolve this script's own directory so the step scripts (and config) load
# regardless of the working directory. Shared with the steps via SCRIPTS_DIR.
if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])

# Config is sourced by each script, but we parse args here first
# to display what we're about to do
args <- commandArgs(trailingOnly = TRUE)
cat("=== Climate Health Burden Pipeline ===\n")
cat("Arguments:", paste(args, collapse = " "), "\n\n")

# When COLOMBIA_VERIFICATION = TRUE, the burden calculation in 05 needs SEVs,
# so 06 must run before 05. In normal mode, order doesn't matter (SEVs are
# diagnostic only), but we keep this order for consistency.
scripts <- c(
  "01_load_erf.R",
  "02_load_tmrel.R",
  "03_load_temperature.R",
  "04_load_mortality.R",
  "06_compute_sevs.R",
  "05_compute_pafs.R",
  "07_compute_ylls.R",
  "08_outputs.R"
)

t_start <- Sys.time()

for (script in scripts) {
  cat(paste0("\n--- Running ", script, " ---\n"))
  t0 <- Sys.time()
  tryCatch(
    source(file.path(SCRIPTS_DIR, script), local = new.env()),
    error = function(e) {
      cat(paste0("ERROR in ", script, ": ", conditionMessage(e), "\n"))
      stop(e)
    }
  )
  t1 <- Sys.time()
  cat(paste0("--- ", script, " completed in ",
             round(difftime(t1, t0, units = "secs"), 1), "s ---\n"))
}

t_end <- Sys.time()
cat(paste0("\n=== Pipeline complete for location ",
           Sys.getenv("LOCATION_ID", unset = "unknown"),
           " in ", round(difftime(t_end, t_start, units = "mins"), 1), " min ===\n"))
