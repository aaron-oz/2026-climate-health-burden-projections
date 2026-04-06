# run_location.R — Run the full pipeline for a single location
#
# Usage:
#   Rscript run_location.R --location_id=125
#   Rscript run_location.R --location_id=125 --use_draws=TRUE
#   Rscript run_location.R --location_id=125 --run_diagnostics=FALSE

# Config is sourced by each script, but we parse args here first
# to display what we're about to do
args <- commandArgs(trailingOnly = TRUE)
cat("=== Climate Health Burden Pipeline ===\n")
cat("Arguments:", paste(args, collapse = " "), "\n\n")

scripts <- c(
  "01_load_erf.R",
  "02_load_tmrel.R",
  "03_load_temperature.R",
  "04_load_mortality.R",
  "05_compute_pafs.R",
  "06_compute_sevs.R",
  "07_compute_ylls.R",
  "08_outputs.R"
)

t_start <- Sys.time()

for (script in scripts) {
  cat(paste0("\n--- Running ", script, " ---\n"))
  t0 <- Sys.time()
  tryCatch(
    source(script, local = new.env()),
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
