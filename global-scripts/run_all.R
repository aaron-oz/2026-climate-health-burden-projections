# run_all.R — Run the pipeline for all locations in parallel
#
# Usage:
#   Rscript run_all.R
#   Rscript run_all.R --n_cores=4
#   Rscript run_all.R --use_draws=TRUE --n_cores=8
#
# Reads location IDs from the TMREL directory (one file per location)
# and runs run_location.R for each in parallel.

library(parallel)

# Parse command-line args
args <- commandArgs(trailingOnly = TRUE)
n_cores <- 4  # default, conservative per global CLAUDE.md guidance
use_draws <- FALSE

for (arg in args) {
  if (grepl("^--n_cores=", arg)) n_cores <- as.integer(sub("--n_cores=", "", arg))
  if (grepl("^--use_draws=", arg)) use_draws <- as.logical(sub("--use_draws=", "", arg))
}

# Discover available location IDs from TMREL files
project_root <- here::here()
tmrel_dir <- file.path(project_root, "data", "tmrel")
tmrel_files <- list.files(tmrel_dir, pattern = "^tmrel_\\d+_summaries\\.csv$")
location_ids <- as.integer(gsub("tmrel_(\\d+)_summaries\\.csv", "\\1", tmrel_files))
location_ids <- sort(location_ids)

cat(paste0("=== Global Pipeline Run ===\n"))
cat(paste0("Locations found: ", length(location_ids), "\n"))
cat(paste0("Cores: ", n_cores, "\n"))
cat(paste0("Use draws: ", use_draws, "\n\n"))

# Build commands
run_script <- file.path(project_root, "global-scripts", "run_location.R")
commands <- lapply(location_ids, function(loc) {
  paste("Rscript", run_script,
        paste0("--location_id=", loc),
        paste0("--use_draws=", use_draws))
})

# Run in parallel
t_start <- Sys.time()

results <- mclapply(commands, function(cmd) {
  cat(paste0("Starting: ", cmd, "\n"))
  exit_code <- system(cmd)
  list(command = cmd, exit_code = exit_code)
}, mc.cores = n_cores)

t_end <- Sys.time()

# Report
n_success <- sum(sapply(results, function(r) r$exit_code == 0))
n_fail <- length(results) - n_success

cat(paste0("\n=== Complete ===\n"))
cat(paste0("Succeeded: ", n_success, "/", length(results), "\n"))
cat(paste0("Failed: ", n_fail, "\n"))
cat(paste0("Total time: ", round(difftime(t_end, t_start, units = "mins"), 1), " min\n"))

if (n_fail > 0) {
  cat("\nFailed locations:\n")
  for (r in results) {
    if (r$exit_code != 0) cat(paste0("  ", r$command, "\n"))
  }
}
