# util_benchmark_workflow_b_summary.R: measure, on this machine, how long the
# Workflow B summary (util_summarize_workflow_b.R) will take for all
# locations, how much memory each worker needs, and how much disk the output
# takes, before committing the machine to the full run.
#
# What it does:
#   1. Times the summarizer on a few sample locations, one at a time, with
#      exactly the options the full run will use (every option this script
#      does not know is passed through unchanged). Records wall time, peak
#      memory (if GNU time is installed) and output size per location.
#   2. Runs the same sample locations all at once, to see how much they slow
#      each other down when sharing the disk (contention).
#   3. Measures the input size of every location (its Workflow B and burden
#      files), converts it to a predicted time using the slowest seconds-per-GB
#      seen in step 1, scales by the contention slowdown from step 2, and
#      simulates running all locations on --jobs_full workers.
#
# Usage (from the repo root, same options as the real run plus the three below):
#   Rscript global-scripts/util_benchmark_workflow_b_summary.R \
#     --scenarios=ssp245,ssp126,ssp370,ssp585 --ref_scenario=ssp245 --draw_periods=annual \
#     --sample_locations=163,6,125,33,367 --jobs_full=16,32 [--bench_out=output/benchmark_summary]
#
#   --sample_locations=  locations to time. Include the largest (India 163,
#                        China 6) and a few small ones. Default 163,6,125,33,367.
#   --jobs_full=         worker counts to estimate for, comma separated. Default 16.
#   --bench_out=         scratch directory; deleted and recreated. Default
#                        output/benchmark_summary.
#
# The estimate is only as good as the sample: step 2 measures contention among
# the sample locations only, so contention at a much larger worker count may
# be worse. It prints its inputs so the arithmetic can be checked.

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))
suppressPackageStartupMessages(library(data.table))

defaults <- list(SAMPLE_LOCATIONS = "163,6,125,33,367", JOBS_FULL = "16",
                 BENCH_OUT = file.path(OUTPUT_DIR, "benchmark_summary"),
                 SCENARIOS = "ssp245,ssp585", REF_SCENARIO = "ssp245")
for (k in names(defaults)) {
  if (!exists(k, envir = globalenv())) assign(k, defaults[[k]], envir = globalenv())
}
split_csv <- function(x) { x <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]]); x[nzchar(x)] }
samples <- split_csv(SAMPLE_LOCATIONS)
jobs_full <- as.integer(split_csv(JOBS_FULL))
own <- c("sample_locations", "jobs_full", "bench_out", "out", "locations", "jobs")
pass <- Filter(function(x) !sub("^--([^=]+).*", "\\1", x) %in% own, commandArgs(trailingOnly = TRUE))
summarizer <- file.path(SCRIPTS_DIR, "util_summarize_workflow_b.R")
rscript <- file.path(R.home("bin"), "Rscript")
gtime <- Sys.which("time"); if (!nzchar(gtime) && file.exists("/usr/bin/time")) gtime <- "/usr/bin/time"
has_gtime <- nzchar(gtime) && system2(gtime, c("-v", "true"), stdout = FALSE, stderr = FALSE) == 0
unlink(BENCH_OUT, recursive = TRUE); dir.create(BENCH_OUT, recursive = TRUE)
wb_root <- file.path(RESULTS_ROOT, "workflow_b"); cckp_root <- file.path(RESULTS_ROOT, "cckp")
scen <- split_csv(SCENARIOS)

# Input bytes for one location: its Workflow B files and the burden files the
# summarizer reads (target scenarios and the reference).
input_gb <- function(loc) {
  w <- list.files(file.path(wb_root, loc), pattern = "^wb_\\d+\\.rds$", recursive = TRUE, full.names = TRUE)
  d <- list.dirs(file.path(cckp_root, loc), recursive = FALSE)
  d <- d[grepl(paste0("-(", paste(unique(c(scen, REF_SCENARIO)), collapse = "|"), ")$"), d)]
  b <- unlist(lapply(d, list.files, pattern = "^burden_\\d+\\.rds$", full.names = TRUE))
  sum(file.info(c(w, b))$size, na.rm = TRUE) / 1e9
}
dir_gb <- function(p) sum(file.info(list.files(p, recursive = TRUE, full.names = TRUE))$size, na.rm = TRUE) / 1e9

run_one <- function(loc, tag) {
  out <- file.path(BENCH_OUT, paste0(tag, "_", loc)); log <- paste0(out, ".log"); tf <- paste0(out, ".time")
  args <- c(summarizer, pass, paste0("--locations=", loc), "--jobs=1", paste0("--out=", out))
  if (has_gtime) list(cmd = gtime, args = c("-v", "-o", tf, rscript, args), log = log, tf = tf, out = out)
  else list(cmd = rscript, args = args, log = log, tf = NA, out = out)
}
peak_mb <- function(tf) {
  if (is.na(tf) || !file.exists(tf)) return(NA_real_)
  l <- grep("Maximum resident set size", readLines(tf), value = TRUE)
  if (!length(l)) NA_real_ else as.numeric(sub(".*: *", "", l)) / 1024
}

# --- step 1: one location at a time -----------------------------------------
log_msg("Step 1: timing ", length(samples), " sample locations one at a time")
s1 <- rbindlist(lapply(samples, function(loc) {
  r <- run_one(loc, "serial")
  t0 <- Sys.time(); st <- system2(r$cmd, r$args, stdout = r$log, stderr = r$log)
  sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  x <- data.table(location_id = loc, input_gb = input_gb(loc), seconds = sec, exit = st,
                  peak_mb = peak_mb(r$tf), output_gb = dir_gb(r$out))
  log_msg(sprintf("  loc %s: %.3f GB in, %.0f s, peak %s MB, %.3f GB out, exit %d", loc, x$input_gb, sec,
                  ifelse(is.na(x$peak_mb), "?", round(x$peak_mb)), x$output_gb, st))
  x
}))
if (any(s1$exit != 0)) stop("A sample run failed; see the logs in ", BENCH_OUT)

# --- step 2: all samples at once ---------------------------------------------
log_msg("Step 2: running the ", length(samples), " sample locations at the same time")
t0 <- Sys.time()
procs <- lapply(samples, function(loc) {
  r <- run_one(loc, "parallel")
  system2(r$cmd, r$args, stdout = r$log, stderr = r$log, wait = FALSE); r
})
# wait for every parallel run's output log to report completion
state <- function(r) {
  if (!file.exists(r$log)) return("running")
  l <- readLines(r$log, warn = FALSE)
  if (any(grepl("Execution halted", l))) "failed" else if (any(grepl("Wrote ", l))) "done" else "running"
}
repeat {
  st <- vapply(procs, state, "")
  if (any(st == "failed")) stop("A parallel sample run failed; see the logs in ", BENCH_OUT)
  if (all(st == "done")) break
  Sys.sleep(5)
}
par_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
slowdown <- max(1, par_sec / max(s1$seconds))
log_msg(sprintf("  all at once: %.0f s, against %.0f s for the slowest alone: contention factor %.2f",
                par_sec, max(s1$seconds), slowdown))

# --- step 3: predict every location ------------------------------------------
all_locs <- sort(suppressWarnings(as.integer(basename(list.dirs(wb_root, recursive = FALSE)))))
all_locs <- all_locs[!is.na(all_locs)]
log_msg("Step 3: measuring input size of ", length(all_locs), " locations")
inp <- data.table(location_id = all_locs, input_gb = vapply(all_locs, input_gb, numeric(1)))
rate <- max(s1$seconds / pmax(s1$input_gb, 1e-6))        # slowest seconds per GB
inp[, pred_s := input_gb * rate * slowdown]
makespan <- function(t, J) {           # dynamic scheduling, longest first
  w <- numeric(J); for (x in sort(t, decreasing = TRUE)) { i <- which.min(w); w[i] <- w[i] + x }; max(w)
}
cat("\n================ Workflow B summary: estimate for this machine ================\n")
cat(sprintf("locations: %d   total input: %.1f GB   largest location: %.2f GB (loc %d)\n",
            nrow(inp), sum(inp$input_gb), max(inp$input_gb), inp$location_id[which.max(inp$input_gb)]))
cat("sample timings:\n"); print(s1[, .(location_id, input_gb = round(input_gb, 3), seconds = round(seconds),
                                     sec_per_gb = round(seconds / input_gb), peak_mb = round(peak_mb), output_gb = round(output_gb, 3))])
cat(sprintf("rate used: %.0f s per GB (slowest sample); contention factor: %.2f\n", rate, slowdown))
cat(sprintf("total work: %.2f CPU-hours\n", sum(inp$pred_s) / 3600))
for (J in jobs_full) cat(sprintf("  with %3d workers: about %.1f hours wall time; peak memory about %.1f GB (%d x the largest sample peak)\n",
                                 J, makespan(inp$pred_s, J) / 3600, J * max(s1$peak_mb, na.rm = TRUE) / 1024, J))
out_per_loc <- max(s1$output_gb)
cat(sprintf("output: about %.1f GB (largest sample output x %d locations)\n", out_per_loc * nrow(inp), nrow(inp)))
cat("These are estimates from", length(samples), "sample locations; contention was measured with only",
    length(samples), "running at once.\n")
fwrite(inp, file.path(BENCH_OUT, "predicted_by_location.csv"))
