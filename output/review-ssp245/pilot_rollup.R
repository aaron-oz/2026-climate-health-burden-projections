# Pilot roll-up for the ssp245 burden rerun (stage 1): collapse each pilot
# location's burden_2022.rds files to draw-level national sums, one row per
# (location, model-scenario combo, draw). Run BEFORE the pilot to capture the
# old run's draws, and again AFTER, so the two CSVs support the draw-level
# gates (interval widths, negative-draw fraction) without shipping the full
# RDS files.
#
# Since 2026-09-14 the roll-up also carries deaths_nonopt_exdis, the
# non-optimal-temperature deaths summed over the causes NOT in
# TMREL_WEIGHT_EXCLUDE (config.R; default none, so it equals deaths_nonopt).
# With an exclusion set, the value gate uses it: in Haiti the inj_disaster
# cause alone swings the total by thousands of deaths per draw in the
# earthquake-scale forecast draws.
#
# Usage (repo root): Rscript output/review-ssp245/pilot_rollup.R out.csv
suppressPackageStartupMessages(library(data.table))
LOCS <- c(6, 11, 13, 81, 102, 114, 125, 131, 135, 145, 163, 171, 190, 213, 214)
cfg <- new.env(); sys.source(file.path("global-scripts", "config.R"), envir = cfg)
EXCL <- trimws(unlist(strsplit(as.character(cfg$TMREL_WEIGHT_EXCLUDE), ",")))
EXCL <- EXCL[nzchar(EXCL) & !tolower(EXCL) %in% c("none", "false")]
out_csv <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(out_csv)) stop("usage: Rscript pilot_rollup.R <out.csv>")
res <- list()
for (loc in LOCS) {
  combo_dirs <- list.dirs(file.path("output", "results", "cckp", loc),
                          recursive = FALSE)
  for (d in combo_dirs) {
    f <- file.path(d, "burden_2022.rds")
    if (!file.exists(f)) next
    b <- setDT(readRDS(f))
    grp <- intersect("draw", names(b))
    s <- b[, .(deaths_nonopt = sum(deaths_nonopt, na.rm = TRUE),
               deaths_heat   = sum(deaths_heat,   na.rm = TRUE),
               deaths_cold   = sum(deaths_cold,   na.rm = TRUE),
               deaths_nonopt_exdis = sum(deaths_nonopt[!acause %in% EXCL], na.rm = TRUE)),
           by = grp]
    s[, `:=`(location_id = loc, combo = basename(d))]
    res[[length(res) + 1]] <- s
  }
}
stopifnot(length(res) > 0)
fwrite(rbindlist(res, fill = TRUE), out_csv)
cat("wrote", out_csv, ":", length(res), "combos across",
    uniqueN(rbindlist(res, fill = TRUE)$location_id), "locations\n")
