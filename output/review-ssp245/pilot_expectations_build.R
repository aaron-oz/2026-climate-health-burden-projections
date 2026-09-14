# Build the shipped pilot expectations for the ssp245 rerun pilot (stage 1):
#   pilot_expectations_burden.csv  (per pilot location, access-cm2 2022)
#   pilot_expectations_tmrel.csv   (per pilot location x zone)
# consumed by pilot_check.R. Replaces the hand-assembled 2026-08-24 files,
# which were lifted from analysis17_2x2_sdfield_all204.csv (column B) and
# analysis18_tmrel_anchor.csv (derived_mean).
#
# Replica instrument as in analysis17 (raw exposure only): per-pixel-day
# exposure histogram, signed PAF, 500 ERF draws, IHME production 2022 cause
# deaths as point weights. Variants:
#   A  = production pairing (released TMREL draws recycled to 500)
#   B0 = per-draw argmin TMRELs, point weights over ALL causes (the scheme the
#        2026-08-24 expectations used; kept only as the regression check)
#   B  = per-draw argmin TMRELs, point weights EXCLUDING TMREL_WEIGHT_EXCLUDE
#        (default inj_disaster; Aaron's 2026-09-14 ruling, see config.R)
# Each burden is reported as a total over the 17 causes and as the sum over
# the causes NOT in the exclusion list (suffix _exdis). The value gate uses
# the _exdis quantity: in Haiti the inj_disaster cause alone swings the total
# by thousands of deaths per draw in the earthquake-scale forecast draws
# (PAF of about +/-2 % on 190,000-305,000 deaths), old run and new alike, so
# the total cannot resolve a TMREL implementation difference there.
#
# Usage (repo root): Rscript output/review-ssp245/pilot_expectations_build.R
suppressPackageStartupMessages({library(ncdf4); library(data.table)})
ROOT <- Sys.getenv("PROJECT_ROOT",
                   "/var/home/aoz/code/wbg-climate-health-burden-projections")
setwd(ROOT)
C   <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "output/review-ssp245"          # replica inputs (pixel map, meta, GBD)
# outputs go next to this script, so a worktree checkout writes its own CSVs
OUT_W <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1]))
if (is.na(OUT_W)) OUT_W <- OUT
REV <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245"
LOCS <- c(6, 11, 13, 81, 102, 114, 125, 131, 135, 145, 163, 171, 190, 213, 214)
SEARCH <- c(66L, 346L)
source_cfg <- new.env()
sys.source(file.path(OUT_W, "..", "..", "global-scripts", "config.R"), envir = source_cfg)
EXCL <- trimws(unlist(strsplit(as.character(source_cfg$TMREL_WEIGHT_EXCLUDE), ",")))
EXCL <- EXCL[nzchar(EXCL) & !tolower(EXCL) %in% c("none", "false")]
cat("TMREL weight exclusion:", if (length(EXCL)) paste(EXCL, collapse = ",") else "(none)", "\n")

nc <- nc_open(file.path(C, "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245/timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
v <- names(nc$var)[which.max(sapply(nc$var, function(x) prod(x$size)))]
arr <- ncvar_get(nc, v); nc_close(nc)
nc <- nc_open(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245/climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc"))
pp <- ncvar_get(nc, names(nc$var)[1]); pp[is.na(pp)] <- 0; nc_close(nc)

grid_dt <- readRDS(file.path(OUT, "pixel_loc_map.rds"))
meta <- fread(file.path(OUT, "location_meta.csv"))
M <- fread(file.path(OUT, "revision_all204.csv"))
bc <- fread(file.path(REV, "by_cause.csv"))[model == "access-cm2-r1i1p1f1" & year == 2022]

E <- setDT(readRDS("data/erf/cache/erf_curves_draws_N500.rds"))
draw_ids <- sort(unique(E$draw)); ndr <- length(draw_ids)
if (min(E$rr) < 0) E[, rr := exp(rr)]
Z <- list()
for (z in sort(unique(E$zone))) {
  ez <- dcast(E[zone == z], acause + daily_temp ~ draw, value.var = "rr")
  causes <- sort(unique(ez$acause)); tvals <- sort(unique(ez$daily_temp))
  dcols <- as.character(draw_ids)
  blocks <- lapply(causes, function(c_) as.matrix(ez[acause == c_][match(tvals, daily_temp), ..dcols]))
  Z[[as.character(z)]] <- list(causes = causes, tvals = tvals, blocks = blocks)
}
rm(E); invisible(gc())
f_signed <- function(rel) ifelse(rel >= 1, (rel - 1) / rel, rel - 1)

argmin_tmrel <- function(zi, wv) {
  # point-weight death-weighted curve per draw, argmin over the search range
  keep <- zi$tvals >= SEARCH[1] & zi$tvals <= SEARCH[2]
  W <- matrix(0, sum(keep), ndr)
  wvn <- wv / sum(wv)
  for (ci in seq_along(zi$causes)) {
    if (wvn[ci] == 0) next
    Bm <- zi$blocks[[ci]][keep, , drop = FALSE]; Bm[is.na(Bm)] <- 1
    W <- W + wvn[ci] * Bm
  }
  zi$tvals[keep][apply(W, 2, which.min)]
}

burden_rows <- list(); tmrel_rows <- list()
for (id in LOCS) {
  px <- grid_dt[loc_id == id]; px[, pop := pp[cbind(ilon, ilat)]]; px <- px[pop > 0]
  d <- t(apply(px[, cbind(ilon, ilat)], 1, function(ij) arr[ij[1], ij[2], ]))
  zone <- pmin(pmax(round(rowMeans(d)), 6), 28)
  long <- data.table(pix = rep(seq_len(nrow(px)), ncol(d)),
                     t10 = as.integer(round(10 * as.vector(d))),
                     w = rep(px$pop, ncol(d)))
  long[, zone := zone[pix]]; long[, w := w / sum(w)]
  tmr <- fread(sprintf("data/tmrel/tmrel_%d.csv", id))[year_id == 2020]
  tms <- fread(sprintf("data/tmrel/tmrel_%d_summaries.csv", id))[year_id == 2020]
  wts <- bc[location_id == id, .(acause, deaths)]
  deaths_v <- setNames(wts$deaths, wts$acause)
  acc <- list()
  for (p in c("A", "B0", "B")) acc[[p]] <- c(total = 0, exdis = 0)
  for (z in sort(unique(long$zone))) {
    zi <- Z[[as.character(z)]]
    lz <- long[zone == z]
    prz <- lz[, .(pr = sum(w)), by = t10]
    prz[, t10c := pmin(pmax(t10, min(zi$tvals)), max(zi$tvals))]
    prz <- prz[, .(pr = sum(pr)), by = t10c]
    wv <- deaths_v[zi$causes]; wv[is.na(wv)] <- 0
    if (sum(wv) == 0) next
    wv_ex <- wv; wv_ex[zi$causes %in% EXCL] <- 0
    tB0 <- argmin_tmrel(zi, wv)
    tB  <- argmin_tmrel(zi, wv_ex)
    tmz <- as.numeric(tmr[meanTempCat == z, paste0("tmrel_", 0:99), with = FALSE])
    if (!length(tmz) || anyNA(tmz)) next
    tA <- as.integer(round(10 * tmz[(draw_ids %% 100) + 1]))
    tA <- pmin(pmax(tA, min(zi$tvals)), max(zi$tvals))
    refs <- list(A = match(tA, zi$tvals), B0 = match(tB0, zi$tvals), B = match(tB, zi$tvals))
    # TMREL expectation for the FULL zone set (all 23 zones, as the pipeline
    # derives them), not only the zones this location's pixels fall in
    for (ci in seq_along(zi$causes)) {
      if (wv[ci] == 0) next
      block <- zi$blocks[[ci]]
      sub <- block[match(prz$t10c, zi$tvals), , drop = FALSE]
      for (p in names(refs)) {
        refv <- block[cbind(refs[[p]], seq_len(ndr))]
        paf <- mean(colSums(prz$pr * f_signed(sweep(sub, 2, refv, "/")), na.rm = TRUE)) * wv[ci]
        acc[[p]]["total"] <- acc[[p]]["total"] + paf
        if (!zi$causes[ci] %in% EXCL) acc[[p]]["exdis"] <- acc[[p]]["exdis"] + paf
      }
    }
  }
  # TMREL expectations over every zone (the derived cache holds all 23)
  for (z in names(Z)) {
    zi <- Z[[z]]
    wv <- deaths_v[zi$causes]; wv[is.na(wv)] <- 0
    wv[zi$causes %in% EXCL] <- 0
    if (sum(wv) == 0) next
    td <- argmin_tmrel(zi, wv) / 10
    rel <- tms[meanTempCat == as.integer(z)]
    tmrel_rows[[length(tmrel_rows) + 1]] <- data.table(
      location_id = id, zone = as.integer(z),
      expected_tmrel_c = round(mean(td), 3),
      released_tmrel_c = if (nrow(rel) == 1) rel$tmrelMean else NA_real_)
  }
  burden_rows[[length(burden_rows) + 1]] <- data.table(
    location_id = id,
    repA = acc$A["total"], repA_exdis = acc$A["exdis"],
    repB0 = acc$B0["total"],
    repB = acc$B["total"], repB_exdis = acc$B["exdis"])
  cat("loc", id, "done\n")
}
B <- rbindlist(burden_rows)
B <- merge(B, M[, .(location_id, location_name, gbd_val = val_nonopt,
                    gbd_lo = lower_nonopt, gbd_hi = upper_nonopt)], by = "location_id")
setcolorder(B, c("location_id", "location_name"))

# regression against the 2026-08-24 shipped values (analysis17 A and B)
prev <- fread(file.path(OUT, "analysis17_2x2_sdfield_all204.csv"))[location_id %in% LOCS, .(location_id, A17 = A, B17 = B)]
cmp <- merge(B, prev, by = "location_id")
cat(sprintf("regression vs analysis17: max |repA/A17 - 1| = %.2e, max |repB0/B17 - 1| = %.2e\n",
            max(abs(cmp$repA / cmp$A17 - 1)), max(abs(cmp$repB0 / cmp$B17 - 1))))
B[, repB0 := NULL]
fwrite(B, file.path(OUT_W, "pilot_expectations_burden.csv"))
Tm <- rbindlist(tmrel_rows)
fwrite(Tm, file.path(OUT_W, "pilot_expectations_tmrel.csv"))
cat("wrote", nrow(B), "burden rows and", nrow(Tm), "tmrel rows\n")
print(B[, .(location_name, repA = round(repA), repB = round(repB), repB_exdis = round(repB_exdis), gbd = round(gbd_val))])
