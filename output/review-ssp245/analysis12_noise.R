# Temperature-uncertainty noise test. Hypothesis: GBD 2019 propagated daily
# temperature uncertainty draws (their exposure is draw-indexed); our
# production run had none (CCKP conversion supplies no temp_sd, so the
# pipeline's optional temperature-draw path never fired). Adding N(0, s)
# noise to daily temperature widens the exposure distribution, which raises
# burden most in narrow-spread, mid-TMREL (cool-side) countries: the
# tropical low cluster's exact signature.
#
# Implementation: exact expectation over noise draws via convolution of the
# per-zone binned (0.1 C) exposure distribution with a Gaussian kernel,
# BEFORE zone-range truncation (matching 03_load_temperature.R's order:
# noise first, re-truncate after). Burden via the validated replica
# (summary curves, access-cm2 2022, IHME cause-death weights).
suppressPackageStartupMessages({library(ncdf4); library(data.table)})
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
C <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "output/review-ssp245"

nc <- nc_open(file.path(C, "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245/timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
v <- names(nc$var)[which.max(sapply(nc$var, function(x) prod(x$size)))]
arr <- ncvar_get(nc, v); nc_close(nc)
nc <- nc_open(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245/climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc"))
pp <- ncvar_get(nc, names(nc$var)[1]); pp[is.na(pp)] <- 0; nc_close(nc)
grid_dt <- readRDS(file.path(OUT, "pixel_loc_map.rds"))
erf <- setDT(readRDS("data/erf/cache/erf_curves_summary.rds"))[, .(zone, daily_temp, acause, rr_mean)]
zlim <- erf[, .(tmin = min(daily_temp), tmax = max(daily_temp)), by = zone]
M <- fread(file.path(OUT, "revision_all204.csv"))
meta <- fread(file.path(OUT, "location_meta.csv"))
low23 <- M[val_nonopt > 50 & ours_nonopt < lower_nonopt, location_id]
targets <- c(low23, 125L, 161L)   # cluster + controls (Colombia, Bangladesh)
bc <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/by_cause.csv")
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022 & location_id %in% targets]

SDS <- c(0, 0.5, 1.0, 1.5, 2.0)

burden_conv <- function(id, s) {
  px <- grid_dt[loc_id == id]
  px[, pop := pp[cbind(ilon, ilat)]]; px <- px[pop > 0]
  d <- t(apply(px[, cbind(ilon, ilat)], 1, function(ij) arr[ij[1], ij[2], ]))
  zone <- pmin(pmax(round(rowMeans(d)), 6), 28)
  tm <- fread(sprintf("data/tmrel/tmrel_%d_summaries.csv", id))[year_id == 2020]
  tmrel10 <- setNames(round(tm$tmrelMean * 10), tm$meanTempCat)
  nd <- ncol(d)
  long <- data.table(pix = rep(seq_len(nrow(px)), nd),
                     t10 = as.integer(round(10 * as.vector(d))), w = rep(px$pop, nd))
  long[, zone := zone[pix]]; long[, w := w / sum(w)]
  agg <- long[, .(pr = sum(w)), by = .(zone, t10)]
  sapply(s, function(sd_c) {
    a <- copy(agg)
    if (sd_c > 0) {
      ks <- seq(-4 * sd_c * 10, 4 * sd_c * 10)          # kernel in 0.1 C steps
      kw <- dnorm(ks / 10, sd = sd_c); kw <- kw / sum(kw)
      a <- a[, .(t10 = t10 + ks, pr = pr * kw), by = .(zone, orig = t10)][
             , .(pr = sum(pr)), by = .(zone, t10)]
    }
    a <- merge(a, zlim, by = "zone")
    a[, t10c := pmin(pmax(t10, tmin), tmax)]
    a[, tmrel := tmrel10[as.character(zone)]]
    a <- a[, .(pr = sum(pr)), by = .(zone, t10c, tmrel)][t10c != tmrel]
    cs <- erf[a, on = c(zone = "zone", daily_temp = "t10c"), allow.cartesian = TRUE]
    ref <- erf[unique(a[, .(zone, tmrel)]), on = c(zone = "zone", daily_temp = "tmrel")]
    cs <- merge(cs, ref[, .(zone, acause, rr_ref = rr_mean)], by = c("zone", "acause"))
    cs[, rel := rr_mean / rr_ref]
    paf <- cs[, .(paf = sum(fifelse(rel >= 1, pr * (rel - 1) / rel, pr * (rel - 1)))),
              by = acause]
    merge(paf, bc[location_id == id, .(acause, deaths)], by = "acause")[, sum(paf * deaths)]
  })
}

res <- list()
for (id in targets) {
  b <- tryCatch(burden_conv(id, SDS), error = function(e) rep(NA_real_, length(SDS)))
  res[[length(res) + 1]] <- data.table(location_id = id, sd = SDS, burden = b)
  cat("done", id, "\n")
}
R <- dcast(rbindlist(res), location_id ~ sd, value.var = "burden")
setnames(R, c("location_id", paste0("s", SDS)))
R <- merge(R, meta[, .(location_id, location_name)], by = "location_id")
R <- merge(R, M[, .(location_id, paper = val_nonopt, paper_lo = lower_nonopt,
                    paper_hi = upper_nonopt, ratio0 = ours_vs_paper)], by = "location_id")
fwrite(R, file.path(OUT, "noise_test.csv"))
cat("\n== replica burden vs temperature-noise sd (deg C); paper-era target ==\n")
print(R[order(ratio0), .(location_name, s0 = round(s0), s05 = round(s0.5), s1 = round(s1),
      s15 = round(s1.5), s2 = round(s2), paper = round(paper),
      paper_ui = sprintf("[%d, %d]", round(paper_lo), round(paper_hi)))], nrows = 30)
cat("\ncountries whose s1 (1.0 C noise) value enters the paper UI:",
    R[s1 >= paper_lo & s1 <= paper_hi, .N], "of", nrow(R),
    "| at s0:", R[s0 >= paper_lo & s0 <= paper_hi, .N], "\n")