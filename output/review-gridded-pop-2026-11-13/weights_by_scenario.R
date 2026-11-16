# weights_by_scenario.R: how much the within-country population weights differ between
# the SSP gridded population files the production run uses (CCKP, gpw-v4-rev11-<ssp>,
# 20-year means 2020-2039 and 2040-2059), relative to SSP2-4.5.
#
# The pipeline weights each pixel by pi_p = pixel population / location's gridded total
# (03_load_temperature.R); a pixel belongs to a location when its center falls inside the
# location's polygon (util_convert_cckp_temperature.R, st_intersects on pixel centers,
# planar geometry). This script uses the same assignment on the level-3 polygons of the
# augmented GBD shapefile. Locations with no pixel center inside (the pipeline falls back
# to the nearest pixel there) are reported as such and left out of the summaries.
#
# Per location, bucket and scenario s (against SSP2-4.5, same bucket):
#   tv        total variation distance, 0.5 * sum_p |pi_s - pi_245|: the share of the
#             population that sits in a different place (0 = identical weights, 1 = disjoint)
#   dT        sum_p (pi_s - pi_245) * T_p, in degrees C, with T_p one fixed field (annual
#             mean daily temperature of access-cm2 ssp585 2050): how far the reweighting
#             alone moves the population-weighted mean temperature. Descriptive only; it is
#             not the burden effect, which also depends on the daily distribution and the
#             risk curves (see honduras_swap.R for the burden effect)
#   tot_ratio gridded total under s / under SSP2-4.5 (not used by the pipeline, which
#             takes counts from IHME; shown so the size of the SSP difference is visible)
#   dT_clim   for scale: sum_p pi_245 * (T_p - T_ref_p), T_ref the same model and year under
#             SSP2-4.5 (5th argument): the climate difference in population-weighted mean
#             temperature with the weights held at SSP2-4.5. One model, one year: a rough
#             yardstick, not an estimate of the scenario difference.
# Usage: Rscript weights_by_scenario.R <cckp cache dir> <shapefile.shp> <temperature nc> <out dir> <ssp245 temperature nc>
suppressPackageStartupMessages({library(data.table); library(ncdf4); library(sf)})
sf_use_s2(FALSE)
a <- commandArgs(TRUE); C <- a[1]; SHP <- a[2]; TNC <- a[3]; OUT <- a[4]; TREF <- a[5]
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
SC <- c("ssp126", "ssp245", "ssp370", "ssp585"); BK <- c("2020-2039", "2040-2059")
rd <- function(f) {
  nc <- nc_open(f); on.exit(nc_close(nc))
  v <- names(nc$var)[grepl("pop", names(nc$var))][1]
  x <- ncvar_get(nc, v); x[x >= 1e19] <- NA; x[is.na(x)] <- 0
  list(lon = ncvar_get(nc, "lon"), lat = ncvar_get(nc, "lat"), x = x)
}
P <- list()
for (s in SC) for (b in BK) {
  f <- file.path(C, sprintf("climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-%s_climatology_mean_%s.nc", s, b))
  P[[paste(s, b)]] <- rd(f)
}
g <- P[[1]]; nlon <- length(g$lon); nlat <- length(g$lat)
cat("grid", nlon, "x", nlat, "; lon", range(g$lon), "; lat", range(g$lat), "\n")
pix <- CJ(j = seq_len(nlat), i = seq_len(nlon))[, `:=`(lon = g$lon[i], lat = g$lat[j])]
for (k in names(P)) pix[, (k) := as.numeric(P[[k]]$x[cbind(i, j)])]
pix <- pix[rowSums(pix[, names(P), with = FALSE]) > 0]
cat("pixels with population in any file:", nrow(pix), "\n")
# fixed temperature field: annual mean of daily mean temperature
tann <- function(f) {
  nc <- nc_open(f); on.exit(nc_close(nc)); tv <- names(nc$var)[grepl("tas", names(nc$var))][1]
  nt <- nc$dim$time$len; acc <- matrix(0, nlon, nlat); cnt <- 0
  for (t0 in seq(1, nt, by = 30)) {
    k <- min(30, nt - t0 + 1)
    x <- ncvar_get(nc, tv, start = c(1, 1, t0), count = c(-1, -1, k)); x[x >= 1e19] <- NA
    acc <- acc + apply(x, c(1, 2), sum); cnt <- cnt + k
  }
  acc / cnt
}
pix[, Tann := tann(TNC)[cbind(i, j)]]
pix[, Tref := tann(TREF)[cbind(i, j)]]
cat("pixels with population but no temperature:", pix[is.na(Tann), .N], "\n")
s <- st_read(SHP, quiet = TRUE); s <- s[s$level == 3, "loc_id"]
pts <- st_as_sf(pix[, .(lon, lat)], coords = c("lon", "lat"), crs = 4326)
s <- st_transform(s, 4326)
hit <- st_intersects(pts, s)
pix[, loc := vapply(hit, function(h) if (length(h)) as.integer(s$loc_id[h[1]]) else NA_integer_, 1L)]
pix <- pix[!is.na(loc)]
cat("locations with at least one pixel center:", uniqueN(pix$loc), "\n")
res <- rbindlist(lapply(BK, function(b) {
  ref <- paste("ssp245", b)
  rbindlist(lapply(SC, function(sc) {
    k <- paste(sc, b)
    pix[, .(bucket = b, scenario = sc, n_pix = .N,
            tot_ratio = sum(get(k)) / sum(get(ref)),
            tv = 0.5 * sum(abs(get(k) / sum(get(k)) - get(ref) / sum(get(ref)))),
            dT = sum((get(k) / sum(get(k)) - get(ref) / sum(get(ref))) * Tann, na.rm = TRUE),
            T245 = sum(get(ref) / sum(get(ref)) * Tann, na.rm = TRUE),
            dT_clim = sum(get(ref) / sum(get(ref)) * (Tann - Tref), na.rm = TRUE)), by = loc]
  }))
}))
fwrite(res, file.path(OUT, "weights_by_scenario.csv"))
q <- function(v) paste(sprintf("%.4f", quantile(v, c(0, .5, .9, .99, 1), na.rm = TRUE)), collapse = " / ")
cat("\nper location, against SSP2-4.5: quantiles min / median / 90% / 99% / max\n")
print(res[scenario != "ssp245", .(n_loc = .N, tv = q(tv), abs_dT = q(abs(dT)), tot_ratio = q(tot_ratio)), by = .(bucket, scenario)])
x <- res[bucket == "2040-2059" & scenario == "ssp585"]
cat("\nssp585, 2040-2059 weights: |dT| / |dT_clim| (climate: access-cm2 2050, ssp585 minus ssp245):", q(abs(x$dT) / abs(x$dT_clim)),
    "\n  dT_clim quantiles:", q(x$dT_clim), "\n  locations with |dT| > 0.25 |dT_clim|:", x[abs(dT) > 0.25 * abs(dT_clim), .N], "of", nrow(x), "\n")
cat("\nlargest |dT|, 2040-2059:\n")
print(res[bucket == "2040-2059" & scenario != "ssp245"][order(-abs(dT))][1:15])
cat("\nHonduras (129):\n"); print(res[loc == 129])
