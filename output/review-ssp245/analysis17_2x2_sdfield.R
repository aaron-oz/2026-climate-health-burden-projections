# All-204 pairing x exposure-noise comparison with the MEASURED per-pixel
# ERA5 EDA-spread field (handoff steps B and C, 2026-08-20). Extends
# analysis16_2x2_all204.R:
#   Pairings: A  = production recycled released-TMREL pairing
#             B  = per-draw argmin TMRELs, fixed (point) cause-death weights
#             Br = B with TMRELs rounded to whole degrees (IHME
#                  pafCalc_sevFix.R:94 behavior; step C.1)
#             Bw = B with draw-specific weights for cvd_ihd (exact IHME
#                  forecast mortality draws; other causes point; step C.2)
#             Bs = B with ALL cause weights perturbed per draw by a lognormal
#                  with cvd_ihd's measured relative draw-sd (stress bound for
#                  the causes whose draws we lack locally; seeded)
#   Exposures: raw; +noise from the measured field, per-pixel-month sd,
#             two intra-day aggregation bounds:
#               corr  = mean 3-hourly spread   (fully correlated errors)
#               indep = sqrt(mean sd^2 / 8)    (independent errors)
#   Noise applied as per-(zone, sd-bin) Gaussian convolution of the exposure
#   histogram BEFORE truncation to the zone grid -- mathematically identical
#   to per-pixel-day convolution because pixel-days with the same (zone,
#   sd-bin, t10) are exchangeable under the kernel.
# Output: analysis17_2x2_sdfield_all204.csv with burden columns
#   A, B, Br, Bw, Bs (raw exposure), Ac, Bc (corr noise), Ai, Bi (indep
#   noise), the pop-weighted sd summaries, and the production/GBD merges.
# access-cm2-r1i1p1f1, 2022, 500 ERF draws, signed PAF, IHME cause-death
# weights for burden aggregation (as analysis16).
suppressPackageStartupMessages({library(ncdf4); library(data.table)})
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
C <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "output/review-ssp245"
SD_NC <- "/var/home/aoz/data/wb-temp-attr-projections/era5_sd/era5_t2m_spread_daily_clim_2022_cckp025.nc"

nc <- nc_open(file.path(C, "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245/timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
v <- names(nc$var)[which.max(sapply(nc$var, function(x) prod(x$size)))]
arr <- ncvar_get(nc, v); nc_close(nc)
nc <- nc_open(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245/climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc"))
pp <- ncvar_get(nc, names(nc$var)[1]); pp[is.na(pp)] <- 0; nc_close(nc)

# --- measured sd field, forced to (lon, lat, month) index order ---
nc <- nc_open(SD_NC)
read_sd <- function(name) {
  dn <- sapply(nc$var[[name]]$dim, function(d) d$name)
  a <- ncvar_get(nc, name)
  aperm(a, match(c("lon", "lat", "month"), dn))
}
sdC_f <- read_sd("sd_corr"); sdI_f <- read_sd("sd_indep")
nc_close(nc)
stopifnot(dim(sdC_f)[1] == dim(arr)[1], dim(sdC_f)[2] == dim(arr)[2])

grid_dt <- readRDS(file.path(OUT, "pixel_loc_map.rds"))
meta <- fread(file.path(OUT, "location_meta.csv"))
M <- fread(file.path(OUT, "revision_all204.csv"))
bc <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/by_cause.csv")
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022]
nat <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/national_by_year.csv")
nat <- nat[model == "access-cm2-r1i1p1f1" & year == 2022]

# month of each day-of-year, 2022 (non-leap)
mdays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
month_of <- rep(1:12, mdays)

E <- setDT(readRDS("data/erf/cache/erf_curves_draws_N500.rds"))
draw_ids <- sort(unique(E$draw)); ndr <- length(draw_ids)
if (min(E$rr) < 0) E[, rr := exp(rr)]
zones_all <- sort(unique(E$zone))
Z <- list()
for (z in zones_all) {
  ez <- dcast(E[zone == z], acause + daily_temp ~ draw, value.var = "rr")
  causes <- sort(unique(ez$acause)); tvals <- sort(unique(ez$daily_temp))
  dcols <- as.character(draw_ids)
  blocks <- lapply(causes, function(c_) {
    sub <- ez[acause == c_]
    as.matrix(sub[match(tvals, daily_temp), ..dcols])
  })
  Z[[as.character(z)]] <- list(causes = causes, tvals = tvals, blocks = blocks)
  cat("zone", z, "prepped\n")
}
rm(E); gc()

f_signed <- function(rel) ifelse(rel >= 1, (rel - 1) / rel, rel - 1)

# per-draw cvd_ihd total deaths for 2022 where the converted draw file exists
ihd_draw_deaths <- function(id) {
  f <- sprintf("data/mortality/%d_mortality_ihme_cvd_ihd_draws.rds", id)
  if (!file.exists(f)) return(NULL)
  d <- setDT(readRDS(f))[year_id == 2022, .(deaths = sum(deaths)), by = draw]
  setorder(d, draw)
  if (nrow(d) < ndr) return(NULL)
  d$deaths[seq_len(ndr)]
}

res <- list(); sd_stats <- list()
for (id in meta$location_id) {
  ok <- tryCatch({
    px <- grid_dt[loc_id == id]; px[, pop := pp[cbind(ilon, ilat)]]; px <- px[pop > 0]
    if (nrow(px) == 0) stop("no pixels")
    d <- t(apply(px[, cbind(ilon, ilat)], 1, function(ij) arr[ij[1], ij[2], ]))
    zone <- pmin(pmax(round(rowMeans(d)), 6), 28)
    nd <- ncol(d)
    # per-pixel-month sd lookups (both bounds), then per pixel-day
    # matrix(): sapply collapses to a plain vector when the country has a
    # single pixel, and the [cbind(pix, mon)] lookup below needs a matrix
    sdC_pm <- matrix(sapply(1:12, function(m) sdC_f[cbind(px$ilon, px$ilat, m)]),
                     nrow = nrow(px))
    sdI_pm <- matrix(sapply(1:12, function(m) sdI_f[cbind(px$ilon, px$ilat, m)]),
                     nrow = nrow(px))
    mo <- month_of[seq_len(nd)]
    long <- data.table(pix = rep(seq_len(nrow(px)), nd),
                       t10 = as.integer(round(10 * as.vector(d))),
                       w = rep(px$pop, nd),
                       mon = rep(mo, each = nrow(px)))
    long[, zone := zone[pix]]; long[, w := w / sum(w)]
    long[, sdc10 := as.integer(round(10 * sdC_pm[cbind(pix, mon)]))]
    long[, sdi10 := as.integer(round(10 * sdI_pm[cbind(pix, mon)]))]
    # pop-day-weighted distribution of the measured sd
    wq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- cumsum(w[o]) / sum(w)
                              x[findInterval(p, w) + 1L] }
    sd_stats[[length(sd_stats) + 1]] <- data.table(
      location_id = id,
      sd_corr_pw  = long[, sum(w * sdc10) / 10],
      sd_corr_p10 = wq(long$sdc10, long$w, 0.10) / 10,
      sd_corr_p90 = wq(long$sdc10, long$w, 0.90) / 10,
      sd_indep_pw  = long[, sum(w * sdi10) / 10],
      sd_indep_p10 = wq(long$sdi10, long$w, 0.10) / 10,
      sd_indep_p90 = wq(long$sdi10, long$w, 0.90) / 10)

    tmr <- fread(sprintf("data/tmrel/tmrel_%d.csv", id))[year_id == 2020]
    wts <- bc[location_id == id, .(acause, deaths)]
    deaths_v <- setNames(wts$deaths, wts$acause)
    ihd_d <- ihd_draw_deaths(id)   # NULL if no draw file
    # seeded lognormal weight perturbations for the stress variant: relative
    # draw-sd taken from cvd_ihd's own draws when available, else 0.05
    rel_sd <- if (!is.null(ihd_d)) sd(ihd_d) / mean(ihd_d) else 0.05
    set.seed(20260820 + id)

    vars <- c("A", "B", "Br", "Bw", "Bs", "Ac", "Bc", "Ai", "Bi")
    burden <- setNames(numeric(length(vars)), vars)
    for (z in sort(unique(long$zone))) {
      zi <- Z[[as.character(z)]]
      lz <- long[zone == z]
      # exposure histograms: raw at t10; noise variants convolved per sd-bin
      pr_raw <- lz[, .(pr = sum(w)), by = t10]
      conv_field <- function(sdcol) {
        a <- lz[, .(pr = sum(w)), by = c("t10", sdcol)]
        setnames(a, sdcol, "s10")
        out <- rbindlist(lapply(split(a, a$s10), function(g) {
          s10 <- g$s10[1]
          if (s10 <= 0) return(g[, .(t10, pr)])
          K <- max(10L, as.integer(ceiling(4 * s10)))
          ks <- seq(-K, K)
          kw <- dnorm(ks / 10, sd = s10 / 10); kw <- kw / sum(kw)
          g[, .(t10 = t10 + ks, pr = pr * kw), by = .(orig = t10)][
            , .(pr = sum(pr)), by = t10]
        }))
        out[, .(pr = sum(pr)), by = t10]
      }
      clampz <- function(a) {
        a[, t10c := pmin(pmax(t10, min(zi$tvals)), max(zi$tvals))]
        a[, .(pr = sum(pr)), by = t10c]
      }
      przs <- list(raw = clampz(copy(pr_raw)),
                   cor = clampz(conv_field("sdc10")),
                   ind = clampz(conv_field("sdi10")))
      wv <- deaths_v[zi$causes]; wv[is.na(wv)] <- 0
      if (sum(wv) == 0) next
      wvn <- wv / sum(wv)
      nz <- length(zi$tvals)
      # point-weight curve for B; draw-weight curves for Bw / Bs
      Wm <- matrix(0, nz, ndr)
      Ww <- matrix(0, nz, ndr)
      Ws <- matrix(0, nz, ndr)
      # per-draw weight matrices (causes x draws)
      wmat_w <- matrix(wv, length(wv), ndr)          # cvd_ihd row replaced below
      if (!is.null(ihd_d) && "cvd_ihd" %in% zi$causes)
        wmat_w[match("cvd_ihd", zi$causes), ] <- ihd_d
      wmat_s <- matrix(wv, length(wv), ndr) *
        matrix(exp(rnorm(length(wv) * ndr, -rel_sd^2 / 2, rel_sd)),
               length(wv), ndr)
      wmat_w <- sweep(wmat_w, 2, colSums(wmat_w), "/")
      wmat_s <- sweep(wmat_s, 2, colSums(wmat_s), "/")
      for (ci in seq_along(zi$causes)) {
        Bm <- zi$blocks[[ci]]; Bm[is.na(Bm)] <- 1
        Wm <- Wm + wvn[ci] * Bm
        Ww <- Ww + sweep(Bm, 2, wmat_w[ci, ], "*")
        Ws <- Ws + sweep(Bm, 2, wmat_s[ci, ], "*")
      }
      tB  <- zi$tvals[apply(Wm, 2, which.min)]
      tBw <- zi$tvals[apply(Ww, 2, which.min)]
      tBs <- zi$tvals[apply(Ws, 2, which.min)]
      tBr <- pmin(pmax(10L * as.integer(round(tB / 10)), min(zi$tvals)), max(zi$tvals))
      tmz <- as.numeric(tmr[meanTempCat == z, paste0("tmrel_", 0:99), with = FALSE])
      if (!length(tmz) || anyNA(tmz)) next
      tA <- as.integer(round(10 * tmz[(draw_ids %% 100) + 1]))
      tA <- pmin(pmax(tA, min(zi$tvals)), max(zi$tvals))
      refs <- list(A  = match(tA,  zi$tvals), B  = match(tB,  zi$tvals),
                   Br = match(tBr, zi$tvals), Bw = match(tBw, zi$tvals),
                   Bs = match(tBs, zi$tvals))
      # (variant, exposure) combos actually computed
      combos <- list(A  = c("raw", "cor", "ind"), B = c("raw", "cor", "ind"),
                     Br = "raw", Bw = "raw", Bs = "raw")
      colname <- function(p, ex) {
        if (ex == "raw") p else paste0(substr(p, 1, 1), c(cor = "c", ind = "i")[ex])
      }
      for (ci in seq_along(zi$causes)) {
        if (wv[ci] == 0) next
        block <- zi$blocks[[ci]]
        subs <- lapply(przs, function(prz)
          block[match(prz$t10c, zi$tvals), , drop = FALSE])
        for (p in names(combos)) {
          refv <- block[cbind(refs[[p]], seq_len(ndr))]
          for (ex in combos[[p]]) {
            prz <- przs[[ex]]
            paf <- mean(colSums(prz$pr * f_signed(sweep(subs[[ex]], 2, refv, "/")),
                                na.rm = TRUE)) * wv[ci]
            cn <- colname(p, ex)
            burden[cn] <- burden[cn] + paf
          }
        }
      }
    }
    res[[length(res) + 1]] <- data.table(location_id = id, t(burden))
    TRUE
  }, error = function(e) {cat("loc", id, "failed:", conditionMessage(e), "\n"); FALSE})
  if (isTRUE(ok) && length(res) %% 20 == 0) cat("done", length(res), "locations\n")
}
R <- rbindlist(res)
S <- rbindlist(sd_stats)
R <- merge(R, S, by = "location_id")
R <- merge(R, nat[, .(location_id, production = deaths_nonopt_mean)], by = "location_id")
R <- merge(R, M[, .(location_id, location_name, paper = val_nonopt,
                    lo = lower_nonopt, hi = upper_nonopt, in_ui_prod = in_paper_ui)],
           by = "location_id")
fwrite(R, file.path(OUT, "analysis17_2x2_sdfield_all204.csv"))

prev <- fread(file.path(OUT, "pairing2x2_all204.csv"))
cmp <- merge(R[, .(location_id, A, B)], prev[, .(location_id, A16 = A, B16 = B)],
             by = "location_id")
cat("\n== regression vs analysis16 (A and B should be identical) ==\n")
cat("max |A - A16|:", cmp[, max(abs(A - A16))],
    "| max |B - B16|:", cmp[, max(abs(B - B16))], "\n")

cat("\n== measured sd, pop-day-weighted, across countries ==\n")
for (vv in c("sd_corr_pw", "sd_indep_pw"))
  cat(sprintf("%-12s min %.2f | median %.2f | max %.2f degC\n", vv,
              min(S[[vv]]), median(S[[vv]]), max(S[[vv]])))

cat("\n== global sums ==\n")
cols <- c("A", "B", "Br", "Bw", "Bs", "Ac", "Bc", "Ai", "Bi")
cat("production", round(sum(R$production)), "| GBD2019", round(sum(R$paper)), "\n")
for (vv in cols) cat(sprintf("%-3s %d\n", vv, round(sum(R[[vv]]))))

for (vv in cols) R[, paste0("in", vv) := get(vv) >= lo & get(vv) <= hi]
cat("\n== countries inside GBD-2019 UI (of", nrow(R), ") ==\n")
for (vv in cols) cat(sprintf("%-3s %d\n", vv, sum(R[[paste0("in", vv)]])))
big <- R[paper > 100]
cat("\n== median ratio to GBD2019 (paper > 100), n =", nrow(big), "==\n")
for (vv in cols)
  cat(sprintf("%-3s median ratio %.2f | median |log ratio| %.3f\n", vv,
              median(big[[vv]] / big$paper),
              median(abs(log(pmax(big[[vv]], 1) / big$paper)))))
lowc <- R[location_id %in% M[val_nonopt > 50 & ours_nonopt < lower_nonopt, location_id]]
cat("\n== low cluster (n =", nrow(lowc), ") inside UI ==\n")
for (vv in cols) cat(sprintf("%-3s %d\n", vv, sum(lowc[[paste0("in", vv)]])))
prev_ok <- R[in_ui_prod == TRUE]
cat("\n== previously-in-UI defectors (of", nrow(prev_ok), ") ==\n")
for (vv in setdiff(cols, "A"))
  cat(sprintf("%-3s %d\n", vv, prev_ok[get(paste0("in", vv)) == FALSE, .N]))
