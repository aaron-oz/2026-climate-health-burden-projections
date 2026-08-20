# All-204 draw-pairing comparison: replica burden under (A) production's
# recycled ERF-draw x TMREL-draw pairing vs (B) per-draw argmin TMRELs
# (fixed country cause-death weights), vs production and GBD 2019.
# access-cm2-r1i1p1f1, 2022, 500 ERF draws, signed PAF, IHME cause-death
# weights for burden aggregation.
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
meta <- fread(file.path(OUT, "location_meta.csv"))
M <- fread(file.path(OUT, "revision_all204.csv"))
bc <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/by_cause.csv")
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022]
nat <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/national_by_year.csv")
nat <- nat[model == "access-cm2-r1i1p1f1" & year == 2022]

E <- setDT(readRDS("data/erf/cache/erf_curves_draws_N500.rds"))
draw_ids <- sort(unique(E$draw)); ndr <- length(draw_ids)
if (min(E$rr) < 0) E[, rr := exp(rr)]
# pre-build per-zone per-cause blocks (temps x draws), complete temp grid
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

res <- list()
for (id in meta$location_id) {
  ok <- tryCatch({
    px <- grid_dt[loc_id == id]; px[, pop := pp[cbind(ilon, ilat)]]; px <- px[pop > 0]
    if (nrow(px) == 0) stop("no pixels")
    d <- t(apply(px[, cbind(ilon, ilat)], 1, function(ij) arr[ij[1], ij[2], ]))
    zone <- pmin(pmax(round(rowMeans(d)), 6), 28)
    nd <- ncol(d)
    long <- data.table(pix = rep(seq_len(nrow(px)), nd),
                       t10 = as.integer(round(10 * as.vector(d))), w = rep(px$pop, nd))
    long[, zone := zone[pix]]; long[, w := w / sum(w)]
    tmr <- fread(sprintf("data/tmrel/tmrel_%d.csv", id))[year_id == 2020]
    wts <- bc[location_id == id, .(acause, deaths)]
    deaths_v <- setNames(wts$deaths, wts$acause)
    burden_A <- 0; burden_B <- 0
    for (z in sort(unique(long$zone))) {
      zi <- Z[[as.character(z)]]
      lz <- long[zone == z]
      lz[, t10c := pmin(pmax(t10, min(zi$tvals)), max(zi$tvals))]
      prz <- lz[, .(pr = sum(w)), by = t10c]
      ti <- match(prz$t10c, zi$tvals)
      wv <- deaths_v[zi$causes]; wv[is.na(wv)] <- 0
      if (sum(wv) == 0) next
      wvn <- wv / sum(wv)
      Wm <- matrix(0, length(zi$tvals), ndr)
      for (ci in seq_along(zi$causes)) { B <- zi$blocks[[ci]]; B[is.na(B)] <- 1
        Wm <- Wm + wvn[ci] * B }
      tB <- zi$tvals[apply(Wm, 2, which.min)]
      tmz <- as.numeric(tmr[meanTempCat == z, paste0("tmrel_", 0:99), with = FALSE])
      if (!length(tmz) || anyNA(tmz)) next
      tA <- as.integer(round(10 * tmz[(draw_ids %% 100) + 1]))
      tA <- pmin(pmax(tA, min(zi$tvals)), max(zi$tvals))
      iA <- match(tA, zi$tvals); iB <- match(tB, zi$tvals)
      for (ci in seq_along(zi$causes)) {
        block <- zi$blocks[[ci]]
        refA <- block[cbind(iA, seq_len(ndr))]
        refB <- block[cbind(iB, seq_len(ndr))]
        sub <- block[ti, , drop = FALSE]
        burden_A <- burden_A + mean(colSums(prz$pr * f_signed(sweep(sub, 2, refA, "/")), na.rm = TRUE)) * wv[ci]
        burden_B <- burden_B + mean(colSums(prz$pr * f_signed(sweep(sub, 2, refB, "/")), na.rm = TRUE)) * wv[ci]
      }
    }
    res[[length(res)+1]] <- data.table(location_id = id, A = burden_A, B = burden_B)
    TRUE
  }, error = function(e) {cat("loc", id, "failed:", conditionMessage(e), "\n"); FALSE})
}
R <- rbindlist(res)
R <- merge(R, nat[, .(location_id, production = deaths_nonopt_mean)], by = "location_id")
R <- merge(R, M[, .(location_id, location_name, paper = val_nonopt,
                    lo = lower_nonopt, hi = upper_nonopt, in_ui_prod = in_paper_ui)], by = "location_id")
fwrite(R, file.path(OUT, "pairing_all204.csv"))

cat("\n== emulation check: replica-A vs production ==\n")
R[, a_ratio := A / production]
cat("median A/production:", R[abs(production) > 50, round(median(a_ratio), 3)],
    "| within 5%:", R[abs(production) > 50 & abs(a_ratio - 1) < 0.05, .N],
    "of", R[abs(production) > 50, .N], "\n")
cat("worst emulation mismatches (|prod|>200):\n")
print(R[abs(production) > 200][order(-abs(log(pmax(a_ratio, 0.01))))][1:8,
      .(location_name, production = round(production), A = round(A), a_ratio = round(a_ratio, 2))])

cat("\n== A vs B vs GBD 2019 ==\n")
cat("global sums: production", round(sum(R$production)), "| A", round(sum(R$A)),
    "| B", round(sum(R$B)), "| GBD2019", round(sum(R$paper)), "\n")
R[, `:=`(inA = A >= lo & A <= hi, inB = B >= lo & B <= hi)]
cat("countries inside GBD-2019 UI: A", sum(R$inA), "| B", sum(R$inB), "of", nrow(R), "\n")
big <- R[paper > 100]
cat("median ratio to GBD (paper>100,", nrow(big), "countries): A",
    round(median(big$A / big$paper), 2), "| B", round(median(big$B / big$paper), 2), "\n")
cat("median |log ratio|: A", round(median(abs(log(pmax(big$A, 1) / big$paper))), 3),
    "| B", round(median(abs(log(pmax(big$B, 1) / big$paper))), 3), "\n")
lowc <- R[location_id %in% M[val_nonopt > 50 & ours_nonopt < lower_nonopt, location_id]]
cat("low-23 cluster: inside UI A", sum(lowc$inA), "| B", sum(lowc$inB), "of", nrow(lowc),
    "| median ratio A", round(median(lowc$A / lowc$paper), 2),
    "| B", round(median(lowc$B / lowc$paper), 2), "\n")
prev_ok <- R[in_ui_prod == TRUE]
cat("previously-in-UI countries that LEAVE the UI under B:", prev_ok[inB == FALSE, .N],
    "of", nrow(prev_ok), "(", paste(head(prev_ok[inB == FALSE, location_name], 12), collapse = ", "), ")\n")