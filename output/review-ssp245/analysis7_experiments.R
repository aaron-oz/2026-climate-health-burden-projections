# Baby truncation experiment + Haiti/Cambodia case studies.
#
# Replicates the pipeline's summary-mode PAF stage (05_compute_pafs.R lines
# 174-222: RR rescaled to the TMREL bin, per-bin contribution
# pr*(rr-1)/rr for rr>=1 else pr*(rr-1), heat = bins above TMREL) at
# national level for access-cm2-r1i1p1f1 / ssp245 / 2022, validates it
# against the production run's own numbers, then re-evaluates the heat side
# with the ERF log-linearly extrapolated past the zone max instead of
# capped. Summary curves only (no draws), one model, one year.
suppressPackageStartupMessages({library(ncdf4); library(data.table)})

C   <- "/var/home/aoz/data/wb-temp-attr-projections/cckp-test"
OUT <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"
TARGETS <- data.table(loc = c(145L, 213L, 522L, 114L, 10L),
                      name = c("Kuwait", "Niger", "Sudan", "Haiti", "Cambodia"))

read_tas <- function(path) {
  nc <- nc_open(path); on.exit(nc_close(nc))
  v <- names(nc$var)[which.max(sapply(nc$var, function(x) prod(x$size)))]
  arr <- ncvar_get(nc, v)
  dn <- sapply(nc$var[[v]]$dim, function(d) d$name)
  ilon <- grep("lon", dn); ilat <- grep("lat", dn)
  itime <- setdiff(seq_along(dn), c(ilon, ilat))
  if (!identical(c(ilon, ilat, itime), seq_along(dn))) arr <- aperm(arr, c(ilon, ilat, itime))
  if (max(arr, na.rm = TRUE) > 200) arr <- arr - 273.15
  arr
}
arr <- read_tas(file.path(C, "cmip6-daily-x0.25/tas/access-cm2-r1i1p1f1-ssp245",
  "timeseries-tas-daily-mean_cmip6-daily-x0.25_access-cm2-r1i1p1f1-ssp245_timeseries_mean_2022.nc"))
nc <- nc_open(file.path(C, "pop-x0.25/popcount/gpw-v4-rev11-ssp245",
  "climatology-popcount-annual-mean_pop-x0.25_gpw-v4-rev11-ssp245_climatology_mean_2020-2039.nc"))
pp <- ncvar_get(nc, names(nc$var)[1]); pp[is.na(pp)] <- 0; nc_close(nc)
grid_dt <- readRDS(file.path(OUT, "pixel_loc_map.rds"))

# ERF: summary curves + per-(zone, cause) hot-end slope of log(rr_mean),
# fit over the last 6 grid points (0.5 deg C window)
erf <- setDT(readRDS("data/erf/cache/erf_curves_summary.rds"))[, .(zone, daily_temp, acause, rr_mean)]
zlim <- erf[, .(tmin = min(daily_temp), tmax = max(daily_temp)), by = zone]
slopes <- erf[, {
  o <- order(daily_temp); dt6 <- tail(daily_temp[o], 6); lr6 <- log(rr_mean[o][daily_temp[o] %in% dt6])
  .(slope10 = coef(lm(lr6 ~ dt6))[2])   # per 0.1 deg C
}, by = .(zone, acause)]

# production per-cause results for the same combo, for validation + weights
bc <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/by_cause.csv")
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022 & location_id %in% TARGETS$loc]

wq <- function(v, w, p) {   # weighted quantiles
  o <- order(v); cw <- cumsum(w[o]) / sum(w)
  v[o][pmin(findInterval(p, cw) + 1L, length(v))]
}

res <- list(); diag <- list()
for (k in seq_len(nrow(TARGETS))) {
  id <- TARGETS$loc[k]; nm <- TARGETS$name[k]
  px <- grid_dt[loc_id == id]
  px[, pop := pp[cbind(ilon, ilat)]]; px <- px[pop > 0]
  daily <- t(apply(px[, cbind(ilon, ilat)], 1, function(ij) arr[ij[1], ij[2], ]))
  zone <- pmin(pmax(round(rowMeans(daily)), 6), 28)
  tm <- fread(sprintf("data/tmrel/tmrel_%d_summaries.csv", id))[year_id == 2020]
  tmrel10 <- setNames(round(tm$tmrelMean * 10), tm$meanTempCat)

  nd <- ncol(daily)
  long <- data.table(pix = rep(seq_len(nrow(px)), nd),
                     t10 = as.integer(round(10 * as.vector(daily))),
                     w = rep(px$pop, nd))
  long[, zone := zone[pix]]
  long <- merge(long, zlim, by = "zone")
  long[, `:=`(t10c = pmin(pmax(t10, tmin), tmax), over = pmax(t10 - tmax, 0L))]
  long[, w := w / sum(w)]
  long[, tmrel := tmrel10[as.character(zone)]]
  agg <- long[, .(pr = sum(w), over = over[1]), by = .(zone, t10c, t10, tmrel, tmax)]

  cs <- erf[agg, on = c(zone = "zone", daily_temp = "t10c"), allow.cartesian = TRUE]
  ref <- erf[unique(agg[, .(zone, tmrel)]), on = c(zone = "zone", daily_temp = "tmrel")]
  cs <- merge(cs, ref[, .(zone, acause, rr_ref = rr_mean)], by = c("zone", "acause"))
  cs <- merge(cs, slopes, by = c("zone", "acause"))
  cs[, risk := fifelse(t10 > tmrel, "heat", fifelse(t10 < tmrel, "cold", NA_character_))]
  cs <- cs[!is.na(risk)]
  contrib <- function(rel, pr) fifelse(rel >= 1, pr * (rel - 1) / rel, pr * (rel - 1))
  cs[, rel_cap := rr_mean / rr_ref]
  cs[, rr_ext := exp(log(rr_mean) + pmax(slope10, 0) * over)]
  cs[, rel_ext := rr_ext / rr_ref]
  paf <- cs[, .(cap = sum(contrib(rel_cap, pr)), ext = sum(contrib(rel_ext, pr))),
            by = .(acause, risk)]
  w_paf <- dcast(paf, acause ~ risk, value.var = c("cap", "ext"), fill = 0)
  w_paf[, `:=`(loc = id, name = nm)]
  res[[k]] <- w_paf

  q <- wq(as.vector(daily), rep(px$pop, nd), c(.05, .5, .95))
  zmode <- as.integer(names(sort(-tapply(px$pop, zone, sum)))[1])
  diag[[k]] <- data.table(name = nm, n_pix = nrow(px),
    t_p05 = round(q[1], 1), t_p50 = round(q[2], 1), t_p95 = round(q[3], 1),
    zone_modal = zmode, tmrel_modal = tmrel10[as.character(zmode)] / 10,
    heat_days_share = round(long[t10 > tmrel, sum(w)], 3),
    trunc_share = round(long[over > 0, sum(w)], 3))
}
R <- rbindlist(res); Dg <- rbindlist(diag)
fwrite(R, file.path(OUT, "extrapolation_experiment.csv"))
fwrite(Dg, file.path(OUT, "case_study_diagnostics.csv"))

V <- merge(R, bc[, .(loc = location_id, acause, deaths,
                     paf_heat_prod = deaths_heat / deaths,
                     paf_cold_prod = deaths_cold / deaths)],
           by = c("loc", "acause"))
cat("== validation: replica capped per-cause heat PAF vs production (same combo) ==\n")
cat("corr:", round(V[, cor(cap_heat, paf_heat_prod)], 4),
    " | median |diff|:", format(V[, median(abs(cap_heat - paf_heat_prod))], digits = 3), "\n")
cat("worst 3 disagreements:\n")
print(V[order(-abs(cap_heat - paf_heat_prod))][1:3,
      .(name, acause, replica = round(cap_heat, 4), production = round(paf_heat_prod, 4))])

cat("\n== per-country heat burden: extrapolated / capped (weighted by IHME cause deaths) ==\n")
tot <- V[, .(heat_deaths_cap = sum(cap_heat * deaths), heat_deaths_ext = sum(ext_heat * deaths),
             nonopt_cap = sum((cap_heat + cap_cold) * deaths),
             nonopt_ext = sum((ext_heat + cap_cold) * deaths)), by = name]
tot[, `:=`(heat_ratio = heat_deaths_ext / heat_deaths_cap,
           nonopt_ratio = nonopt_ext / nonopt_cap)]
print(tot[, .(name, heat_cap = round(heat_deaths_cap), heat_ext = round(heat_deaths_ext),
              heat_ratio = round(heat_ratio, 2), nonopt_ratio = round(nonopt_ratio, 2))])
cat("\n== per-cause heat ratios, Kuwait (top by capped burden) ==\n")
print(V[name == "Kuwait", .(acause, cap = round(cap_heat, 4),
       ratio = round(ext_heat / pmax(cap_heat, 1e-9), 2), deaths = round(deaths))][order(-cap * deaths)][1:8])
cat("\n== case-study diagnostics ==\n")
print(Dg)
