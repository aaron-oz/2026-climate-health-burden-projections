# trace_colombia_2022.R: every number in the worked-example document
# (docs/methodology/worked-example-2026-11/worked-example.tex), recomputed from the
# pipeline's inputs and checked against the pipeline's own outputs.
#
# Example: Colombia (location 125), 2022, climate model ACCESS-CM2, draw 0, ischemic
# heart disease (cvd_ihd), SSP2-4.5 and SSP5-8.5. The inputs come from a local rerun of
# the production pipeline for that combination (util_run_cckp_pipeline.R and
# util_run_cckp_burden.R, 500 draws, all 17 causes' IHME mortality from the
# 2026-09-24 review bundle), which reproduces the accepted ssp585 test bundle
# (review/ssp585/bundle-v2) exactly for both scenarios and for Workflow B.
#
# Usage: Rscript trace_colombia_2022.R <run root> <pooled output dir> <life table dir>
#   run root: PROJECT_ROOT of the local rerun (data/temperature, data/tmrel,
#             data/erf/cache, data/mortality, output/results, output/wb_2022_local.rds)
suppressPackageStartupMessages(library(data.table))
options(width = 160, digits = 7)
a <- commandArgs(TRUE); R <- a[1]; POOL <- a[2]; LT <- a[3]
CAUSE <- "cvd_ihd"; D <- 0L; MODEL <- "access-cm2-r1i1p1f1"
say <- function(...) cat(sprintf(...), "\n")
chk <- function(label, mine, pipe, tol = 1e-9) say("CHECK %-55s mine %.10g  pipeline %.10g  %s", label, mine, pipe, if (abs(mine - pipe) <= tol * max(1, abs(pipe))) "OK" else "MISMATCH")

erf <- setDT(readRDS(file.path(R, "data/erf/cache/erf_curves_draws_N500.rds")))
lim <- setDT(readRDS(file.path(R, "data/erf/cache/temp_limits_draws_N500.rds")))
tmc <- readRDS(file.path(R, "data/tmrel/derived_cache/125_2022_N500.rds"))
tm <- setDT(tmc$tmrel); cod <- setDT(tmc$cod)

# ---- 1. Minimum-risk temperature (TMREL) for one zone and draw ---------------------
w <- cod[draw == D][, share := deaths / sum(deaths)]
say("\n== 1. Cause-death shares, Colombia 2022, draw %d (TMREL weights) ==", D)
print(w[order(-share), .(acause, deaths = round(deaths), share = round(share, 4))])
tmrel_of <- function(z) {
  e <- erf[zone == z & draw == D & daily_temp >= 66 & daily_temp <= 346]
  e <- merge(e, w[, .(acause, share)], by = "acause")
  W <- e[, .(wrr = sum(rr * share)), by = daily_temp][order(daily_temp)]
  W
}

# ---- 2. Exposure: pixel-days, zones, weights ---------------------------------------
prep <- function(scen) {
  t <- setDT(readRDS(file.path(R, sprintf("data/temperature/cckp/125/%s-%s/daily_temp_2022.rds", MODEL, scen))))
  t[, zone := as.integer(round(mean(daily_temp))), by = pixel_id]
  t[zone < 6, zone := 6L]; t[zone > 28, zone := 28L]
  t[, t10 := as.integer(round(daily_temp * 10))]
  t <- merge(t, lim, by = "zone")
  t[, t10c := pmin(pmax(t10, min_temp), max_temp)]
  t[, pr := pop / sum(pop)]
  t
}
paf_of <- function(t, cause) {
  e <- erf[acause == cause & draw == D, .(zone, daily_temp, rr)]
  tz <- tm[draw == D, .(zone, tmrel)]
  ref <- merge(e, tz, by.x = c("zone", "daily_temp"), by.y = c("zone", "tmrel"))[, .(zone, rr_ref = rr)]
  x <- merge(t, e, by.x = c("zone", "t10c"), by.y = c("zone", "daily_temp"), all.x = TRUE)
  x <- merge(x, ref, by = "zone"); x <- merge(x, tz, by = "zone")
  x[, rrs := rr / rr_ref]
  x[, risk := fifelse(t10c < tmrel, "cold", fifelse(t10c > tmrel, "heat", NA_character_))]
  x[, contrib := pr * fifelse(rrs >= 1, (rrs - 1) / rrs, rrs - 1)]
  x
}
res <- list()
for (scen in c("ssp245", "ssp585")) {
  t <- prep(scen)
  x <- paf_of(t, CAUSE)
  P <- x[!is.na(risk), .(paf = sum(contrib)), by = risk]
  b <- setDT(readRDS(file.path(R, sprintf("output/results/cckp/125/%s-%s/burden_2022.rds", MODEL, scen))))[acause == CAUSE & draw == D]
  say("\n== 2. %s: %d pixels, %s pixel-days, total pixel population %s ==", scen, uniqueN(t$pixel_id), format(nrow(t), big.mark = ","), format(round(t[, sum(pop) / 365]), big.mark = ","))
  chk(paste(scen, "PAF heat, cvd_ihd, draw 0"), P[risk == "heat", paf], b$paf_heat[1])
  chk(paste(scen, "PAF cold, cvd_ihd, draw 0"), P[risk == "cold", paf], b$paf_cold[1])
  res[[scen]] <- list(t = t, x = x, P = P, b = b)
}

# example pixels: the most populous pixel with an annual mean below 16 C (cool highlands),
# and the most populous pixel with an annual mean of 27 C or more (hot lowlands)
t <- res$ssp245$t
pp <- t[, .(pop = pop[1], ann = mean(daily_temp), zone = zone[1], lat = NA_real_), by = pixel_id]
p1 <- pp[ann < 16][which.max(pop)]; p2 <- pp[ann >= 27][which.max(pop)]
say("\n== 3. Example pixels ==")
print(rbind(p1, p2))
x <- res$ssp245$x
for (p in c(p1$pixel_id, p2$pixel_id)) {
  xp <- x[pixel_id == p]
  for (which in c("coldest", "hottest")) {
    r <- if (which == "coldest") xp[which.min(daily_temp)] else xp[which.max(daily_temp)]
    say("pixel %d %s day %s: T = %.3f C (t10 %d, used %d), zone %d, TMREL %.1f C, raw RR %.6f, RR at TMREL %.6f, rescaled RR %.6f, risk %s, pr %.4e, contribution %.4e",
        p, which, format(r$date), r$daily_temp, r$t10, r$t10c, r$zone, r$tmrel / 10, r$rr, r$rr_ref, r$rrs, r$risk, r$pr, r$contrib)
  }
  say("pixel %d over the year: heat contribution %.4e, cold contribution %.4e; pixel share of person-days %.4f", p, xp[risk == "heat", sum(contrib)], xp[risk == "cold", sum(contrib)], xp[, sum(pr)])
}
for (z in unique(c(p1$zone, p2$zone))) {
  W <- tmrel_of(z); tz <- tm[zone == z & draw == D, tmrel]
  say("zone %d, draw %d: TMREL from cache %d; argmin of death-weighted RR here %d (weighted RR there %.5f; at -5 C %.5f; at +5 C %.5f)",
      z, D, tz, W[which.min(wrr), daily_temp], min(W$wrr), W[daily_temp == tz - 50, wrr], W[daily_temp == tz + 50, wrr])
  chk(sprintf("TMREL zone %d draw 0", z), W[which.min(wrr), daily_temp], tz)
}

# ---- 4. Deaths and Workflow B ------------------------------------------------------
say("\n== 4. Deaths, cvd_ihd, draw 0 ==")
for (scen in c("ssp245", "ssp585")) {
  b <- res[[scen]]$b
  say("%s: IHME deaths %.3f; PAF heat %.6f cold %.6f nonopt %.6f; unscaled attributable heat %.3f cold %.3f",
      scen, sum(b$deaths), b$paf_heat[1], b$paf_cold[1], b$paf_heat[1] + b$paf_cold[1], sum(b$deaths_heat), sum(b$deaths_cold))
}
p245 <- res$ssp245$b[, sum(deaths_nonopt) / sum(deaths)]; p585 <- res$ssp585$b[, sum(deaths_nonopt) / sum(deaths)]
sf <- (1 - p245) / (1 - p585)
wb <- setDT(readRDS(file.path(R, "output/wb_2022_local.rds")))[acause == CAUSE & draw == D]
say("scale factor (1 - %.6f) / (1 - %.6f) = %.6f", p245, p585, sf)
chk("scale factor cvd_ihd draw 0", sf, wb$scale_factor[1])
m585 <- sum(wb$ihme_deaths) * sf
say("scaled deaths %.3f; attributable heat %.3f, cold %.3f", m585, m585 * wb$paf_heat[1], m585 * wb$paf_cold[1])
chk("SSP5-8.5 heat attributable cvd_ihd draw 0", m585 * wb$paf_heat[1], sum(wb$deaths_heat_attrib), 1e-8)
chk("SSP5-8.5 cold attributable cvd_ihd draw 0", m585 * wb$paf_cold[1], sum(wb$deaths_cold_attrib), 1e-8)

# ---- 5. YLLs for one age-sex cell --------------------------------------------------
lt <- fread(file.path(LT, "125_lifetable.csv"))
cell <- res$ssp245$b[age_group_id == 70 & sex_id == 1]
ex <- lt[year_id == 2022 & age_group_id == 70 & sex_id == 1, ex]
say("\n== 5. YLLs: men 70-74, SSP2-4.5, cvd_ihd, draw 0: deaths %.4f, cold attributable %.4f, life expectancy at 70 %.3f, cold YLLs %.4f",
    cell$deaths, cell$deaths_cold, ex, cell$deaths_cold * ex)

# ---- 6. All causes, mean over draws, this model and year ---------------------------
say("\n== 6. All 17 causes, access-cm2, 2022, mean over 500 draws ==")
for (scen in c("ssp245", "ssp585")) {
  f <- if (scen == "ssp245") file.path(R, sprintf("output/results/cckp/125/%s-ssp245/burden_2022.rds", MODEL)) else file.path(R, "output/wb_2022_local.rds")
  b <- setDT(readRDS(f))
  if (scen == "ssp585") setnames(b, c("deaths_heat_attrib", "deaths_cold_attrib"), c("deaths_heat", "deaths_cold"))
  s <- b[, .(h = sum(deaths_heat), c = sum(deaths_cold)), by = draw]
  say("%s: heat mean %.1f (2.5%% %.1f, 97.5%% %.1f); cold mean %.1f (%.1f, %.1f)", scen, mean(s$h), quantile(s$h, .025), quantile(s$h, .975), mean(s$c), quantile(s$c, .025), quantile(s$c, .975))
}

# ---- 7. Pooled result for Colombia --------------------------------------------------
x <- readRDS(file.path(POOL, "tidy_common.rds"))$tot
say("\n== 7. Pooled (24 models x 500 draws), Colombia ==")
print(x[unit == "125" & measure %in% c("deaths_heat", "deaths_cold") & scenario %in% c("ssp245", "ssp585"),
        .(scenario, period, measure, mean = round(mean), lower95 = round(lower95), upper95 = round(upper95), diff = round(diff_mean), dl = round(diff_lower95), du = round(diff_upper95))][order(measure, scenario, period)])
