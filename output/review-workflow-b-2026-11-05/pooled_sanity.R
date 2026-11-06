# pooled_sanity.R: reads the output of draw_export_check.R and prints (a) the
# roll-up of the mechanical checks and (b) the sanity tables on the pooled draws.
# Usage: Rscript pooled_sanity.R <review_out_dir> <summary_dir> <bundle_root> <population.csv> <hierarchy.xlsx>
suppressPackageStartupMessages({library(data.table); library(readxl)})
setDTthreads(6)
a <- commandArgs(TRUE); OUT <- a[1]; SUM <- a[2]; BUNDLE <- a[3]; POPF <- a[4]; HIER <- a[5]
options(width = 170, datatable.print.nrows = 300)
TAPER <- c(l95 = .025, l80 = .1, l66 = .17, med = .5, u66 = .83, u80 = .9, u95 = .975)
sec <- function(x) cat("\n\n================ ", x, " ================\n")
fm <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")

h <- as.data.table(read_excel(HIER))
names_loc <- unique(h[Level == 3, .(location_id = as.integer(`Location ID`), name = `Location Name`)], by = "location_id")
ms <- readRDS(file.path(OUT, "model_sets.rds")); common <- ms$common

# ---- 1. mechanical checks -------------------------------------------------
sec("1. Mechanical checks, roll-up over locations")
ck <- rbindlist(lapply(list.files(file.path(OUT, "loc_checks"), full.names = TRUE), fread))
cat("locations with check files:", uniqueN(ck$location_id), "\n")
print(ck[, .(n_loc = uniqueN(location_id), min = signif(min(value), 3), median = signif(median(value), 3), max = signif(max(value), 3)), by = check])

# ---- 2. pooled tables ------------------------------------------------------
pl <- rbindlist(lapply(list.files(file.path(OUT, "loc_pooled"), "^\\d+\\.rds$", full.names = TRUE), readRDS))
pl <- merge(pl, names_loc, by = "location_id", all.x = TRUE)
saveRDS(pl, file.path(OUT, "loc_pooled_all.rds"))
W <- readRDS(file.path(OUT, "world", "world.rds"))
ann <- W$ann; setDT(ann)
dec_of <- function(y) fifelse(y <= 2030, "2022-2030", fifelse(y <= 2040, "2031-2040", "2041-2050"))
DY <- c("2022-2030" = 9, "2031-2040" = 10, "2041-2050" = 10)
ann[, period := dec_of(year)]
WD <- ann[, .(deaths_heat = sum(deaths_heat), deaths_cold = sum(deaths_cold), yll_heat = sum(yll_heat), yll_cold = sum(yll_cold)),
          by = .(model, scenario, period, draw)]
for (v in c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold")) set(WD, j = v, value = WD[[v]] / DY[WD$period])
WD[, `:=`(deaths_nonopt = deaths_heat + deaths_cold, yll_nonopt = yll_heat + yll_cold)]
MS <- c("deaths_heat", "deaths_cold", "deaths_nonopt", "yll_heat", "yll_cold", "yll_nonopt")
LW <- melt(WD, id.vars = c("model", "scenario", "period", "draw"), measure.vars = MS, variable.name = "measure", variable.factor = FALSE)
sumv <- function(v) {
  q <- quantile(v, TAPER, names = FALSE)
  list(mean = mean(v), l95 = q[1], l80 = q[2], l66 = q[3], med = q[4], u66 = q[5], u80 = q[6], u95 = q[7])
}
sec("2. World, common 24, per year: pooled mean and intervals (independent of the pooling script)")
tab <- LW[model %in% common, c(sumv(value)), by = .(measure, scenario, period)][order(measure, scenario, period)]
fwrite(tab, file.path(OUT, "world_common24_decades.csv"))
print(tab[, .(measure, scenario, period, mean = fm(mean), l95 = fm(l95), u95 = fm(u95), l66 = fm(l66), u66 = fm(u66))])

sec("2b. Expectation: target / ssp245, heat and cold, common 24 (ratio of pooled means), with per-model range and draw-paired difference")
w2 <- dcast(LW[model %in% common & measure %in% MS], model + period + draw + measure ~ scenario, value.var = "value")
rt <- rbindlist(lapply(c("ssp126", "ssp370", "ssp585"), function(t) {
  w2[, .(target = t, ratio = mean(get(t)) / mean(ssp245), d_mean = mean(get(t) - ssp245),
         d_l95 = quantile(get(t) - ssp245, .025, names = FALSE), d_u95 = quantile(get(t) - ssp245, .975, names = FALSE),
         d_l66 = quantile(get(t) - ssp245, .17, names = FALSE), d_u66 = quantile(get(t) - ssp245, .83, names = FALSE),
         t_l95 = quantile(get(t), .025, names = FALSE), t_u95 = quantile(get(t), .975, names = FALSE),
         share_pos = mean(get(t) > ssp245)), by = .(measure, period)]
}))
mm <- w2[, .(s126 = mean(ssp126), s245 = mean(ssp245), s370 = mean(ssp370), s585 = mean(ssp585)), by = .(model, period, measure)]
mmr <- melt(mm, id.vars = c("model", "period", "measure", "s245"), variable.factor = FALSE)
mmr[, target := sub("^s", "ssp", variable)]
mmr <- mmr[, .(mod_min = min(value / s245), mod_max = max(value / s245), n_above1 = sum(value > s245)), by = .(measure, period, target)]
rt <- merge(rt, mmr, by = c("measure", "period", "target"))
rt[, `:=`(width_ratio_d_over_t = (d_u95 - d_l95) / (t_u95 - t_l95))]
print(rt[measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt", "yll_nonopt"), .(measure, period, target, ratio = round(ratio, 3), mod_min = round(mod_min, 3), mod_max = round(mod_max, 3),
          n_above1, d_mean = fm(d_mean), d_l95 = fm(d_l95), d_u95 = fm(d_u95), d_l66 = fm(d_l66), d_u66 = fm(d_u66), share_pos = round(share_pos, 3), width_d_over_t = round(width_ratio_d_over_t, 2))][order(measure, target, period)])

sec("2c. Time trend, World, common 24, every year: ensemble mean heat and cold (thousands) and largest year-on-year jump per model")
yr <- ann[model %in% common, .(h = sum(deaths_heat), c = sum(deaths_cold)), by = .(scenario, year, model)][, .(h = mean(h) / 500, c = mean(c) / 500), by = .(scenario, model, year)]
print(dcast(yr[, .(h = round(mean(h) / 1e3), c = round(mean(c) / 1e3)), by = .(scenario, year)][year %in% seq(2022, 2050, 4)], year ~ scenario, value.var = c("h", "c")))
setorder(yr, scenario, model, year)
yr[, `:=`(jh = h / shift(h) - 1, jc = c / shift(c) - 1), by = .(scenario, model)]
print(yr[, .(max_jump_heat = round(max(abs(jh), na.rm = TRUE), 3), max_jump_cold = round(max(abs(jc), na.rm = TRUE), 3)), by = scenario])
print(head(yr[order(-abs(jh))], 6)); print(head(yr[order(-abs(jc))], 6))

sec("3. Draw coherence: World target vs reference correlation across draws within model; between large countries; across years")
print(rt[, .N, by = .(target)][0])
cw <- LW[model %in% common & measure %in% c("deaths_heat", "deaths_cold")]
cw2 <- dcast(cw, model + period + draw + measure ~ scenario, value.var = "value")
print(cw2[, .(cor126 = cor(ssp126, ssp245), cor370 = cor(ssp370, ssp245), cor585 = cor(ssp585, ssp245)), by = .(measure, period, model)][
  , .(cor126_min = round(min(cor126), 3), cor126_med = round(median(cor126), 3), cor370_min = round(min(cor370), 3), cor585_min = round(min(cor585), 3), cor585_med = round(median(cor585), 3)), by = .(measure, period)])
sf <- list.files(file.path(OUT, "world"), "^sample_", full.names = TRUE)
smp <- rbindlist(lapply(sf, function(f) readRDS(f)[, loc := sub("sample_(\\d+)\\.rds", "\\1", basename(f))]))
print(smp[1:2])
sc <- dcast(smp[scenario == "ssp245" & period == "2041-2050"], model + draw ~ loc, value.var = "deaths_cold")
scm <- sc[, lapply(.SD, function(x) x - mean(x)), by = model, .SDcols = setdiff(names(sc), c("model", "draw"))]
cat("between-country correlation of within-model draw deviations, ssp245 2041-2050, cold deaths (6=China 163=India 102=USA 135=Brazil 214=Nigeria):\n")
print(round(cor(scm[, -1]), 2))
sc <- dcast(smp[scenario == "ssp245" & period == "2041-2050"], model + draw ~ loc, value.var = "deaths_heat")
scm <- sc[, lapply(.SD, function(x) x - mean(x)), by = model, .SDcols = setdiff(names(sc), c("model", "draw"))]
cat("same, heat deaths:\n"); print(round(cor(scm[, -1]), 2))
cat("adjacent-year and 20-year draw correlation at location level, median over locations (from checks):\n")
print(ck[check %like% "^cor_draw", .(p05 = round(quantile(value, .05, na.rm = TRUE), 3), med = round(median(value, na.rm = TRUE), 3)), by = check])
all_cor <- pl[slice == "all" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt")]
cat("location-level within-model correlation, target vs reference draws:\n")
print(all_cor[, .(p05 = round(quantile(cor_tr, .05, na.rm = TRUE), 3), med = round(median(cor_tr, na.rm = TRUE), 3), min = round(min(cor_tr, na.rm = TRUE), 3)), by = .(measure, target, period)][order(measure, target, period)])
print(all_cor[target == "ssp585" & period == "2041-2050" & measure == "deaths_heat"][order(cor_tr)][1:10, .(name, t_mean = round(t_mean, 1), r_mean = round(r_mean, 1), cor_tr = round(cor_tr, 3))])

sec("4. How much of the pooled variance is climate-model spread? Share = between-model variance / total pooled variance")
vs <- LW[model %in% common & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt"), {
  mm <- .SD[, .(m = mean(value)), by = model]$m
  .(share_between = var(mm) / var(value), cv_total = sd(value) / mean(value))
}, by = .(measure, scenario, period)]
print(dcast(vs[, .(measure, scenario, period, s = round(share_between, 2))], measure + period ~ scenario, value.var = "s"))
print(dcast(vs[, .(measure, scenario, period, s = round(cv_total, 3))], measure + period ~ scenario, value.var = "s"))
cat("location level, heat 2041-2050 under ssp585: quantiles of between-model variance share\n")
print(pl[slice == "all" & measure == "deaths_heat" & period == "2041-2050" & target == "ssp585", quantile((t_sd_models^2) / (t_sd^2), c(.05, .25, .5, .75, .95), na.rm = TRUE)])

sec("5. Causes, World, common 24 (pooled mean and 95% interval, per year, 2041-2050)")
Cd <- W$Cd; setDT(Cd)
Cd[, ny := DY[period]]
for (v in c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold")) set(Cd, j = v, value = Cd[[v]] / Cd$ny)
cw_ <- Cd[, c(setNames(lapply(c(.025, .975), function(p) quantile(deaths_heat, p, names = FALSE)), c("hl", "hu")),
              setNames(lapply(c(.025, .975), function(p) quantile(deaths_cold, p, names = FALSE)), c("cl", "cu")),
              .(h = mean(deaths_heat), c = mean(deaths_cold), yh = mean(yll_heat), yc = mean(yll_cold))),
          by = .(slice, scenario, period)]
fwrite(cw_, file.path(OUT, "world_common24_cause.csv"))
x <- cw_[scenario == "ssp245" & period == "2041-2050"][order(-h)]
print(x[, .(cause = slice, heat = fm(h), heat_l95 = fm(hl), heat_u95 = fm(hu), cold = fm(c), cold_l95 = fm(cl), cold_u95 = fm(cu), yll_per_death_heat = round(yh / h, 1), yll_per_death_cold = round(yc / c, 1))])
tot <- tab[measure == "deaths_heat" & scenario == "ssp245" & period == "2041-2050", mean]
cat("causes sum to heat total:", round(sum(x$h) / tot, 6), "; share of 17 causes in heat by largest:\n"); print(round(x[, .(slice, s = h / sum(h))][1:5, s], 3))
cz <- pl[!slice %like% "_sex" & slice != "all" & measure %in% c("deaths_heat", "deaths_cold") & period == "2041-2050"]
cat("cause x country cells (ssp585, 2041-2050) where the 95% interval includes 0, of those with nonzero mean; and share with pooled mean below 0:\n")
print(cz[target == "ssp585" & t_mean != 0, .(n = .N, incl0 = round(mean(t_l95 < 0 & t_u95 > 0), 3), mean_neg = round(mean(t_mean < 0), 3)), by = measure])
print(cz[target == "ssp585" & t_mean != 0, .(incl0 = round(mean(t_l95 < 0 & t_u95 > 0), 2), mean_neg = round(mean(t_mean < 0), 2), any_mean_abs = round(median(abs(t_mean)), 1)), by = .(measure, slice)][order(measure, -incl0)])
cat("World x cause: interval includes 0? ssp585 2041-2050\n")
print(cw_[scenario == "ssp585" & period == "2041-2050", .(slice, heat_incl0 = hl < 0 & hu > 0, cold_incl0 = cl < 0 & cu > 0)])

sec("6. Age and sex, World, common 24, per year")
Ad <- W$Ad; setDT(Ad); Ad[, ny := DY[period]]
for (v in c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold")) set(Ad, j = v, value = Ad[[v]] / Ad$ny)
aw <- Ad[, .(h = mean(deaths_heat), hl = quantile(deaths_heat, .025, names = FALSE), hu = quantile(deaths_heat, .975, names = FALSE),
             c = mean(deaths_cold), cl = quantile(deaths_cold, .025, names = FALSE), cu = quantile(deaths_cold, .975, names = FALSE),
             yh = mean(yll_heat), yc = mean(yll_cold)), by = .(slice, scenario, period)]
fwrite(aw, file.path(OUT, "world_common24_age.csv"))
th <- tab[measure == "deaths_heat" & scenario == "ssp245", .(period, TH = mean)]; tc <- tab[measure == "deaths_cold" & scenario == "ssp245", .(period, TC = mean)]
tyh <- tab[measure == "yll_heat" & scenario == "ssp245", .(period, TYH = mean)]; tyc <- tab[measure == "yll_cold" & scenario == "ssp245", .(period, TYC = mean)]
aa <- merge(merge(merge(merge(aw[scenario == "ssp245"], th, by = "period"), tc, by = "period"), tyh, by = "period"), tyc, by = "period")
print(aa[period %in% c("2022-2030", "2041-2050"), .(period, slice, heat = fm(h), heat_l95 = fm(hl), heat_u95 = fm(hu), cold = fm(c), cold_l95 = fm(cl), cold_u95 = fm(cu),
          share_heat = round(h / TH, 3), share_cold = round(c / TC, 3), share_yll_heat = round(yh / TYH, 3), share_yll_cold = round(yc / TYC, 3),
          yll_per_heat = round(yh / h, 1), yll_per_cold = round(yc / c, 1))][order(period, slice)])
cat("\nCheck against the ssp245 bundle by_age_sex.csv (common models, year 2030 alone vs block mean 2026-2030 is not exact; decade 2022-2030 here vs mean of 2022 and 2030):\n")
bas <- fread(file.path(BUNDLE, "ssp245", "by_age_sex.csv"))
bas <- bas[sub("-r\\d+.*$", "", model) %in% sub("-r\\d+.*$", "", common) & model %in% common]
sl_map <- function(a) fifelse(a == 0, "under5", fifelse(a >= 15 & a <= 45, "a15_49", fifelse(a >= 65, "a65plus", NA_character_)))
bas[, age_group := sl_map(age_group_id)]
bs <- bas[year %in% c(2022, 2030), .(h = sum(deaths_heat), c = sum(deaths_cold)), by = .(age_group, sex_id, model, year)][, .(h = mean(h), c = mean(c)), by = .(age_group, sex_id, year)][, .(h = mean(h), c = mean(c)), by = .(age_group, sex_id)][!is.na(age_group)]
bs[, slice := paste0(age_group, "_sex", sex_id)]
cmp <- merge(bs, aw[scenario == "ssp245" & period == "2022-2030"], by = "slice", suffixes = c("_bundle", "_export"))
print(cmp[, .(slice, heat_bundle = fm(h_bundle), heat_export = fm(h_export), ratio_h = round(h_export / h_bundle, 3), cold_bundle = fm(c_bundle), cold_export = fm(c_export), ratio_c = round(c_export / c_bundle, 3))])

sec("7. Country-level: tails, widths, signs (common 24)")
t0 <- pl[slice == "all" & period == "2041-2050" & target == "ssp585"]
wd <- t0[measure == "deaths_nonopt", .(name, location_id, mean = round(t_mean, 1), l95 = round(t_l95, 1), u95 = round(t_u95, 1), rel_width = round((t_u95 - t_l95) / abs(t_mean), 2), min = round(t_min, 1), max = round(t_max, 1), tail_ratio = round((t_max - t_mean) / t_sd, 1), between_share = round(t_sd_models^2 / t_sd^2, 2))]
cat("non-optimal deaths, ssp585 2041-2050, widest relative 95% intervals (top 15) and largest absolute means (top 12):\n")
print(wd[order(-rel_width)][1:15]); print(wd[order(-mean)][1:12])
cat("rel_width quantiles:", round(quantile(wd$rel_width, c(.05, .25, .5, .75, .95)), 2), "\n")
print(wd[order(-tail_ratio)][1:12])
cat("Haiti 114, Honduras 129:\n"); print(wd[location_id %in% c(114, 129)])
print(t0[location_id %in% c(114, 129), .(name, measure, t_mean = round(t_mean, 1), t_l95 = round(t_l95, 1), t_u95 = round(t_u95, 1), t_max = round(t_max, 1))])
cat("\ncountry-level signs, per scenario-period: pooled means below zero; 95% interval including zero\n")
print(pl[slice == "all", .(n = .N, neg_mean = sum(t_mean < 0), incl0 = sum(t_l95 < 0 & t_u95 > 0), zero_mean = sum(t_mean == 0)), by = .(measure, target, period)][order(measure, target, period)][target == "ssp585"])
s <- dcast(pl[slice == "all" & measure %in% c("deaths_cold", "yll_cold", "deaths_heat", "yll_heat")], location_id + name + target + period ~ measure, value.var = "t_mean")
cat("\ndeaths and YLLs with opposite pooled-mean signs, any scenario-period:\n")
print(s[sign(deaths_cold) * sign(yll_cold) < 0 | sign(deaths_heat) * sign(yll_heat) < 0][, .(n = .N, ex = paste(unique(name)[1:min(8, uniqueN(name))], collapse = "; ")), by = .(target)])
print(s[sign(deaths_cold) * sign(yll_cold) < 0 | sign(deaths_heat) * sign(yll_heat) < 0][target == "ssp245" | period == "2041-2050"][1:15])
cat("\nYLL per death, country level, ssp585 2041-2050, non-optimal: range\n")
yy <- dcast(pl[slice == "all" & period == "2041-2050" & target == "ssp585" & measure %in% c("deaths_nonopt", "yll_nonopt")], name ~ measure, value.var = "t_mean")
yy[, r := yll_nonopt / deaths_nonopt]; print(yy[order(r)][c(1:6, (.N - 5):.N)]); print(quantile(yy$r, c(.05, .5, .95)))

sec("8. Spikes at country-model-year level, from national_by_year.csv draw means")
nat <- fread(file.path(SUM, "national_by_year.csv"), select = c("location_id", "model", "scenario", "year", "deaths_heat_attrib_mean", "deaths_cold_attrib_mean"))
nat <- nat[scenario != "ssp245"]
setorder(nat, location_id, model, scenario, year)
sp <- nat[, {
  mh <- median(deaths_heat_attrib_mean); mc <- median(deaths_cold_attrib_mean)
  .(year, x = deaths_heat_attrib_mean, med = mh, r = deaths_heat_attrib_mean / mh, xc = deaths_cold_attrib_mean, medc = mc, rc = deaths_cold_attrib_mean / mc)
}, by = .(location_id, model, scenario)]
sp <- merge(sp, names_loc, by = "location_id")
cat("largest heat year / run median, for locations with median heat > 500:\n")
print(sp[med > 500][order(-r)][1:12, .(name, model, scenario, year, x = round(x), med = round(med), r = round(r, 2))])
cat("largest cold year / run median, median cold > 500:\n")
print(sp[medc > 500][order(-rc)][1:8, .(name, model, scenario, year, xc = round(xc), medc = round(medc), rc = round(rc, 2))])
# identical runs across models or scenarios
dupchk <- nat[, .(s = sum(deaths_heat_attrib_mean * (year - 2021)) + 1e3 * sum(deaths_cold_attrib_mean * (year - 2021))), by = .(location_id, model, scenario)]
cat("exact duplicate model-scenario runs within a location, via CSV signature:", dupchk[, .N, by = .(location_id, s)][N > 1, .N], "\n")

sec("9. Model-set sensitivity, World: common 24 vs paired vs all available (scaled)")
sets <- ms$exp_models
ms_tab <- rbindlist(lapply(c("ssp126", "ssp370", "ssp585"), function(t) {
  rbindlist(list(
    LW[measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt") & model %in% common & scenario %in% c(t, "ssp245"), .(set = "common", tm = mean(value[scenario == t]), rm = mean(value[scenario == "ssp245"]), l = quantile(value[scenario == t], .025, names = FALSE), u = quantile(value[scenario == t], .975, names = FALSE)), by = .(measure, period)],
    LW[measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt") & model %in% sets[[t]] & scenario %in% c(t, "ssp245"), .(set = "paired", tm = mean(value[scenario == t]), rm = mean(value[scenario == "ssp245"]), l = quantile(value[scenario == t], .025, names = FALSE), u = quantile(value[scenario == t], .975, names = FALSE)), by = .(measure, period)],
    {x <- LW[measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt") & ((scenario == t & model %in% sets[[t]]) | scenario == "ssp245")]
     x[, .(set = "all", tm = mean(value[scenario == t]), rm = mean(value[scenario == "ssp245"]), l = quantile(value[scenario == t], .025, names = FALSE), u = quantile(value[scenario == t], .975, names = FALSE)), by = .(measure, period)]})
  )[, target := t]
}))
print(dcast(ms_tab[period == "2041-2050", .(measure, target, set, ratio = round(tm / rm, 3))], measure + target ~ set, value.var = "ratio"))
print(dcast(ms_tab[period == "2041-2050", .(measure, target, set, tm = fm(tm))], measure + target ~ set, value.var = "tm"))

sec("10. Rates per 100,000, IHME population forecast (mean of draws)")
pc <- file.path(OUT, "pop_by_loc.rds")
if (!file.exists(pc)) {
  pop <- fread(POPF)
  setnames(pop, c("Location", "Age Group", "Sex", "Year"), c("loc_name", "age_name", "sex_name", "year"))
  dc <- grep("^draw_", names(pop), value = TRUE)
  pop <- pop[sex_name %in% c("Male", "Female") & year %in% 2022:2050]
  pop[, pop := rowMeans(.SD), .SDcols = dc]
  pop <- pop[, .(loc_name, age_name, sex_name, year, pop)]
  hh <- rbind(names_loc[, .(location_id, loc_name = name)], data.table(location_id = c(205L, 8L, 155L), loc_name = c("Côte d'Ivoire", "Taiwan (Province of China)", "Turkey")))
  hh <- unique(hh, by = "loc_name")
  pop <- merge(pop, hh, by = "loc_name", all.x = TRUE)
  cat("population locations without GBD id:", paste(unique(pop[is.na(location_id), loc_name]), collapse = "; "), "\n")
  saveRDS(pop[!is.na(location_id)], pc)
}
pop <- readRDS(pc); setDT(pop)
cat("ages in population file:", paste(unique(pop$age_name), collapse = "; "), "\n")
cat("burden locations without population:", paste(setdiff(pl$location_id, pop$location_id), collapse = ","), "; population locations:", uniqueN(pop$location_id), "\n")
pop[, period := dec_of(year)]
pall <- pop[, .(N = sum(pop)), by = .(location_id, year, period)][, .(N = mean(N)), by = .(location_id, period)]
cat("World population, mean per year:\n"); print(pall[, .(N_bn = round(sum(N) / 1e9, 2)), by = period])
r <- merge(pl[slice == "all" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt")], pall, by = c("location_id", "period"))
r[, rate := 1e5 * t_mean / N]
cat("World rates per 100,000 per year, common 24, target scenarios; reference row uses r_mean:\n")
print(r[, .(rate = round(1e5 * sum(t_mean) / sum(N), 2), ref = round(1e5 * sum(r_mean) / sum(N), 2)), by = .(measure, target, period)][order(measure, target, period)])
cat("largest and smallest rates, ssp245 2041-2050 (reference columns):\n")
r2 <- r[target == "ssp585" & period == "2041-2050"][, ref_rate := 1e5 * r_mean / N]
for (m in c("deaths_heat", "deaths_cold")) { print(r2[measure == m][order(-ref_rate)][1:8, .(name, measure, ref_rate = round(ref_rate, 1), N = round(N), r_mean = round(r_mean))]); print(r2[measure == m][order(ref_rate)][1:4, .(name, measure, ref_rate = round(ref_rate, 2), N = round(N))]) }
