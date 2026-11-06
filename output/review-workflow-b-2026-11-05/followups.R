# followups.R: explains the items pooled_sanity.R left open.
# Usage: Rscript followups.R <review_out_dir> <summary_dir> <lifetable_dir> <population.csv>
suppressPackageStartupMessages(library(data.table))
setDTthreads(6)
a <- commandArgs(TRUE); OUT <- a[1]; SUM <- a[2]; LTDIR <- a[3]; POPF <- a[4]
options(width = 170, datatable.print.nrows = 300)
sec <- function(x) cat("\n\n================ ", x, " ================\n")
fm <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
pl <- readRDS(file.path(OUT, "loc_pooled_all.rds"))
names_loc <- unique(pl[, .(location_id, name)])
ms <- readRDS(file.path(OUT, "model_sets.rds")); common <- ms$common
VALS <- c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold")
ck <- rbindlist(lapply(list.files(file.path(OUT, "loc_checks"), full.names = TRUE), fread))

sec("A. Largest disagreements with the CSVs: is each within what 6-digit rounding can produce?")
cat("Worst locations per check (value is |a-b| / max(|b|, 1)):\n")
for (cc in c("nat_target_max_rel_mean", "nat_target_max_rel_lower", "bundle_ref_max_rel_mean", "bundle_ref_max_rel_lower", "cause_target_max_rel", "cause_ref_max_rel"))
  print(merge(ck[check == cc][order(-value)][1:3], names_loc, by = "location_id")[, .(check, name, location_id, value = signif(value, 3))])
worst <- unique(c(ck[check %in% c("cause_target_max_rel", "cause_ref_max_rel")][order(-value)][1:4, location_id]))
cau <- fread(file.path(SUM, "by_cause.csv"), select = c("location_id", "model", "scenario", "year", "acause", "deaths_heat_attrib", "deaths_cold_attrib", "yll_heat", "yll_cold"))[location_id %in% worst & scenario != "ssp245"]
BLOCK <- function(y) c("2022-2025", "2026-2030", "2031-2035", "2036-2040", "2041-2045", "2046-2050")[findInterval(y, c(2022, 2026, 2031, 2036, 2041, 2046))]
cau[, period := BLOCK(year)]
cb <- cau[, .(h = sum(deaths_heat_attrib), c = sum(deaths_cold_attrib), y = sum(yll_heat), z = sum(yll_cold)), by = .(location_id, model, scenario, period, acause)]
for (l in worst) {
  C <- setDT(readRDS(file.path(SUM, "draws_cause", paste0(l, ".rds"))))[scenario != "ssp245"]
  m <- C[, .(mh = mean(deaths_heat), mc = mean(deaths_cold), my = mean(yll_heat), mz = mean(yll_cold),
             ah = mean(abs(deaths_heat)), ac = mean(abs(deaths_cold)), ay = mean(abs(yll_heat)), az = mean(abs(yll_cold))), by = .(model, scenario, period, acause)]
  j <- merge(m, cb[location_id == l], by = c("model", "scenario", "period", "acause"))
  # Rounding bound: each value moves by at most 5e-6 of itself, so a mean moves at most 5e-6 of mean(|x|).
  # (A block value is already the sum of the exact annual values before rounding.)
  r <- j[, .(cell_ratio = pmax(abs(mh - h) / pmax(ah, 1e-12), abs(mc - c) / pmax(ac, 1e-12), abs(my - y) / pmax(ay, 1e-12), abs(mz - z) / pmax(az, 1e-12)))]
  cat(sprintf("loc %d: cells %d; worst |mean dev| / mean|x| = %.2e (bound 5e-6)\n", l, nrow(j), max(r$cell_ratio)))
}

sec("B. YLLs per death against the life table, by age slice (mean over draws, per run and block)")
am <- rbindlist(lapply(list.files(file.path(OUT, "loc_pooled"), "^agemeans_", full.names = TRUE), readRDS))
AGE_IDS <- list(under5 = 0, a15_49 = seq(15, 45, 5), a65plus = seq(65, 80, 5))
lts <- rbindlist(lapply(unique(am$location_id), function(l) {
  lt <- fread(file.path(LTDIR, paste0(l, "_lifetable.csv")))[year_id %in% 2022:2050]
  lt[, period := BLOCK(year_id)]
  rbindlist(lapply(names(AGE_IDS), function(ag) lt[age_group_id %in% AGE_IDS[[ag]], .(ex_lo = min(ex), ex_hi = max(ex)), by = .(period, sex_id)][, `:=`(age_group = ag, location_id = l)]))
}))
ar <- merge(am, lts, by = c("location_id", "period", "age_group", "sex_id"))
ar[, `:=`(rh = y / h, rc = z / c)]
print(ar[h > 10, .(n = .N, heat_out = sum(rh < ex_lo * 0.999 | rh > ex_hi * 1.001)), by = age_group])
print(ar[c > 10, .(n = .N, cold_out = sum(rc < ex_lo * 0.999 | rc > ex_hi * 1.001)), by = age_group])
cat("Out-of-range rows by whether the slice holds mixed signs is not observable at this level; list the worst heat rows:\n")
print(ar[h > 10 & (rh < ex_lo * 0.999 | rh > ex_hi * 1.001)][order(-abs(rh - (ex_lo + ex_hi) / 2))][1:5, .(location_id, model, scenario, period, age_group, sex_id, h = round(h, 1), y = round(y), rh = round(rh, 1), ex_lo, ex_hi)])
print(ar[c > 10 & age_group == "under5" & (rc < ex_lo * 0.999 | rc > ex_hi * 1.001)][1:5, .(location_id, model, scenario, period, age_group, sex_id, c = round(c, 1), z = round(z), rc = round(rc, 1), ex_lo, ex_hi)])
cat("Under-5 heat: male ratio below female ratio, share of location-run-blocks (h>10 both):\n")
u <- dcast(ar[age_group == "under5" & h > 10], location_id + model + scenario + period ~ sex_id, value.var = "rh")
print(u[!is.na(`1`) & !is.na(`2`), .(n = .N, male_below_female = mean(`1` < `2`))])

sec("C. Locations with low draw correlation across years (cold), and their cold burden")
cc <- dcast(ck[check %like% "^cor_draw"], location_id ~ check, value.var = "value")
cm <- pl[slice == "all" & measure == "deaths_cold" & target == "ssp585" & period == "2022-2030", .(location_id, name, cold = round(r_mean, 2))]
cc <- merge(cc, cm, by = "location_id")
print(cc[order(cor_draw_2030_2031_cold)][1:10])
print(cc[cold > 100, .(n = .N, min_adjacent = round(min(cor_draw_2030_2031_cold), 3), min_20yr = round(min(cor_draw_2030_2050_cold), 3))])

sec("D. Rates per 100,000 with single age rows only (the first sanity pass summed aggregate age rows too)")
pop <- readRDS(file.path(OUT, "pop_by_loc.rds")); setDT(pop)
atomic <- c("Under 5", paste(seq(5, 75, 5), "to", seq(9, 79, 5)), "80 to 84", "85 to 89", "90 to 94", "95 plus")
pop <- pop[age_name %in% atomic]
dec_of <- function(y) fifelse(y <= 2030, "2022-2030", fifelse(y <= 2040, "2031-2040", "2041-2050"))
pop[, period := dec_of(year)]
cat("age rows kept:", uniqueN(pop$age_name), "; rows per location-year-sex:", unique(pop[, .N, by = .(location_id, year, sex_name)]$N), "\n")
pall <- pop[, .(N = sum(pop)), by = .(location_id, year, period)][, .(N = mean(N)), by = .(location_id, period)]
print(pall[, .(world_bn = round(sum(N) / 1e9, 2)), by = period])
saveRDS(pall, file.path(OUT, "pop_all_ages_by_decade.rds"))
r <- merge(pl[slice == "all" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt")], pall, by = c("location_id", "period"))
print(r[, .(rate = round(1e5 * sum(t_mean) / sum(N), 2), ref = round(1e5 * sum(r_mean) / sum(N), 2)), by = .(measure, target, period)][order(measure, target, period)])
r2 <- r[target == "ssp585" & period == "2041-2050"][, ref_rate := 1e5 * r_mean / N]
for (m in c("deaths_heat", "deaths_cold")) { print(r2[measure == m][order(-ref_rate)][1:6, .(name, measure, ref_rate = round(ref_rate, 1), N = round(N), r_mean = round(r_mean))]); print(r2[measure == m][order(ref_rate)][1:3, .(name, measure, ref_rate = round(ref_rate, 2), N = round(N))]) }
cat("rate quantiles over locations, ssp245 2041-2050:\n"); print(r2[, as.list(round(quantile(ref_rate, c(0, .05, .5, .95, 1)), 1)), by = measure])

sec("E. Anatomy of the heavy tails (common 24, 2041-2050, ssp585): which cause carries them")
# tail measure at location level
all <- pl[slice == "all" & target == "ssp585" & period == "2041-2050" & measure == "deaths_nonopt"]
all[, `:=`(rel_width = (t_u95 - t_l95) / t_mean, tail = (t_max - t_mean) / t_sd)]
foc <- c(all[order(-rel_width)][1:6, location_id], all[order(-tail)][1:5, location_id], 13L, 129L)
foc <- unique(foc)
cz <- pl[target == "ssp585" & period == "2041-2050" & measure == "deaths_nonopt" & !slice %like% "_sex" & slice != "all" & location_id %in% foc]
for (l in foc) {
  x <- cz[location_id == l]; tt <- all[location_id == l]
  cat(sprintf("\n%s (%d): non-optimal mean %.1f, 95%% %.1f to %.1f, max %.1f\n", tt$name, l, tt$t_mean, tt$t_l95, tt$t_u95, tt$t_max))
  print(x[order(-(t_max))][1:3, .(cause = slice, mean = round(t_mean, 1), share_of_mean = round(t_mean / tt$t_mean, 3), l95 = round(t_l95, 1), u95 = round(t_u95, 1), max = round(t_max, 1), sd_share = round(t_sd^2 / tt$t_sd^2, 2))])
}
cat("\ninj_disaster share of mean non-optimal deaths by location, ssp245 reference, 2041-2050 (top 12):\n")
d <- pl[slice == "inj_disaster" & target == "ssp585" & period == "2041-2050" & measure == "deaths_nonopt", .(location_id, name, dis = r_mean, dis_max = r_max)]
t <- pl[slice == "all" & target == "ssp585" & period == "2041-2050" & measure == "deaths_nonopt", .(location_id, tot = r_mean, tot_u95 = r_u95)]
dd <- merge(d, t, by = "location_id")[, share := dis / tot]
print(dd[order(-share)][1:12, .(name, location_id, inj_disaster = round(dis, 1), total = round(tot, 1), share = round(share, 3), dis_max = round(dis_max))])

cat("\nPuerto Rico detail: top draws of yearly totals\n")
for (l in c(385L, 69L)) {
  D <- setDT(readRDS(file.path(SUM, "draws", paste0(l, ".rds"))))[model %in% common & scenario == "ssp585"]
  D[, nonopt := deaths_heat + deaths_cold]
  D[, year := as.integer(period)]
  dec <- D[year >= 2041, .(v = sum(nonopt) / 10), by = .(model, draw)]
  print(dec[order(-v)][1:6]); cat("per-draw quantiles, decade:", round(quantile(dec$v, c(.5, .975, .99, .999, 1))), "\n")
  print(dec[, .(mean_top = mean(sort(v, decreasing = TRUE)[1:5]), med = median(v)), by = draw][order(-mean_top)][1:5])
  print(D[year >= 2041, .(hm = mean(deaths_heat), cm = mean(deaths_cold), hmax = max(deaths_heat), cmax = max(deaths_cold))])
}

sec("F. Spikes after removing the trend: year / median of the 4 neighboring years, per run")
nat <- fread(file.path(SUM, "national_by_year.csv"), select = c("location_id", "model", "scenario", "year", "ihme_deaths_mean", "deaths_heat_attrib_mean", "deaths_cold_attrib_mean", "pipeline_deaths_heat_mean", "temp_popw"))
tg <- nat[scenario != "ssp245"]; setorder(tg, location_id, scenario, model, year)
nb <- function(x) { n <- length(x); vapply(seq_len(n), function(i) median(x[setdiff(max(1, i - 2):min(n, i + 2), i)]), 0) }
tg[, `:=`(hn = nb(deaths_heat_attrib_mean), cn = nb(deaths_cold_attrib_mean)), by = .(location_id, scenario, model)]
tg[, `:=`(rh = deaths_heat_attrib_mean / hn, rc = deaths_cold_attrib_mean / cn)]
tg <- merge(tg, names_loc, by = "location_id")
cat("heat, neighbors' median > 500: quantiles of ratio\n"); print(round(quantile(tg[hn > 500, rh], c(.5, .95, .99, .999, 1)), 2))
print(tg[hn > 500][order(-rh)][1:10, .(name, model, scenario, year, heat = round(deaths_heat_attrib_mean), nb = round(hn), r = round(rh, 2))])
cat("cold, neighbors' median > 500: quantiles of ratio\n"); print(round(quantile(tg[cn > 500, rc], c(.001, .5, .95, .99, .999, 1)), 2))
print(tg[cn > 500][order(-rc)][1:6, .(name, model, scenario, year, cold = round(deaths_cold_attrib_mean), nb = round(cn), r = round(rc, 2))])
cat("same year spiking in many models (would suggest an input problem, not weather): location-scenario-year with most models over 2x neighbors, hn>100\n")
print(tg[hn > 100 & rh > 2, .(n = .N), by = .(name, scenario, year)][order(-n)][1:8])
print(tg[hn > 100, .(n_runs_years = .N, over2 = mean(rh > 2))])

cat("\nWhich countries drive the largest World jump (miroc-es2l, ssp370, 2031 to 2032)?\n")
x <- nat[model == "miroc-es2l-r1i1p1f2" & scenario == "ssp370" & year %in% 2031:2032, .(location_id, year, h = deaths_heat_attrib_mean)]
x <- dcast(x, location_id ~ year, value.var = "h"); x[, d := `2032` - `2031`]; x <- merge(x, names_loc, by = "location_id")
cat("World change:", round(sum(x$d)), "\n"); print(x[order(-d)][1:6, .(name, y2031 = round(`2031`), y2032 = round(`2032`), d = round(d))])
cat("World year-on-year change in heat, per run: quantiles, by scenario\n")
w <- nat[scenario != "ssp245", .(h = sum(deaths_heat_attrib_mean)), by = .(scenario, model, year)]; setorder(w, scenario, model, year)
w[, j := h / shift(h) - 1, by = .(scenario, model)]
print(w[!is.na(j), as.list(round(quantile(j, c(.01, .05, .5, .95, .99)), 3)), by = scenario])

cat("\nUnited Arab Emirates cold by scenario (mean of the 24 models), and 17-cause deaths\n")
u <- nat[location_id == 160 | location_id == 150][0]
uae <- names_loc[name == "United Arab Emirates", location_id]
ux <- tg[location_id == uae & model %in% common, .(cold = mean(deaths_cold_attrib_mean), heat = mean(deaths_heat_attrib_mean), ihme = mean(ihme_deaths_mean)), by = .(scenario, year)][year %in% seq(2022, 2050, 4)]
print(dcast(ux[, .(scenario, year, cold = round(cold), heat = round(heat), ihme = round(ihme))], year ~ scenario, value.var = c("cold", "heat", "ihme")))
cat("single-run example noresm2-lm ssp126, 2040-2050:\n")
print(tg[location_id == uae & model == "noresm2-lm-r1i1p1f1" & scenario == "ssp126" & year >= 2040, .(year, cold = round(deaths_cold_attrib_mean), heat = round(deaths_heat_attrib_mean), ihme = round(ihme_deaths_mean), temp_popw)])

sec("G. Why does cold rise? World 17-cause deaths (IHME forecast, before scaling) vs attributable deaths")
tw <- tg[model %in% common & scenario == "ssp370"]
tw[, period := dec_of(year)]
g <- tw[, .(ih = sum(ihme_deaths_mean), h = sum(deaths_heat_attrib_mean), c = sum(deaths_cold_attrib_mean)), by = .(model, period)][
  , .(ih = mean(ih), h = mean(h), c = mean(c)), by = period]
g[, ny := c(9, 10, 10)[match(period, c("2022-2030", "2031-2040", "2041-2050"))]]
print(g[, .(period, ihme_17cause_deaths_per_year = fm(ih / ny), heat_share = round(h / ih, 4), cold_share = round(c / ih, 4))])

sec("H. How many countries have target-minus-ssp245 intervals excluding zero, 2041-2050?")
print(pl[slice == "all" & period == "2041-2050" & measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt"),
         .(above_95 = sum(d_l95 > 0), below_95 = sum(d_u95 < 0), above_66 = sum(d_l66 > 0), below_66 = sum(d_u66 < 0)), by = .(measure, target)][order(measure, target)])
