# followups2.R: second round. (a) quantile disagreements against the rounding bound,
# (b) single-draw outliers in the yearly totals, (c) non-optimal deaths with and
# without inj_disaster, side by side, (d) growth in IHME 17-cause deaths per country.
# Usage: Rscript followups2.R <review_out_dir> <summary_dir>
suppressPackageStartupMessages(library(data.table))
setDTthreads(1)
a <- commandArgs(TRUE); OUT <- a[1]; SUM <- a[2]
options(width = 170, datatable.print.nrows = 300)
sec <- function(x) cat("\n\n================ ", x, " ================\n")
pl <- readRDS(file.path(OUT, "loc_pooled_all.rds")); names_loc <- unique(pl[, .(location_id, name)])
ms <- readRDS(file.path(OUT, "model_sets.rds")); common <- ms$common
files <- as.integer(sub("\\.rds", "", list.files(file.path(SUM, "draws"), "\\.rds$")))

sec("A. Lower-quantile disagreements with national_by_year.csv, against the rounding bound")
nat <- fread(file.path(SUM, "national_by_year.csv"), select = c("location_id", "model", "scenario", "year", "deaths_heat_attrib_lower", "deaths_cold_attrib_lower", "yll_heat_lower", "yll_cold_lower", "ihme_deaths_mean"))
for (l in c(161L, 11L, 18L, 201L)) {
  D <- setDT(readRDS(file.path(SUM, "draws", paste0(l, ".rds"))))[scenario != "ssp245"]
  D[, year := as.integer(period)]
  N <- nat[location_id == l & scenario != "ssp245"]
  out <- D[, {
    # type-7 quantile at .025 of 500 values lies between order statistics 13 and 14
    f <- function(x) { s <- sort(x); list(q = s[13] + 0.475 * (s[14] - s[13]), b = 5e-6 * max(abs(s[13]), abs(s[14]))) }
    h <- f(deaths_heat); cc <- f(deaths_cold); y <- f(yll_heat); z <- f(yll_cold)
    .(qh = h$q, bh = h$b, qc = cc$q, bc = cc$b, qy = y$q, by_ = y$b, qz = z$q, bz = z$b)
  }, by = .(model, scenario, year)]
  j <- merge(out, N, by = c("model", "scenario", "year"))
  r <- j[, pmax(abs(qh - deaths_heat_attrib_lower) / pmax(bh, 1e-12), abs(qc - deaths_cold_attrib_lower) / pmax(bc, 1e-12),
                abs(qy - yll_heat_lower) / pmax(by_, 1e-12), abs(qz - yll_cold_lower) / pmax(bz, 1e-12))]
  cat(sprintf("loc %d: rows %d; worst |dev| / rounding bound = %.3f (<= 1 means rounding explains it)\n", l, nrow(j), max(r)))
}

sec("B. Single-draw outliers in the yearly totals, and (C) with and without inj_disaster")
one <- function(l) {
  D <- setDT(readRDS(file.path(SUM, "draws", paste0(l, ".rds"))))[model %in% common]
  D[, nonopt := deaths_heat + deaths_cold]
  # pooled over models and years, one run-year-draw per row: how far does the top value sit from the bulk?
  b <- D[, .(q50 = median(nonopt), q99 = quantile(nonopt, .99, names = FALSE), q999 = quantile(nonopt, .999, names = FALSE), mx = max(nonopt),
             iqr = IQR(nonopt), mean = mean(nonopt))]
  top <- D[which.max(nonopt), .(top_model = model, top_scen = scenario, top_year = period, top_draw = draw)]
  # how many distinct draws hold the top 0.1% of annual values?
  th <- b$q999; nd <- D[nonopt >= th, uniqueN(draw)]
  ty <- D[nonopt >= th, .N, by = period][order(-N)][1]
  # draw-by-year mean across models, per draw-year excess
  C <- setDT(readRDS(file.path(SUM, "draws_cause", paste0(l, ".rds"))))[model %in% common & scenario %in% c("ssp245", "ssp585") & period %in% c("2041-2045", "2046-2050")]
  C[, nonopt := deaths_heat + deaths_cold]
  all_ <- C[, .(v = sum(nonopt) / 10), by = .(model, scenario, draw)]
  nod <- C[acause != "inj_disaster", .(v = sum(nonopt) / 10), by = .(model, scenario, draw)]
  s <- function(x, tag) x[, setNames(list(mean(v), quantile(v, .025, names = FALSE), quantile(v, .975, names = FALSE), max(v)), paste0(tag, c("_mean", "_l95", "_u95", "_max"))), by = scenario]
  sens <- merge(s(all_, "all"), s(nod, "no_dis"), by = "scenario")
  list(b = cbind(data.table(location_id = l), b, top, data.table(n_draws_top = nd, busy_year_n = ty$N, busy_year = ty$period)),
       sens = cbind(data.table(location_id = l), sens))
}
res <- parallel::mclapply(files, one, mc.cores = 6, mc.preschedule = FALSE)
b <- rbindlist(lapply(res, `[[`, "b")); sens <- rbindlist(lapply(res, `[[`, "sens"))
b <- merge(b, names_loc, by = "location_id"); sens <- merge(sens, names_loc, by = "location_id")
fwrite(sens, file.path(OUT, "with_without_inj_disaster.csv"))
b[, `:=`(rmax = (mx - q50) / iqr, r999 = (q999 - q50) / iqr)]
cat("yearly non-optimal deaths, common 24, all scenarios and years; outlier size = (max - median) / IQR\n")
print(b[order(-rmax)][1:15, .(name, location_id, mean = round(mean, 1), q50 = round(q50, 1), q999 = round(q999, 1), max = round(mx, 1), rmax = round(rmax, 1), top_scen, top_year, top_draw, n_draws_top)])
cat("quantiles of rmax:", round(quantile(b$rmax, c(.05, .5, .95, 1)), 1), "\n")
cat("Countries with fewer than 10 distinct draws holding the top 0.1% of values (top 0.1% = 696 rows of a 696,000-row pool):\n")
print(b[n_draws_top < 10][order(n_draws_top), .(name, n_draws_top, rmax = round(rmax, 1))])
cat("\nWith and without inj_disaster, reference ssp245, 2041-2050, non-optimal deaths per year: countries where the upper 95% bound moves most\n")
s2 <- sens[scenario == "ssp245"]
s2[, `:=`(u_ratio = all_u95 / no_dis_u95, m_ratio = all_mean / no_dis_mean)]
print(s2[order(-u_ratio)][1:14, .(name, all_mean = round(all_mean, 1), all_l95 = round(all_l95, 1), all_u95 = round(all_u95, 1), all_max = round(all_max), no_dis_mean = round(no_dis_mean, 1), no_dis_l95 = round(no_dis_l95, 1), no_dis_u95 = round(no_dis_u95, 1), no_dis_max = round(no_dis_max), m_ratio = round(m_ratio, 3), u_ratio = round(u_ratio, 3))])
cat("countries with upper bound moved by more than 2%:", s2[u_ratio > 1.02, .N], "; by more than 10%:", s2[u_ratio > 1.10, .N], "\n")

sec("D. Growth in IHME 17-cause deaths, 2022 to 2050, per country (same in every model and scenario)")
n1 <- nat[scenario == "ssp585" & year %in% c(2022, 2050), .(ih = mean(ihme_deaths_mean)), by = .(location_id, year)]
g <- dcast(n1, location_id ~ year, value.var = "ih"); g[, r := `2050` / `2022`]; g <- merge(g, names_loc, by = "location_id")
pop <- readRDS(file.path(OUT, "pop_by_loc.rds")); setDT(pop)
atomic <- c("Under 5", paste(seq(5, 75, 5), "to", seq(9, 79, 5)), "80 to 84", "85 to 89", "90 to 94", "95 plus")
pp <- pop[age_name %in% atomic & year %in% c(2022, 2050), .(N = sum(pop)), by = .(location_id, year)]
pp65 <- pop[age_name %in% c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 plus") & year %in% c(2022, 2050), .(N65 = sum(pop)), by = .(location_id, year)]
gp <- merge(dcast(pp, location_id ~ year, value.var = "N"), dcast(pp65, location_id ~ year, value.var = "N65"), by = "location_id", suffixes = c("_all", "_65"))
g <- merge(g, gp, by = "location_id")
cat("World: ", round(sum(g$`2050`) / sum(g$`2022`), 2), "; quantiles:", round(quantile(g$r, c(.05, .5, .95)), 2), "\n")
print(g[order(-r)][1:12, .(name, deaths_2022 = round(`2022`), deaths_2050 = round(`2050`), ratio = round(r, 2), pop_2022 = round(`2022_all`), pop_2050 = round(`2050_all`), pop65_2022 = round(`2022_65`), pop65_2050 = round(`2050_65`), rate_per_100k_65plus_ratio = round((`2050` / `2050_65`) / (`2022` / `2022_65`), 2))])

sec("E. Lithuania and Myanmar: why is the cold draw correlation across years low?")
for (l in c(60L, 15L, 131L)) {
  D <- setDT(readRDS(file.path(SUM, "draws", paste0(l, ".rds"))))[model == common[1] & scenario == "ssp585"]
  w <- dcast(D[period %in% c("2030", "2031")], draw ~ period, value.var = "deaths_cold")
  cat(names_loc[location_id == l, name], ": cor all draws", round(cor(w$`2030`, w$`2031`), 3), "; Spearman", round(cor(w$`2030`, w$`2031`, method = "spearman"), 3), "\n")
  print(w[order(-abs(`2030` - median(`2030`)))][1:3]); print(w[, .(med30 = median(`2030`), med31 = median(`2031`), max30 = max(`2030`), max31 = max(`2031`), min30 = min(`2030`), min31 = min(`2031`))])
}
