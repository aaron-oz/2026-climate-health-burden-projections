# Independent checks on decompose_heat.R output.
#   1. The starting and ending heat deaths match national_by_year.csv.
#   2. The Shapley parts match a brute-force average over all 24 orders of
#      switching factors, recomputed here from by_age_sex.csv and the
#      population file for a sample of locations and models.
#   3. Population totals and the share aged 65+ look sensible.
# Usage: Rscript check_decomposition.R <scenario bundles dir> <population.csv> <GBD hierarchy xlsx> <decomposition out dir> <from> <to>
suppressMessages({ library(data.table); library(readxl) })
a <- commandArgs(TRUE); SC <- a[1]; POPF <- a[2]; HIER <- a[3]; DD <- a[4]; y1 <- as.integer(a[5]); y2 <- as.integer(a[6])
ok <- function(lbl, cond) { cat(sprintf("  %-70s %s\n", lbl, if (isTRUE(cond)) "PASS" else "FAIL")); invisible(cond) }
lm <- fread(file.path(DD, "decomposition_location_model.csv"))

# 1. totals against national_by_year.csv
for (s in unique(lm$scenario)) {
  n <- fread(file.path(SC, s, s, "national_by_year.csv"), select = c("location_id", "model", "year", "deaths_heat_mean"))
  n <- dcast(n[year %in% c(y1, y2) & model %in% unique(lm$model)], location_id + model ~ year, value.var = "deaths_heat_mean")
  setnames(n, as.character(c(y1, y2)), c("n1", "n2"))
  m <- merge(lm[scenario == s], n, by = c("location_id", "model"))
  dev <- max(abs(m$H1 - m$n1) / pmax(m$n1, 1e-9), abs(m$H2 - m$n2) / pmax(m$n2, 1e-9))
  ok(sprintf("%s: heat deaths in %d and %d match national_by_year (worst rel. dev %.1e)", s, y1, y2, dev),
     nrow(m) == nrow(lm[scenario == s]) && dev < 1e-9)
  ok(sprintf("%s: four parts sum to the change", s), max(abs(m$eff_P + m$eff_s + m$eff_r + m$eff_f - (m$H2 - m$H1))) < 1e-6)
}

# 2. brute force over all 24 orders for a sample
b <- fread(file.path(SC, "ssp585", "ssp585", "by_age_sex.csv"),
           select = c("location_id", "model", "year", "age_group_id", "sex_id", "deaths", "deaths_heat"))
pop <- fread(POPF)
setnames(pop, c("Location", "Age Group", "Sex", "Year"), c("loc_name", "age_name", "sex_name", "year"))
dcols <- grep("^draw_", names(pop), value = TRUE)
pop <- pop[sex_name %in% c("Male", "Female") & year %in% c(y1, y2)]
pop[, pop := rowMeans(.SD), .SDcols = dcols]
age_map <- data.table(age_name = c("Under 5", paste(seq(5, 75, 5), "to", seq(9, 79, 5)), "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                      age_group_id = c(seq(0, 75, 5), 80, 80, 80, 80))
pop <- merge(pop, age_map, by = "age_name")[, .(N = sum(pop)), by = .(loc_name, year, age_group_id, sex_id = fifelse(sex_name == "Male", 1L, 2L))]
h <- as.data.table(read_excel(HIER))[Level == 3, .(location_id = as.integer(`Location ID`), loc_name = `Location Name`)]
set.seed(1)
samp <- unique(lm[scenario == "ssp585", .(location_id, model)])[sample(.N, 6)]
samp <- rbind(samp, data.table(location_id = c(163L, 6L), model = lm$model[1]))   # always India, China
perms <- function(v) if (length(v) == 1) list(v) else do.call(c, lapply(v, function(x) lapply(perms(setdiff(v, x)), function(p) c(x, p))))
P24 <- perms(c("P", "s", "r", "f"))
worst <- 0
for (i in seq_len(nrow(samp))) {
  L <- samp$location_id[i]; M <- samp$model[i]
  nm <- unique(h[location_id == L, loc_name])
  pp <- pop[loc_name %in% nm]
  if (nrow(pp) == 0) { cat("  (skip", L, ": name not matched here)\n"); next }
  x <- merge(b[location_id == L & model == M & year %in% c(y1, y2)], pp[, .(year, age_group_id, sex_id, N)],
             by = c("year", "age_group_id", "sex_id"))
  x[, Pt := sum(N), by = year]
  x[, `:=`(s = N / Pt, r = fifelse(N > 0, deaths / N, 0), f = fifelse(deaths > 0, deaths_heat / deaths, 0))]
  g <- function(yr, col) x[year == yr][order(age_group_id, sex_id)][[col]]
  fac <- list(P = c("Pt"), s = "s", r = "r", f = "f")
  H <- function(state) sum(g(state[["P"]], "Pt") * g(state[["s"]], "s") * g(state[["r"]], "r") * g(state[["f"]], "f"))
  eff <- c(P = 0, s = 0, r = 0, f = 0)
  for (o in P24) {
    st <- list(P = y1, s = y1, r = y1, f = y1)
    for (k in o) { before <- H(st); st[[k]] <- y2; eff[k] <- eff[k] + (H(st) - before) / 24 }
  }
  got <- lm[scenario == "ssp585" & location_id == L & model == M]
  d <- max(abs(eff - c(got$eff_P, got$eff_s, got$eff_r, got$eff_f)) / max(abs(got$H2 - got$H1), 1))
  worst <- max(worst, d)
}
ok(sprintf("Shapley parts match brute force over 24 orders, %d samples (worst %.1e of the change)", nrow(samp), worst), worst < 1e-9)

# 3. population sanity
wp <- pop[, .(total_bn = sum(N) / 1e9, share65 = sum(N[age_group_id >= 65]) / sum(N)), by = year]
print(wp, digits = 3)
ok("world population 7.5-8.5 bn in 2022 and above that in 2050", wp[year == y1, total_bn > 7.5 & total_bn < 8.5] && wp[year == y2, total_bn] > wp[year == y1, total_bn])
ok("share aged 65+ rises", wp[year == y2, share65] > wp[year == y1, share65])
