# make_report_tables.R: LaTeX table fragments for the report, and a text digest of the
# numbers quoted in the prose (numbers_for_prose.txt), all from the pooled tables.
# Usage: Rscript make_report_tables.R <pool_dir> <tex out dir> <income_groups_by_location.csv> <repo tables dir>
suppressPackageStartupMessages(library(data.table))
setDTthreads(4)
a <- commandArgs(TRUE); POOL <- a[1]; OUT <- a[2]; INCF <- a[3]; TAB <- a[4]
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
x <- readRDS(file.path(POOL, "tidy_common.rds")); tot <- x$tot; cau <- x$cau; age <- x$age
inc <- fread(INCF, encoding = "UTF-8")
SC <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")
PER <- c("2022-2030", "2031-2040", "2041-2050")
pr <- function(p) sub("-", "--", p, fixed = TRUE)
n0 <- function(v) { s <- formatC(abs(v), format = "f", digits = 0, big.mark = ","); ifelse(round(v) < 0, paste0("$-$", s), s) }
n1 <- function(v) { s <- formatC(abs(v), format = "f", digits = 1, big.mark = ","); ifelse(round(v, 1) < 0, paste0("$-$", s), s) }
nn <- function(v) ifelse(abs(v) >= 10, n0(v), n1(v))
ci <- function(l, u, f = n0) paste0("(", f(l), " to ", f(u), ")")
tex <- function(s) gsub("&", "\\&", s, fixed = TRUE)
wr <- function(lines, f) writeLines(lines, file.path(OUT, f), useBytes = FALSE)
tabular <- function(spec, head, rows, foot = NULL) c(paste0("\\begin{tabular}{", spec, "}"), "\\toprule", head, "\\midrule", rows, "\\bottomrule", "\\end{tabular}")

# --- World by scenario and decade ---
W <- tot[unit == "World"]
g <- function(u, s, p, m) W[unit == u & scenario == s & period == p & measure == m]
rows <- unlist(lapply(PER, function(p) c(lapply(names(SC), function(s) {
  cell <- function(m) { r <- W[scenario == s & period == p & measure == m]; paste0(n0(r$mean), " ", ci(r$lower95, r$upper95)) }
  paste0(if (s == "ssp126") pr(p) else "", " & ", SC[s], " & ", cell("deaths_heat"), " & ", cell("deaths_cold"), " & ", cell("deaths_nonopt"), " \\\\")
}), if (p != PER[3]) "\\addlinespace")))
wr(tabular("llccc", "Period & Scenario & Heat deaths & Cold deaths & Heat + cold \\\\", rows), "tab_world.tex")

# --- World, the three interval levels ---
rows <- unlist(lapply(c("ssp245", "ssp585"), function(s) lapply(c("deaths_heat", "deaths_cold", "deaths_nonopt"), function(m) {
  r <- W[scenario == s & period == "2041-2050" & measure == m]
  paste0(SC[s], " & ", c(deaths_heat = "Heat", deaths_cold = "Cold", deaths_nonopt = "Heat + cold")[m], " & ", n0(r$mean), " & ", n0(r$lower95), " to ", n0(r$upper95), " & ", n0(r$lower80), " to ", n0(r$upper80), " & ", n0(r$lower66), " to ", n0(r$upper66), " & ", n0(r$model_min), " to ", n0(r$model_max), " \\\\")
})))
wr(tabular("llccccc", "Scenario & Measure & Mean & 95\\% & 80\\% & 66\\% & Range of model means \\\\", rows), "tab_levels.tex")

# --- Differences ---
rows <- unlist(lapply(c("ssp126", "ssp370", "ssp585"), function(s) c(lapply(PER, function(p) {
  cell <- function(m) { r <- W[scenario == s & period == p & measure == m]; paste0(sprintf("%.3f", r$ratio), " & ", n0(r$diff_mean), " ", ci(r$diff_lower95, r$diff_upper95)) }
  paste0(if (p == PER[1]) SC[s] else "", " & ", pr(p), " & ", cell("deaths_heat"), " & ", cell("deaths_cold"), " & ", cell("deaths_nonopt"), " \\\\")
}), if (s != "ssp585") "\\addlinespace")))
wr(tabular("llcccccc", "Scenario & Period & \\multicolumn{2}{c}{Heat} & \\multicolumn{2}{c}{Cold} & \\multicolumn{2}{c}{Heat + cold} \\\\ & & Ratio & Difference & Ratio & Difference & Ratio & Difference \\\\", rows), "tab_diff.tex")

# --- Income groups ---
G <- c("World", "High income", "Low and middle income", "Upper middle income", "Lower middle income", "Low income")
GT <- tot[unit %in% G]
nloc <- rbind(inc[, .(unit = income)], inc[income %in% G[4:6], .(unit = "Low and middle income")], inc[, .(unit = "World")])[, .N, by = unit]
rows <- unlist(lapply(G, function(u) {
  nc <- nloc[unit == u, N]
  cell <- function(p, m) { r <- GT[unit == u & scenario == "ssp245" & period == p & measure == m]; paste0(n0(r$mean), " & ", sprintf("%.1f", r$rate_mean), " ", ci(r$rate_lower95, r$rate_upper95, function(v) sprintf("%.1f", v))) }
  ra <- function(m) sprintf("%.3f", GT[unit == u & scenario == "ssp585" & period == "2041-2050" & measure == m, ratio])
  paste0(u, " (", nc, ") & ", cell("2041-2050", "deaths_heat"), " & ", cell("2041-2050", "deaths_cold"), " & ", ra("deaths_heat"), " & ", ra("deaths_cold"), " \\\\")
}))
wr(tabular("lcccccc", "Group (countries) & \\multicolumn{2}{c}{Heat, SSP2-4.5} & \\multicolumn{2}{c}{Cold, SSP2-4.5} & \\multicolumn{2}{c}{SSP5-8.5 / SSP2-4.5} \\\\ & Deaths & Rate (95\\%) & Deaths & Rate (95\\%) & Heat & Cold \\\\", rows), "tab_income.tex")

# --- Causes ---
CN <- c(cvd_ihd = "Ischemic heart disease", cvd_stroke = "Stroke", resp_copd = "COPD", lri = "Lower respiratory infections", diabetes = "Diabetes",
        ckd = "Chronic kidney disease", cvd_htn = "Hypertensive heart disease", cvd_cmp = "Cardiomyopathy and myocarditis", inj_trans_road = "Road injuries",
        inj_suicide = "Self-harm", inj_homicide = "Interpersonal violence", inj_drowning = "Drowning", inj_mech = "Exposure to mechanical forces", inj_disaster = "Exposure to forces of nature",
        inj_animal = "Animal contact", inj_trans_other = "Other transport injuries", inj_othunintent = "Other unintentional injuries")
c0 <- cau[scenario == "ssp245" & period == "2041-2050" & measure %in% c("deaths_heat", "deaths_cold")]
rows <- vapply(names(CN), function(k) {
  w <- function(m) c0[unit == "World" & slice == k & measure == m]
  v <- function(u, m) n0(c0[unit == u & slice == k & measure == m, mean])
  paste0(CN[k], " & ", n0(w("deaths_heat")$mean), " ", ci(w("deaths_heat")$lower95, w("deaths_heat")$upper95), " & ", v("High income", "deaths_heat"), " & ", v("Low and middle income", "deaths_heat"),
         " & ", n0(w("deaths_cold")$mean), " ", ci(w("deaths_cold")$lower95, w("deaths_cold")$upper95), " & ", v("High income", "deaths_cold"), " & ", v("Low and middle income", "deaths_cold"), " \\\\")
}, "")
wr(tabular("lcrrcrr", "& \\multicolumn{3}{c}{Heat} & \\multicolumn{3}{c}{Cold} \\\\ Cause & World (95\\%) & High & LMIC & World (95\\%) & High & LMIC \\\\", rows), "tab_cause.tex")

# --- Age and sex ---
SL <- c(under5_sex1 = "Under 5, male", under5_sex2 = "Under 5, female", a15_49_sex1 = "15--49, male", a15_49_sex2 = "15--49, female", a65plus_sex1 = "65+, male", a65plus_sex2 = "65+, female")
a0 <- age[unit == "World" & scenario == "ssp245" & measure %in% c("deaths_heat", "deaths_cold", "yll_heat", "yll_cold")]
rows <- unlist(lapply(PER[c(1, 3)], function(p) c(lapply(names(SL), function(k) {
  v <- function(m) a0[period == p & slice == k & measure == m]
  rt <- function(m) sub("^-", "$-$", sprintf("%.1f", v(m)$rate_mean))
  paste0(if (k == names(SL)[1]) pr(p) else "", " & ", SL[k], " & ", n0(v("deaths_heat")$mean), " ", ci(v("deaths_heat")$lower95, v("deaths_heat")$upper95), " & ", rt("deaths_heat"),
         " & ", n0(v("deaths_cold")$mean), " ", ci(v("deaths_cold")$lower95, v("deaths_cold")$upper95), " & ", rt("deaths_cold"), " \\\\")
}), if (p == PER[1]) "\\addlinespace")))
wr(tabular("llcrcr", "Period & Group & Heat deaths & Rate & Cold deaths & Rate \\\\", rows), "tab_age.tex")

# --- Sensitivity ---
s <- fread(file.path(TAB, "model_set_sensitivity.csv"))[unit == "World" & period == "2041-2050"]
rows <- unlist(lapply(c("deaths_heat", "deaths_cold", "deaths_nonopt"), function(m) lapply(c("ssp126", "ssp370", "ssp585"), function(t) {
  r <- function(st) s[measure == m & target == t & set == st]
  paste0(if (t == "ssp126") c(deaths_heat = "Heat", deaths_cold = "Cold", deaths_nonopt = "Heat + cold")[m] else "", " & ", SC[t], " & ",
         paste(vapply(c("common", "paired", "all"), function(st) paste0(n0(r(st)$target_mean), " & ", sprintf("%.3f", r(st)$ratio)), ""), collapse = " & "), " \\\\")
})))
wr(tabular("llcccccc", "& & \\multicolumn{2}{c}{Common 24} & \\multicolumn{2}{c}{Paired} & \\multicolumn{2}{c}{All available} \\\\ Measure & Scenario & Deaths & Ratio & Deaths & Ratio & Deaths & Ratio \\\\", rows), "tab_sensitivity.tex")

# --- Asterisk countries ---
wc <- fread(file.path(POOL, "extras", "with_without_inj_disaster_country.csv"))
rows <- unlist(lapply(list(c(114L, "Haiti"), c(129L, "Honduras")), function(l) lapply(c("2022-2030", "2041-2050"), function(p) {
  r <- wc[location_id == as.integer(l[1]) & scenario == "ssp245" & period == p & m == "n"]
  A <- r[variant == "all_causes"]; B <- r[variant == "without_inj_disaster"]
  paste0(if (p == "2022-2030") paste0(l[2], "$^{*}$") else "", " & ", pr(p), " & ", n0(A$mean), " ", ci(A$lower95, A$upper95), " & ", n0(B$mean), " ", ci(B$lower95, B$upper95), " & ", sprintf("%.2f", A$mean / B$mean), " \\\\")
})))
wr(tabular("llccc", "Country & Period & All 17 causes (95\\%) & Without forces of nature (95\\%) & Ratio of means \\\\", rows), "tab_asterisk.tex")

# --- Crossover ---
cr <- merge(fread(file.path(POOL, "extras", "crossover_country.csv")), inc[, .(location_id, name, income)], by = "location_id")
cr[, cat := fifelse(med == -Inf, "Heat above cold already in 2022", fifelse(med == Inf, "Heat stays below cold through 2050", "Median crossing within 2022--2050"))]
ct <- dcast(cr[, .N, by = .(scenario, cat)], cat ~ scenario, value.var = "N", fill = 0)
ct <- ct[match(c("Heat above cold already in 2022", "Median crossing within 2022--2050", "Heat stays below cold through 2050"), cat)]
wr(tabular("lcccc", "Where the median falls & SSP1-2.6 & SSP2-4.5 & SSP3-7.0 & SSP5-8.5 \\\\", ct[, paste0(cat, " & ", ssp126, " & ", ssp245, " & ", ssp370, " & ", ssp585, " \\\\")]), "tab_crossover_counts.tex")
yrf <- function(v) ifelse(v == -Inf, "before 2022", ifelse(v == Inf, "after 2050", sprintf("%d", as.integer(round(v)))))
cw <- cr[scenario == "ssp245" & is.finite(med)][order(med)]
wr(tabular("lcccc", "Country & Median year & Before 2022 & Within 2022--2050 & After 2050 \\\\",
           cw[, paste0(tex(name), " & ", yrf(med), " & ", sprintf("%.0f\\%%", 100 * share_before_2022), " & ", sprintf("%.0f\\%%", 100 * share_within), " & ", sprintf("%.0f\\%%", 100 * share_after_2050), " \\\\")]), "tab_crossover_list.tex")
fwrite(cr[, .(location_id, name, income, scenario, median_year = yrf(med), lower95 = yrf(lo), upper95 = yrf(hi), share_before_2022, share_within, share_after_2050)][order(name, scenario)], file.path(TAB, "crossover_by_country.csv"))

# --- Appendix A: country totals, SSP2-4.5, 2041-2050 ---
ca <- tot[unit_type == "location" & period == "2041-2050"]
IA <- c("High income" = "H", "Upper middle income" = "UM", "Lower middle income" = "LM", "Low income" = "L", "Not classified" = "--")
nm <- ca[measure == "deaths_heat" & scenario == "ssp245"][order(name), .(location_id, name, income, asterisk)]
rows <- vapply(seq_len(nrow(nm)), function(i) {
  l <- nm$location_id[i]; v <- function(m, s = "ssp245") ca[location_id == l & measure == m & scenario == s]
  h <- v("deaths_heat"); c_ <- v("deaths_cold"); n_ <- v("deaths_nonopt"); r <- v("deaths_heat", "ssp585")
  paste0(tex(nm$name[i]), if (nm$asterisk[i]) "$^{*}$" else "", " & ", IA[nm$income[i]], " & ", nn(h$mean), " ", ci(h$lower95, h$upper95, nn), " & ", sub("^-", "$-$", sprintf("%.1f", h$rate_mean)),
         " & ", nn(c_$mean), " ", ci(c_$lower95, c_$upper95, nn), " & ", sub("^-", "$-$", sprintf("%.1f", c_$rate_mean)), " & ", sub("^-", "$-$", sprintf("%.1f", n_$rate_mean)), " & ", sprintf("%.2f", r$ratio), " \\\\")
}, "")
wr(c("\\begin{longtable}{lccrcrrr}", "\\caption{Country totals under SSP2-4.5, 2041--2050, on the 24 common models: mean deaths per year with 95\\% interval, crude rate per 100,000 people per year, and the SSP5-8.5 to SSP2-4.5 ratio of mean heat deaths. Income group: H high, UM upper middle, LM lower middle, L low, -- not classified. An asterisk marks the two countries whose totals carry a disaster-forecast tail.}\\label{tab:country}\\\\",
     "\\toprule", "Country & Inc. & Heat deaths (95\\%) & Rate & Cold deaths (95\\%) & Rate & Net rate & Ratio \\\\", "\\midrule", "\\endfirsthead",
     "\\toprule", "Country & Inc. & Heat deaths (95\\%) & Rate & Cold deaths (95\\%) & Rate & Net rate & Ratio \\\\", "\\midrule", "\\endhead", "\\bottomrule", "\\endfoot", rows, "\\end{longtable}"), "tab_app_country.tex")

# --- Appendix B: country by cause, means, parentheses when the 95% interval includes zero ---
CA <- c(cvd_ihd = "IHD", cvd_stroke = "Str", resp_copd = "COPD", lri = "LRI", diabetes = "Diab", ckd = "CKD", cvd_htn = "HHD", cvd_cmp = "Card", inj_trans_road = "Road",
        inj_suicide = "Self", inj_homicide = "Viol", inj_drowning = "Drwn", inj_mech = "Mech", inj_disaster = "Natr", inj_animal = "Anim", inj_trans_other = "OTr", inj_othunintent = "OUn")
cl <- cau[unit_type == "location" & scenario == "ssp245" & period == "2041-2050"]
setkey(cl, measure, name, slice)
app <- function(m, lbl, file, tag) {
  z <- cl[measure == m]
  rows <- vapply(sort(unique(z$name)), function(nmv) {
    y <- z[name == nmv]; y <- y[match(names(CA), slice)]
    cells <- ifelse(y$lower95 < 0 & y$upper95 > 0, paste0("(", nn(y$mean), ")"), nn(y$mean))
    paste0(tex(nmv), if (y$asterisk[1]) "$^{*}$" else "", " & ", paste(cells, collapse = " & "), " \\\\")
  }, "")
  hd <- paste0("Country & ", paste(CA, collapse = " & "), " \\\\")
  wr(c(paste0("\\begin{longtable}{l", strrep("r", length(CA)), "}"), paste0("\\caption{", lbl, "}\\label{", tag, "}\\\\"), "\\toprule", hd, "\\midrule", "\\endfirsthead", "\\toprule", hd, "\\midrule", "\\endhead", "\\bottomrule", "\\endfoot", rows, "\\end{longtable}"), file)
}
key <- "IHD ischemic heart disease; Str stroke; COPD chronic obstructive pulmonary disease; LRI lower respiratory infections; Diab diabetes; CKD chronic kidney disease; HHD hypertensive heart disease; Card cardiomyopathy and myocarditis; Road road injuries; Self self-harm; Viol interpersonal violence; Drwn drowning; Mech mechanical forces; Natr forces of nature; Anim animal contact; OTr other transport; OUn other unintentional."
app("deaths_heat", paste0("Heat deaths per year by country and cause, SSP2-4.5, 2041--2050, mean over the 24 common models. Parentheses mark cells whose 95\\% interval includes zero. ", key), "tab_app_cause_heat.tex", "tab:causeheat")
app("deaths_cold", "Cold deaths per year by country and cause, on the same basis and with the same abbreviations as Table~\\ref{tab:causeheat}.", "tab_app_cause_cold.tex", "tab:causecold")

# --- digest ---
sink(file.path(dirname(OUT), "numbers_for_prose.txt"))
options(width = 170, datatable.print.nrows = 300)
cat("YLLs World, per year\n"); print(W[measure %in% c("yll_heat", "yll_cold", "yll_nonopt") & scenario %in% c("ssp245", "ssp585") & period %in% PER[c(1, 3)], .(scenario, period, measure, mean = round(mean), l = round(lower95), u = round(upper95), rate = round(rate_mean), ratio = round(ratio, 3))][order(measure, scenario, period)])
cat("Shares of World, SSP2-4.5 2041-2050\n")
t <- tot[unit_type == "location" & scenario == "ssp245" & period == "2041-2050" & measure %in% c("deaths_heat", "deaths_cold")]
t[, sh := mean / sum(mean), by = measure]
print(t[location_id %in% c(6, 163)][, .(name, measure, mean = round(mean), share = round(sh, 3))])
print(t[, .(top5_share = round(sum(sort(sh, decreasing = TRUE)[1:5]), 3), top5 = paste(name[order(-sh)][1:5], collapse = ", ")), by = measure])
cat("Heat ratio growth\n"); print(W[measure %in% c("deaths_heat", "deaths_cold", "deaths_nonopt") & scenario == "ssp245", .(period, measure, mean = round(mean), rate = round(rate_mean, 2))][order(measure, period)])
cat("Heat share of nonopt\n"); print(dcast(W[measure %in% c("deaths_heat", "deaths_nonopt"), .(scenario, period, measure, mean)], scenario + period ~ measure, value.var = "mean")[, share := round(deaths_heat / deaths_nonopt, 3)][])
print(cau[unit_type == "group" & measure %in% c("deaths_heat", "deaths_cold") & scenario == "ssp585" & period == "2041-2050", .(incl0 = sum(lower95 < 0 & upper95 > 0)), by = .(unit, measure)][unit %in% c("World", "High income", "Low and middle income")])
print(cau[unit_type == "group" & measure %in% c("deaths_heat", "deaths_cold") & scenario == "ssp245" & period == "2041-2050", .(incl0 = sum(lower95 < 0 & upper95 > 0)), by = .(unit, measure)][unit %in% c("World", "High income", "Low and middle income")])
print(tot[unit_type == "location" & measure %in% c("yll_cold") & period == "2041-2050", .(neg = sum(mean < 0)), by = scenario])
print(tot[unit_type == "location" & measure %in% c("yll_cold"), .(neg = sum(mean < 0), inc0 = sum(lower95 < 0 & upper95 > 0)), by = .(scenario, period)][order(scenario, period)])
cat("non-ASCII names:", paste(unique(grep("[^ -~]", inc$name, value = TRUE)), collapse = "; "), "\n")
cat("Haiti/Honduras income:", paste(inc[location_id %in% c(114, 129), paste(name, income)], collapse = "; "), "\n")
sink()
cat("ok\n")
