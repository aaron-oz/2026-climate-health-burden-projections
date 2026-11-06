# followups3.R: (a) rank correlation of draws across adjacent years, all locations,
# to separate "a few outlier draws" (Pearson low, Spearman high) from "draw identity
# is not persistent" (both low); (b) Puerto Rico draw 209 by year; (c) Lithuania
# against the IHME all-cause draws.
# Usage: Rscript followups3.R <review_out_dir> <summary_dir> <all_causes.csv>
suppressPackageStartupMessages(library(data.table))
setDTthreads(1)
a <- commandArgs(TRUE); OUT <- a[1]; SUM <- a[2]; AC <- a[3]
options(width = 170, datatable.print.nrows = 300)
sec <- function(x) cat("\n\n================ ", x, " ================\n")
pl <- readRDS(file.path(OUT, "loc_pooled_all.rds")); names_loc <- unique(pl[, .(location_id, name)])
ms <- readRDS(file.path(OUT, "model_sets.rds")); common <- ms$common
files <- as.integer(sub("\\.rds", "", list.files(file.path(SUM, "draws"), "\\.rds$")))
one <- function(l) {
  D <- setDT(readRDS(file.path(SUM, "draws", paste0(l, ".rds"))))[model %in% common[c(1, 8, 15)] & scenario == "ssp245"]
  D[, year := as.integer(period)]
  out <- list()
  for (m in unique(D$model)) for (y in c(2025L, 2030L, 2040L)) {
    x <- D[model == m & year == y][order(draw)]; z <- D[model == m & year == y + 1L][order(draw)]
    out[[length(out) + 1L]] <- data.table(location_id = l, model = m, year = y,
      p_c = cor(x$deaths_cold, z$deaths_cold), s_c = cor(x$deaths_cold, z$deaths_cold, method = "spearman"),
      p_h = suppressWarnings(cor(x$deaths_heat, z$deaths_heat)), s_h = suppressWarnings(cor(x$deaths_heat, z$deaths_heat, method = "spearman")))
  }
  rbindlist(out)
}
r <- rbindlist(parallel::mclapply(files, one, mc.cores = 6, mc.preschedule = FALSE))
r <- r[, .(p_c = median(p_c), s_c = median(s_c), p_h = median(p_h, na.rm = TRUE), s_h = median(s_h, na.rm = TRUE)), by = location_id]
cm <- pl[slice == "all" & target == "ssp585" & period == "2031-2040" & measure %in% c("deaths_cold", "deaths_heat"), .(location_id, measure, r_mean)]
cm <- dcast(cm, location_id ~ measure, value.var = "r_mean")
r <- merge(merge(r, cm, by = "location_id"), names_loc, by = "location_id")
sec("A. Adjacent-year draw correlation, ssp245, median of 3 models and 3 year pairs")
print(r[, lapply(.SD, function(x) round(quantile(x, c(.05, .5), na.rm = TRUE), 3)), .SDcols = c("p_c", "s_c", "p_h", "s_h")])
cat("lowest Spearman, cold, cold deaths above 100/yr:\n")
print(r[deaths_cold > 100][order(s_c)][1:12, .(name, location_id, cold = round(deaths_cold), p_c = round(p_c, 2), s_c = round(s_c, 2), heat = round(deaths_heat), p_h = round(p_h, 2), s_h = round(s_h, 2))])
cat("lowest Spearman, heat, heat deaths above 100/yr:\n")
print(r[deaths_heat > 100][order(s_h)][1:8, .(name, location_id, heat = round(deaths_heat), p_h = round(p_h, 2), s_h = round(s_h, 2))])
cat("Pearson well below Spearman (outlier draws), cold deaths above 100/yr:\n")
print(r[deaths_cold > 100 & s_c - p_c > .2][order(p_c - s_c)][1:12, .(name, cold = round(deaths_cold), p_c = round(p_c, 2), s_c = round(s_c, 2))])

sec("B. Puerto Rico, draw 209 and the next few, by year (ssp585, mean over common 24)")
D <- setDT(readRDS(file.path(SUM, "draws", "385.rds")))[model %in% common & scenario == "ssp585"]
D[, year := as.integer(period)]
x <- D[draw == 209, .(h = mean(deaths_heat), c = mean(deaths_cold), hmax = max(deaths_heat), hmin = min(deaths_heat)), by = year]
print(round(x[order(-abs(h))][1:8], 1))
print(D[draw != 209, .(h = mean(deaths_heat), c = mean(deaths_cold), hmax = max(deaths_heat))])
print(D[year == x[which.max(h), year], .(models = .N / 500, hm209 = mean(deaths_heat[draw == 209]), hm_other = mean(deaths_heat[draw != 209]), nabove = sum(deaths_heat > 3000))])
Cb <- setDT(readRDS(file.path(SUM, "draws_cause", "385.rds")))[model %in% common & scenario == "ssp585" & draw == 209]
print(Cb[, .(heat = round(mean(deaths_heat), 1), cold = round(mean(deaths_cold), 1)), by = .(period, acause)][, .SD[order(-abs(heat))][1:3], by = period][period == Cb[, .(h = sum(deaths_heat)), by = period][which.max(h), period]])

sec("C. Lithuania: cold deaths in a run vs IHME all-cause draws")
hdr <- names(fread(AC, nrows = 0))
tmp <- tempfile(); on.exit(unlink(tmp))
system(paste0("grep -E '^(Lithuania|Latvia|Estonia),All Ages,Both,(2030|2031|2040|2041),' ", shQuote(AC), " > ", tmp))
ac <- fread(tmp, header = FALSE); setnames(ac, hdr)
ac <- melt(ac, id.vars = c("Location", "Age Group", "Sex", "Year"), variable.factor = FALSE); ac[, draw := as.integer(sub("draw_", "", variable))]
acw <- dcast(ac, Location + draw ~ Year, value.var = "value")
cat("IHME all-cause deaths, all ages both sexes, draw correlation across adjacent years:\n")
print(acw[, .(cor_30_31 = round(cor(`2030`, `2031`), 3), cor_40_41 = round(cor(`2040`, `2041`), 3), cv_2030 = round(sd(`2030`) / mean(`2030`), 3)), by = Location])
for (ll in list(c(60L, "Lithuania"), c(59L, "Latvia"), c(58L, "Estonia"))) {
  l <- as.integer(ll[1])
  D <- setDT(readRDS(file.path(SUM, "draws", paste0(l, ".rds"))))[model == common[1] & scenario == "ssp245" & period %in% c("2030", "2031")]
  w <- dcast(D, draw ~ period, value.var = "deaths_cold")
  z <- merge(w, acw[Location == ll[2]], by = "draw", suffixes = c("_cold", "_ihme"))
  cat(ll[2], ": cold 2030 vs 2031", round(cor(z$`2030_cold`, z$`2031_cold`), 3), "; cold 2030 vs IHME 2030", round(cor(z$`2030_cold`, z$`2030_ihme`), 3), "; cold 2031 vs IHME 2031", round(cor(z$`2031_cold`, z$`2031_ihme`), 3), "; cold sd/mean", round(sd(z$`2030_cold`) / mean(z$`2030_cold`), 3), "\n")
}
