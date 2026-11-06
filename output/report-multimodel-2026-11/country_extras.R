# country_extras.R: the two things the pooling script does not produce, both on the 24
# common models.
# (1) Non-optimal, heat and cold deaths with and without the inj_disaster cause, side by
#     side, per country and income group, every scenario and decade (the asterisk
#     columns for Haiti and Honduras; nothing is removed from the headline numbers).
# (2) The year heat overtakes cold per country: for each (model, draw, scenario), a
#     straight line fitted to yearly heat minus cold, 2022-2050; crossing year where the
#     line reaches zero, "before 2022" if it is already above zero in 2022, "after 2050"
#     if still below in 2050. Also the share of (model, draw) pairs with heat above cold
#     per decade, which needs no straight line.
# Usage: Rscript country_extras.R <summary_dir> <income_groups.csv> <out dir> <model_sets.rds>
suppressPackageStartupMessages(library(data.table))
setDTthreads(1)
a <- commandArgs(TRUE); SUM <- a[1]; GF <- a[2]; OUT <- a[3]; ms <- readRDS(a[4]); common <- ms$common
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
g <- fread(GF)
DEC <- c("2022-2025" = "2022-2030", "2026-2030" = "2022-2030", "2031-2035" = "2031-2040", "2036-2040" = "2031-2040", "2041-2045" = "2041-2050", "2046-2050" = "2041-2050")
DY <- c("2022-2030" = 9, "2031-2040" = 10, "2041-2050" = 10)
files <- as.integer(sub("\\.rds", "", list.files(file.path(SUM, "draws"), "\\.rds$")))
summ <- function(x, ...) x[, .(mean = mean(v), lower95 = quantile(v, .025, names = FALSE), upper95 = quantile(v, .975, names = FALSE), max = max(v)), by = c(...)]

one <- function(l) {
  C <- setDT(readRDS(file.path(SUM, "draws_cause", paste0(l, ".rds"))))[model %in% common]
  C[, period := DEC[period]]
  z <- rbind(C[, .(variant = "all_causes", h = sum(deaths_heat), c = sum(deaths_cold)), by = .(model, scenario, period, draw)],
             C[acause != "inj_disaster", .(variant = "without_inj_disaster", h = sum(deaths_heat), c = sum(deaths_cold)), by = .(model, scenario, period, draw)])
  z[, `:=`(h = h / DY[period], c = c / DY[period])]
  rm(C)
  long <- melt(z, id.vars = c("variant", "model", "scenario", "period", "draw"), measure.vars = c("h", "c"), variable.name = "m", value.name = "v", variable.factor = FALSE)
  nn <- z[, .(variant, model, scenario, period, draw, m = "n", v = h + c)]
  long <- rbind(long, nn)
  sens <- summ(long, "variant", "scenario", "period", "m")[, location_id := l]
  D <- setDT(readRDS(file.path(SUM, "draws", paste0(l, ".rds"))))[model %in% common]
  D[, year := as.integer(period)]
  D[, d := deaths_heat - deaths_cold]
  # OLS of d on year, per (model, scenario, draw), closed form; 29 equally spaced years
  x <- D$year - 2036
  fit <- D[, .(b0 = mean(d), b1 = sum((year - 2036) * d) / sum((year - 2036)^2)), by = .(model, scenario, draw)]
  fit[, `:=`(d22 = b0 + b1 * (2022 - 2036), d50 = b0 + b1 * (2050 - 2036))]
  fit[, cross := fifelse(d22 > 0, -Inf, fifelse(d50 <= 0, Inf, 2036 - b0 / b1))]
  cr <- fit[, .(location_id = l, med = quantile(cross, .5, names = FALSE, type = 1), lo = quantile(cross, .025, names = FALSE, type = 1), hi = quantile(cross, .975, names = FALSE, type = 1),
                share_before_2022 = mean(cross == -Inf), share_within = mean(is.finite(cross)), share_after_2050 = mean(cross == Inf)), by = scenario]
  D[, period2 := fifelse(year <= 2030, "2022-2030", fifelse(year <= 2040, "2031-2040", "2041-2050"))]
  dd <- D[, .(d = mean(d)), by = .(model, scenario, draw, period = period2)]
  ds <- dd[, .(location_id = l, share_heat_above = mean(d > 0)), by = .(scenario, period)]
  list(sens = sens, cr = cr, ds = ds, gz = z[, .(location_id = l, variant, model, scenario, period, draw, h, c)])
}
run <- function(ls, id) {
  res <- list(sens = list(), cr = list(), ds = list()); acc <- list()
  for (l in ls) {
    x <- one(l)
    res$sens[[length(res$sens) + 1L]] <- x$sens; res$cr[[length(res$cr) + 1L]] <- x$cr; res$ds[[length(res$ds) + 1L]] <- x$ds
    for (gr in c("World", g[location_id == l, group])) {
      y <- x$gz[, .(variant, model, scenario, period, draw, h, c)]; setkey(y, variant, model, scenario, period, draw)
      if (is.null(acc[[gr]])) acc[[gr]] <- y else { acc[[gr]][, `:=`(h = h + y$h, c = c + y$c)] }
    }
    rm(x); invisible(gc(FALSE))
  }
  f <- file.path(OUT, paste0(".tmp_", id, ".rds"))
  saveRDS(list(sens = rbindlist(res$sens), cr = rbindlist(res$cr), ds = rbindlist(res$ds), acc = acc), f, compress = FALSE); f
}
sh <- split(files, rep_len(1:8, length(files)))
tf <- parallel::mclapply(seq_along(sh), function(i) run(sh[[i]], i), mc.cores = 8, mc.preschedule = FALSE)
stopifnot(all(vapply(tf, is.character, TRUE)))
sens <- list(); cr <- list(); ds <- list(); acc <- list()
for (f in unlist(tf)) {
  r <- readRDS(f); unlink(f)
  sens[[length(sens) + 1L]] <- r$sens; cr[[length(cr) + 1L]] <- r$cr; ds[[length(ds) + 1L]] <- r$ds
  for (gr in names(r$acc)) { if (is.null(acc[[gr]])) acc[[gr]] <- r$acc[[gr]] else acc[[gr]][, `:=`(h = h + r$acc[[gr]]$h, c = c + r$acc[[gr]]$c)] }
}
fwrite(rbindlist(sens), file.path(OUT, "with_without_inj_disaster_country.csv"))
fwrite(rbindlist(cr), file.path(OUT, "crossover_country.csv"))
fwrite(rbindlist(ds), file.path(OUT, "heat_above_cold_share_country.csv"))
gs <- rbindlist(lapply(names(acc), function(gr) {
  z <- acc[[gr]]
  long <- rbind(z[, .(variant, scenario, period, m = "h", v = h)], z[, .(variant, scenario, period, m = "c", v = c)], z[, .(variant, scenario, period, m = "n", v = h + c)])
  summ(long, "variant", "scenario", "period", "m")[, group := gr]
}))
fwrite(gs, file.path(OUT, "with_without_inj_disaster_group.csv"))
cat("done\n")
