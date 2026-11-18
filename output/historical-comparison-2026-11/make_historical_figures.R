# make_historical_figures.R: figures for the historical comparison, in a light (report)
# and a dark (slides) version, with the palettes of output/report-multimodel-2026-11/make_figures.R.
# Reads historical.rds written by build_historical.R.
# Usage: Rscript make_historical_figures.R <out dir of build_historical.R>
suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(scales) })
OUT <- commandArgs(TRUE)[1]
x <- readRDS(file.path(OUT, "historical.rds"))
FR <- file.path(OUT, "figures-report"); FS <- file.path(OUT, "figures-slides")
for (d in c(FR, FS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
SC <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5 (reference)", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")
RISK <- c(heat = "Heat", cold = "Cold", total = "Heat + cold")
themes <- list(
  light = list(bg = "#ffffff", ink = "#14141F", soft = "#4A4A5E", grid = "#E6E6EE", family = "serif",
               sc = c("#0B8CA6", "#7C3AED", "#0E9F6E", "#C026D3"), g19 = "#14141F", g23 = "#C2410C", dir = FR, base = 9.5),
  dark = list(bg = "#06060C", ink = "#E8E8F0", soft = "#A8A8C0", grid = "#1F1F35", family = "sans",
              sc = c("#0AA5C0", "#A855F7", "#0FA875", "#E040D0"), g19 = "#E8E8F0", g23 = "#EA6A10", dir = FS, base = 7))
mk_theme <- function(p) theme_minimal(base_size = p$base, base_family = p$family) + theme(
  plot.background = element_rect(fill = p$bg, color = NA), panel.background = element_rect(fill = p$bg, color = NA),
  legend.background = element_rect(fill = p$bg, color = NA), legend.key = element_rect(fill = p$bg, color = NA),
  text = element_text(color = p$ink), axis.text = element_text(color = p$soft), strip.text = element_text(color = p$ink, hjust = 0),
  panel.grid.major = element_line(color = p$grid, linewidth = 0.3), panel.grid.minor = element_blank(),
  legend.position = "top", legend.justification = "left", legend.title = element_blank(), legend.margin = margin(0, 0, 0, 0),
  plot.margin = margin(4, 8, 4, 4), panel.spacing = unit(10, "pt"))
save_both <- function(name, f, w, h, ws, hs) for (p in themes) {
  light <- p$family == "serif"
  ggsave(file.path(p$dir, paste0(name, ".pdf")), f(p), width = if (light) w else ws, height = if (light) h else hs,
         device = cairo_pdf, bg = p$bg)
}

# ---- Figure 1: world crude rates, 1990-2050 -----------------------------------------
oy <- copy(x$oy); oy[, `:=`(R = factor(RISK[risk], RISK), S = factor(SC[scenario], SC))]
gb <- rbind(x$world[source == "GBD 2019 round", .(src = "GBD 2019 round (our method)", year = as.numeric(period), risk, rate, lower, upper)],
            x$world[source == "GBD 2023 round", .(src = "GBD 2023 round (revised)", year = as.numeric(period), risk, rate, lower, upper)])
gb[, R := factor(RISK[as.character(risk)], RISK)]
gb[src %like% "2023", year := year + 0.8]  # nudge so 2019 points of the two rounds don't overlap
save_both("fig_hist_world", function(p) {
  ggplot() +
    geom_ribbon(data = oy[scenario == "ssp245"], aes(year, ymin = lower, ymax = upper), fill = p$sc[2], alpha = 0.15) +
    geom_line(data = oy, aes(year, rate, color = S), linewidth = 0.45) +
    geom_errorbar(data = gb[!is.na(lower)], aes(year, ymin = lower, ymax = upper), color = p$g19, width = 0, linewidth = 0.4) +
    geom_point(data = gb, aes(year, rate, shape = src, fill = src), color = p$g19, size = 1.8, stroke = 0.4) +
    scale_shape_manual(values = c(21, 23)) + scale_fill_manual(values = c(p$g19, p$g23)) +
    scale_color_manual(values = p$sc) +
    facet_wrap(~R, scales = "free_y") + expand_limits(y = 0) +
    scale_x_continuous(breaks = c(1990, 2010, 2030, 2050)) +
    labs(x = NULL, y = "Deaths per 100,000 (crude, all ages)") + mk_theme(p) +
    guides(color = guide_legend(order = 2, nrow = 1, override.aes = list(linewidth = 1.2)), shape = guide_legend(order = 1), fill = guide_legend(order = 1)) +
    theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"))
}, 6.6, 3.0, 5.5, 2.75)

# ---- Figure 2: groups ----------------------------------------------------------------
grp <- copy(x$grp)
wr <- x$world[(source != "Ours" & period %in% c("2019", "2022")) | (source == "Ours" & scenario == "ssp245" & period %like% "-"),
              .(group = "World", period, risk, source = fifelse(source == "Ours", "Ours SSP2-4.5", source), rate, lower, upper)]
grp <- rbind(grp[, .(group, period, risk, source, rate, lower, upper)], wr)
grp[, col := paste(fcase(source == "GBD 2019 round", "GBD 2019 rd", source == "GBD 2023 round", "GBD 2023 rd", default = "Ours"),
                   sub("-20(\\d\\d)$", "-\\1", period))]
lev <- c("GBD 2019 rd 2019", "GBD 2023 rd 2019", "GBD 2023 rd 2022", "Ours 2022-30", "Ours 2031-40", "Ours 2041-50")
grp[, col := factor(col, lev)]
grp[, R := factor(RISK[as.character(risk)], RISK)]
grp[, kind := fifelse(source %like% "Ours", "Ours, SSP2-4.5 (95% range)", fifelse(source %like% "2019", "GBD 2019 round (our method)", "GBD 2023 round (revised)"))]
plot_groups <- function(G, ord, name, w, h, ws, hs) {
  G <- G[group %in% ord & risk != "total"]
  # one row per group; the six columns sit at fixed offsets inside it, top to bottom in time order
  G[, y := (length(ord) + 1 - match(group, ord)) + (2.5 - (as.integer(col) - 1)) * 0.13]
  save_both(name, function(p) {
    ggplot(G, aes(rate, y, color = kind, shape = kind)) +
      geom_segment(data = G[!is.na(lower)], aes(x = lower, xend = upper, yend = y), linewidth = 0.35, show.legend = FALSE) +
      geom_point(size = 1.3) +
      facet_wrap(~R, scales = "free_x") + expand_limits(x = 0) +
      scale_y_continuous(breaks = seq_along(ord), labels = rev(sub("Middle East, North Africa, ", "Middle East, N. Africa,\n", ord)), expand = expansion(add = 0.5)) +
      scale_color_manual(values = c(p$g19, p$g23, p$sc[2])) + scale_shape_manual(values = c(16, 18, 17)) +
      labs(x = "Deaths per 100,000 (crude, all ages)", y = NULL) + mk_theme(p) + theme(panel.grid.major.y = element_blank())
  }, w, h, ws, hs)
  fwrite(dcast(G, group + R ~ col, value.var = "rate", fun.aggregate = function(v) round(v, 2)), file.path(OUT, paste0(name, ".csv")))
}
plot_groups(grp, c("World", "High income", "Upper middle income", "Lower middle income", "Low income", "Low and middle income"),
            "fig_hist_income", 6.6, 3.6, 5.5, 2.9)
plot_groups(grp, c("World", "East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean",
                   "Middle East, North Africa, Afghanistan & Pakistan", "North America", "South Asia", "Sub-Saharan Africa"),
            "fig_hist_region", 6.6, 4.2, 5.5, 3.1)

# ---- Figure 3: countries ---------------------------------------------------------------
cw <- copy(x$cwide)[risk != "total"]
cw[, `:=`(gbd = gbd19_2019, ours = `ours245_2022-2030`, R = factor(RISK[risk], RISK))]
off <- cw[!(gbd > 0 & ours > 0)]
cat("Not shown on the log-scale country figure (a value <= 0 on either axis):\n")
print(off[, .(name, risk, gbd19_2019 = signif(gbd, 3), ours = signif(ours, 3))])
fwrite(off[, .(name, risk, gbd19_2019 = gbd, ours_2022_2030 = ours)], file.path(OUT, "fig_hist_countries_not_shown.csv"))
inc4 <- c("High income", "Upper middle income", "Lower middle income", "Low income")
cw[, I := factor(income, inc4)]
save_both("fig_hist_countries", function(p) {
  ggplot(cw[gbd > 0 & ours > 0], aes(gbd, ours, color = I)) +
    geom_abline(slope = 1, intercept = 0, color = p$soft, linewidth = 0.3) +
    geom_abline(slope = 1, intercept = log10(2), color = p$soft, linewidth = 0.25, linetype = "dashed") +
    geom_abline(slope = 1, intercept = -log10(2), color = p$soft, linewidth = 0.25, linetype = "dashed") +
    geom_point(size = 0.9, alpha = 0.85) +
    scale_x_log10(labels = label_number(drop0trailing = TRUE)) + scale_y_log10(labels = label_number(drop0trailing = TRUE)) +
    scale_color_manual(values = p$sc, na.value = p$soft) +
    facet_wrap(~R, scales = "free") +
    labs(x = "GBD 2019 round, 2019 (deaths per 100,000)", y = "Ours, SSP2-4.5, 2022-2030") + mk_theme(p)
}, 6.6, 3.3, 5.5, 2.75)
cat("figures done\n")
