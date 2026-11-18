# make_country_examples.R: example country panels for the historical comparison: GBD
# 2019-round rates (1990, 2010, 2019, with 95% ranges), GBD 2023-round rates (2019, 2022,
# with 95% ranges), and our decade rates under SSP2-4.5 (95% ranges), crude deaths per
# 100,000. Light (report) and dark (slides) versions, palettes as make_historical_figures.R.
# Usage: Rscript make_country_examples.R <gbd2019 csv> <gbd2023 anchor csv> <pooled dir> <out dir>
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
a <- commandArgs(TRUE); F19 <- a[1]; F23 <- a[2]; POOL <- a[3]; OUT <- a[4]
CT <- c(`163` = "India", `6` = "China", `214` = "Nigeria", `102` = "United States")
RISK <- c(heat = "Heat", cold = "Cold")
rk <- function(x) fcase(x %like% "High", "heat", x %like% "Low", "cold", default = "total")
mid <- c(`2022-2030` = 2026, `2031-2040` = 2035.5, `2041-2050` = 2045.5)

P <- fread(F19)[measure_name == "Deaths" & age_group_name == "All Ages" & metric_name == "Rate" & location_id %in% as.integer(names(CT))]
P <- P[, .(location_id, year = as.numeric(year_id), risk = rk(rei_name), rate = 1e5 * val, lower = 1e5 * lower, upper = 1e5 * upper,
           src = "GBD 2019 round (our method)")]
G <- fread(F23)[measure_name == "Deaths" & metric_name == "Rate" & location_id %in% as.integer(names(CT))]
G <- G[, .(location_id, year = as.numeric(year) + 0.8, risk = rk(rei_name), rate = val, lower, upper, src = "GBD 2023 round (revised)")]
O <- fread(file.path(POOL, "tables", "country_totals.csv"))[scenario == "ssp245" & location_id %in% as.integer(names(CT)) &
                                                             measure %in% c("deaths_heat", "deaths_cold")]
O <- O[, .(location_id, year = mid[period], risk = sub("deaths_", "", measure), rate = rate_mean, lower = rate_lower95,
           upper = rate_upper95, src = "Ours, SSP2-4.5, decade mean")]
D <- rbind(P, G, O)[risk %in% names(RISK)]
D[, `:=`(C = factor(CT[as.character(location_id)], CT), R = factor(RISK[risk], RISK),
         src = factor(src, c("GBD 2019 round (our method)", "GBD 2023 round (revised)", "Ours, SSP2-4.5, decade mean")))]
fwrite(D[, .(country = C, risk, src, year, rate, lower, upper)][order(country, risk, year)], file.path(OUT, "fig_hist_country_examples.csv"))

themes <- list(
  light = list(bg = "#ffffff", ink = "#14141F", soft = "#4A4A5E", grid = "#E6E6EE", family = "serif",
               cols = c("#14141F", "#C2410C", "#7C3AED"), dir = file.path(OUT, "figures-report"), base = 9.5, w = 6.6, h = 3.4),
  dark = list(bg = "#06060C", ink = "#E8E8F0", soft = "#A8A8C0", grid = "#1F1F35", family = "sans",
              cols = c("#E8E8F0", "#EA6A10", "#A855F7"), dir = file.path(OUT, "figures-slides"), base = 7, w = 5.5, h = 2.3))
for (p in themes) {
  g <- ggplot(D, aes(year, rate, color = src, shape = src)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, linewidth = 0.4) +
    geom_point(size = 1.5) +
    facet_grid(R ~ C, scales = "free_y") + expand_limits(y = 0) +
    scale_x_continuous(breaks = c(1990, 2010, 2030, 2050)) +
    scale_color_manual(values = p$cols) + scale_shape_manual(values = c(16, 18, 17)) +
    labs(x = NULL, y = "Deaths per 100,000 (crude)") +
    theme_minimal(base_size = p$base, base_family = p$family) + theme(
      plot.background = element_rect(fill = p$bg, color = NA), panel.background = element_rect(fill = p$bg, color = NA),
      legend.background = element_rect(fill = p$bg, color = NA), legend.key = element_rect(fill = p$bg, color = NA),
      text = element_text(color = p$ink), axis.text = element_text(color = p$soft), strip.text = element_text(color = p$ink, hjust = 0),
      panel.grid.major = element_line(color = p$grid, linewidth = 0.3), panel.grid.minor = element_blank(),
      legend.position = "top", legend.justification = "left", legend.title = element_blank(), legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(4, 8, 4, 4), panel.spacing = unit(8, "pt"))
  ggsave(file.path(p$dir, "fig_hist_country_examples.pdf"), g, width = p$w, height = p$h, device = cairo_pdf, bg = p$bg)
}
cat("done\n")
