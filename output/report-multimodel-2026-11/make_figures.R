# make_figures.R: every figure in the report and the 2026-11-09 slides, in a light
# version (report) and a dark version (slides). Palettes checked with the dataviz
# validator on 2026-11-06, light surface #ffffff, dark surface #06060C.
# Usage: Rscript make_figures.R <pool_dir> <review_dir> <shapefile.shp> <fig_report_dir> <fig_slides_dir>
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(patchwork); library(sf); library(scales)})
setDTthreads(4)
a <- commandArgs(TRUE); POOL <- a[1]; REV <- a[2]; SHP <- a[3]; FR <- a[4]; FS <- a[5]
for (d in c(FR, FS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
x <- readRDS(file.path(POOL, "tidy_common.rds")); tot <- x$tot; cau <- x$cau; age <- x$age
ms <- readRDS(file.path(REV, "model_sets.rds")); common <- ms$common
SC <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")
MEAS <- c(deaths_heat = "Heat", deaths_cold = "Cold", deaths_nonopt = "Heat + cold")
CAUSE <- c(cvd_ihd = "Ischemic heart disease", cvd_stroke = "Stroke", resp_copd = "COPD", lri = "Lower respiratory infections", diabetes = "Diabetes",
           ckd = "Chronic kidney disease", cvd_htn = "Hypertensive heart disease", cvd_cmp = "Cardiomyopathy and myocarditis", inj_trans_road = "Road injuries",
           inj_suicide = "Self-harm", inj_homicide = "Interpersonal violence", inj_drowning = "Drowning", inj_mech = "Mechanical forces", inj_disaster = "Forces of nature",
           inj_animal = "Animal contact", inj_trans_other = "Other transport", inj_othunintent = "Other unintentional")
INC <- c("High income", "Upper middle income", "Lower middle income", "Low income")
SLICE <- c(under5_sex1 = "Under 5, male", under5_sex2 = "Under 5, female", a15_49_sex1 = "15-49, male", a15_49_sex2 = "15-49, female", a65plus_sex1 = "65+, male", a65plus_sex2 = "65+, female")

themes <- list(
  light = list(bg = "#ffffff", ink = "#14141F", soft = "#4A4A5E", grid = "#E6E6EE", zero = "#8888A8", family = "serif",
               sc = c("#0B8CA6", "#7C3AED", "#0E9F6E", "#C026D3"), heat = "#C2410C", cold = "#2563EB", na = "#D8D8E4", dir = FR, base = 9.5),
  dark = list(bg = "#06060C", ink = "#E8E8F0", soft = "#A8A8C0", grid = "#1F1F35", zero = "#8888A8", family = "sans",
              sc = c("#0AA5C0", "#A855F7", "#0FA875", "#E040D0"), heat = "#EA6A10", cold = "#3B82F6", na = "#2A2A44", dir = FS, base = 11.5))
mk_theme <- function(p) theme_minimal(base_size = p$base, base_family = p$family) + theme(
  plot.background = element_rect(fill = p$bg, color = NA), panel.background = element_rect(fill = p$bg, color = NA),
  legend.background = element_rect(fill = p$bg, color = NA), legend.key = element_rect(fill = p$bg, color = NA),
  text = element_text(color = p$ink), axis.text = element_text(color = p$soft), strip.text = element_text(color = p$ink, face = "plain", hjust = 0),
  panel.grid.major = element_line(color = p$grid, linewidth = 0.3), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
  legend.position = "top", legend.justification = "left", legend.title = element_blank(), legend.margin = margin(0, 0, 0, 0),
  plot.margin = margin(4, 8, 4, 4), panel.spacing = unit(10, "pt"))
fmt_k <- function(v) comma(v / 1e3, accuracy = 1)
save_both <- function(name, f, w, h, ws = w, hs = h) for (p in themes) {
  g <- f(p)
  ggsave(file.path(p$dir, paste0(name, ".pdf")), g, width = if (p$family == "serif") w else ws, height = if (p$family == "serif") h else hs, device = cairo_pdf, bg = p$bg)
}
# nested intervals: 95 thin, 80 medium, 66 thick, mean as a point
nested <- function(pos, sz = 1, col = NULL) {
  aesc <- if (is.null(col)) list() else list(color = col)
  list(do.call(geom_linerange, c(list(aes(ymin = lower95, ymax = upper95), position = pos, linewidth = 0.35 * sz), aesc)),
       do.call(geom_linerange, c(list(aes(ymin = lower80, ymax = upper80), position = pos, linewidth = 0.8 * sz), aesc)),
       do.call(geom_linerange, c(list(aes(ymin = lower66, ymax = upper66), position = pos, linewidth = 1.4 * sz), aesc)))
}

# --- Figure: World by decade, every scenario ---
d1 <- tot[unit == "World" & measure %in% names(MEAS)]
d1[, `:=`(M = factor(MEAS[measure], MEAS), S = factor(SC[scenario], SC))]
save_both("fig_world_decades", function(p) {
  pos <- position_dodge(width = 0.72)
  ggplot(d1, aes(period, mean, color = S, shape = S, group = S)) + nested(pos) + geom_point(position = pos, size = 1.9, fill = p$bg) +
    facet_wrap(~M, scales = "free_y") + scale_y_continuous(labels = fmt_k, limits = c(0, NA)) +
    scale_color_manual(values = p$sc) + scale_shape_manual(values = c(21, 16, 24, 17)) +
    labs(x = NULL, y = "Deaths per year (thousands)") + mk_theme(p) + theme(axis.text.x = element_text(size = rel(0.82)))
}, 6.6, 3.1, 8.4, 3.5)

# --- Figure: World yearly ---
W <- readRDS(file.path(REV, "world", "world.rds"))$ann; setDT(W)
W <- W[model %in% common]
yr <- melt(W, id.vars = c("model", "scenario", "year", "draw"), measure.vars = c("deaths_heat", "deaths_cold"), variable.factor = FALSE)
ys <- yr[, .(mean = mean(value), lower95 = quantile(value, .025, names = FALSE), upper95 = quantile(value, .975, names = FALSE)), by = .(scenario, year, variable)]
ys[, `:=`(M = factor(MEAS[variable], MEAS), S = factor(SC[scenario], SC))]
fwrite(ys, file.path(POOL, "world_yearly_common24.csv"))
save_both("fig_world_yearly", function(p) {
  ggplot(ys, aes(year, mean, color = S)) +
    geom_ribbon(data = ys[scenario == "ssp245"], aes(ymin = lower95, ymax = upper95), fill = p$sc[2], alpha = 0.13, color = NA) +
    geom_line(linewidth = 0.55) + facet_wrap(~M, scales = "free_y") +
    scale_y_continuous(labels = fmt_k, limits = c(0, NA)) + scale_color_manual(values = p$sc) +
    labs(x = NULL, y = "Deaths per year (thousands)") + mk_theme(p)
}, 6.6, 3.0, 8.4, 3.3)

# --- Figure: difference from SSP2-4.5 ---
d3 <- tot[unit == "World" & measure %in% names(MEAS) & scenario != "ssp245"]
d3[, `:=`(M = factor(MEAS[measure], MEAS), S = factor(SC[scenario], SC[c(1, 3, 4)]))]
save_both("fig_world_diff", function(p) {
  pos <- position_dodge(width = 0.66)
  ggplot(d3, aes(period, diff_mean, color = S, shape = S)) + geom_hline(yintercept = 0, color = p$zero, linewidth = 0.4) +
    geom_linerange(aes(ymin = diff_lower95, ymax = diff_upper95), position = pos, linewidth = 0.35) +
    geom_linerange(aes(ymin = diff_lower80, ymax = diff_upper80), position = pos, linewidth = 0.8) +
    geom_linerange(aes(ymin = diff_lower66, ymax = diff_upper66), position = pos, linewidth = 1.4) +
    geom_point(position = pos, size = 1.9, fill = p$bg) + facet_wrap(~M) +
    scale_y_continuous(labels = fmt_k) + scale_color_manual(values = p$sc[c(1, 3, 4)]) + scale_shape_manual(values = c(21, 24, 17)) +
    labs(x = NULL, y = "Difference (thousands per year)") + mk_theme(p) + theme(axis.text.x = element_text(size = rel(0.82)))
}, 6.6, 3.1, 8.4, 3.5)

# --- Figure: causes, World ---
d4 <- cau[unit == "World" & scenario == "ssp245" & period == "2041-2050" & measure %in% c("deaths_heat", "deaths_cold")]
ord <- d4[measure == "deaths_cold"][order(mean), slice]
d4[, `:=`(M = factor(MEAS[measure], MEAS[1:2]), C = factor(CAUSE[slice], CAUSE[ord]))]
save_both("fig_causes", function(p) {
  pd <- position_identity()
  ggplot(d4, aes(C, mean, color = M)) + geom_hline(yintercept = 0, color = p$zero, linewidth = 0.4) +
    nested(pd) + geom_point(size = 1.5) + coord_flip() + facet_wrap(~M, scales = "free_x") +
    scale_y_continuous(labels = fmt_k) + scale_color_manual(values = c(p$heat, p$cold), guide = "none") +
    labs(x = NULL, y = "Deaths per year (thousands), SSP2-4.5, 2041-2050") + mk_theme(p) +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = p$grid, linewidth = 0.3))
}, 6.6, 3.9, 8.4, 3.7)

# --- Figure: rates by income group ---
d5 <- tot[unit %in% INC & measure %in% c("deaths_heat", "deaths_cold") & ((scenario == "ssp245" & period %in% c("2022-2030", "2041-2050")) | (scenario == "ssp585" & period == "2041-2050"))]
for (v in c("mean", "lower95", "upper95", "lower80", "upper80", "lower66", "upper66")) set(d5, j = v, value = d5[[paste0("rate_", v)]])
d5[, `:=`(M = factor(MEAS[measure], MEAS[1:2]), I = factor(unit, INC), Sr = factor(paste(SC[scenario], period), c("SSP2-4.5 2022-2030", "SSP2-4.5 2041-2050", "SSP5-8.5 2041-2050")))]
save_both("fig_income_rates", function(p) {
  pos <- position_dodge(width = 0.66)
  ggplot(d5, aes(I, mean, color = Sr, shape = Sr)) + nested(pos) + geom_point(position = pos, size = 1.9, fill = p$bg) + facet_wrap(~M, scales = "free_y") +
    scale_y_continuous(limits = c(0, NA)) + scale_x_discrete(labels = function(v) gsub(" income", "", v)) +
    scale_color_manual(values = c(p$sc[1], p$sc[2], p$sc[4])) + scale_shape_manual(values = c(21, 16, 17)) +
    labs(x = NULL, y = "Deaths per 100,000 people per year") + mk_theme(p)
}, 6.6, 3.1, 8.4, 3.5)

# --- Figure: age and sex ---
d6 <- age[unit == "World" & scenario == "ssp245" & measure %in% c("deaths_heat", "deaths_cold") & period %in% c("2022-2030", "2041-2050")]
d6[, `:=`(M = factor(MEAS[measure], MEAS[1:2]), A = factor(SLICE[slice], rev(SLICE)))]
save_both("fig_age", function(p) {
  pos <- position_dodge(width = 0.6)
  ggplot(d6, aes(A, mean, color = period, shape = period)) + geom_hline(yintercept = 0, color = p$zero, linewidth = 0.4) +
    nested(pos) + geom_point(position = pos, size = 1.7, fill = p$bg) + coord_flip() + facet_wrap(~M, scales = "free_x") +
    scale_y_continuous(labels = fmt_k) + scale_color_manual(values = c(p$sc[1], p$sc[2])) + scale_shape_manual(values = c(21, 16)) +
    labs(x = NULL, y = "Deaths per year (thousands), SSP2-4.5") + mk_theme(p) +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = p$grid, linewidth = 0.3))
}, 6.6, 2.9, 8.4, 3.2)

# --- Figure: maps of rates ---
s <- st_read(SHP, quiet = TRUE)
s <- s[s$level == 3, c("loc_id")]
s <- suppressWarnings(st_simplify(st_transform(s, "+proj=robin"), dTolerance = 15000, preserveTopology = TRUE))
r <- tot[unit_type == "location" & scenario == "ssp245" & period == "2041-2050" & measure %in% c("deaths_heat", "deaths_cold"), .(loc_id = location_id, measure, rate = rate_mean)]
rw <- dcast(r, loc_id ~ measure, value.var = "rate")
sm <- merge(s, as.data.frame(rw), by = "loc_id", all.x = TRUE)
cat("polygons drawn:", nrow(sm), "; of 204 locations, without geometry:", paste(setdiff(rw$loc_id, s$loc_id), collapse = ","), "\n")
one_map <- function(p, col, hue, ttl) {
  low <- if (p$family == "serif") "#F4F4F8" else "#16162A"
  ggplot(sm) + geom_sf(aes(fill = .data[[col]]), color = p$bg, linewidth = 0.05) +
    scale_fill_gradient(low = low, high = hue, na.value = p$na, name = NULL, limits = c(0, NA)) + labs(title = ttl) +
    mk_theme(p) + theme(panel.grid.major = element_blank(), axis.text = element_blank(), legend.position = "right", legend.key.height = unit(26, "pt"), legend.key.width = unit(8, "pt"),
                        plot.title = element_text(size = rel(0.95), color = p$ink))
}
for (p in themes) {
  g <- one_map(p, "deaths_heat", p$heat, "Heat deaths per 100,000 per year") + one_map(p, "deaths_cold", p$cold, "Cold deaths per 100,000 per year") + plot_layout(ncol = if (p$family == "serif") 1 else 2) &
    theme(plot.background = element_rect(fill = p$bg, color = NA))
  ggsave(file.path(p$dir, "fig_maps.png"), g, width = if (p$family == "serif") 6.2 else 8.4, height = if (p$family == "serif") 6.2 else 2.5, dpi = 220, bg = p$bg)
}
cat("figures done\n")
