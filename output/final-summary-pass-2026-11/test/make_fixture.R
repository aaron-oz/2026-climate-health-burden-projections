# make_fixture.R: a small, internally consistent results tree for testing the
# final pass (util_export_compact.R, util_final_tables.R). Built from the one
# real Colombia sample (accepted ssp585 test bundle v2: the SSP2-4.5 pipeline
# burden for 2022, 500 draws), copied to 3 locations x 29 years x 5 climate
# models x 4 scenarios with known, smooth changes, and scaled with the
# production Workflow B code (apply_ratio() in global-scripts/util_workflow_b_ratio.R).
#   - IHME deaths depend on location and year only (as in production).
#   - PAFs change with model, scenario and year.
#   - Models m1-m3 run every scenario (the common set); m4 only SSP2-4.5 and
#     SSP5-8.5; m5 only SSP2-4.5. So the common set is m1-m3.
#   - Draws: the first --draws of the 500 (default 40) to keep it quick.
# Usage (repo root, emacs-r distrobox):
#   Rscript output/final-summary-pass-2026-11/test/make_fixture.R <bundle-v2 sample dir> <fixture root> [draws]
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); S <- a[1]; R <- a[2]; ND <- if (length(a) >= 3) as.integer(a[3]) else 40L
SCRIPTS_DIR <- "global-scripts"
Sys.setenv(PROJECT_ROOT = R)
unlink(R, recursive = TRUE); dir.create(file.path(R, "data"), recursive = TRUE)
source(file.path(SCRIPTS_DIR, "util_workflow_b_ratio.R"))   # config.R and apply_ratio(); runs nothing when sourced

b0 <- setDT(readRDS(file.path(S, "cckp/125/access-cm2-r1i1p1f1-ssp245/burden_2022.rds")))[draw < ND]
locs <- c(125L, 163L, 102L); years <- 2022:2050
gl <- c(`125` = 1, `163` = 3, `102` = 2)
runs <- rbind(CJ(model = c("m1", "m2", "m3"), scenario = c("ssp126", "ssp245", "ssp370", "ssp585")),
              data.table(model = "m4", scenario = c("ssp245", "ssp585")), data.table(model = "m5", scenario = "ssp245"))
sf <- c(ssp126 = 0.5, ssp245 = 1, ssp370 = 1.2, ssp585 = 1.5)
tmp <- file.path(R, "tmp"); dir.create(tmp)
for (l in locs) for (y in years) {
  t <- y - 2022
  ih <- b0[, .(year_id = y, age_group_id, sex_id, acause, draw, deaths = deaths * gl[[as.character(l)]] * (1 + 0.02 * t) *
                 (1 + 0.003 * (age_group_id / 5)))]
  fi <- file.path(tmp, sprintf("ihme_%d_%d.rds", l, y)); saveRDS(ih, fi)
  bfile <- function(m, s) file.path(R, "output/results/cckp", l, paste0(m, "-", s), sprintf("burden_%d.rds", y))
  for (i in seq_len(nrow(runs))) {
    m <- runs$model[i]; s <- runs$scenario[i]; mi <- match(m, c("m1", "m2", "m3", "m4", "m5"))
    b <- copy(b0)[, `:=`(year = y, location_id = l, subloc_id = as.character(l), model = m, scenario = s)]
    b[ih, deaths := i.deaths, on = .(age_group_id, sex_id, acause, draw)]
    b[, `:=`(paf_heat = paf_heat * (1 + 0.05 * mi) * (1 + 0.01 * t * sf[[s]]) * (if (l == 102L) 0.7 else 1),
             paf_cold = paf_cold * (1 + 0.02 * mi) * (1 - 0.004 * t * sf[[s]]))]
    b[, `:=`(paf_nonopt = paf_heat + paf_cold, deaths_heat = deaths * paf_heat, deaths_cold = deaths * paf_cold)]
    b[, deaths_nonopt := deaths_heat + deaths_cold]
    dir.create(dirname(bfile(m, s)), recursive = TRUE, showWarnings = FALSE); saveRDS(b, bfile(m, s))
  }
  for (i in seq_len(nrow(runs))) {
    m <- runs$model[i]; s <- runs$scenario[i]
    if (s == "ssp245" || !nrow(runs[model == m & scenario == "ssp245"])) next
    out <- file.path(R, "output/results/workflow_b", l, paste0(m, "-", s), sprintf("wb_%d.rds", y))
    apply_ratio(ref_burden = bfile(m, "ssp245"), target_burden = bfile(m, s), ihme_mortality = fi, output = out,
                location_id = l, model = m, scenario = s, verbose = FALSE)
  }
}
unlink(tmp, recursive = TRUE)
cat("fixture: ", length(locs), " locations x ", length(years), " years x ", nrow(runs), " runs, ", ND, " draws -> ", R, "\n", sep = "")
