# Build a small fake project tree to test the summarizer's draw export
# (--draw_periods) and the pooling script. Starts from the one real Colombia
# sample pair in the accepted ssp585 test bundle (burden and Workflow B files
# with 500 draws, 2022) and copies it to 2 locations x 4 models x 3 years,
# multiplying deaths by a known factor per copy so sums are traceable.
# Usage: Rscript make_fixture.R <sample dir> <fixture root> <repo root>
suppressMessages(library(data.table))
a <- commandArgs(TRUE); S <- a[1]; R <- a[2]; REPO <- a[3]
unlink(R, recursive = TRUE)
dir.create(file.path(R, "data"), recursive = TRUE)
file.symlink(file.path(REPO, "data", "lifetables"), file.path(R, "data", "lifetables"))
b245 <- setDT(readRDS(file.path(S, "cckp/125/access-cm2-r1i1p1f1-ssp245/burden_2022.rds")))
b585 <- setDT(readRDS(file.path(S, "cckp/125/access-cm2-r1i1p1f1-ssp585/burden_2022.rds")))
w585 <- setDT(readRDS(file.path(S, "workflow_b/125/access-cm2-r1i1p1f1-ssp585/wb_2022.rds")))
locs <- c(125L, 126L); years <- c(2022L, 2023L, 2030L)
# models: m1-m3 have ssp245; m4 has only a target burden (cannot be scaled);
# ssp126 runs only for m1, m2.
runs <- CJ(model = c("m1", "m2", "m3", "m4"), scenario = c("ssp245", "ssp585", "ssp126"))
runs <- runs[!(model == "m4" & scenario == "ssp245") & !(scenario == "ssp126" & model %in% c("m3", "m4"))]
fac <- function(loc, model, scen, year) (1 + 0.1 * match(model, c("m1", "m2", "m3", "m4"))) *
  (1 + 0.01 * (year - 2022)) * (if (scen == "ssp245") 1 else if (scen == "ssp585") 1.2 else 0.9) * (if (loc == 126L) 0.5 else 1)
for (loc in locs) for (i in seq_len(nrow(runs))) for (y in years) {
  m <- runs$model[i]; s <- runs$scenario[i]; f <- fac(loc, m, s, y)
  bd <- file.path(R, "output/results/cckp", loc, paste0(m, "-", s)); dir.create(bd, recursive = TRUE, showWarnings = FALSE)
  b <- copy(if (s == "ssp245") b245 else b585)
  b[, `:=`(year = y, location_id = loc, deaths_heat = deaths_heat * f, deaths_cold = deaths_cold * f,
           deaths_nonopt = deaths_nonopt * f)]
  saveRDS(b, file.path(bd, sprintf("burden_%d.rds", y)))
  if (s != "ssp245" && m != "m4") {
    wd <- file.path(R, "output/results/workflow_b", loc, paste0(m, "-", s)); dir.create(wd, recursive = TRUE, showWarnings = FALSE)
    w <- copy(w585)
    w[, `:=`(year = y, location_id = loc, deaths_heat_attrib = deaths_heat_attrib * f,
             deaths_cold_attrib = deaths_cold_attrib * f, deaths_nonopt_attrib = deaths_nonopt_attrib * f)]
    saveRDS(w, file.path(wd, sprintf("wb_%d.rds", y)))
  }
}
cat("fixture written to", R, "\n")
