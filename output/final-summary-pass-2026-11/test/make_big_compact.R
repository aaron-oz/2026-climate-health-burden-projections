# make_big_compact.R: a production-sized compact copy (500 draws, 24 models x 4
# scenarios, 29 years) for two locations, built from the compact export of the
# real Colombia 2022 sample, to time util_final_tables.R and measure its memory.
# The values are synthetic (smooth changes by model, scenario, year); only the
# sizes matter.
# Usage: Rscript make_big_compact.R <compact dir with location 125 from the real sample> <out dir>
a <- commandArgs(TRUE); SRC <- a[1]; OUT <- a[2]
source("global-scripts/util_compact_io.R")
x <- read_compact(SRC, 125)
D <- length(x$draws); years <- 2022:2050; Y <- length(years)
f0 <- read_factor(x, "access-cm2-r1i1p1f1", "ssp585")[, , , 1]           # [D, 17, 3]
models <- sprintf("model%02d", 1:24); scen <- c("ssp126", "ssp245", "ssp370", "ssp585")
for (loc in c(125L, 163L)) {
  ld <- file.path(OUT, loc); dir.create(ld, recursive = TRUE, showWarnings = FALSE)
  ih <- array(0, c(D, 2L, 17L, 17L, Y))
  for (yi in seq_len(Y)) ih[, , , , yi] <- x$ihme[, , , , 1] * (1 + 0.02 * (yi - 1)) * (if (loc == 163L) 3 else 1)
  con <- file(file.path(ld, "ihme.f32"), "wb"); writeBin(as.vector(ih), con, size = 4L); close(con)
  combos <- data.table::CJ(scenario = scen, model = models)[, .(model, scenario)]
  con <- file(file.path(ld, "factor.f32"), "wb")
  for (k in seq_len(nrow(combos))) {
    mi <- match(combos$model[k], models); s <- match(combos$scenario[k], scen)
    fac <- array(0, c(D, 17L, 3L, Y))
    for (yi in seq_len(Y)) {
      fac[, , 1, yi] <- if (combos$scenario[k] == "ssp245") 1 else f0[, , 1]
      fac[, , 2, yi] <- f0[, , 2] * (1 + 0.01 * mi) * (1 + 0.005 * s * (yi - 1))
      fac[, , 3, yi] <- f0[, , 3] * (1 - 0.003 * s * (yi - 1) / 10)
    }
    writeBin(as.vector(fac), con, size = 4L)
  }
  close(con)
  combos[, `:=`(years_present = paste(years, collapse = ","), n_years = Y)]
  meta <- x[c("draws", "sexes", "ages", "causes", "quantities", "ref_scenario", "format")]
  meta$location_id <- loc; meta$years <- years; meta$combos <- combos; meta$temp <- data.table::data.table()
  saveRDS(meta, file.path(ld, "meta.rds"))
}
cat("big compact written to", OUT, "\n")
