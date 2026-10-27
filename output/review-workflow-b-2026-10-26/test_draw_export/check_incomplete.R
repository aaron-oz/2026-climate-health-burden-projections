# Check the pooling script's handling of a model missing from part of a group:
# drop m2's ssp585 rows from location 126, pool, and expect m2 to be left out
# of World for ssp585 (and recorded), but kept for location 125.
# Usage: Rscript check_incomplete.R <fixture root> <repo root>
suppressMessages(library(data.table))
a <- commandArgs(TRUE); R <- a[1]; REPO <- a[2]
src <- file.path(R, "output/summary_workflow_b/draws"); dst <- file.path(R, "output/draws_incomplete")
unlink(dst, recursive = TRUE); dir.create(dst)
for (f in list.files(src, full.names = TRUE)) {
  d <- readRDS(f)
  if (basename(f) == "126.rds") d <- d[!(model == "m2" & scenario == "ssp585")]
  saveRDS(d, file.path(dst, basename(f)))
}
out <- file.path(R, "output/pool_incomplete")
system2("Rscript", c(file.path(REPO, "global-scripts/util_pool_workflow_b_draws.R"), paste0("--draws_dir=", dst),
                     "--model_set=paired", paste0("--out=", out)), stdout = FALSE, stderr = FALSE)
P <- fread(file.path(out, "pooled.csv")); S <- fread(file.path(out, "model_sets.csv"))
ok <- function(lbl, cond) { cat(sprintf("  %-66s %s\n", lbl, if (isTRUE(cond)) "PASS" else "FAIL")); invisible(cond) }
ok("World ssp585 pools 2 models (m2 incomplete)", all(P[unit == "World" & target == "ssp585", n_models] == 2))
ok("location 125 ssp585 still pools 3 models", all(P[unit == "125" & target == "ssp585", n_models] == 3))
ok("location 126 ssp585 pools 2 models", all(P[unit == "126" & target == "ssp585", n_models] == 2))
ok("model_sets.csv records m2 left out of World", nrow(S[model == "m2" & grepl("left out of group World", note)]) == 2)
