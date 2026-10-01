# ssp585 test v2: row-by-row recompute of the scaled Colombia 2022 sample, and
# Haiti / Maldives / Kuwait / Indonesia ensemble trajectories vs ssp245 round 2.
# Run after v2check.R with the same REVIEW_TMP.
suppressPackageStartupMessages(library(data.table))
SCRIPTS_DIR <- "/var/home/aoz/code/wbg-climate-health-burden-projections/global-scripts"
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
source(file.path(SCRIPTS_DIR, "util_workflow_b_ratio.R"))
v2 <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp585/bundle-v2/bundle"
inp <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/followup-review-bundle/review_bundle/inputs"
mort <- paste(list.files(inp, "^125_mortality_ihme_.*_draws\\.rds$", full.names=TRUE), collapse=",")
out <- file.path(Sys.getenv("REVIEW_TMP", tempdir()), "wb_2022_local.rds")
apply_ratio(ref_burden = file.path(v2,"sample/cckp/125/access-cm2-r1i1p1f1-ssp245/burden_2022.rds"),
            target_burden = file.path(v2,"sample/cckp/125/access-cm2-r1i1p1f1-ssp585/burden_2022.rds"),
            ihme_mortality = mort, output = out, location_id = 125L,
            model = "access-cm2-r1i1p1f1", scenario = "ssp585", verbose = FALSE)
a <- as.data.table(readRDS(out)); b <- as.data.table(readRDS(file.path(v2,"sample/workflow_b/125/access-cm2-r1i1p1f1-ssp585/wb_2022.rds")))
cat("rows local", nrow(a), "caspar", nrow(b), "; same columns:", identical(sort(names(a)), sort(names(b))), "\n")
keys <- intersect(c("year","subloc_id","age_group_id","sex_id","acause","draw","location_id"), names(a))
m <- merge(a, b, by = keys, suffixes = c(".l",".c"))
cat("matched rows", nrow(m), "\n")
for (v in setdiff(names(a)[sapply(a, is.numeric)], keys)) {
  x <- m[[paste0(v,".l")]]; y <- m[[paste0(v,".c")]]
  cat(sprintf("  %-22s max abs diff %.3g\n", v, max(abs(x-y), na.rm=TRUE)))
}
# Haiti and Maldives shock tail / smoothness vs ssp245 round 2
P <- fread(file.path(Sys.getenv("REVIEW_TMP", tempdir()), "P.csv"))
S <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/followup-review-bundle/review_bundle/summary/national_by_year.csv")
S <- S[location_id %in% c(14,114,145,11) & year %in% 2022:2050]
f <- function(D, lab) D[location_id %in% c(14,114,145,11), .(mean=mean(deaths_nonopt_mean), median=mean(deaths_nonopt_median)), by=.(location_id,year)][order(location_id,year)][, `:=`(scen=lab, jump=nonopt_j <- mean/shift(mean)-1), by=location_id]
A <- rbind(f(P,"ssp585"), f(S,"ssp245"))
A[, gap := (mean-median)/abs(median)]
cat("\nensemble mean of per-model mean and median, selected locations:\n")
print(dcast(A, location_id+year ~ scen, value.var=c("mean","median","jump"))[, lapply(.SD, function(z) if (is.numeric(z)) signif(z,4) else z)], nrows=200)
cat("\nHaiti mean-median gap, share of model-years with |gap|>0.10: ssp585",
    P[location_id==114, mean(abs(deaths_nonopt_mean-deaths_nonopt_median)/abs(deaths_nonopt_median) > 0.10)],
    " ssp245", S[location_id==114, mean(abs(deaths_nonopt_mean-deaths_nonopt_median)/abs(deaths_nonopt_median) > 0.10)], "\n")
cat("Maldives same: ssp585", P[location_id==14, mean(abs(deaths_nonopt_mean-deaths_nonopt_median)/abs(deaths_nonopt_median) > 0.10)],
    " ssp245", S[location_id==14, mean(abs(deaths_nonopt_mean-deaths_nonopt_median)/abs(deaths_nonopt_median) > 0.10)], "\n")
cat("ssp245 models:", S[, uniqueN(model)], "\n")
