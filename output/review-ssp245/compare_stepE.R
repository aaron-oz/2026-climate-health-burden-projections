# Step E comparison: the REAL pipeline under (A = default released-recycled,
# B = derived per-draw TMRELs, Bc = B + measured era5_sd/sd_corr noise)
# against (i) the ssp245 production values, (ii) the replica predictions
# (analysis17_2x2_sdfield_all204.csv), and (iii) GBD 2019.
# All runs: access-cm2-r1i1p1f1, ssp245, 2022, 500 draws, validation
# mortality (production by_cause totals, point weights).
suppressPackageStartupMessages(library(data.table))
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
OUT <- "output/review-ssp245"
LOCS <- c(114, 131, 145, 13, 190, 11, 125, 135, 171, 6)

grab <- function(mode) {
  rbindlist(lapply(LOCS, function(loc) {
    f <- sprintf("%s/stepE/%s/burden_%d.rds", OUT, mode, loc)
    if (!file.exists(f)) return(NULL)
    b <- setDT(readRDS(f))
    data.table(location_id = loc, val = b[, sum(deaths_nonopt)])
  }))
}
P <- Reduce(function(a, b) merge(a, b, by = "location_id"),
            list(setnames(grab("modeA"),  "val", "pipeA"),
                 setnames(grab("modeB"),  "val", "pipeB"),
                 setnames(grab("modeBc"), "val", "pipeBc")))

R17 <- fread(file.path(OUT, "analysis17_2x2_sdfield_all204.csv"))
P <- merge(P, R17[, .(location_id, location_name, repA = A, repB = B, repBc = Bc,
                      production, paper, lo, hi)], by = "location_id")
P[, `:=`(pipeA_vs_prod = pipeA / production,
         pipeA_vs_repA = pipeA / repA,
         pipeB_vs_repB = pipeB / repB,
         pipeBc_vs_repBc = pipeBc / repBc,
         B_in_ui  = pipeB  >= lo & pipeB  <= hi,
         Bc_in_ui = pipeBc >= lo & pipeBc <= hi)]
fwrite(P, file.path(OUT, "stepE_validation_comparison.csv"))
print(P[, .(location_name, production = round(production), pipeA = round(pipeA),
            pipeB = round(pipeB), pipeBc = round(pipeBc),
            repB = round(repB), paper = round(paper))])
cat("\nratios:\n")
print(P[, .(location_name,
            pipeA_vs_prod   = round(pipeA_vs_prod, 3),
            pipeA_vs_repA   = round(pipeA_vs_repA, 3),
            pipeB_vs_repB   = round(pipeB_vs_repB, 3),
            pipeBc_vs_repBc = round(pipeBc_vs_repBc, 3),
            B_in_ui, Bc_in_ui)])
