# Build minimal per-location mortality inputs for the local validation of the
# uncertainty-draws fix (handoff step E). Source: the production run's
# by_cause.csv total cause deaths (access-cm2-r1i1p1f1, ssp245, 2022), placed
# in a single (age_group_id = 0, sex_id = 1) bin. Deaths x PAF burden totals
# are unaffected by the age/sex split (PAF broadcasts within cause), so this
# validates deaths_nonopt against replica/production; YLLs from these files
# are NOT meaningful. Derived-TMREL weights from these files equal the
# replica-B point weights by construction.
suppressPackageStartupMessages(library(data.table))
setwd("/var/home/aoz/code/wbg-climate-health-burden-projections")
LOCS <- c(6, 11, 13, 114, 125, 131, 135, 145, 171, 190)
bc <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245/by_cause.csv")
bc <- bc[model == "access-cm2-r1i1p1f1" & year == 2022 & location_id %in% LOCS]
for (loc in LOCS) {
  m <- bc[location_id == loc,
          .(location_id, year_id = 2022L, age_group_id = 0L, sex_id = 1L,
            acause, deaths)]
  stopifnot(nrow(m) == 17)
  out <- sprintf("data/mortality/%d_mortality_validation.rds", loc)
  saveRDS(m, out)
  cat(out, ":", nrow(m), "causes,", round(sum(m$deaths)), "deaths\n")
}
