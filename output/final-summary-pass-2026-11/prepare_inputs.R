# prepare_inputs.R: the small input files util_final_tables.R needs besides the
# compact copy. Run on our side; the outputs travel with the code.
#   population.csv  location_id, year, age_group_id (0, 5, ..., 80; 80 = 80+), sex_id, pop:
#                   IHME's forecast population (mean over its draws), 2022-2050, from
#                   review/workflow-b-2026-11-05-review/pop_by_loc.rds (the population
#                   the 2026-11 report's rates used). Ages 80-84, 85-89, 90-94, 95+ summed into 80+.
#   groups.csv      location_id, group: the four World Bank income groups, "Low and middle
#                   income" (low + lower middle + upper middle), the seven World Bank
#                   regions, from output/report-multimodel-2026-11/income_groups_by_location.csv
#                   (official CLASS_2026_10_08.csv). World is added by util_final_tables.R.
#   names.csv       location_id, name
# Usage (repo root, emacs-r distrobox):
#   Rscript output/final-summary-pass-2026-11/prepare_inputs.R <pop_by_loc.rds> <out dir>
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); POPRDS <- a[1]; OUT <- a[2]
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
inc <- fread("output/report-multimodel-2026-11/income_groups_by_location.csv", encoding = "UTF-8")
stopifnot(nrow(inc) == 204)

p <- setDT(readRDS(POPRDS))
amap <- data.table(age_name = c("Under 5", paste(seq(5, 75, 5), "to", seq(9, 79, 5)), "80 to 84", "85 to 89", "90 to 94", "95 plus"),
                   age_group_id = c(seq(0L, 75L, 5L), rep(80L, 4)))
p <- merge(p, amap, by = "age_name")
p[, sex_id := fifelse(sex_name == "Male", 1L, fifelse(sex_name == "Female", 2L, NA_integer_))]
p <- p[!is.na(sex_id), .(pop = sum(pop)), by = .(location_id, year, age_group_id, sex_id)]
stopifnot(uniqueN(p$location_id) == 204, all(inc$location_id %in% p$location_id),
          p[, .N, by = .(location_id, year)][, all(N == 34L)])
setorder(p, location_id, year, sex_id, age_group_id)
fwrite(p, file.path(OUT, "population.csv"))
cat("population: ", nrow(p), " rows; world 2022 = ", format(p[year == 2022, sum(pop)], big.mark = ","),
    "; world 2050 = ", format(p[year == 2050, sum(pop)], big.mark = ","), "\n", sep = "")

g <- rbind(inc[, .(location_id, group = income)], inc[, .(location_id, group = region)],
           inc[income %in% c("Low income", "Lower middle income", "Upper middle income"), .(location_id, group = "Low and middle income")])
g <- g[!is.na(group) & nzchar(group)]
fwrite(g, file.path(OUT, "groups.csv"))
print(g[, .N, by = group])
fwrite(inc[, .(location_id, name)], file.path(OUT, "names.csv"))
