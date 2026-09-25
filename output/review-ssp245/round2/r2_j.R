suppressPackageStartupMessages(library(data.table))
SP <- "output/review-ssp245/round2/work"
m <- readRDS(file.path(SP, "m.rds"))
print(m[!(deaths_nonopt_mean_2 > deaths_nonopt_mean_1), .(location_id, model, year, deaths_nonopt_mean_1, deaths_nonopt_mean_2)])
rc <- fread("/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/review_bundle/sample_normalised/reconstruction_check.csv")
print(rc[, .N, by = reconstructs])
# ensemble 2022 for the stale four: how much does their one stale model move the 27-model mean? use B/A ratio
a <- fread(file.path(SP, "replicaB_check.csv"))[location_id %in% c(50, 89, 101, 126), .(location_id, location_name, BoverA = B / A)]
e <- m[year == 2022 & location_id %in% c(50, 89, 101, 126), .(ens = mean(deaths_nonopt_mean_2), acc = deaths_nonopt_mean_2[model == "access-cm2-r1i1p1f1"]), by = location_id]
x <- merge(a, e); x[, ens_shift_pct := 100 * acc * (BoverA - 1) / 27 / ens]; print(x)
