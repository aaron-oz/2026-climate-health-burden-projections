suppressPackageStartupMessages(library(data.table))
SP <- "output/review-ssp245/round2/work"
R2 <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/review_bundle"
R1 <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245"
bc <- fread(file.path(R2, "summary/by_cause.csv"))
b1 <- fread(file.path(R1, "by_cause.csv"))
cat("by_cause rows new/old:", nrow(bc), nrow(b1), "\n")
x <- bc[model == "access-cm2-r1i1p1f1"]
sh <- x[, .(dis_share = deaths[acause == "inj_disaster"] / sum(deaths), dis_deaths = deaths[acause == "inj_disaster"]), by = .(location_id, year)]
s22 <- sh[year == 2022]
scr <- fread(file.path(SP, "shock_screen.csv"))
rb <- fread(file.path(SP, "replicaB_check.csv"))[, .(location_id, rB)]
z <- merge(merge(s22, scr[, .(location_id, location_name, gap_tot, gap_heat, heat_ul, mean22, med22)], by = "location_id"), rb, by = "location_id", all.x = TRUE)
# disaster share over years: max over 2022-2050
mx <- sh[, .(dis_share_max = max(dis_share), dis_share_med = median(dis_share)), by = location_id]
z <- merge(z, mx, by = "location_id")
options(width = 220)
cat("inj_disaster share of 17-cause mean deaths, access-cm2 2022, quantiles:\n"); print(quantile(z$dis_share, c(0,.25,.5,.75,.9,.95,.99,1)))
print(z[order(-dis_share)][1:25], digits = 3)
cat("\nSpearman(dis_share, gap_tot):", cor(z$dis_share, z$gap_tot, method = "spearman"), "\n")
# Per-cause heat ratio new/old for Haiti, Honduras, Venezuela, Singapore, Colombia (2022, access-cm2)
y <- merge(bc[model == "access-cm2-r1i1p1f1" & year == 2022 & location_id %in% c(114, 129, 133, 69, 125, 131)],
           b1[model == "access-cm2-r1i1p1f1" & year == 2022, .(location_id, acause, heat_old = deaths_heat, cold_old = deaths_cold)], by = c("location_id", "acause"))
y[, `:=`(rh = deaths_heat / heat_old, rc = deaths_cold / cold_old)]
print(dcast(y, acause ~ location_id, value.var = "rh"), digits = 3)
cat("disaster mean deaths by year, Haiti/Honduras/Venezuela (access-cm2):\n")
print(dcast(sh[location_id %in% c(114, 129, 133, 131, 11, 17, 15) & year %in% c(2022, 2030, 2040, 2050)], location_id ~ year, value.var = "dis_share"), digits = 3)
fwrite(z, file.path(SP, "disaster_share.csv"))
