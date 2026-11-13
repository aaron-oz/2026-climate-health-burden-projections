# build_groups_wbg.R: World Bank income group and region per GBD location, from the
# team's designated files (wbg-shapes-loc-data, confirmed official by Aaron 2026-11-13):
#   CLASS_2026_10_08.csv  World Bank classification (Economy, Code, Region, Income group)
#   WBG_adm0.csv          World Bank admin-0 boundary attributes (ISO_A3, WB_REGION, WB_INCOME)
# CLASS is the primary source. Three GBD locations are not in CLASS (Cook Islands, Niue,
# Tokelau); WBG_adm0 lists them as territories of New Zealand with New Zealand's region
# and income (EAS, HIC), and we take those, marked in group_note. WBG_adm0 is also
# compared with CLASS for every other location, and any disagreement is printed.
# Replaces build_income_groups.R (World Bank API list of 2026-11-06); the two give the
# same income group for every location CLASS covers (checked 2026-11-13).
# Groups written to income_groups.csv: the four income groups, "Low and middle income"
# (low + lower middle + upper middle), and the seven regions. "World" is added by the
# pooling script itself. Nothing is dropped: every location is in exactly one income
# group and one region.
# Usage: Rscript build_groups_wbg.R <wbg-shapes-loc-data dir> <old income_groups_by_location.csv> <out dir>
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); D <- a[1]; OLD <- a[2]; OUT <- a[3]
options(width = 200, datatable.print.nrows = 300)
cl <- fread(file.path(D, "CLASS_2026_10_08.csv"), encoding = "UTF-8")
cl <- cl[, .(iso3 = Code, wb_name = Economy, region = Region, income = `Income group`)]
stopifnot(!anyDuplicated(cl$iso3), all(cl$income %in% c("High income", "Upper middle income", "Lower middle income", "Low income")))
ad <- fread(file.path(D, "WBG_adm0.csv"), encoding = "UTF-8")
ad <- unique(ad[, .(iso3 = ISO_A3, adm_region = WB_REGION, adm_income = WB_INCOME, sov = SOV_NAME, status = WB_STATUS)], by = "iso3")
# location_id, name and iso3 for the 204 GBD locations, from the earlier builder's output
# (shapefile ihme_lc_id and the GBD hierarchy's names).
old <- fread(OLD, encoding = "UTF-8")[, .(location_id, name, iso3, income_old = income)]
stopifnot(nrow(old) == 204, !anyNA(old$iso3))
m <- merge(old, cl, by = "iso3", all.x = TRUE)
m <- merge(m, ad, by = "iso3", all.x = TRUE)
RC <- c(EAS = "East Asia & Pacific", ECS = "Europe & Central Asia", LCN = "Latin America & Caribbean",
        MEA = "Middle East, North Africa, Afghanistan & Pakistan", NAC = "North America",
        SAS = "South Asia", SSF = "Sub-Saharan Africa")
IC <- c(HIC = "High income", UMC = "Upper middle income", LMC = "Lower middle income", LIC = "Low income")
m[, group_note := ""]
fill <- is.na(m$income) & !is.na(m$adm_income) & m$adm_income %in% names(IC) & m$adm_region %in% names(RC)
m[fill, `:=`(income = IC[adm_income], region = RC[adm_region],
             group_note = paste0("not in CLASS; WBG_adm0 lists it as a territory of ", sov, " (", adm_region, ", ", adm_income, ")"))]
stopifnot(!anyNA(m$income), !anyNA(m$region))
cat("Filled from WBG_adm0:\n"); print(m[group_note != "", .(location_id, name, iso3, income, region, group_note)])
cat("\nIncome changes against the 2026-11-06 mapping:\n"); print(m[income != income_old, .(location_id, name, iso3, income_old, income)])
chk <- m[!is.na(adm_income) & group_note == "" & (IC[adm_income] != income | RC[adm_region] != region),
         .(location_id, name, iso3, income, adm_income, region, adm_region)]
cat("\nCLASS vs WBG_adm0 disagreements (CLASS is used):", nrow(chk), "\n"); print(chk)
cat("\nNot in WBG_adm0:", paste(m[is.na(adm_income), paste0(name, " (", iso3, ")")], collapse = "; "), "\n")
m <- m[, .(location_id, name, iso3, wb_name, income, region, group_note)]
setorder(m, location_id)
fwrite(m, file.path(OUT, "income_groups_by_location.csv"))
print(m[, .N, by = income]); print(m[, .N, by = region])
g <- rbind(m[, .(location_id, group = income)],
           m[income != "High income", .(location_id, group = "Low and middle income")],
           m[, .(location_id, group = region)])
fwrite(g, file.path(OUT, "income_groups.csv"))
print(g[, .N, by = group])
