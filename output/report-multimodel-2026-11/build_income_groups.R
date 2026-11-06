# build_income_groups.R: World Bank income group per GBD location, by ISO3 code,
# from the World Bank API's country list (countries.json, downloaded 2026-11-06 from
# https://api.worldbank.org/v2/country?format=json&per_page=400). The list carries
# the classification current at download; it does not state the fiscal year.
# Locations with no match stay "Not classified" and keep their own group, so World
# is still the sum of the income groups. Nothing is dropped.
# Usage: Rscript build_income_groups.R <countries.json> <shapefile .dbf> <hierarchy xlsx> <out dir>
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(foreign); library(readxl)})
a <- commandArgs(TRUE)
j <- fromJSON(a[1])[[2]]
wb <- data.table(iso3 = j$id, wb_name = j$name, income = j$incomeLevel$value)
wb <- wb[income != "Aggregates"]
s <- as.data.table(read.dbf(a[2], as.is = TRUE))
cat("dbf columns:", paste(names(s), collapse = " "), "\n")
s <- s[level == 3 & !is.na(ihme_lc_id) & ihme_lc_id != "", .(location_id = as.integer(loc_id), iso3 = toupper(ihme_lc_id))]
s <- unique(s, by = "location_id")
h <- as.data.table(read_excel(a[3]))
h <- unique(h[Level == 3, .(location_id = as.integer(`Location ID`), name = `Location Name`)], by = "location_id")
cat("locations in shapefile:", nrow(s), "; in hierarchy:", nrow(h), "\n")
m <- merge(h, s, by = "location_id", all.x = TRUE)
m <- merge(m, wb, by = "iso3", all.x = TRUE)
m[is.na(income) | income == "Not classified" | income == "", income_note := fifelse(is.na(wb_name), "no World Bank entry", "listed, not classified")]
m[is.na(income) | income == "", income := "Not classified"]
# Taiwan is absent from the API's list but is in the World Bank's classification
# spreadsheet (CLASS.xlsx, downloaded 2026-11-06, file dated April 2025) as "Taiwan,
# China", High income. Cook Islands, Niue and Tokelau are in neither.
m[iso3 == "TWN", `:=`(income = "High income", income_note = "from CLASS.xlsx, not in the API list")]
# The hierarchy spreadsheet's names were stored with a double encoding.
fix <- grepl("Ã", m$name, fixed = TRUE)
m[fix, name := iconv(name, "UTF-8", "latin1")]
Encoding(m$name) <- "UTF-8"
setcolorder(m, c("location_id", "name", "iso3", "wb_name", "income", "income_note"))
setorder(m, location_id)
fwrite(m, file.path(a[4], "income_groups_by_location.csv"))
print(m[, .N, by = income])
print(m[income == "Not classified"])
cat("name disagreements worth a look (hierarchy vs World Bank), first 30 where the first 4 letters differ:\n")
print(m[!is.na(wb_name) & tolower(substr(name, 1, 4)) != tolower(substr(wb_name, 1, 4)), .(location_id, name, iso3, wb_name, income)][1:30])
g <- rbind(m[, .(location_id, group = income)],
           m[income %in% c("Upper middle income", "Lower middle income", "Low income"), .(location_id, group = "Low and middle income")])
fwrite(g, file.path(a[4], "income_groups.csv"))
print(g[, .N, by = group])
