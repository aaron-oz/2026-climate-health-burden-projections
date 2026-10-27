# First look at Samuel's question: in which year does heat burden overtake
# cold burden in each country? Year-to-year weather noise makes single years
# unreliable, so per location and model the annual heat and cold deaths
# (scaled, draw means, 2022-2050) are each fitted with a straight line, and the
# crossover is the year the two lines cross. Reported per location as the
# median over the 24 models common to all scenarios, and the share of models
# with a crossover by 2050. "before 2022" = heat already above cold at the
# start of the fitted lines; "after 2050" = no crossing by 2050.
# This is a quick first pass: straight lines are a strong simplification, and
# no uncertainty beyond the model spread.
# Usage: Rscript heat_cold_crossover.R <workflow_b dir> <ssp245 bundle national_by_year.csv> <hierarchy xlsx>
suppressMessages({ library(data.table); library(readxl) })
a <- commandArgs(TRUE)
n <- fread(file.path(a[1], "national_by_year.csv"),
           select = c("location_id", "model", "scenario", "year", "deaths_heat_attrib_mean", "deaths_cold_attrib_mean"))[scenario != "ssp245"]
r <- fread(a[2], select = c("location_id", "model", "scenario", "year", "deaths_heat_mean", "deaths_cold_mean"))
setnames(r, c("deaths_heat_mean", "deaths_cold_mean"), c("deaths_heat_attrib_mean", "deaths_cold_attrib_mean"))
n <- rbind(n, r)
common <- Reduce(intersect, split(n$model, n$scenario))
n <- n[model %in% common]
cross <- function(y, h, c) {
  d <- coef(lm(I(h - c) ~ y))          # straight line through heat minus cold
  yr <- -d[1] / d[2]
  if (d[2] <= 0) return(if (d[1] + d[2] * 2022 > 0) 2021 else 2051)   # not rising
  min(max(yr, 2021), 2051)
}
x <- n[, .(yr = cross(year, deaths_heat_attrib_mean, deaths_cold_attrib_mean)), by = .(scenario, location_id, model)]
h <- as.data.table(read_excel(a[3]))[Level == 3, .(location_id = as.integer(`Location ID`), name = `Location Name`)]
h <- unique(h, by = "location_id")
loc <- x[, .(median_year = median(yr), share_models_by_2050 = mean(yr <= 2050)), by = .(scenario, location_id)]
loc[, when := fcase(median_year < 2022, "before 2022", median_year > 2050, "after 2050", default = "2022-2050")]
loc <- merge(loc, h, by = "location_id", all.x = TRUE)
fwrite(loc, "heat_cold_crossover_by_location.csv")
cat("Locations by when heat overtakes cold (median over", length(common), "models):\n")
print(dcast(loc[, .N, by = .(scenario, when)], when ~ scenario, value.var = "N"))
cat("\nssp245, crossover within 2022-2050, earliest 15:\n")
print(loc[scenario == "ssp245" & when == "2022-2050"][order(median_year)][1:15, .(name, median_year = round(median_year), share_models_by_2050)])
