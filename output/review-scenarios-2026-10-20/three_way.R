suppressMessages(library(data.table)); D <- commandArgs(TRUE)[1]
n <- rbindlist(lapply(c("ssp245","ssp370","ssp585"), function(s) fread(file.path(D,s,s,"national_by_year.csv"),
       select=c("location_id","model","scenario","year","deaths_heat_mean","deaths_cold_mean","deaths_nonopt_mean"))))
cm <- Reduce(intersect, split(n$model, n$scenario)); cat("common models", length(cm), "\n")
n <- n[model %in% cm]
g <- n[, .(h=sum(deaths_heat_mean), c=sum(deaths_cold_mean), t=sum(deaths_nonopt_mean)), by=.(scenario,model,year)][
       , .(heat=round(mean(h)), cold=round(mean(c)), nonopt=round(mean(t))), by=.(scenario,year)]
print(dcast(g[year %in% c(2022,2030,2040,2050)], year ~ scenario, value.var=c("heat","cold")))
d <- n[year>=2041, .(h=sum(deaths_heat_mean)), by=.(scenario,location_id,model)]
w <- dcast(d, location_id+model ~ scenario, value.var="h")
cat("(location, model) pairs 2041-2050 heat: 585>245", w[,mean(ssp585>ssp245)], " 585>370", w[,mean(ssp585>ssp370)], " 370>245", w[,mean(ssp370>ssp245)], "\n")
pm <- d[, .(h=sum(h)), by=.(scenario,model)]; pw <- dcast(pm, model~scenario, value.var="h")
cat("models with global 2041-2050 heat ordered 245<370<585:", pw[, sum(ssp245<ssp370 & ssp370<ssp585)], "of", nrow(pw), "; 585 highest:", pw[, sum(ssp585>pmax(ssp245,ssp370))], "\n")
