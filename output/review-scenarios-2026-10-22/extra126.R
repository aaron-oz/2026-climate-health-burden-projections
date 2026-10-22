suppressMessages(library(data.table)); D <- commandArgs(TRUE)[1]
S <- c("ssp126","ssp245","ssp370","ssp585")
n <- rbindlist(lapply(S, function(s) fread(file.path(D,s,s,"national_by_year.csv"),
       select=c("location_id","model","scenario","year","deaths_heat_mean","deaths_cold_mean","deaths_nonopt_mean","deaths_nonopt_median","yll_nonopt_mean"))))
ms <- split(n$model, n$scenario); ms <- lapply(ms, unique)
cat("models per scenario:", sapply(ms, length), "\n")
cat("in ssp126 not ssp245:", setdiff(ms$ssp126, ms$ssp245), "\n")
cat("in ssp245 not ssp126:", setdiff(ms$ssp245, ms$ssp126), "\n")
cat("in ssp585 not ssp126:", setdiff(ms$ssp585, ms$ssp126), "\n")
src <- readLines("../../global-scripts/config.R"); i <- grep("^MODELS_ALL", src)
env <- new.env(); eval(parse(text = src[i:(i+20)][1:which(grepl("\\)\\s*$", src[i:(i+20)]))[1]]), env)
cat("MODELS_ALL", length(env$MODELS_ALL), "; not in ssp126:", setdiff(env$MODELS_ALL, ms$ssp126), "\n")
# NA per-location ratio
n1 <- n[scenario %in% c("ssp126","ssp245") & year>=2041, .(h=sum(deaths_heat_mean)), by=.(scenario,location_id,model)]
w1 <- dcast(n1, location_id+model~scenario, value.var="h")
cat("\nlocations with zero/neg ssp245 heat 2041-2050 in some model:\n"); print(w1[ssp245<=0 | ssp126<=0, .N, by=location_id])
# four-way global table on common models
cm <- Reduce(intersect, ms); cat("\ncommon models (all four)", length(cm), "\n")
g <- n[model %in% cm, .(h=sum(deaths_heat_mean), c=sum(deaths_cold_mean)), by=.(scenario,model,year)][
      , .(heat=round(mean(h)), cold=round(mean(c))), by=.(scenario,year)]
print(dcast(g[year %in% c(2022,2025,2030,2035,2040,2045,2050)], year~scenario, value.var=c("heat","cold")))
d <- n[model %in% cm & year>=2041, .(h=sum(deaths_heat_mean)), by=.(scenario,location_id,model)]
w <- dcast(d, location_id+model~scenario, value.var="h")
cat("(location,model) pairs 2041-2050 heat: 126<245", w[,mean(ssp126<ssp245)], " 126<370", w[,mean(ssp126<ssp370)], " 126<585", w[,mean(ssp126<ssp585)], "\n")
pw <- dcast(d[, .(h=sum(h)), by=.(scenario,model)], model~scenario, value.var="h")
cat("models: 126 lowest of four", pw[, sum(ssp126 < pmin(ssp245,ssp370,ssp585))], "of", nrow(pw),
    "; 126<245<585", pw[, sum(ssp126<ssp245 & ssp245<ssp585)], "\n")
# yearly global heat ratio 126/245 on 26 common models: share of models >1 per year
c2 <- intersect(ms$ssp126, ms$ssp245)
y <- n[model %in% c2 & scenario %in% c("ssp126","ssp245"), .(h=sum(deaths_heat_mean)), by=.(scenario,model,year)]
yw <- dcast(y, model+year~scenario, value.var="h")
cat("\nper year: global heat ratio 126/245 (model mean of ratios) and share of models with ratio > 1\n")
print(yw[, .(ratio=round(mean(ssp126/ssp245),3), share_up=round(mean(ssp126>ssp245),2)), by=year][year %% 2 == 0])
# early bump by location: 2026-2035 heat ratio, biggest heat locations
e <- n[model %in% c2 & scenario %in% c("ssp126","ssp245") & year %between% c(2026,2035), .(h=sum(deaths_heat_mean)), by=.(scenario,location_id,model)]
ew <- dcast(e, location_id+model~scenario, value.var="h")[, .(r=median(ssp126/ssp245), up=mean(ssp126>ssp245), h245=sum(ssp245)/length(c2)/10), by=location_id]
cat("\n2026-2035 heat ratio 126/245, median over models, 15 largest heat-burden locations\n")
print(ew[order(-h245)][1:15][, .(location_id, ratio=round(r,3), share_up=round(up,2), heat245_per_yr=round(h245))])
cat("summary of per-location 2026-2035 ratio\n"); print(summary(ew$r))
# YLL/death
cat("\nYLL per nonopt death 2050:", n[year==2050, .(r=round(sum(yll_nonopt_mean)/sum(deaths_nonopt_mean),2)), by=scenario][, paste(scenario, r)], "\n")
# shock tail locations same as ssp245?
st <- n[, .(gap=mean(abs(deaths_nonopt_mean-deaths_nonopt_median)/abs(deaths_nonopt_median) > 0.1)), by=.(scenario,location_id)][gap>0.25]
print(dcast(st, location_id~scenario, value.var="gap")[order(-ssp126)])
