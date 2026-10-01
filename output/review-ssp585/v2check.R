# ssp585 test, repaired bundle (v2) vs v1: changed rows, pre-registered Colombia values,
# sample draw means, manifests, year-to-year smoothness. Writes P.csv to $REVIEW_TMP
# (set REVIEW_TMP to the same directory before running v2check2.R).
suppressPackageStartupMessages(library(data.table))
R <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp585"
v1 <- file.path(R,"bundle"); v2 <- file.path(R,"bundle-v2/bundle")
k <- c("location_id","model","scenario","year")
cmp <- function(f){
  a <- fread(file.path(v1,f)); b <- fread(file.path(v2,f))
  cat("\n==", f, "rows v1", nrow(a), "v2", nrow(b), "\n")
  m <- merge(a,b,by=k,suffixes=c(".1",".2"),all=TRUE)
  cat("rows only in one:", m[is.na(n_draws.1)|is.na(n_draws.2),.N], "\n")
  num <- setdiff(names(a), k)
  num <- num[sapply(a[,..num], is.numeric)]
  d <- m[, .(maxrel = max(sapply(num, function(v){x<-get(paste0(v,".1"));y<-get(paste0(v,".2")); r<-abs(y-x)/pmax(abs(x),1e-9); max(c(0,r[is.finite(r)]))}))), by=k]
  ch <- d[maxrel > 1e-12]
  cat("changed rows:", nrow(ch), "\n"); print(ch[, .(N=.N, yrs=paste(min(year),max(year),sep="-"), maxrel=signif(max(maxrel),3)), by=.(location_id, model, scenario)])
  invisible(m)
}
p <- cmp("summary_pipeline/national_by_year.csv")
w <- cmp("summary_workflow_b/national_by_year.csv")
cmp2 <- function(f){ a<-fread(file.path(v1,f)); b<-fread(file.path(v2,f)); cat("\n==",f,"identical:", isTRUE(all.equal(a,b)), " rows", nrow(a), nrow(b), "\n") }
cmp2("summary_pipeline/by_cause.csv"); 
# Colombia pre-registered
P <- fread(file.path(v2,"summary_pipeline/national_by_year.csv")); W <- fread(file.path(v2,"summary_workflow_b/national_by_year.csv"))
cat("\nColombia access-cm2 unscaled:\n"); print(P[location_id==125 & model=="access-cm2-r1i1p1f1", .(scenario,year,deaths_nonopt_mean,deaths_heat_mean,deaths_cold_mean,yll_nonopt_mean)][year %in% c(2022,2050)])
cat("Colombia access-cm2 scaled:\n"); print(W[location_id==125 & model=="access-cm2-r1i1p1f1" & scenario=="ssp585" & year %in% c(2022,2050), .(year,deaths_nonopt_attrib_mean,deaths_heat_attrib_mean,deaths_cold_attrib_mean,yll_nonopt_mean)])
# sample files
s <- readRDS(file.path(v2,"sample/cckp/125/access-cm2-r1i1p1f1-ssp585/burden_2022.rds")); setDT(s)
dcol <- intersect(c("draw","draw_id"), names(s))
cat("\nsample burden ssp585 2022 cols:", paste(names(s), collapse=","), "\n")
print(s[, .(nonopt=sum(deaths_nonopt), heat=sum(deaths_heat), cold=sum(deaths_cold)), by=dcol][, lapply(.SD, mean), .SDcols=c("nonopt","heat","cold")])
# workflow B manifest: 119 combos have new ok rows?
M <- fread(file.path(v2,"manifests/workflow_b_batch_manifest.csv"))
cat("\nWB manifest ok rows after 2026-09-29T12:00 by loc/model:\n"); print(M[status=="ok" & run_ts > "2026-09-29T12", .(n=.N, yrs=paste(range(year),collapse="-"), first=min(run_ts)), by=.(location_id,model,target)])
B <- fread(file.path(v2,"manifests/burden_manifest_ssp585.csv"))
cat("burden manifest ok rows after 2026-09-29T12:\n"); print(B[status=="ok" & run_ts > "2026-09-29T12", .(n=.N, yrs=paste(range(year),collapse="-"), first=min(run_ts), last=max(run_ts)), by=.(location_id,model)])
cat("burden manifest non-ok non-skip statuses since 2026-09-28:\n"); print(B[run_ts >= "2026-09-28" , .N, by=status])
# smoothness: ensemble (27 models with both) per location/scenario by year
E <- P[, .(nonopt=mean(deaths_nonopt_mean), heat=mean(deaths_heat_mean), cold=mean(deaths_cold_mean), nmed=mean(deaths_nonopt_median)), by=.(location_id,year)][order(location_id,year)]
E[, jump := abs(nonopt/shift(nonopt)-1), by=location_id]
cat("\nlargest year-to-year change in ensemble-mean unscaled ssp585 non-optimal deaths, per location:\n"); print(E[, .SD[which.max(jump)], by=location_id][, .(location_id, year, jump=round(jump,4))])
EW <- W[scenario=="ssp585", .(nonopt=mean(deaths_nonopt_attrib_mean)), by=.(location_id,year)][order(location_id,year)]
EW[, jump := abs(nonopt/shift(nonopt)-1), by=location_id]
cat("scaled:\n"); print(EW[, .SD[which.max(jump)], by=location_id][, .(location_id, year, jump=round(jump,4))])
fwrite(P, file.path(Sys.getenv("REVIEW_TMP", tempdir()), "P.csv"))
