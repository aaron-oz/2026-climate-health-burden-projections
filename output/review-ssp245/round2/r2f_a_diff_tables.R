# ssp245 round-2 follow-up review (2026-09-28), see docs/reviews/ssp245-round2-followup-review-2026-09-28.org. Run from the main checkout (needs data/erf symlink).
library(data.table)
R <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2"
key <- c("location_id","model","scenario","year")
for (f in c("by_cause.csv","by_age_sex.csv","coverage.csv")) {
  o <- fread(file.path(R,"review_bundle/summary",f)); n <- fread(file.path(R,"followup-review-bundle/review_bundle/summary",f))
  k <- intersect(names(o), c(key,"acause","age_group_id","sex_id"))
  num <- setdiff(names(o)[sapply(o,is.numeric)], k)
  cat("\n==", f, "rows old/new:", nrow(o), nrow(n), " cols equal:", identical(names(o),names(n)), "\n")
  m <- merge(o, n, by=k, suffixes=c(".o",".n"), all=TRUE)
  cat("unmatched rows:", sum(!complete.cases(m[, paste0(num[1],c(".o",".n")), with=FALSE])), "\n")
  d <- m[, .(k=do.call(paste, .SD)), .SDcols=k]
  rel <- sapply(num, function(c) { a<-m[[paste0(c,".o")]]; b<-m[[paste0(c,".n")]]; fifelse(abs(b-a)<1e-9, 0, abs(b-a)/pmax(abs(a),1e-9)) })
  ch <- apply(rel,1,function(r) any(r>1e-9, na.rm=TRUE) || any(is.na(r)!=FALSE & FALSE))
  cat("rows changed:", sum(ch), "\n")
  if (sum(ch)) print(unique(m[ch, .(location_id, model, year)]))
  if (f=="national_by_year.csv") print(m[ch, .(location_id,year, nonopt_old=deaths_nonopt_mean.o, nonopt_new=deaths_nonopt_mean.n, heat_old=deaths_heat_mean.o, heat_new=deaths_heat_mean.n, cold_old=deaths_cold_mean.o, cold_new=deaths_cold_mean.n, yll_old=yll_nonopt_mean.o, yll_new=yll_nonopt_mean.n, mort_old=deaths_mean.o, mort_new=deaths_mean.n)])
}
