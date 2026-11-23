suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); TB <- a[1]; OLD <- a[2]
old <- rbind(fread(file.path(OLD, "country_totals.csv"))[, unit := as.character(location_id)],
             fread(file.path(OLD, "world_and_income_groups_totals.csv")), fill = TRUE)[slice == "all"]
fs <- list.files(file.path(TB, "main"), "\\.csv\\.gz$", full.names = TRUE)
new <- rbindlist(lapply(fs, function(f) fread(cmd = paste("zcat", shQuote(f)))[cause == "all" & age == "all" & sex == "both"]), fill = TRUE)
cols <- c("mean","lower95","diff_mean","diff_lower95","diff_upper95")
m <- merge(old[, c("unit","period","scenario","measure",cols), with=FALSE], new[, c("unit","period","scenario","measure",cols), with=FALSE], by=c("unit","period","scenario","measure"), suffixes=c(".old",".new"))
for (cl in cols[-1]) {
 o <- m[[paste0(cl,".old")]]; n <- m[[paste0(cl,".new")]]
 m[, (paste0("rel_",cl)) := abs(n-o)/pmax(abs(o),1)]
 m[, (paste0("relmean_",cl)) := abs(n-o)/pmax(abs(mean.old),1)]
 m[, (paste0("abs_",cl)) := abs(n-o)]
}
bad <- m[rel_lower95>1e-4 | rel_diff_mean>1e-4 | rel_diff_lower95>1e-4 | rel_diff_upper95>1e-4]
cat("bad rows", nrow(bad), "\n")
options(width=250)
print(bad[, .(unit,period,scenario,measure,mean.old,lower95.old,lower95.new,diff_mean.old,diff_mean.new,diff_lower95.old,diff_lower95.new,diff_upper95.old,diff_upper95.new)][order(-abs(diff_mean.old))], nrows=100)
cat("max change relative to the level (mean), all rows:\n")
print(m[, lapply(.SD, max, na.rm=TRUE), .SDcols=patterns("^relmean_")])
cat("max absolute change in bad rows:\n")
print(bad[, lapply(.SD, max, na.rm=TRUE), .SDcols=patterns("^abs_")])
cat("units in bad:\n"); print(bad[, .N, by=unit][order(-N)])
cat("top rows by change relative to the level:\n")
m[, worst := pmax(relmean_lower95, relmean_diff_lower95, relmean_diff_upper95, relmean_diff_mean)]
print(m[order(-worst)][1:8, .(unit,period,scenario,measure,mean.old,lower95.old,lower95.new,diff_lower95.old,diff_lower95.new,diff_upper95.old,diff_upper95.new,worst)])
cat("rows with worst > 1e-5:", m[worst>1e-5, .N], "of", nrow(m), "\n")
m[, scale := pmax(abs(lower95.old), abs(mean.old), 1)]
m[, w2 := pmax(abs(lower95.new-lower95.old), abs(diff_lower95.new-diff_lower95.old), abs(diff_upper95.new-diff_upper95.old), abs(diff_mean.new-diff_mean.old), na.rm=TRUE)/scale]
cat("max change relative to max(|mean|,|lower95|):", max(m$w2, na.rm=TRUE), "; rows > 1e-5:", m[w2>1e-5,.N], "\n")
