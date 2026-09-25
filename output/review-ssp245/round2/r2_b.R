suppressPackageStartupMessages(library(data.table))
R2 <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/review_bundle"
bm <- fread(file.path(R2, "manifests/cckp_burden_manifest.csv"))
bm[, ts := as.POSIXct(run_ts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")]
s <- bm[scenario == "ssp245" & year >= 2022 & year <= 2050]
sep_ok <- unique(s[status == "ok" & ts >= as.POSIXct("2026-09-12", tz = "UTC"), .(location_id, model, year)])
nb <- fread(file.path(R2, "summary/national_by_year.csv"), select = 1:5)
nb[, model := sub("-ssp245$", "", model)]
cat("nb model example:", nb$model[1], " manifest:", s$model[1], "\n")
miss <- nb[!sep_ok, on = .(location_id, model, year)]
cat("summary combos with no Sep ok burden row:", nrow(miss), "\n"); print(miss)
for (i in seq_len(nrow(miss))) print(s[location_id == miss$location_id[i] & model == miss$model[i] & year == miss$year[i]][order(ts), .(status, elapsed_s, message = substr(message,1,80), run_ts)])
cat("\nall Sep fails:\n"); print(s[status == "fail" & ts >= as.POSIXct("2026-09-12", tz = "UTC")])
for (k in which(s$status == "fail" & s$ts >= as.POSIXct("2026-09-12", tz = "UTC"))) { r <- s[k]; print(s[location_id == r$location_id & model == r$model & year == r$year & ts >= as.POSIXct("2026-09-12", tz="UTC"), .(location_id, model, year, status, run_ts)]) }
# missing-temp models: same as round 1?
mt <- s[status == "missing-temp" & ts >= as.POSIXct("2026-09-12", tz = "UTC")]
print(mt[, .(locs = uniqueN(location_id), yrs = uniqueN(year)), by = model])
cat("models in summary:", uniqueN(nb$model), "\n")
# duplicate ok rows in Sep per combo (reruns)
d <- s[status == "ok" & ts >= as.POSIXct("2026-09-12", tz = "UTC"), .N, by = .(location_id, model, year)][N > 1]
cat("combos with >1 Sep ok row:", nrow(d), "\n"); print(head(d))
