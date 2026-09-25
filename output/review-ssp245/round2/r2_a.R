suppressPackageStartupMessages(library(data.table))
R2 <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/review_bundle"
R1 <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245"
REV <- "/var/home/aoz/code/wbg-climate-health-burden-projections/output/review-ssp245"  # untracked analysis CSVs from the round-1 review live in the main checkout
bm <- fread(file.path(R2, "manifests/cckp_burden_manifest.csv"))
cat("burden manifest rows:", nrow(bm), "\n")
bm[, ts := as.POSIXct(run_ts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")]
print(bm[, .N, by = .(month = format(ts, "%Y-%m"), status)][order(month, status)])
s <- bm[scenario == "ssp245" & year >= 2022 & year <= 2050]
# rows after the stage-2 email (2026-09-14)
sep <- s[ts >= as.POSIXct("2026-09-12", tz = "UTC")]
cat("\nSep-2026 rows by day/status:\n"); print(sep[, .N, by = .(day = as.Date(ts), status)][order(day)])
cat("Sep span:", format(min(sep$ts)), "to", format(max(sep$ts)), "\n")
cat("Sep ok elapsed_s quantiles:\n"); print(quantile(sep[status == "ok"]$elapsed_s, c(0, .05, .5, .95, 1)))
# latest ok per combo
lastok <- s[status == "ok", .SD[which.max(ts)], by = .(model, year, location_id)]
cat("\ncombos with an ok row:", nrow(lastok), "\n")
cat("latest ok before 2026-09-14:", lastok[ts < as.POSIXct("2026-09-14", tz = "UTC"), .N], "\n")
print(lastok[, .N, by = .(day = as.Date(ts))][order(day)])
# latest row of any status per combo
lastany <- s[, .SD[which.max(ts)], by = .(model, year, location_id)]
print(lastany[, .N, by = status])
print(head(lastany[status != "ok"], 10))
cat("\nSep messages (non-empty):\n"); print(sep[message != "", .N, by = .(status, msg = substr(message, 1, 120))][order(-N)][1:15])
