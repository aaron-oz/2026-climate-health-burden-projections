# Check Caspar's Workflow B batch manifest (output/workflow_b_batch_manifest.csv),
# received 2026-10-26. The manifest is append-only: one row per combo attempt
# (location x model x target x year), so the final state of a combo is its
# latest row. A "skip" means the output already existed; it is only trusted
# if an earlier "ok" row for the same combo is dated after the S = 1/(1-PAF)
# formula fix (2026-09-28).
# Usage: Rscript manifest_check.R <manifest.csv>
library(data.table)
m <- fread(commandArgs(TRUE)[1])
m[, day := substr(run_ts, 1, 10)]

cat("== rows by run day and target\n"); print(m[, .N, by = .(day, target)][order(day, target)])
cat("\n== any fail rows ever\n"); print(nrow(m[status == "fail"]))
cat("\n== ssp585 re-scale of 2026-10-01 (expect the 119 repaired combos: 114 access-cm2 19, 125 x3 models 29+29+15, 145 access-cm2 27)\n")
print(m[day == "2026-10-01", .N, by = .(location_id, model, status)])

setorder(m, location_id, model, target, year, run_ts)
fin <- m[, .SD[.N], by = .(location_id, model, target, year)]
okever <- m[status == "ok" & run_ts >= "2026-09-28T12", .(okts = max(run_ts)),
            by = .(location_id, model, target, year)]
fin <- merge(fin, okever, all.x = TRUE, by = c("location_id", "model", "target", "year"))
cat("\n== final status by target\n"); print(fin[, .N, by = .(target, status)][order(target, status)])
cat("\n== final skip with no post-fix ok row (expect none)\n"); print(nrow(fin[status == "skip" & is.na(okts)]))
cat("\n== models with output (final ok/skip), by target\n")
g <- fin[status %in% c("ok", "skip"), .(nloc = uniqueN(location_id), nyr = uniqueN(year), n = .N), by = .(target, model)]
print(g[, .(nmodels = .N, min_n = min(n), max_n = max(n), min_loc = min(nloc), min_yr = min(nyr), max_yr = max(nyr)), by = target])
cat("\n== models without output, by target\n")
print(fin[!status %in% c("ok", "skip"), .(N = .N, nloc = uniqueN(location_id)), by = .(target, model, status)][order(target, model)])
cat("\n== ssp245 self-scaling check: years\n"); print(fin[target == "ssp245", .N, by = .(year, status)])
cat("\n== run windows (Caspar's clock)\n"); print(m[, .(first = min(run_ts), last = max(run_ts)), by = .(day, target)])
cat("\n== seconds per ok combo\n"); print(m[status == "ok", as.list(round(quantile(elapsed_s, c(0, .5, .99, 1)), 2)), by = .(day, target)])
