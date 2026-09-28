# ssp245 round-2 follow-up review (2026-09-28), see docs/reviews/ssp245-round2-followup-review-2026-09-28.org. Run from the main checkout (needs data/erf symlink).
suppressPackageStartupMessages(library(data.table)); options(width=220)
H <- "/var/home/aoz/data/wb-temp-attr-projections/review/ssp245-round2/followup-review-bundle/honduras_cache/data/tmrel/derived_cache"
x <- readRDS(file.path(H,"129_2022_N500.rds")); cod <- x$cod; his <- x$tmrel
erf <- setDT(readRDS("data/erf/cache/erf_curves_draws_N500.rds"))
erf_s <- erf[daily_temp >= 66L & daily_temp <= 346L & zone >= 6]; rm(erf); invisible(gc())
draw_ids <- sort(unique(erf_s$draw))
derive <- function(wmat_fn) { out <- list()
  for (z in sort(unique(erf_s$zone))) {
    ez <- dcast(erf_s[zone == z], acause + daily_temp ~ draw, value.var = "rr")
    zc <- sort(unique(ez$acause)); tv <- sort(unique(ez$daily_temp)); dc <- as.character(draw_ids)
    blocks <- lapply(zc, function(c_) { b <- as.matrix(ez[acause == c_][match(tv, daily_temp), ..dc]); b[is.na(b)] <- 1; b })
    wmat <- wmat_fn(zc); wmat <- sweep(wmat, 2, colSums(wmat), "/")
    W <- matrix(0, length(tv), length(draw_ids))
    for (ci in seq_along(zc)) W <- W + sweep(blocks[[ci]], 2, wmat[ci, ], "*")
    out[[length(out)+1]] <- data.table(zone = z, draw = draw_ids, tmrel = tv[apply(W, 2, which.min)]) }
  rbindlist(out) }
perdraw <- function(zc, tr = identity) { m <- matrix(0, length(zc), length(draw_ids), dimnames = list(zc, NULL))
  wd <- tr(copy(cod))[acause %in% zc & draw %in% draw_ids]; m[cbind(match(wd$acause, zc), match(wd$draw, draw_ids))] <- wd$deaths; m }
A <- derive(function(zc) perdraw(zc))
cat("scheme A reproduces Caspar's Honduras cache exactly:", isTRUE(all.equal(A[order(zone,draw)]$tmrel, his[order(zone,draw)]$tmrel)), " (", nrow(A), "values)\n")
E <- derive(function(zc) perdraw(zc, function(d) d[acause != "inj_disaster"]))
sh <- cod[, .(share=sum(deaths[acause=="inj_disaster"])/sum(deaths)), by=draw]
hi <- sh[share>.3, draw]
m <- merge(A, E, by=c("zone","draw"), suffixes=c("_A","_E"))
cat("\nzone-draws that change when inj_disaster is dropped from the weights:", m[tmrel_A!=tmrel_E, .N], "of", nrow(m),
    "; in the 17 high-share draws:", m[tmrel_A!=tmrel_E & draw %in% hi, .N], "; elsewhere:", m[tmrel_A!=tmrel_E & !(draw %in% hi), .N], "\n")
cat("max |change| outside the 17 draws (C):", m[!(draw %in% hi), max(abs(tmrel_A-tmrel_E))/10], "\n")
rel <- fread("data/tmrel/tmrel_129_summaries.csv"); print(head(rel,3))
z <- m[, .(A_all=mean(tmrel_A)/10, E_all=mean(tmrel_E)/10, A_hi=mean(tmrel_A[draw %in% hi])/10, E_hi=mean(tmrel_E[draw %in% hi])/10,
   A_hi_floor=sum(tmrel_A[draw %in% hi]==66L)), by=zone]
print(z[, lapply(.SD, round, 2), by=zone])
cat("\nover zones: mean(A_all)", round(mean(z$A_all),2), " mean(E_all)", round(mean(z$E_all),2), " floored zone-draws A:", m[tmrel_A==66L,.N], " E:", m[tmrel_E==66L,.N], "\n")
