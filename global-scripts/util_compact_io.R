# util_compact_io.R: read (and rebuild from) the compact copy written by
# util_export_compact.R. Base R only, so it runs anywhere R does; copied into
# the compact copy's folder as read_compact.R.
#
# The compact copy holds, per location:
#   ihme   [draw, sex, age, cause, year]  IHME forecast deaths (same in every model and scenario)
#   factor [draw, cause, quantity, year]  per (model, scenario); quantity = S (scale factor),
#                                         paf_heat, paf_cold
# Attributable deaths for one (model, scenario, year), at full detail:
#   heat[draw, sex, age, cause] = ihme[draw, sex, age, cause] x S[draw, cause] x paf_heat[draw, cause]
#   cold likewise with paf_cold. rebuild_combo() returns exactly that.
#
# Example:
#   source("read_compact.R")
#   x <- read_compact("compact", 125)                     # meta + ihme array
#   f <- read_factor(x, "access-cm2-r1i1p1f1", "ssp585")  # [draw, cause, quantity, year]
#   r <- rebuild_combo(x, f, 2030)                        # list(heat, cold): [draw, sex, age, cause]

`%||%` <- function(a, b) if (is.null(a)) b else a

# Round doubles to 4-byte float precision (what the files store).
f32 <- function(x) readBin(writeBin(as.double(x), raw(), size = 4L), "double", n = length(x), size = 4L)

read_compact <- function(dir, loc) {
  ld <- file.path(dir, loc)
  meta <- readRDS(file.path(ld, "meta.rds"))
  D <- length(meta$draws); Y <- length(meta$years)
  n <- D * 2L * 17L * 17L * Y
  con <- file(file.path(ld, "ihme.f32"), "rb"); on.exit(close(con))
  ihme <- array(readBin(con, "double", n = n, size = 4L), c(D, 2L, 17L, 17L, Y),
                dimnames = list(NULL, meta$sexes, meta$ages, meta$causes, meta$years))
  c(meta, list(dir = ld, ihme = ihme))
}

# Factor block for one (model, scenario): [draw, cause, quantity, year], NaN where a year is missing.
read_factor <- function(x, model, scenario) {
  k <- which(x$combos$model == model & x$combos$scenario == scenario)
  if (length(k) != 1) stop("combo not found: ", model, " ", scenario)
  D <- length(x$draws); Y <- length(x$years); blk <- D * 17L * 3L * Y
  con <- file(file.path(x$dir, "factor.f32"), "rb"); on.exit(close(con))
  if (k > 1) seek(con, where = (k - 1) * blk * 4, origin = "start")
  array(readBin(con, "double", n = blk, size = 4L), c(D, 17L, 3L, Y),
        dimnames = list(NULL, x$causes, x$quantities, x$years))
}

# All factor blocks at once: [draw, cause, quantity, year, combo].
read_factors <- function(x) {
  D <- length(x$draws); Y <- length(x$years); K <- nrow(x$combos)
  con <- file(file.path(x$dir, "factor.f32"), "rb"); on.exit(close(con))
  array(readBin(con, "double", n = D * 17L * 3L * Y * K, size = 4L), c(D, 17L, 3L, Y, K))
}

# Full-detail attributable deaths for one year of one combo.
rebuild_combo <- function(x, fac, year) {
  yi <- match(year, x$years); D <- length(x$draws)
  ih <- x$ihme[, , , , yi]                                   # [draw, sex, age, cause]
  S <- fac[, , 1, yi]; ph <- fac[, , 2, yi]; pc <- fac[, , 3, yi]  # [draw, cause]
  # broadcast [draw, cause] over sex and age
  b <- function(m) aperm(array(m, c(D, 17L, 2L, 17L)), c(1, 3, 4, 2))
  list(heat = ih * b(S * ph), cold = ih * b(S * pc))
}
