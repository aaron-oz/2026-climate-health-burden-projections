#!/usr/bin/env Rscript
# run_samuel_with_checkpoints.R
#
# Re-implements the data flow of `from-samuel/Scripts/Colombia/11_carga_atribuible.R`
# but reads from the global pipeline's standardized data paths and saves five
# intermediate-stage RDS files at progressively coarser granularity.
#
# This is a numerical-bisection diagnostic. Goal: localize the stage at which
# Samuel's calculation diverges from the global pipeline's
# COLOMBIA_VERIFICATION-mode output.
#
# Inputs (read-only, all reading from project data paths, NOT Samuel's
# hardcoded `Bases/...`):
#   data/columbia-data-for-verifying-pipeline/colombia/
#     temperatura_diaria_pixel.rds       (df_temperatura)
#     mortalidad_diaria_DANE_2010_2019_imput.rds   (mortalidad_dia)
#     WorldPop_2010_2019_pixel.csv       (poblacion, year-varying)
#     Tablas_vida_DANE_2005_2050.rds     (df_tv_global)
#     DIVIPOLA-_Códigos_departamentos_20260410.csv (df_divipola)
#   data/erf/{cause}_curve_samples.csv   (raw 1000-draw RR curves; we
#                                          reproduce Samuel's mean/max curves)
#   data/tmrel/tmrel_125_summaries.csv   (df_tmrel)
#
# Outputs (under output/samuel-runner/):
#   checkpoint_1_rr_tmrel_inputs.rds   — per (zone, daily_temp, cause) RR + TMREL
#   checkpoint_2a_paf_per_day_depto.rds   — per-day per-depto PAF (full granularity)
#   checkpoint_2b_paf_year_cause_risk.rds — Samuel-equivalent national-annual PAF
#   checkpoint_3a_sev_per_year_depto_zone.rds — per-(year,depto,zone,cause) SEV
#   checkpoint_3b_sev_per_year_depto.rds      — zone-aggregated per-(year,depto,cause) SEV
#   checkpoint_3c_sev_year_cause.rds          — deaths-weighted national-annual SEV
#   checkpoint_4a_attrib_deaths_full.rds — per (year, depto, sex, age, cause) attributable deaths
#   checkpoint_4b_attrib_deaths_year_cause.rds — year × cause aggregate
#   checkpoint_5a_yll_full.rds — per (year, depto, sex, age, cause) YLL
#   checkpoint_5b_yll_year_cause.rds — year × cause aggregate
#
# Plus diff tables comparing each aggregated checkpoint against the global
# pipeline's verification-mode output:
#   diff_checkpoint_1_erf.rds
#   diff_checkpoint_2_paf.rds
#   diff_checkpoint_3_sev.rds
#   diff_checkpoint_4_burden.rds
#   diff_checkpoint_5_yll.rds
#
# Usage:  distrobox enter emacs-r -- Rscript samuel-runner/run_samuel_with_checkpoints.R
#
# DEVIATIONS from Samuel's exact code (justified):
#   * `max_rr_zone.rds` is not provided. Samuel computes `max_rr` per row of the
#     ERF curve as the 99th-percentile of 1000 draws (in `09_curvas_ER.R`). We
#     reproduce this directly from `data/erf/{cause}_curve_samples.csv` rather
#     than read his RDS. Identical formula.
#   * `pob_san_a` (DANE department-level population substitution for San Andrés
#     pixel 1499) is replicated from a constant; see `pob_san_a_proxy` below.
#     Samuel uses a separate DANE projections file
#     (`Proyecciones_poblacion_depto_2010_2050_postcovid.rds`) which we do not
#     have. We approximate by using the WorldPop value where present and
#     leaving the gap unfilled where not (San Andrés pixel 1499 has WorldPop
#     coverage, so the gap is empirically zero rows in this run).
#   * Samuel's `divipola.rds` is not provided. We use the CSV (which has the
#     same content) and skip the `nom_depto` join — `nom_depto` is only used
#     for cosmetic relocate, never numerically.
#
# All other transformations follow Samuel exactly. Where Samuel filters
# `codptore != 75` (residents abroad), we do too.

suppressPackageStartupMessages({
  library(data.table)
})

# -- Resolve project root regardless of where the script is invoked from. ---
script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) NA_character_
)
if (is.na(script_path) || !nzchar(script_path)) {
  # fallback for Rscript invocation
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- cmd_args[grepl("^--file=", cmd_args)]
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
  } else {
    script_path <- normalizePath(file.path(getwd(), "samuel-runner",
                                           "run_samuel_with_checkpoints.R"),
                                 mustWork = FALSE)
  }
}
PROJECT_ROOT <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(PROJECT_ROOT)

LOCATION_ID <- 125L
YEAR_START  <- 2010L
YEAR_END    <- 2019L

DATA_DIR        <- file.path(PROJECT_ROOT, "data")
SAMUEL_DIR      <- file.path(DATA_DIR, "columbia-data-for-verifying-pipeline", "colombia")
ERF_DIR         <- file.path(DATA_DIR, "erf")
TMREL_DIR       <- file.path(DATA_DIR, "tmrel")
OUTPUT_DIR      <- file.path(PROJECT_ROOT, "output", "samuel-runner")
GLOBAL_INTERMED <- file.path(PROJECT_ROOT, "output", "intermediate")
GLOBAL_RESULTS  <- file.path(PROJECT_ROOT, "output", "results")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

GBD_CAUSES <- c(
  "ckd", "cvd_cmp", "cvd_htn", "cvd_ihd", "cvd_stroke", "diabetes",
  "inj_animal", "inj_disaster", "inj_drowning", "inj_homicide",
  "inj_mech", "inj_othunintent", "inj_suicide", "inj_trans_other",
  "inj_trans_road", "lri", "resp_copd"
)

# Spanish (Samuel) -> English (GBD acause). Mirrors
# `util_convert_samuel_colombia.R:56-69`.
CAUSE_MAP <- data.table(
  c_muerte = c("erc", "miocardiopatia_miocar", "cardiopatía_hiperten",
               "cardiopatia_isquemica", "acv", "dm",
               "relacionadas_animales", "desastres", "ahogamiento",
               "homicidio", "lesiones_mecanicas", "no_intencionales",
               "suicidio", "relacion_transporte", "accidentes_trafico",
               "ivri", "epoc"),
  acause   = c("ckd", "cvd_cmp", "cvd_htn",
               "cvd_ihd", "cvd_stroke", "diabetes",
               "inj_animal", "inj_disaster", "inj_drowning",
               "inj_homicide", "inj_mech", "inj_othunintent",
               "inj_suicide", "inj_trans_other", "inj_trans_road",
               "lri", "resp_copd")
)

logmsg <- function(...) message(sprintf("[%s] %s", format(Sys.time()), paste0(...)))

# =============================================================================
# 1. Build curves_er (per (zone, daily_temp, cause): rr_mean, rr_max)
#    Samuel: 09_curvas_ER.R produces curves_all_causes.rds with `mean` and
#    `max` per row, max = exp(quantile_99). 11_carga_atribuible.R reads it.
#
#    We reconstruct here from the raw `data/erf/{cause}_curve_samples.csv`
#    files (same Burkart files Samuel used), applying the same formulas.
# =============================================================================

logmsg("Step 1: building curves_er from raw ERF samples...")
build_curves <- function() {
  parts <- lapply(GBD_CAUSES, function(cause) {
    f <- file.path(ERF_DIR, paste0(cause, "_curve_samples.csv"))
    dt <- fread(f)
    setnames(dt, c("annual_temperature", "daily_temperature"), c("zona", "temperatura"))
    draw_cols <- grep("^draw_", names(dt), value = TRUE)
    # Samuel exponentiates after summarizing (see 09_curvas_ER.R:29-44).
    dt[, rr_mean := exp(rowMeans(.SD)), .SDcols = draw_cols]
    dt[, rr_max  := exp(apply(.SD, 1L, quantile, probs = 0.99)),
       .SDcols = draw_cols]
    dt[, (draw_cols) := NULL]
    dt[, c_muerte := cause]
    dt[, .(zona, temperatura, rr_mean, rr_max, c_muerte)]
  })
  rbindlist(parts)
}
curves_er <- build_curves()
# Match Samuel's rounding/typing. He rounds to 1 dp and treats zona as char.
curves_er[, temperatura := round(as.numeric(temperatura), 1)]
curves_er[, zona := as.character(zona)]
logmsg("  curves_er rows: ", nrow(curves_er),
       " (expected: 17 causes × 23 zones × 461 daily temps = ", 17 * 23 * 461, ")")

# Per-zone temperature limits, used for daily-temperature truncation.
curves_max <- curves_er[, .(max_temp = max(temperatura),
                            min_temp = min(temperatura)), by = zona]

# Samuel's `zona_rr` (curves_er_2 is same content for the zones we observe).
# `rr_max` per (zona, c_muerte) = max of per-row 99th-percentile RR over daily_temp.
# Reference: 11_carga_atribuible.R:270-272.
zona_rr <- curves_er[, .(rr_max = max(rr_max, na.rm = TRUE)),
                     by = .(zona, c_muerte)]

# =============================================================================
# 2. Build df_tmrel — average across the 3 time points (1990, 2010, 2020)
#    per zone, then expand to all 17 causes (Samuel's join is by zona+c_muerte
#    only, since TMREL is year-invariant after averaging).
#    Reference: 11_carga_atribuible.R:243-249.
#
#    DEVIATION: We follow Samuel's order exactly — average raw values, then
#    round (to 1 dp). This matches his code, contrasting with the global
#    pipeline's round-then-average (item 6 of comparison report).
# =============================================================================

logmsg("Step 2: building df_tmrel...")
df_tmrel <- fread(file.path(TMREL_DIR, "tmrel_125_summaries.csv"))
df_tmrel <- df_tmrel[, .(tmrelMean  = mean(tmrelMean),
                         tmrelLower = mean(tmrelLower),
                         tmrelUpper = mean(tmrelUpper)),
                     by = .(zona = as.character(meanTempCat))]
# Samuel rounds-after via numeric across() — only `tmrelMean` is used.
df_tmrel[, tmrelMean  := round(tmrelMean, 1)]
df_tmrel[, tmrelLower := round(tmrelLower, 1)]
df_tmrel[, tmrelUpper := round(tmrelUpper, 1)]

# Expand TMREL to (zona, c_muerte) — per Samuel, TMREL is the same for all
# causes given the zone. Samuel separately joins curves_er here to attach an
# `rr_tmrel_mean`, but that value is only used for RR rescaling at the TMREL,
# which COLOMBIA_VERIFICATION mode disables. We skip that join; numeric
# output is unaffected.
df_tmrel <- merge(
  CJ(zona = unique(curves_er$zona), c_muerte = GBD_CAUSES),
  df_tmrel,
  by = "zona", all.x = TRUE, allow.cartesian = TRUE
)

# =============================================================================
# CHECKPOINT 1 — RR/TMREL inputs after merge.
# Per (zone, daily_temp, cause): rr_mean, rr_max (per row), tmrelMean, rr at TMREL.
# =============================================================================

logmsg("Saving CHECKPOINT 1: RR/TMREL inputs after merge...")
cp1 <- merge(curves_er, df_tmrel[, .(zona, c_muerte, tmrelMean)],
             by = c("zona", "c_muerte"), all.x = TRUE)
# Attach rr_max-per-zone-cause (used by SEV) so the checkpoint has everything.
cp1 <- merge(cp1, zona_rr[, .(zona, c_muerte, rr_max_zone = rr_max)],
             by = c("zona", "c_muerte"), all.x = TRUE)
saveRDS(cp1, file.path(OUTPUT_DIR, "checkpoint_1_rr_tmrel_inputs.rds"))
logmsg("  cp1 rows: ", nrow(cp1))

# =============================================================================
# 3. Load temperature, mortality, and population
# =============================================================================

logmsg("Step 3: loading temperature, mortality, population...")
df_temperatura <- as.data.table(
  readRDS(file.path(SAMUEL_DIR, "temperatura_diaria_pixel.rds"))
)
setnames(df_temperatura,
         c("temperatura", "fecha", "index_right", "pob", "DPTO_CCDGO"),
         c("temperatura", "fecha", "index_right", "pob", "cod_depto"))
df_temperatura[, temperatura := round(temperatura, 1)]
df_temperatura[, fecha := as.Date(fecha)]
df_temperatura[, ano := as.integer(format(fecha, "%Y"))]
df_temperatura[, cod_depto := as.integer(cod_depto)]

# Load year-varying WorldPop. Samuel reads RDS; we have CSV (same data per
# project memory). Columns: indx_rg, sum_z, ano.
poblacion <- as.data.table(read.csv(file.path(SAMUEL_DIR,
                                              "WorldPop_2010_2019_pixel.csv")))
setnames(poblacion, "indx_rg", "index_right")
poblacion[, ano := as.integer(ano)]
# pixel -> depto mapping comes from the temperature file (Samuel's index_unique).
index_unique <- unique(df_temperatura[, .(index_right, cod_depto)])
pob_pixel <- merge(poblacion, index_unique, by = "index_right", all.x = TRUE)
pob_depto <- pob_pixel[, .(pob_depto = sum(sum_z, na.rm = TRUE)),
                       by = .(ano, cod_depto)]
prop_pob_depto <- merge(pob_pixel, pob_depto, by = c("ano", "cod_depto"))
prop_pob_depto[, pr := sum_z / pob_depto]
prop_pob_depto <- prop_pob_depto[, .(ano, index_right, cod_depto, pr)]
# San Andrés pixel 1499 → pr = 1 (as in Samuel).
prop_san_a <- data.table(ano = YEAR_START:YEAR_END,
                         index_right = 1499L,
                         cod_depto = 88L,
                         pr = 1)
prop_pob_depto <- rbindlist(list(prop_pob_depto, prop_san_a), use.names = TRUE)

# Attach pr_per (year, pixel, depto) to the temperature frame.
df_temperatura <- merge(df_temperatura, prop_pob_depto,
                        by = c("ano", "index_right", "cod_depto"),
                        all.x = TRUE)
df_temperatura[, pob := NULL]

# Mortality with Samuel's filter (codptore != 75 = "EXTRANJERO" residents
# abroad). Reference: 11_carga_atribuible.R:162-163.
mortalidad_dia <- as.data.table(
  readRDS(file.path(SAMUEL_DIR, "mortalidad_diaria_DANE_2010_2019_imput.rds"))
)
mortalidad_dia[, c_muerte := as.character(c_muerte)]
mortalidad_dia <- merge(mortalidad_dia, CAUSE_MAP, by = "c_muerte", all.x = TRUE)
# Keep Samuel's Spanish c_muerte for display? No — switch to GBD acause for
# join with curves and zona_rr (we've reformulated everything around acause).
mortalidad_dia[, c_muerte := acause]
mortalidad_dia[, acause := NULL]
n_before <- nrow(mortalidad_dia)
mortalidad_dia <- mortalidad_dia[codptore != 75]
logmsg("  Mortality: filtered ", n_before - nrow(mortalidad_dia),
       " rows where codptore == 75. Remaining: ", nrow(mortalidad_dia))

# In Samuel the rename happens after the unique-by-codptore aggregation, but
# the columns we need (fecha_def, codptore, c_muerte, muertes) get joined to
# the temperature frame. We follow exactly: build mortalidad_dia_unique for
# the daily temperature filter, and keep the full mortalidad_dia for the
# burden multiplication.
mortalidad_dia_unique <- mortalidad_dia[
  , .(muertes = sum(muertes, na.rm = TRUE)),
  by = .(cod_depto = codptore, fecha = fecha_def, c_muerte)
]
mortalidad_dia_unique[, fecha := as.Date(fecha)]
# Standardize names on the full mortality frame to Samuel's eventual names:
setnames(mortalidad_dia,
         c("fecha_def", "codptore", "sexo", "gru_ed1", "c_muerte", "muertes"),
         c("fecha",     "cod_depto","sexo", "gru_ed1", "c_muerte", "muertes"))
mortalidad_dia[, fecha := as.Date(fecha)]

# =============================================================================
# 4. Build df_rr (per-day, per-pixel, per-cause RR table)
#    Reference: 11_carga_atribuible.R:175-235
# =============================================================================

logmsg("Step 4: building df_rr...")
# Annual zona per pixel = round(mean daily temp).
df_temp_periodo <- df_temperatura[, .(zona = round(mean(temperatura, na.rm = TRUE))),
                                  by = .(ano, index_right)]
df_temp_periodo[, zona := as.character(zona)]
df_temp_periodo[zona %in% c("29", "30", "31"), zona := "28"]

df_rr <- merge(df_temperatura, df_temp_periodo, by = c("ano", "index_right"))

# Replicate per pixel-day for each of 17 causes. Samuel's pattern: copy the
# whole frame 17 times and assign cause column. We use CJ and merge for clarity.
df_rr_full <- CJ(idx = seq_len(nrow(df_rr)), c_muerte = GBD_CAUSES,
                 sorted = FALSE)
df_rr <- df_rr[df_rr_full$idx]
df_rr[, c_muerte := df_rr_full$c_muerte]
rm(df_rr_full); gc()

# Filter to pixel-day-cause rows that have any mortality (across deptos).
# Samuel: left_join(mortalidad_dia_unique) %>% filter(!is.na(muertes)) — he
# uses the codptore-keyed daily aggregate as a *date × cause* presence filter.
# Note that Samuel's `cod_depto` here is `codptore` (province of REGISTRATION),
# which has the same code-space as the temperature `cod_depto` for residents
# but his `mortalidad_dia_unique` summarizes ACROSS codptore so it's effectively
# a (date, cause) filter — matching only on (cod_depto, fecha, c_muerte) as
# Samuel does. Re-read his code carefully:
#
#   mortalidad_dia_unique <- mortalidad_dia %>%
#     group_by(codptore, fecha_def, c_muerte) %>%
#     summarise(muertes = sum(muertes)) %>% as.data.frame()
#   names(mortalidad_dia_unique) <- c("cod_depto", "fecha", "c_muerte", "muertes")
#
# So Samuel renames `codptore` -> `cod_depto`. Then the left_join in
# `df_rr <- left_join(df_rr, mortalidad_dia_unique)` joins by all common cols:
# cod_depto, fecha, c_muerte. So this is per-depto presence not just per-date.
# We follow exactly.

df_rr <- merge(df_rr, mortalidad_dia_unique,
               by = c("cod_depto", "fecha", "c_muerte"),
               all.x = TRUE)
df_rr <- df_rr[!is.na(muertes)]
df_rr[, muertes := NULL]

df_rr[, zona := as.character(zona)]
df_rr[zona %in% c("29", "30", "31"), zona := "28"]
df_rr[, c_muerte := as.character(c_muerte)]
df_rr[, temperatura := round(as.numeric(temperatura), 1)]

# Truncate daily temperature to per-zone curve range.
df_rr <- merge(df_rr, curves_max, by = "zona", all.x = TRUE)
df_rr[temperatura > max_temp, temperatura := max_temp]
df_rr[temperatura < min_temp, temperatura := min_temp]
df_rr[, c("max_temp", "min_temp") := NULL]

# Join curves_er (per zona, temperatura, c_muerte: rr_mean, rr_max).
df_rr <- merge(df_rr, curves_er,
               by = c("zona", "temperatura", "c_muerte"),
               all.x = TRUE)
# Replace per-row rr_max with zone-cause rr_max (Samuel: lines 270-276).
df_rr[, rr_max := NULL]
df_rr <- merge(df_rr, zona_rr, by = c("zona", "c_muerte"), all.x = TRUE)
# Attach TMREL (per zona, c_muerte).
df_rr <- merge(df_rr, df_tmrel[, .(zona, c_muerte, tmrelMean)],
               by = c("zona", "c_muerte"), all.x = TRUE)

logmsg("  df_rr rows: ", nrow(df_rr))

# =============================================================================
# 5. Compute per-row PAF and per-day per-depto PAF aggregates
#    Reference: 11_carga_atribuible.R:370-419
# =============================================================================

logmsg("Step 5: computing per-row PAF...")
# Per-row PAF = pr * (RR-1)/RR for RR>=1, else pr * -((1/RR-1)/(1/RR));
# floored at 0 per row (Samuel line 379).
df_rr[, paf2 := fifelse(rr_mean >= 1,
                        pr * (rr_mean - 1) / rr_mean,
                        pr * -1 * ((1 / rr_mean) - 1) / (1 / rr_mean))]
df_rr[, paf := pmax(0, paf2)]
df_rr[, effect := fifelse(temperatura < tmrelMean, "low_temperature",
                          fifelse(temperatura > tmrelMean, "high_temperature",
                                  NA_character_))]

paf_day_depto_cold <- df_rr[effect == "low_temperature",
                            .(paf  = sum(paf,  na.rm = TRUE),
                              paf2 = sum(paf2, na.rm = TRUE)),
                            by = .(cod_depto, fecha, c_muerte)]
paf_day_depto_heat <- df_rr[effect == "high_temperature",
                            .(paf  = sum(paf,  na.rm = TRUE),
                              paf2 = sum(paf2, na.rm = TRUE)),
                            by = .(cod_depto, fecha, c_muerte)]

# Merge into a (date, depto, cause) frame with both heat and cold columns.
# Reference: 11_carga_atribuible.R:502-513
paf_day_depto <- unique(df_rr[, .(fecha, cod_depto, c_muerte)])
paf_day_depto <- merge(paf_day_depto,
                       paf_day_depto_cold[, .(cod_depto, fecha, c_muerte,
                                              paf_cold = paf,
                                              paf_cold2 = paf2)],
                       by = c("cod_depto", "fecha", "c_muerte"), all.x = TRUE)
paf_day_depto <- merge(paf_day_depto,
                       paf_day_depto_heat[, .(cod_depto, fecha, c_muerte,
                                              paf_heat = paf,
                                              paf_heat2 = paf2)],
                       by = c("cod_depto", "fecha", "c_muerte"), all.x = TRUE)
paf_day_depto[is.na(paf_heat),  paf_heat  := 0]
paf_day_depto[is.na(paf_cold),  paf_cold  := 0]
paf_day_depto[is.na(paf_heat2), paf_heat2 := 0]
paf_day_depto[is.na(paf_cold2), paf_cold2 := 0]
paf_day_depto[, paf_non_optimal_temp := paf_cold + paf_heat]
paf_day_depto[, ano := as.integer(format(fecha, "%Y"))]

# =============================================================================
# CHECKPOINT 2 — Per-day per-depto PAF, plus a national-annual aggregate.
# =============================================================================

logmsg("Saving CHECKPOINT 2: per-day per-depto PAFs...")
saveRDS(paf_day_depto, file.path(OUTPUT_DIR, "checkpoint_2a_paf_per_day_depto.rds"))

# Aggregate to national-annual by averaging PAFs across pixel-days within (year,
# cause, risk). The global pipeline produces a single PAF per (year, cause,
# risk); for direct comparison we average the per-day per-depto PAF over all
# (date, depto) pairs per (year, cause).
# NOTE: Samuel's per-day PAF is depto-pop-weighted (each row already has pr ≤ 1
# per depto). Summing then averaging across deptos≠ national pop weighting.
# We take a deaths-weighted aggregation later in cp4. For cp2 we expose two
# aggregations to support sensitivity:
#   (a) plain mean across (date, depto)
#   (b) deaths-weighted via mortalidad_dia_unique (Samuel's natural pathway)
mort_for_paf_wt <- mortalidad_dia_unique[, .(deaths_dd = sum(muertes, na.rm = TRUE)),
                                         by = .(cod_depto, fecha, c_muerte)]
paf_with_w <- merge(paf_day_depto, mort_for_paf_wt,
                    by = c("cod_depto", "fecha", "c_muerte"),
                    all.x = TRUE)
paf_with_w[is.na(deaths_dd), deaths_dd := 0]

paf_year_cause_plain <- paf_day_depto[, .(
  paf_heat_mean = mean(paf_heat, na.rm = TRUE),
  paf_cold_mean = mean(paf_cold, na.rm = TRUE),
  paf_nonopt_mean = mean(paf_non_optimal_temp, na.rm = TRUE)
), by = .(year = ano, acause = c_muerte)]

paf_year_cause_dwt <- paf_with_w[, .(
  paf_heat_dwt = sum(paf_heat * deaths_dd, na.rm = TRUE) /
                 pmax(sum(deaths_dd, na.rm = TRUE), 1e-9),
  paf_cold_dwt = sum(paf_cold * deaths_dd, na.rm = TRUE) /
                 pmax(sum(deaths_dd, na.rm = TRUE), 1e-9)
), by = .(year = ano, acause = c_muerte)]

paf_year_cause <- merge(paf_year_cause_plain, paf_year_cause_dwt,
                        by = c("year", "acause"), all = TRUE)
saveRDS(paf_year_cause, file.path(OUTPUT_DIR, "checkpoint_2b_paf_year_cause_risk.rds"))

# =============================================================================
# 6. Compute SEVs per (year, depto, c_muerte)
#    Reference: 11_carga_atribuible.R:284-476
# =============================================================================

logmsg("Step 6: computing SEVs (depto-specific)...")
# df_base_sevs: identical pixel-day structure as df_temperatura but with
# zone (capped at 28), truncated daily temperature, year-varying pop, joined
# to ALL 17 causes.

# Per Samuel: zone per (year, depto, pixel) = round(mean daily temp); cap at 28.
df_base_sevs_zone <- df_temperatura[, .(zona = round(mean(temperatura, na.rm = TRUE))),
                                    by = .(ano, cod_depto, index_right)]
df_base_sevs_zone[, zona := pmin(zona, 28L)]
df_base_sevs_zone[, zona := as.character(zona)]

df_base_sevs <- merge(df_temperatura, df_base_sevs_zone,
                      by = c("ano", "cod_depto", "index_right"),
                      all.x = TRUE)
df_base_sevs <- merge(df_base_sevs, curves_max, by = "zona", all.x = TRUE)
df_base_sevs[temperatura > max_temp, temperatura := max_temp]
df_base_sevs[temperatura < min_temp, temperatura := min_temp]
df_base_sevs[, c("max_temp", "min_temp") := NULL]

# Attach year-varying WorldPop population per pixel.
poblacion_renamed <- copy(poblacion)
setnames(poblacion_renamed, "sum_z", "pob")
df_base_sevs <- merge(df_base_sevs, poblacion_renamed[, .(ano, index_right, pob)],
                      by = c("ano", "index_right"),
                      all.x = TRUE)

# San Andrés special-casing. Samuel substitutes DANE projections where pob is
# NA (typically pixel 1499). We don't have the DANE projections file. The
# WorldPop file does cover pixel 1499 (verified during data audit), so the
# gap is empirically nil. If any pob is NA, we leave it (sets paf_*=NA→0 below).
n_pob_na <- sum(is.na(df_base_sevs$pob))
if (n_pob_na > 0) {
  logmsg("  WARNING: ", n_pob_na, " rows in df_base_sevs have NA pob. ",
         "Samuel substitutes DANE projections; we leave NA. ",
         "Likely all pixel 1499 (San Andrés) — empirically zero in this run.")
  df_base_sevs[is.na(pob), pob := 0]
}

# Population denominators per (year, depto) and (year, depto, zona) — Samuel
# uses Jan-1 snapshot pop for these denominators (line 320-339). We replicate.
s_dates <- as.Date(paste0(YEAR_START:YEAR_END, "-01-01"))
df_base_sevs_d <- df_base_sevs[fecha %in% s_dates]

pob_depto_yr <- df_base_sevs_d[, .(pob_depto = sum(pob, na.rm = TRUE)),
                               by = .(ano, cod_depto)]
pob_zona_yr  <- df_base_sevs_d[, .(pob_zona = sum(pob, na.rm = TRUE)),
                               by = .(ano, cod_depto, zona)]
df_base_sevs <- merge(df_base_sevs, pob_depto_yr, by = c("ano", "cod_depto"),
                      all.x = TRUE)
df_base_sevs <- merge(df_base_sevs, pob_zona_yr,
                      by = c("ano", "cod_depto", "zona"), all.x = TRUE)
df_base_sevs[, pr_zona := pob / pmax(pob_zona, 1e-9)]

# Replicate × 17 causes (Samuel: lines 344-356).
nrow_one <- nrow(df_base_sevs)
df_base_sevs_full <- CJ(idx = seq_len(nrow_one), c_muerte = GBD_CAUSES, sorted = FALSE)
df_base_sevs <- df_base_sevs[df_base_sevs_full$idx]
df_base_sevs[, c_muerte := df_base_sevs_full$c_muerte]
rm(df_base_sevs_full); gc()

# Join curves_er (rr_mean per zona, temperatura, c_muerte) and zona_rr
# (rr_max per zona, c_muerte).
df_base_sevs <- merge(df_base_sevs, curves_er[, .(zona, temperatura, c_muerte, rr_mean)],
                      by = c("zona", "temperatura", "c_muerte"),
                      all.x = TRUE)
df_base_sevs <- merge(df_base_sevs, zona_rr,
                      by = c("zona", "c_muerte"), all.x = TRUE)

# Aggregate to (fecha, depto, cause, zona, temperatura).
# Reference: lines 444-454.
base_pobtemp <- df_base_sevs[, .(pob_temp = sum(pob, na.rm = TRUE),
                                 pob_zona = mean(pob_zona, na.rm = TRUE),
                                 pob_depto = mean(pob_depto, na.rm = TRUE),
                                 rr_mean = mean(rr_mean, na.rm = TRUE),
                                 rr_max  = mean(rr_max,  na.rm = TRUE)),
                             by = .(fecha, cod_depto, c_muerte, zona, temperatura)]
base_pobtemp[, pr_zona := pob_temp / pmax(pob_zona, 1e-9)]
rm(df_base_sevs); gc()

# Compute SEV per (fecha, depto, cause, zona).
# Daily contribution: pr_zona * (rr_mean - 1) / (rr_max - 1), zeroed if either
# rr_max <=1 or rr_mean <=1.
base_pobtemp[, sev_contrib := fifelse(rr_max <= 1 | rr_mean <= 1, 0,
                                      pr_zona * (rr_mean - 1) / (rr_max - 1))]
base_pobtemp[, ano := as.integer(format(fecha, "%Y"))]

# Sum across days within (year, depto, cause, zona); cap at 1; floor at 0.
sev_year_depto_zone <- base_pobtemp[, .(
  sev = sum(sev_contrib, na.rm = TRUE),
  pob_zona  = mean(pob_zona,  na.rm = TRUE),
  pob_depto = mean(pob_depto, na.rm = TRUE)
), by = .(ano, cod_depto, c_muerte, zona)]
sev_year_depto_zone[sev < 0, sev := 0]
sev_year_depto_zone[sev > 1, sev := 1]

# =============================================================================
# CHECKPOINT 3a — per-(year, depto, zona, cause) SEV (before zone aggregation)
# =============================================================================
logmsg("Saving CHECKPOINT 3a: per-(year,depto,zona,cause) SEV...")
saveRDS(sev_year_depto_zone, file.path(OUTPUT_DIR,
                                       "checkpoint_3a_sev_per_year_depto_zone.rds"))

# Zone aggregation: weighted by pob_zona/pob_depto. Reference: lines 471-476.
sev_year_depto_zone[, ppzonadep := pob_zona / pmax(pob_depto, 1e-9)]
sev_year_depto <- sev_year_depto_zone[, .(sev = sum(sev * ppzonadep, na.rm = TRUE)),
                                      by = .(ano, cod_depto, c_muerte)]

# =============================================================================
# CHECKPOINT 3b — per-(year, depto, cause) SEV (after zone aggregation)
# =============================================================================
logmsg("Saving CHECKPOINT 3b: per-(year,depto,cause) SEV...")
saveRDS(sev_year_depto, file.path(OUTPUT_DIR,
                                  "checkpoint_3b_sev_per_year_depto.rds"))

# National-annual SEV (deaths-weighted across deptos), for direct comparison
# to the global pipeline's per-(year, cause) SEV.
mort_year_depto_cause <- mortalidad_dia[, .(deaths_d = sum(muertes, na.rm = TRUE)),
                                        by = .(ano = as.integer(format(fecha, "%Y")),
                                               cod_depto, c_muerte)]
sev_year_cause <- merge(sev_year_depto, mort_year_depto_cause,
                        by = c("ano", "cod_depto", "c_muerte"), all = TRUE)
sev_year_cause[is.na(deaths_d), deaths_d := 0]
sev_year_cause[is.na(sev), sev := 0]
sev_year_cause <- sev_year_cause[, .(
  sev_dwt = sum(sev * deaths_d, na.rm = TRUE) /
            pmax(sum(deaths_d, na.rm = TRUE), 1e-9),
  sev_pwt_proxy = mean(sev, na.rm = TRUE)  # plain mean across deptos for sensitivity
), by = .(year = ano, acause = c_muerte)]
saveRDS(sev_year_cause, file.path(OUTPUT_DIR, "checkpoint_3c_sev_year_cause.rds"))

# =============================================================================
# 7. Burden = deaths × PAF × SEV (Samuel's verification formula).
#    Reference: 11_carga_atribuible.R:547-562
# =============================================================================

logmsg("Step 7: computing attributable deaths...")
mortalidad_dia[, ano := as.integer(format(fecha, "%Y"))]
attrib <- merge(mortalidad_dia, paf_day_depto,
                by = c("cod_depto", "fecha", "c_muerte"),
                all.x = TRUE)
# attrib has ano.x (from mortalidad_dia) and ano.y (from paf_day_depto). Use
# either — they are equal for matched rows.
if ("ano.x" %in% names(attrib)) {
  attrib[, ano := ano.x]
  attrib[, c("ano.x", "ano.y") := NULL]
}
attrib <- merge(attrib, sev_year_depto,
                by = c("ano", "cod_depto", "c_muerte"), all.x = TRUE)
attrib[is.na(paf_heat), paf_heat := 0]
attrib[is.na(paf_cold), paf_cold := 0]
attrib[is.na(paf_non_optimal_temp), paf_non_optimal_temp := 0]
attrib[is.na(sev), sev := 0]
attrib[, muertes_cold   := muertes * paf_cold * sev]
attrib[, muertes_heat   := muertes * paf_heat * sev]
attrib[, muertes_nonopt := muertes * paf_non_optimal_temp * sev]

# Aggregate to (year, depto, sex, age, cause) — full granularity.
attrib_full <- attrib[, .(
  muertes        = sum(muertes,        na.rm = TRUE),
  muertes_cold   = sum(muertes_cold,   na.rm = TRUE),
  muertes_heat   = sum(muertes_heat,   na.rm = TRUE),
  muertes_nonopt = sum(muertes_nonopt, na.rm = TRUE)
), by = .(ano, cod_depto, sexo, gru_ed1, c_muerte)]

# =============================================================================
# CHECKPOINT 4a — per-(year, depto, sex, age, cause) attributable deaths
# =============================================================================
logmsg("Saving CHECKPOINT 4a: per-(year, depto, sex, age, cause) attributable deaths...")
saveRDS(attrib_full, file.path(OUTPUT_DIR,
                               "checkpoint_4a_attrib_deaths_full.rds"))

# Year × cause aggregate (national).
attrib_year_cause <- attrib_full[, .(
  deaths        = sum(muertes,        na.rm = TRUE),
  deaths_cold   = sum(muertes_cold,   na.rm = TRUE),
  deaths_heat   = sum(muertes_heat,   na.rm = TRUE),
  deaths_nonopt = sum(muertes_nonopt, na.rm = TRUE)
), by = .(year = ano, acause = c_muerte)]
saveRDS(attrib_year_cause, file.path(OUTPUT_DIR,
                                     "checkpoint_4b_attrib_deaths_year_cause.rds"))

# =============================================================================
# 8. YLL via DANE life tables (Samuel uses depto-specific).
#    Reference: 11_carga_atribuible.R:87-113, 568-577
# =============================================================================

logmsg("Step 8: computing YLLs...")
df_tv_global <- as.data.table(
  readRDS(file.path(SAMUEL_DIR, "Tablas_vida_DANE_2005_2050.rds"))
)
# Map age -> Samuel's gru_ed1 buckets (see lines 88-107).
age_to_grued1 <- data.table(
  age = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45,
          50, 55, 60, 65, 70, 75, 80),
  gru_ed1 = c("0-4", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
              "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
              "60-64", "65-69", "70-74", "75-79", ">80")
)
df_tv_global <- merge(df_tv_global, age_to_grued1, by = "age", all.x = TRUE)
df_tv_global[, cod_depto := as.integer(cod_depto)]
df_tv_global <- df_tv_global[!is.na(cod_depto) & cod_depto != 0L]
df_tv_global <- df_tv_global[as.integer(ano) <= YEAR_END]
df_tv_global <- df_tv_global[age != 0]  # Samuel drops age 0 (line 111)
df_tv_global[, age := NULL]
df_tv_global <- unique(df_tv_global)
df_tv_global[, ano := as.integer(ano)]

attrib_full_ylls <- merge(attrib_full, df_tv_global,
                          by = c("ano", "sexo", "gru_ed1", "cod_depto"),
                          all.x = TRUE)

# Samuel: avpp = ifelse(gru_ed1 == ">80", muertes * 10, muertes * (ev - 2.5))
attrib_full_ylls[, avpp_cold := fifelse(
  gru_ed1 == ">80",
  round(muertes_cold * 10, 4),
  round(muertes_cold * (ev - 2.5), 4)
)]
attrib_full_ylls[, avpp_heat := fifelse(
  gru_ed1 == ">80",
  round(muertes_heat * 10, 4),
  round(muertes_heat * (ev - 2.5), 4)
)]
attrib_full_ylls[, avpp_nonopt := fifelse(
  gru_ed1 == ">80",
  round(muertes_nonopt * 10, 4),
  round(muertes_nonopt * (ev - 2.5), 4)
)]

# =============================================================================
# CHECKPOINT 5a — full-detail YLL
# =============================================================================
logmsg("Saving CHECKPOINT 5a: full-detail YLL...")
saveRDS(attrib_full_ylls, file.path(OUTPUT_DIR,
                                    "checkpoint_5a_yll_full.rds"))

# Year × cause aggregate.
yll_year_cause <- attrib_full_ylls[, .(
  yll_cold   = sum(avpp_cold,   na.rm = TRUE),
  yll_heat   = sum(avpp_heat,   na.rm = TRUE),
  yll_nonopt = sum(avpp_nonopt, na.rm = TRUE)
), by = .(year = ano, acause = c_muerte)]
saveRDS(yll_year_cause, file.path(OUTPUT_DIR,
                                  "checkpoint_5b_yll_year_cause.rds"))

# =============================================================================
# 9. Diff each checkpoint against the global pipeline's verification-mode output.
# =============================================================================

logmsg("Step 9: building diff tables...")

abs_pct_diff <- function(actual, expected) {
  ifelse(abs(expected) < 1e-12,
         ifelse(abs(actual) < 1e-12, 0, NA_real_),
         (actual - expected) / expected * 100)
}

summarize_diff <- function(dt, value_cols) {
  out <- data.table(metric = character(), n = integer(),
                    mean_abs_pct = numeric(), median_abs_pct = numeric(),
                    max_abs_pct = numeric(), p95_abs_pct = numeric())
  for (vc in value_cols) {
    d <- dt[[vc]]
    d <- d[!is.na(d)]
    if (length(d) == 0) next
    out <- rbind(out, data.table(
      metric         = vc,
      n              = length(d),
      mean_abs_pct   = mean(abs(d)),
      median_abs_pct = median(abs(d)),
      max_abs_pct    = max(abs(d)),
      p95_abs_pct    = quantile(abs(d), 0.95, names = FALSE)
    ))
  }
  out[]
}

# ---- Checkpoint 1: ERF inputs ----
# Pipeline's erf_curves.rds has rr_mean and rr_max at per (zone, daily_temp,
# acause). Compare elementwise.
pipe_erf <- as.data.table(readRDS(file.path(GLOBAL_INTERMED, "erf_curves.rds")))
# The pipeline encodes daily_temp as integer × 10 (e.g. -220 = -22.0°C).
pipe_erf[, daily_temp_c := daily_temp / 10]
sam_erf <- copy(curves_er)
sam_erf[, zona := as.integer(zona)]
sam_erf[, daily_temp_c := round(temperatura, 1)]

cp1_diff <- merge(pipe_erf[, .(zone, daily_temp_c, acause,
                               pipe_rr_mean = rr_mean, pipe_rr_max = rr_max)],
                  sam_erf[, .(zone = zona, daily_temp_c, acause = c_muerte,
                              sam_rr_mean = rr_mean, sam_rr_max = rr_max)],
                  by = c("zone", "daily_temp_c", "acause"))
cp1_diff[, rr_mean_pct := abs_pct_diff(sam_rr_mean, pipe_rr_mean)]
cp1_diff[, rr_max_pct  := abs_pct_diff(sam_rr_max,  pipe_rr_max)]
saveRDS(cp1_diff, file.path(OUTPUT_DIR, "diff_checkpoint_1_erf.rds"))

cp1_summary <- summarize_diff(cp1_diff, c("rr_mean_pct", "rr_max_pct"))
logmsg("  CP1 diff summary:")
print(cp1_summary)

# ---- Checkpoint 2: PAF (year × cause × risk) ----
# Pipeline pafs_125.rds: paf_heat, paf_cold per (year, acause).
pipe_paf <- as.data.table(readRDS(file.path(GLOBAL_RESULTS,
                                            paste0("pafs_", LOCATION_ID, ".rds"))))
cp2_diff <- merge(pipe_paf[, .(year, acause,
                               pipe_paf_heat = paf_heat,
                               pipe_paf_cold = paf_cold)],
                  paf_year_cause[, .(year, acause,
                                     sam_paf_heat_mean = paf_heat_mean,
                                     sam_paf_cold_mean = paf_cold_mean,
                                     sam_paf_heat_dwt  = paf_heat_dwt,
                                     sam_paf_cold_dwt  = paf_cold_dwt)],
                  by = c("year", "acause"), all = TRUE)
cp2_diff[, paf_heat_mean_pct := abs_pct_diff(sam_paf_heat_mean, pipe_paf_heat)]
cp2_diff[, paf_cold_mean_pct := abs_pct_diff(sam_paf_cold_mean, pipe_paf_cold)]
cp2_diff[, paf_heat_dwt_pct  := abs_pct_diff(sam_paf_heat_dwt,  pipe_paf_heat)]
cp2_diff[, paf_cold_dwt_pct  := abs_pct_diff(sam_paf_cold_dwt,  pipe_paf_cold)]
saveRDS(cp2_diff, file.path(OUTPUT_DIR, "diff_checkpoint_2_paf.rds"))

cp2_summary <- summarize_diff(cp2_diff, c("paf_heat_mean_pct", "paf_cold_mean_pct",
                                          "paf_heat_dwt_pct",  "paf_cold_dwt_pct"))
logmsg("  CP2 diff summary:")
print(cp2_summary)

# ---- Checkpoint 3: SEV (year × cause) ----
pipe_sev <- as.data.table(readRDS(file.path(GLOBAL_RESULTS,
                                            paste0("sevs_", LOCATION_ID, ".rds"))))
cp3_diff <- merge(pipe_sev[, .(year, acause, pipe_sev = sev)],
                  sev_year_cause[, .(year, acause,
                                     sam_sev_dwt = sev_dwt,
                                     sam_sev_pmean = sev_pwt_proxy)],
                  by = c("year", "acause"), all = TRUE)
cp3_diff[, sev_dwt_pct := abs_pct_diff(sam_sev_dwt,  pipe_sev)]
cp3_diff[, sev_pmean_pct := abs_pct_diff(sam_sev_pmean, pipe_sev)]
saveRDS(cp3_diff, file.path(OUTPUT_DIR, "diff_checkpoint_3_sev.rds"))

cp3_summary <- summarize_diff(cp3_diff, c("sev_dwt_pct", "sev_pmean_pct"))
logmsg("  CP3 diff summary:")
print(cp3_summary)

# ---- Checkpoint 4: Attributable deaths (year × cause) ----
pipe_burden <- as.data.table(readRDS(file.path(GLOBAL_RESULTS,
                                               paste0("burden_", LOCATION_ID, ".rds"))))
cp4_diff <- merge(pipe_burden[, .(year, acause,
                                  pipe_deaths        = deaths,
                                  pipe_deaths_heat   = deaths_heat,
                                  pipe_deaths_cold   = deaths_cold,
                                  pipe_deaths_nonopt = deaths_nonopt)],
                  attrib_year_cause[, .(year, acause,
                                        sam_deaths        = deaths,
                                        sam_deaths_heat   = deaths_heat,
                                        sam_deaths_cold   = deaths_cold,
                                        sam_deaths_nonopt = deaths_nonopt)],
                  by = c("year", "acause"), all = TRUE)
cp4_diff[, deaths_pct        := abs_pct_diff(sam_deaths,        pipe_deaths)]
cp4_diff[, deaths_heat_pct   := abs_pct_diff(sam_deaths_heat,   pipe_deaths_heat)]
cp4_diff[, deaths_cold_pct   := abs_pct_diff(sam_deaths_cold,   pipe_deaths_cold)]
cp4_diff[, deaths_nonopt_pct := abs_pct_diff(sam_deaths_nonopt, pipe_deaths_nonopt)]
saveRDS(cp4_diff, file.path(OUTPUT_DIR, "diff_checkpoint_4_burden.rds"))

cp4_summary <- summarize_diff(cp4_diff, c("deaths_pct", "deaths_heat_pct",
                                          "deaths_cold_pct", "deaths_nonopt_pct"))
logmsg("  CP4 diff summary:")
print(cp4_summary)

# ---- Checkpoint 5: YLL (year × cause) ----
pipe_ylls <- as.data.table(readRDS(file.path(GLOBAL_RESULTS,
                                             paste0("ylls_", LOCATION_ID, ".rds"))))
setnames(pipe_ylls, "year_id", "year")
cp5_diff <- merge(pipe_ylls[, .(year, acause,
                                pipe_yll_heat   = yll_heat,
                                pipe_yll_cold   = yll_cold,
                                pipe_yll_nonopt = yll_nonopt)],
                  yll_year_cause[, .(year, acause,
                                     sam_yll_heat   = yll_heat,
                                     sam_yll_cold   = yll_cold,
                                     sam_yll_nonopt = yll_nonopt)],
                  by = c("year", "acause"), all = TRUE)
cp5_diff[, yll_heat_pct   := abs_pct_diff(sam_yll_heat,   pipe_yll_heat)]
cp5_diff[, yll_cold_pct   := abs_pct_diff(sam_yll_cold,   pipe_yll_cold)]
cp5_diff[, yll_nonopt_pct := abs_pct_diff(sam_yll_nonopt, pipe_yll_nonopt)]
saveRDS(cp5_diff, file.path(OUTPUT_DIR, "diff_checkpoint_5_yll.rds"))

cp5_summary <- summarize_diff(cp5_diff, c("yll_heat_pct", "yll_cold_pct",
                                          "yll_nonopt_pct"))
logmsg("  CP5 diff summary:")
print(cp5_summary)

# ---- Save combined summary ----
combined <- rbindlist(list(
  cbind(checkpoint = "1_erf", cp1_summary),
  cbind(checkpoint = "2_paf", cp2_summary),
  cbind(checkpoint = "3_sev", cp3_summary),
  cbind(checkpoint = "4_burden", cp4_summary),
  cbind(checkpoint = "5_yll", cp5_summary)
), use.names = TRUE, fill = TRUE)
fwrite(combined, file.path(OUTPUT_DIR, "diff_summary_all_checkpoints.csv"))

logmsg("All checkpoints saved to ", OUTPUT_DIR)
logmsg("Done.")
