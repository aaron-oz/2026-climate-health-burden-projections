# util_convert_samuel_colombia.R — Convert Samuel's Colombia data to pipeline format
#
# Transforms the RDS/CSV files from data/columbia-data-for-verifying-pipeline/
# into the standardized format expected by the global pipeline loaders.
#
# Input:  data/columbia-data-for-verifying-pipeline/colombia/
# Output: data/temperature/125_daily_temp.rds
#         data/mortality/125_mortality.rds
#         data/lifetables/125_lifetable.rds
#         data/population/125_population.rds (diagnostic only)
#
# Usage: Rscript util_convert_samuel_colombia.R

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))

library(data.table)

SAMUEL_DIR <- file.path(DATA_DIR, "columbia-data-for-verifying-pipeline", "colombia")

# =============================================================================
# 1. Temperature: temperatura_diaria_pixel.rds
#    Samuel: temperatura, fecha, index_right, pob, DPTO_CCDGO
#    Pipeline: pixel_id, date, daily_temp, pop
# =============================================================================

log_msg("Converting temperature data...")
temp <- readRDS(file.path(SAMUEL_DIR, "temperatura_diaria_pixel.rds"))
setDT(temp)

setnames(temp, c("temperatura", "fecha", "index_right", "pob",   "DPTO_CCDGO"),
               c("daily_temp",  "date",  "pixel_id",    "pop", "subloc_id"))
temp[, date := as.Date(date)]
# subloc_id is kept (department for Colombia); pipeline groups within subloc
# for PAF / SEV / burden computation. Stored as 2-char zero-padded character
# to match the format used in mortality (codptore) and life tables (cod_depto).
temp[, subloc_id := sprintf("%02d", as.integer(subloc_id))]

saveRDS(temp, file.path(TEMP_DIR, "125_daily_temp.rds"))
log_msg("Temperature: ", nrow(temp), " rows, ",
        uniqueN(temp$subloc_id), " subnational locations -> ",
        file.path(TEMP_DIR, "125_daily_temp.rds"))

# =============================================================================
# 2. Mortality: mortalidad_diaria_DANE_2010_2019_imput.rds
#    Samuel: fecha_def, codptore, sexo, gru_ed1, c_muerte, muertes
#    Pipeline: location_id, year_id, age_group_id, sex_id, acause, deaths
# =============================================================================

log_msg("Converting mortality data...")
mort <- readRDS(file.path(SAMUEL_DIR, "mortalidad_diaria_DANE_2010_2019_imput.rds"))
setDT(mort)

# Replicate Samuel (11_carga_atribuible.R:162-163): drop deaths registered to
# residents abroad (codptore == 75, "EXTRANJERO"). About 2,083 of 884,628 deaths.
n_before <- nrow(mort)
mort <- mort[codptore != 75]
log_msg("Filtered codptore=75 (EXTRANJERO): ", n_before - nrow(mort), " rows removed")

# Map Spanish cause names to GBD acause codes
cause_map <- data.table(
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

mort <- merge(mort, cause_map, by = "c_muerte", all.x = TRUE)
if (any(is.na(mort$acause))) {
  warning("Unmapped causes: ", paste(unique(mort[is.na(acause)]$c_muerte), collapse = ", "))
}

# Map sex
mort[, sex_id := fifelse(sexo == "M", 1L, 2L)]

# Map age groups to GBD-style age_group_id
# Using simplified numeric encoding: midpoint of age range
# Use lower bound of age range as age_group_id (matches DANE life table encoding)
age_map <- data.table(
  gru_ed1 = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
              "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
              "60-64", "65-69", "70-74", "75-79", ">80"),
  age_group_id = c(0L, 5L, 10L, 15L, 20L, 25L,
                   30L, 35L, 40L, 45L, 50L, 55L,
                   60L, 65L, 70L, 75L, 80L)
)

mort <- merge(mort, age_map, by = "gru_ed1", all.x = TRUE)

# Extract year and date; codptore is the department of residence for the
# deceased.
mort[, date     := as.Date(fecha_def)]
mort[, year_id  := as.integer(format(date, "%Y"))]
mort[, subloc_id := sprintf("%02d", as.integer(codptore))]  # zero-padded

# Aggregate to annual (subloc x age x sex x cause) — pipeline default input.
mort_annual <- mort[, .(deaths = sum(muertes, na.rm = TRUE)),
                    by = .(year_id, subloc_id, age_group_id, sex_id, acause)]
mort_annual[, location_id := LOCATION_ID]

saveRDS(mort_annual, file.path(MORTALITY_DIR, "125_mortality.rds"))
log_msg("Mortality: ", nrow(mort_annual), " rows, ",
        uniqueN(mort_annual$subloc_id), " subnational locations -> ",
        file.path(MORTALITY_DIR, "125_mortality.rds"))

# Also save daily mortality for COLOMBIA_VERIFICATION mode, which attributes
# at depto-day granularity to match Samuel's approach. Aggregate over rows
# that fall on the same (date, subloc, age, sex, cause) — the raw file has
# one row per death event so most groups have count 1 here.
mort_daily <- mort[!is.na(date) & !is.na(acause),
                   .(deaths = sum(muertes, na.rm = TRUE)),
                   by = .(date, year_id, subloc_id, age_group_id, sex_id, acause)]
mort_daily[, location_id := LOCATION_ID]

saveRDS(mort_daily, file.path(MORTALITY_DIR, "125_mortality_daily.rds"))
log_msg("Daily mortality: ", nrow(mort_daily), " rows -> ",
        file.path(MORTALITY_DIR, "125_mortality_daily.rds"))

# =============================================================================
# 3. Life tables: Tablas_vida_DANE_2005_2050.rds
#    Samuel: cod_depto, sexo, ano, age, ev
#    Pipeline: year_id, age_group_id, sex_id, ex
# =============================================================================

log_msg("Converting life table data...")
lt <- readRDS(file.path(SAMUEL_DIR, "Tablas_vida_DANE_2005_2050.rds"))
setDT(lt)

# Standardize columns. Keep BOTH the national life table (cod_depto == "00")
# and the department-level life tables. 07_compute_ylls.R picks which to use
# based on COLOMBIA_VERIFICATION mode (Samuel uses dept-level).
setnames(lt, c("ano", "ev", "cod_depto"), c("year_id", "ex", "subloc_id"))
lt[, year_id := as.integer(year_id)]
lt[, sex_id := fifelse(sexo == "F", 2L, 1L)]
lt[, age_group_id := as.integer(age)]
lt[, subloc_id := as.character(subloc_id)]

lt_out <- lt[year_id >= YEAR_START & year_id <= YEAR_END,
             .(year_id, subloc_id, age_group_id, sex_id, ex)]
lt_out[, location_id := LOCATION_ID]

saveRDS(lt_out, file.path(LIFETABLE_DIR, "125_lifetable.rds"))
log_msg("Life tables: ", nrow(lt_out), " rows (national + ",
        uniqueN(lt_out$subloc_id) - 1L, " depts) -> ",
        file.path(LIFETABLE_DIR, "125_lifetable.rds"))

# =============================================================================
# 4. DIVIPOLA lookup (informational — save for reference)
# =============================================================================

log_msg("Loading DIVIPOLA lookup...")
divipola <- fread(file.path(SAMUEL_DIR, "DIVIPOLA-_Códigos_departamentos_20260410.csv"))
saveRDS(divipola, file.path(DATA_DIR, "colombia", "divipola.rds"))
log_msg("DIVIPOLA: ", nrow(divipola), " departments")

log_msg("=== Colombia data conversion complete ===")
