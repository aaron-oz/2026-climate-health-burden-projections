# util_convert_colombia.R — Convert Samuel's Colombia data to global pipeline format
#
# This script converts Samuel's intermediate RDS files into the CSV format
# expected by the global pipeline. Run this ONCE after obtaining the data
# from Samuel.
#
# Required input files (from Samuel's pipeline):
#   from-samuel/Bases/Ambientales/temperatura_diaria_pixel.rds
#   from-samuel/Bases/Ambientales/WorldPop/WorldPop_2010_2019_pixel.rds
#   from-samuel/Bases/Mortalidad depurada/mortalidad_diaria_DANE_2010_2019_imput.rds
#   from-samuel/Bases/Tablas de vida/Tablas_vida_DANE_2005_2050.rds
#
# Output:
#   data/temperature/125_daily_temp.rds
#   data/population/125_population.csv
#   data/mortality/125_mortality.csv
#   data/lifetables/125_lifetable.csv

library(data.table)
library(readr)

PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT",
                           unset = normalizePath(file.path(getwd(), "..")))
SAMUEL_DIR <- file.path(PROJECT_ROOT, "from-samuel")
DATA_DIR <- file.path(PROJECT_ROOT, "data")

cat("=== Converting Colombia data for global pipeline ===\n")

# GBD location ID for Colombia
LOC_ID <- 125

# Mapping from Samuel's Spanish cause names to GBD acause labels
CAUSE_REMAP <- data.table(
  c_muerte_samuel = c("cardiopatia_isquemica", "acv",
                       "miocardiopatia_miocarditis", "cardiopatia_hipertensiva",
                       "ivri", "epoc", "dm", "erc",
                       "homicidio", "suicidio", "ahogamiento",
                       "desastres", "lesiones_mecanicas",
                       "accidentes_trafico", "relacion_transporte",
                       "no_intencionales", "relacionadas_animales"),
  acause = c("cvd_ihd", "cvd_stroke",
             "cvd_cmp", "cvd_htn",
             "lri", "resp_copd", "diabetes", "ckd",
             "inj_homicide", "inj_suicide", "inj_drowning",
             "inj_disaster", "inj_mech",
             "inj_trans_road", "inj_trans_other",
             "inj_othunintent", "inj_animal")
)

# =============================================================================
# 1. Temperature data
# =============================================================================

cat("\n--- Temperature ---\n")
temp_file <- file.path(SAMUEL_DIR, "Bases/Ambientales/temperatura_diaria_pixel.rds")
if (file.exists(temp_file)) {
  temp_raw <- readRDS(temp_file)
  setDT(temp_raw)

  # Samuel's columns: temperatura, fecha, index_right, pob, cod_depto
  # Rename to global pipeline format
  setnames(temp_raw,
           c("temperatura", "fecha", "index_right", "pob", "cod_depto"),
           c("daily_temp", "date", "pixel_id", "pop", "admin_code"))

  temp_raw[, date := as.Date(date)]

  # Drop admin_code (not needed in global format — location is implicit)
  temp_out <- temp_raw[, .(pixel_id, date, daily_temp, pop)]

  saveRDS(temp_out, file.path(DATA_DIR, "temperature", paste0(LOC_ID, "_daily_temp.rds")))
  cat("  Saved:", nrow(temp_out), "rows\n")
} else {
  cat("  NOT FOUND:", temp_file, "\n")
  cat("  Ask Samuel for this file or run his scripts 10/10_2 to generate it.\n")
}

# =============================================================================
# 2. Population data
# =============================================================================

cat("\n--- Population ---\n")
pop_file <- file.path(SAMUEL_DIR, "Bases/Ambientales/WorldPop/WorldPop_2010_2019_pixel.rds")
if (file.exists(pop_file)) {
  pop_raw <- readRDS(pop_file)
  setDT(pop_raw)

  # Samuel's columns: indx_rg, sum_z, ano (and possibly DPTO_CCDGO)
  setnames(pop_raw, c("indx_rg", "sum_z", "ano"),
           c("pixel_id", "pop", "year"),
           skip_absent = TRUE)

  pop_out <- pop_raw[, .(pixel_id, year, pop)]
  fwrite(pop_out, file.path(DATA_DIR, "population", paste0(LOC_ID, "_population.csv")))
  cat("  Saved:", nrow(pop_out), "rows\n")
} else {
  cat("  NOT FOUND:", pop_file, "\n")
  cat("  Ask Samuel for this file or run his script 10_2 to generate it.\n")
}

# =============================================================================
# 3. Mortality data
# =============================================================================

cat("\n--- Mortality ---\n")
mort_file <- file.path(SAMUEL_DIR, "Bases/Mortalidad depurada/mortalidad_diaria_DANE_2010_2019_imput.rds")
if (file.exists(mort_file)) {
  mort_raw <- readRDS(mort_file)
  setDT(mort_raw)

  # Samuel's columns: fecha_def/fecha, codptore/cod_depto, sexo, gru_ed1, c_muerte, muertes
  # Standardize column names (Samuel's script renames these in script 11)
  if ("fecha_def" %in% names(mort_raw)) setnames(mort_raw, "fecha_def", "fecha")
  if ("codptore" %in% names(mort_raw)) setnames(mort_raw, "codptore", "cod_depto")

  # Remove foreign residents
  mort_raw <- mort_raw[cod_depto != 75]

  # Fix cause name encoding issues (from Samuel's script 11)
  mort_raw[c_muerte == "cardiopatía_hiperten", c_muerte := "cardiopatia_hipertensiva"]
  mort_raw[c_muerte == "miocardiopatia_miocar", c_muerte := "miocardiopatia_miocarditis"]

  # Map Spanish cause names to GBD acause labels
  mort_raw <- merge(mort_raw, CAUSE_REMAP,
                    by.x = "c_muerte", by.y = "c_muerte_samuel",
                    all.x = TRUE)
  mort_raw <- mort_raw[!is.na(acause)]  # drop unmapped causes

  # Aggregate to year level (the global pipeline expects annual, not daily)
  mort_raw[, year_id := as.integer(format(as.Date(fecha), "%Y"))]

  # Map Samuel's age groups to GBD age_group_ids
  # Samuel uses: "0-4", "5-9", ..., "75-79", ">80"
  # GBD uses numeric age_group_ids; for now keep as text and let the pipeline handle it
  mort_agg <- mort_raw[, .(deaths = sum(muertes, na.rm = TRUE)),
                       by = .(year_id, gru_ed1, sexo, acause)]

  # Rename for pipeline
  setnames(mort_agg, c("gru_ed1", "sexo"),
           c("age_group_id", "sex_id"))
  mort_agg[, location_id := LOC_ID]

  fwrite(mort_agg, file.path(DATA_DIR, "mortality", paste0(LOC_ID, "_mortality.csv")))
  cat("  Saved:", nrow(mort_agg), "rows\n")
} else {
  cat("  NOT FOUND:", mort_file, "\n")
  cat("  Ask Samuel for this file or run his script 01 to generate it.\n")
}

# =============================================================================
# 4. Life tables
# =============================================================================

cat("\n--- Life tables ---\n")
lt_file <- file.path(SAMUEL_DIR, "Bases/Tablas de vida/Tablas_vida_DANE_2005_2050.rds")
if (file.exists(lt_file)) {
  lt_raw <- readRDS(lt_file)
  setDT(lt_raw)

  # Samuel's columns include: age, ano, cod_depto, ev (life expectancy)
  # We need: year_id, age_group_id, sex_id, ex
  if ("ev" %in% names(lt_raw)) setnames(lt_raw, "ev", "ex")
  if ("ano" %in% names(lt_raw)) setnames(lt_raw, "ano", "year_id")

  lt_raw[, year_id := as.integer(year_id)]

  # Map age to age groups matching mortality data
  lt_raw[, age_group_id := fcase(
    age == 0 | age == 1, "0-4",
    age == 5, "5-9",
    age == 10, "10-14",
    age == 15, "15-19",
    age == 20, "20-24",
    age == 25, "25-29",
    age == 30, "30-34",
    age == 35, "35-39",
    age == 40, "40-44",
    age == 45, "45-49",
    age == 50, "50-54",
    age == 55, "55-59",
    age == 60, "60-64",
    age == 65, "65-69",
    age == 70, "70-74",
    age == 75, "75-79",
    age >= 80, ">80",
    default = NA_character_
  )]

  # Filter out national-level tables and take distinct rows
  lt_raw <- lt_raw[as.numeric(cod_depto) != 0]
  lt_raw <- lt_raw[age != 0]  # included in 0-4 group

  lt_out <- lt_raw[, .(ex = mean(ex, na.rm = TRUE)),
                   by = .(year_id, age_group_id)]
  lt_out[, location_id := LOC_ID]

  fwrite(lt_out, file.path(DATA_DIR, "lifetables", paste0(LOC_ID, "_lifetable.csv")))
  cat("  Saved:", nrow(lt_out), "rows\n")
} else {
  cat("  NOT FOUND:", lt_file, "\n")
  cat("  Ask Samuel for this file or run his script 07 to generate it.\n")
}

cat("\n=== Conversion complete ===\n")
cat("Missing files (if any) need to be obtained from Samuel.\n")
cat("His intermediate RDS files are created by running his pipeline scripts\n")
cat("01, 07, 10, and 10_2 against the raw DANE/WorldPop/temperature data.\n")
