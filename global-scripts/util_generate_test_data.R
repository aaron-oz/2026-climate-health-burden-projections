# util_generate_test_data.R — Generate synthetic test data for pipeline testing
#
# Creates minimal synthetic data for location 125 (Colombia) to verify that
# the pipeline runs end-to-end. The data is structurally correct but the
# values are simulated — do NOT use for analysis.
#
# Usage: Rscript util_generate_test_data.R

library(data.table)

PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT",
                           unset = normalizePath(file.path(getwd(), "..")))
DATA_DIR <- file.path(PROJECT_ROOT, "data")

set.seed(42)

LOC_ID <- 125
YEARS <- 2010:2019
N_PIXELS <- 20  # small for fast testing
N_DAYS_PER_YEAR <- 365

cat("=== Generating synthetic test data for location", LOC_ID, "===\n")

# =============================================================================
# 1. Temperature data
# =============================================================================

cat("Generating temperature data...\n")

# Create synthetic daily temperatures for pixels across Colombia
# Colombia spans roughly 20-28°C mean annual temperature
temp_rows <- CJ(pixel_id = 1:N_PIXELS, year = YEARS)
temp_rows[, annual_mean := runif(.N, 20, 28)]

# Expand to daily
temp_daily <- temp_rows[, {
  dates <- seq(as.Date(paste0(year, "-01-01")),
               as.Date(paste0(year, "-12-31")), by = "day")
  data.table(date = dates,
             daily_temp = annual_mean + rnorm(length(dates), 0, 3),
             pop = sample(1000:50000, 1))  # fixed pop per pixel per year
}, by = .(pixel_id, year)]
temp_daily[, year := NULL]

saveRDS(temp_daily[, .(pixel_id, date, daily_temp, pop)],
        file.path(DATA_DIR, "temperature", paste0(LOC_ID, "_daily_temp.rds")))
cat("  Saved:", nrow(temp_daily), "rows\n")

# =============================================================================
# 2. Mortality data
# =============================================================================

cat("Generating mortality data...\n")

causes <- c("cvd_ihd", "cvd_stroke", "cvd_htn", "cvd_cmp",
            "ckd", "diabetes", "lri", "resp_copd",
            "inj_drowning", "inj_homicide", "inj_suicide",
            "inj_disaster", "inj_mech", "inj_trans_road",
            "inj_trans_other", "inj_othunintent", "inj_animal")

age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", ">80")

mort <- CJ(year_id = YEARS, acause = causes,
           age_group_id = age_groups, sex_id = c("M", "F"))

# Generate plausible death counts (higher for CVD/elderly, lower for injuries/young)
mort[, deaths := {
  base <- ifelse(grepl("^cvd|ckd|diabetes|resp_copd|lri", acause), 50, 5)
  age_mult <- ifelse(age_group_id %in% c("65-69", "70-74", "75-79", ">80"), 5,
              ifelse(age_group_id %in% c("50-54", "55-59", "60-64"), 2, 0.5))
  pmax(0, round(rpois(.N, base * age_mult)))
}]
mort[, location_id := LOC_ID]

fwrite(mort, file.path(DATA_DIR, "mortality", paste0(LOC_ID, "_mortality.csv")))
cat("  Saved:", nrow(mort), "rows\n")

# =============================================================================
# 3. Life tables
# =============================================================================

cat("Generating life table data...\n")

lt <- CJ(year_id = YEARS, age_group_id = age_groups)
lt[, ex := {
  # Rough life expectancy by age group
  age_start <- as.numeric(gsub("-.*|>", "", age_group_id))
  pmax(1, 80 - age_start + rnorm(.N, 0, 1))
}]
lt[, location_id := LOC_ID]

fwrite(lt, file.path(DATA_DIR, "lifetables", paste0(LOC_ID, "_lifetable.csv")))
cat("  Saved:", nrow(lt), "rows\n")

cat("\n=== Synthetic test data generated ===\n")
cat("Run the pipeline with: Rscript run_location.R --location_id=125\n")
