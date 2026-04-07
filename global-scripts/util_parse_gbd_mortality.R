# util_parse_gbd_mortality.R — Parse GBD Results Tool mortality downloads
#
# Converts GBD Results Tool CSV exports into the format expected by
# the global pipeline (04_load_mortality.R).
#
# Expected input: CSV downloaded from https://vizhub.healthdata.org/gbd-results/
#   or unzipped from IHME-GBD_*.zip files
#
# Output: data/mortality/{location_id}_mortality.csv
#   Columns: location_id, year_id, age_group_id, sex_id, cause_id, acause, deaths

library(data.table)

PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT",
                           unset = normalizePath(file.path(getwd(), "..")))
DATA_DIR <- file.path(PROJECT_ROOT, "data")
MORTALITY_DIR <- file.path(DATA_DIR, "mortality")

cat("=== Parsing GBD Results Tool Mortality Data ===\n")

# --- GBD cause_id to acause mapping ---
CAUSE_MAP <- data.table(
  cause_id = c(493, 494, 498, 499, 589, 587, 322, 509,
               696, 724, 718, 693, 699, 688, 691, 703, 706),
  acause = c("cvd_ihd", "cvd_stroke", "cvd_htn", "cvd_cmp",
             "ckd", "diabetes", "lri", "resp_copd",
             "inj_drowning", "inj_homicide", "inj_suicide",
             "inj_disaster", "inj_mech", "inj_trans_road",
             "inj_trans_other", "inj_othunintent", "inj_animal")
)

# Also match by cause name in case cause_ids differ across GBD rounds
CAUSE_NAME_MAP <- data.table(
  cause_name_pattern = c(
    "Ischemic heart", "Stroke", "Hypertensive heart", "Cardiomyopathy",
    "Chronic kidney", "Diabetes", "Lower respiratory", "Chronic obstructive",
    "Drowning", "Interpersonal violence", "Self-harm",
    "forces of nature", "Mechanical", "Road injur",
    "Other transport", "Other unintentional", "Animal contact"
  ),
  acause = c(
    "cvd_ihd", "cvd_stroke", "cvd_htn", "cvd_cmp",
    "ckd", "diabetes", "lri", "resp_copd",
    "inj_drowning", "inj_homicide", "inj_suicide",
    "inj_disaster", "inj_mech", "inj_trans_road",
    "inj_trans_other", "inj_othunintent", "inj_animal"
  )
)

# --- GBD age group mapping ---
# Map GBD age names to our standard age group labels
AGE_MAP <- data.table(
  age_name_pattern = c(
    "^<5|^Under 5|^0-4|^Early Neonatal|^Late Neonatal|^Post Neonatal|^1-4",
    "^5-9", "^10-14", "^15-19", "^20-24", "^25-29",
    "^30-34", "^35-39", "^40-44", "^45-49", "^50-54", "^55-59",
    "^60-64", "^65-69", "^70-74", "^75-79", "^80|^85|^90|^95"
  ),
  age_group_id = c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", ">80"
  )
)

# --- Find input files ---
input_files <- list.files(MORTALITY_DIR, pattern = "IHME.*\\.csv$", full.names = TRUE)
if (length(input_files) == 0) {
  # Also check for any CSV that isn't already a parsed output
  input_files <- list.files(MORTALITY_DIR, pattern = "\\.csv$", full.names = TRUE)
  input_files <- input_files[!grepl("^\\d+_mortality\\.csv$", basename(input_files))]
  input_files <- input_files[!grepl("cause_map\\.csv$", basename(input_files))]
}

if (length(input_files) == 0) {
  stop(paste("No GBD data CSV found in", MORTALITY_DIR))
}

cat("Found input file(s):", paste(basename(input_files), collapse = ", "), "\n")

# --- Read and combine ---
raw <- rbindlist(lapply(input_files, function(f) {
  cat("  Reading:", basename(f), "\n")
  fread(f)
}), fill = TRUE)

cat("Total rows loaded:", nrow(raw), "\n")
cat("Columns:", paste(names(raw), collapse = ", "), "\n\n")

# --- Filter to deaths only ---
if ("measure_name" %in% names(raw)) {
  raw <- raw[measure_name == "Deaths"]
}
if ("metric_name" %in% names(raw)) {
  raw <- raw[metric_name == "Number"]
}

# --- Map cause names to acause labels ---
if ("cause_id" %in% names(raw)) {
  raw <- merge(raw, CAUSE_MAP, by = "cause_id", all.x = TRUE)
}

# For rows where cause_id mapping failed, try name matching
if ("cause_name" %in% names(raw)) {
  unmapped <- raw[is.na(acause)]
  if (nrow(unmapped) > 0) {
    for (i in 1:nrow(CAUSE_NAME_MAP)) {
      pattern <- CAUSE_NAME_MAP$cause_name_pattern[i]
      ac <- CAUSE_NAME_MAP$acause[i]
      raw[is.na(acause) & grepl(pattern, cause_name, ignore.case = TRUE), acause := ac]
    }
  }
}

# Filter to our 17 causes only
raw <- raw[!is.na(acause)]
cat("After cause filtering:", nrow(raw), "rows,",
    length(unique(raw$acause)), "causes\n")

if (length(unique(raw$acause)) < 17) {
  missing <- setdiff(CAUSE_MAP$acause, unique(raw$acause))
  cat("WARNING - Missing causes:", paste(missing, collapse = ", "), "\n")
}

# --- Map age groups ---
if ("age_name" %in% names(raw)) {
  raw[, age_group_std := NA_character_]
  for (i in 1:nrow(AGE_MAP)) {
    pattern <- AGE_MAP$age_name_pattern[i]
    label <- AGE_MAP$age_group_id[i]
    raw[is.na(age_group_std) & grepl(pattern, age_name), age_group_std := label]
  }
  # Aggregate sub-groups that map to the same standard group (e.g., neonatal → 0-4)
  cat("Age groups mapped:", length(unique(raw$age_group_std[!is.na(raw$age_group_std)])), "\n")
} else if ("age_id" %in% names(raw)) {
  raw[, age_group_std := as.character(age_id)]
}

# Drop unmapped ages (e.g., "All Ages", "Age-standardized")
raw <- raw[!is.na(age_group_std)]

# --- Standardize sex ---
if ("sex_name" %in% names(raw)) {
  raw[sex_name == "Male", sex_std := "M"]
  raw[sex_name == "Female", sex_std := "F"]
  raw[sex_name == "Both", sex_std := "Both"]
  raw <- raw[!is.na(sex_std)]
} else if ("sex_id" %in% names(raw)) {
  raw[sex_id == 1, sex_std := "M"]
  raw[sex_id == 2, sex_std := "F"]
  raw[sex_id == 3, sex_std := "Both"]
  raw <- raw[!is.na(sex_std)]
}

# --- Standardize value column ---
if ("val" %in% names(raw)) {
  raw[, deaths := as.numeric(val)]
} else if ("value" %in% names(raw)) {
  raw[, deaths := as.numeric(value)]
}

# --- Standardize year ---
if ("year" %in% names(raw) && !"year_id" %in% names(raw)) {
  setnames(raw, "year", "year_id")
}
raw[, year_id := as.integer(year_id)]

# --- Aggregate (sum sub-age-groups within standard groups) ---
out <- raw[, .(deaths = sum(deaths, na.rm = TRUE)),
           by = .(location_id, year_id, age_group_std, sex_std, acause)]
setnames(out, c("age_group_std", "sex_std"), c("age_group_id", "sex_id"))

# --- Output: one CSV per location ---
locations <- unique(out$location_id)
cat("\nWriting mortality data for", length(locations), "location(s):\n")
for (loc in locations) {
  loc_data <- out[location_id == loc]
  out_file <- file.path(MORTALITY_DIR, paste0(loc, "_mortality.csv"))
  fwrite(loc_data, out_file)
  cat("  Location", loc, ":", nrow(loc_data), "rows,",
      length(unique(loc_data$acause)), "causes,",
      paste(range(loc_data$year_id), collapse = "–"), "→",
      basename(out_file), "\n")
}

cat("\n=== GBD mortality parsing complete ===\n")
