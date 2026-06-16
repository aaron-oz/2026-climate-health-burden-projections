# util_parse_un_lifetables.R — Parse UN World Population Prospects life table data
#
# Converts UN WPP life expectancy downloads into the format expected by
# the global pipeline (07_compute_ylls.R).
#
# The UN Data Portal exports life expectancy E(x) with columns that vary
# by download format. This script handles the common variants.
#
# Expected input: CSV downloaded from https://population.un.org/dataportal/
#   Indicator: Life expectancy at exact ages (Ex) - complete (indicator 76)
#
# Output: data/lifetables/{location_id}_lifetable.csv
#   Columns: year_id, age_group_id, sex_id, ex, location_id

library(data.table)

PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT",
                           unset = normalizePath(file.path(getwd(), "..")))
DATA_DIR <- file.path(PROJECT_ROOT, "data")
LIFETABLE_DIR <- file.path(DATA_DIR, "lifetables")
dir.create(LIFETABLE_DIR, showWarnings = FALSE, recursive = TRUE)

cat("=== Parsing UN WPP Life Table Data ===\n")

# --- Find the input file ---
# Look for UN data files in the lifetables directory
input_files <- list.files(LIFETABLE_DIR, pattern = "\\.(csv|CSV)$", full.names = TRUE)
# Exclude already-parsed output files (numeric location ID filenames)
input_files <- input_files[!grepl("^\\d+_lifetable\\.csv$", basename(input_files))]

if (length(input_files) == 0) {
  stop(paste("No UN data CSV found in", LIFETABLE_DIR,
             "\nDownload from https://population.un.org/dataportal/",
             "\nIndicator: Life expectancy E(x) - complete"))
}

cat("Found input file(s):", paste(basename(input_files), collapse = ", "), "\n")

# Read all input files
raw <- rbindlist(lapply(input_files, function(f) {
  cat("  Reading:", basename(f), "\n")
  # Try reading with auto-detection of separator
  dt <- tryCatch(
    fread(f, header = TRUE),
    error = function(e) {
      # Some UN exports use semicolons
      fread(f, header = TRUE, sep = ";")
    }
  )
  dt
}))

cat("Total rows loaded:", nrow(raw), "\n")
cat("Columns:", paste(names(raw), collapse = ", "), "\n\n")

# --- Standardize column names ---
# The UN portal exports use various naming conventions. Handle common ones.
name_map <- c(
  # Location
  "ISO3_code" = "iso3",
  "ISO3 Alpha-code" = "iso3",
  "iso3" = "iso3",
  "Location" = "location_name",
  "location" = "location_name",
  "LocationId" = "location_id_un",
  "LocID" = "location_id_un",
  "locId" = "location_id_un",
  "locationId" = "location_id_un",
  # Year
  "Time" = "year",
  "time" = "year",
  "Year" = "year",
  "year" = "year",
  "TimeLabel" = "year",
  "timeLabel" = "year",
  # Age
  "AgeStart" = "age",
  "ageStart" = "age",
  "Age" = "age",
  "age" = "age",
  "AgeGrp" = "age",
  "AgeLabel" = "age_label",
  "ageLabel" = "age_label",
  # Sex
  "Sex" = "sex",
  "sex" = "sex",
  "SexId" = "sex_id_un",
  "sexId" = "sex_id_un",
  # Value
  "Value" = "ex",
  "value" = "ex",
  "Ex" = "ex",
  "ex" = "ex",
  # Variant/scenario
  "Variant" = "variant",
  "variant" = "variant",
  "VariantLabel" = "variant_label",
  "variantLabel" = "variant_label"
)

for (old_name in names(raw)) {
  if (old_name %in% names(name_map)) {
    new_name <- name_map[old_name]
    if (new_name != old_name && !(new_name %in% names(raw))) {
      setnames(raw, old_name, new_name)
    }
  }
}

cat("Standardized columns:", paste(names(raw), collapse = ", "), "\n\n")

# --- Filter to medium variant (projections) ---
if ("variant" %in% names(raw)) {
  variants <- unique(raw$variant)
  cat("Variants found:", paste(variants, collapse = ", "), "\n")
  # Keep medium/reference variant
  medium_variants <- c("Medium", "medium", "Median", "median",
                        "Medium variant", "Reference")
  raw <- raw[variant %in% medium_variants | is.na(variant)]
  cat("Filtered to medium variant:", nrow(raw), "rows\n")
} else if ("variant_label" %in% names(raw)) {
  raw <- raw[grepl("medium|median|reference", variant_label, ignore.case = TRUE) |
               is.na(variant_label)]
  cat("Filtered to medium variant:", nrow(raw), "rows\n")
}

# --- Parse year ---
if ("year" %in% names(raw)) {
  raw[, year := as.integer(gsub("[^0-9]", "", as.character(year)))]
}

# --- Parse age ---
if ("age" %in% names(raw)) {
  raw[, age := as.integer(gsub("[^0-9]", "", as.character(age)))]
} else if ("age_label" %in% names(raw)) {
  raw[, age := as.integer(gsub("[^0-9]", "", as.character(age_label)))]
}

# --- Parse sex ---
if ("sex" %in% names(raw)) {
  raw[, sex := tolower(trimws(as.character(sex)))]
  raw[sex %in% c("1", "male", "m"), sex_std := "M"]
  raw[sex %in% c("2", "female", "f"), sex_std := "F"]
  raw[sex %in% c("3", "both", "both sexes", "total"), sex_std := "Both"]
} else if ("sex_id_un" %in% names(raw)) {
  raw[, sex_std := fifelse(sex_id_un == 1, "M",
                            fifelse(sex_id_un == 2, "F", "Both"))]
}
# Keep only M and F (not Both — we want sex-specific life tables)
raw <- raw[sex_std %in% c("M", "F")]

# --- Parse life expectancy value ---
if ("ex" %in% names(raw)) {
  raw[, ex := as.numeric(ex)]
}

# --- Map to GBD age groups ---
# Our pipeline uses: "0-4", "5-9", ..., "75-79", ">80"
# The UN complete life table has single-year ages (0, 1, 2, ..., 100+)
# We take the ex at the start of each 5-year age group
age_group_starts <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
age_group_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                       "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                       "60-64", "65-69", "70-74", "75-79", ">80")

# Pipeline format: age_group_id is the integer age-start (0,5,...,80) and
# sex_id is the integer GBD code (1=male, 2=female). This is what the burden
# carries (from the IHME mortality converter) and what 07_compute_ylls.R
# merges on. Text labels ("0-4") or "M"/"F" do NOT merge and silently yield
# all-NA YLLs. age_group_labels below is kept only for the summary printout.
raw_filtered <- raw[age %in% age_group_starts]
raw_filtered[, age_group_id := as.integer(age)]
raw_filtered[, sex_id := fifelse(sex_std == "M", 1L, 2L)]

# --- Map UN location IDs to GBD location IDs ---
# Common mappings (ISO numeric → GBD location_id)
# For a full run we'd use the GBD hierarchy file, but for now handle Colombia
# and provide a lookup mechanism
# NOTE: this is a limited fallback for manually-downloaded files. For full
# coverage use util_download_un_lifetables.R, which builds the UN<->GBD
# crosswalk from the GBD2023 shapefile (loc_id<->ISO3) + UN /locations. Chile is
# GBD loc_id 98 (M49 152); loc_id 148 is Morocco -- the old table had this wrong.
un_to_gbd <- data.table(
  location_id_un = c(170, 76, 152, 156, 320, 484, 554, 710, 840),
  location_id_gbd = c(125, 135, 98,  6,   128, 130, 72,  196, 102),
  location_name_check = c("Colombia", "Brazil", "Chile", "China",
                           "Guatemala", "Mexico", "New Zealand",
                           "South Africa", "United States")
)

if ("location_id_un" %in% names(raw_filtered)) {
  raw_filtered <- merge(raw_filtered, un_to_gbd[, .(location_id_un, location_id_gbd)],
                        by = "location_id_un", all.x = TRUE)
  # For locations not in our lookup, use the UN ID as-is
  raw_filtered[is.na(location_id_gbd), location_id_gbd := location_id_un]
} else if ("iso3" %in% names(raw_filtered)) {
  # Could add ISO3 → GBD mapping here if needed
  cat("WARNING: Using ISO3 codes — GBD location ID mapping may be incomplete\n")
  raw_filtered[, location_id_gbd := NA_integer_]
} else {
  cat("WARNING: No location identifier found — cannot split by location\n")
  raw_filtered[, location_id_gbd := NA_integer_]
}

# --- Output: one CSV per location ---
output_cols <- c("year", "age_group_id", "sex_id", "ex", "location_id_gbd")
available_cols <- intersect(output_cols, names(raw_filtered))
out <- raw_filtered[, ..available_cols]
setnames(out, c("year", "location_id_gbd"),
         c("year_id", "location_id"),
         skip_absent = TRUE)

locations <- unique(out$location_id)
locations <- locations[!is.na(locations)]

cat("\nWriting life tables for", length(locations), "location(s):\n")
for (loc in locations) {
  loc_data <- out[location_id == loc]
  out_file <- file.path(LIFETABLE_DIR, paste0(loc, "_lifetable.csv"))
  fwrite(loc_data, out_file)
  cat("  Location", loc, ":", nrow(loc_data), "rows →", basename(out_file), "\n")
}

cat("\n=== Life table parsing complete ===\n")
cat("Output format: year_id, age_group_id, sex_id, ex, location_id\n")
cat("Age groups:", paste(age_group_labels, collapse = ", "), "\n")
cat("Years:", paste(range(out$year_id, na.rm = TRUE), collapse = "–"), "\n")
