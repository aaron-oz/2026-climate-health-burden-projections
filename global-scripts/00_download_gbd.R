# 00_download_gbd.R — Download GBD cause-specific mortality data
#
# Downloads cause-specific death estimates from the IHME Global Health Data
# Exchange (GHDx) / GBD Results Tool for the 17 temperature-sensitive causes.
#
# The GBD Results Tool provides data via:
#   1. Web interface: https://vizhub.healthdata.org/gbd-results/
#   2. IHME API (requires registration)
#
# This script attempts the API approach. If that fails, it generates the
# query parameters needed for manual download from the web interface.
#
# Output: MORTALITY_DIR/{location_id}_mortality.csv

if (!exists("SCRIPTS_DIR")) SCRIPTS_DIR <- dirname(c(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), ".")[1])
source(file.path(SCRIPTS_DIR, "config.R"))

library(data.table)
library(httr)
library(jsonlite)

# =============================================================================
# GBD cause IDs for the 17 temperature-sensitive causes
# =============================================================================

# Mapping from GBD acause labels to GBD cause_ids
# (From IHME_GBD_2023_HIERARCHIES)
CAUSE_MAP <- data.table(
  acause = c("cvd_ihd", "cvd_stroke", "cvd_htn", "cvd_cmp",
             "ckd", "diabetes", "lri", "resp_copd",
             "inj_drowning", "inj_homicide", "inj_suicide",
             "inj_disaster", "inj_mech", "inj_trans_road",
             "inj_trans_other", "inj_othunintent", "inj_animal"),
  cause_id = c(493, 494, 498, 499,
               589, 587, 322, 509,
               696, 724, 718,
               693, 699, 688,
               691, 703, 706),
  cause_name = c("Ischemic heart disease", "Stroke",
                 "Hypertensive heart disease", "Cardiomyopathy and myocarditis",
                 "Chronic kidney disease", "Diabetes mellitus",
                 "Lower respiratory infections", "COPD",
                 "Drowning", "Interpersonal violence", "Self-harm",
                 "Exposure to forces of nature", "Mechanical forces",
                 "Road injuries", "Other transport injuries",
                 "Other unintentional injuries", "Animal contact")
)

# =============================================================================
# Attempt API download
# =============================================================================

download_gbd_api <- function(location_id, year_start, year_end, cause_ids) {
  # GBD Results API endpoint
  base_url <- "https://api.healthdata.org/sdg/v1/GetResultsByLocation"

  log_msg("Attempting GBD API download for location", location_id)

  # Note: The GBD API may require authentication or may not be publicly available.
  # This is a best-effort attempt. If it fails, use the manual download approach.
  tryCatch({
    resp <- GET(base_url, query = list(
      location_id = location_id,
      year = paste(year_start:year_end, collapse = ","),
      cause_id = paste(cause_ids, collapse = ","),
      measure_id = 1,  # Deaths
      metric_id = 1,   # Number
      sex_id = "1,2",  # Male, Female
      age_group_id = "all"
    ))

    if (status_code(resp) == 200) {
      data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
      log_msg("API download successful:", nrow(data), "rows")
      return(as.data.table(data))
    } else {
      warning(paste("API returned status", status_code(resp)))
      return(NULL)
    }
  }, error = function(e) {
    warning(paste("API request failed:", conditionMessage(e)))
    return(NULL)
  })
}

# =============================================================================
# Manual download instructions
# =============================================================================

generate_manual_instructions <- function(location_id, year_start, year_end) {
  cat("\n")
  cat("=================================================================\n")
  cat("  MANUAL DOWNLOAD REQUIRED\n")
  cat("=================================================================\n")
  cat("\n")
  cat("The GBD API download did not succeed. Please download manually:\n\n")
  cat("1. Go to: https://vizhub.healthdata.org/gbd-results/\n\n")
  cat("2. Set the following parameters:\n")
  cat("   - GBD Estimate: Cause of death\n")
  cat("   - Measure: Deaths\n")
  cat("   - Metric: Number\n")
  cat("   - Cause: Select the following 17 causes:\n")
  for (i in 1:nrow(CAUSE_MAP)) {
    cat(paste0("     - ", CAUSE_MAP$cause_name[i], "\n"))
  }
  cat(paste0("   - Location: Location ID ", location_id, "\n"))
  cat(paste0("   - Year: ", year_start, " to ", year_end, "\n"))
  cat("   - Age: All ages (detailed)\n")
  cat("   - Sex: Male, Female\n\n")
  cat("3. Download the CSV and save it as:\n")
  cat(paste0("   ", file.path(MORTALITY_DIR,
                               paste0(location_id, "_mortality.csv")), "\n\n"))
  cat("4. Required columns: location_id, year_id, age_group_id, sex_id,\n")
  cat("   cause_id, cause_name, val (deaths)\n\n")
  cat("   If the downloaded file uses different column names, rename:\n")
  cat("   'val' -> 'deaths', 'year' -> 'year_id'\n\n")
  cat("=================================================================\n")
}

# =============================================================================
# Main
# =============================================================================

# Try API first
result <- download_gbd_api(LOCATION_ID, YEAR_START, YEAR_END, CAUSE_MAP$cause_id)

if (!is.null(result) && nrow(result) > 0) {
  # Standardize column names
  if ("val" %in% names(result)) setnames(result, "val", "deaths")
  if ("year" %in% names(result) && !"year_id" %in% names(result)) {
    setnames(result, "year", "year_id")
  }

  # Add acause labels
  result <- merge(result, CAUSE_MAP[, .(cause_id, acause)], by = "cause_id", all.x = TRUE)

  # Save
  out_file <- file.path(MORTALITY_DIR, paste0(LOCATION_ID, "_mortality.csv"))
  fwrite(result, out_file)
  log_msg("Mortality data saved to", out_file)

} else {
  generate_manual_instructions(LOCATION_ID, YEAR_START, YEAR_END)

  # Also save the cause map for reference
  fwrite(CAUSE_MAP, file.path(MORTALITY_DIR, "cause_map.csv"))
  log_msg("Cause map saved to", file.path(MORTALITY_DIR, "cause_map.csv"))
}
