# util_download_un_lifetables.R — Download life tables from UN Population Data Portal API
#
# Downloads life expectancy E(x) by single age, sex, year from the
# UN World Population Prospects 2024 via the Data Portal API.
#
# Requires a Bearer token. Get one at:
#   https://population.un.org/dataportalapi/index.html
#
# Usage:
#   export UN_API_TOKEN="your_token_here"
#   Rscript util_download_un_lifetables.R
#
# Output: data/lifetables/{gbd_location_id}_lifetable.csv

library(data.table)

PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT",
                           unset = normalizePath(file.path(getwd(), "..")))
LIFETABLE_DIR <- file.path(PROJECT_ROOT, "data", "lifetables")
dir.create(LIFETABLE_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Bearer token ---
TOKEN <- Sys.getenv("UN_API_TOKEN")
if (TOKEN == "") stop("Set UN_API_TOKEN environment variable first.")

# --- Settings ---
BASE_URL <- "https://population.un.org/dataportalapi/api/v1"
INDICATOR_ID <- 76  # Life expectancy E(x) - complete (single-year ages)

# UN location IDs to download (ISO 3166-1 numeric codes)
# Add more as needed. Full list: curl {BASE_URL}/locations
UN_LOCATIONS <- c(170)  # 170 = Colombia

# UN → GBD location ID mapping
UN_TO_GBD <- data.table(
  un_id = c(170, 76, 152, 156, 320, 484, 554, 710, 840),
  gbd_id = c(125, 135, 148, 6, 128, 130, 72, 196, 102),
  name = c("Colombia", "Brazil", "Chile", "China", "Guatemala",
           "Mexico", "New Zealand", "South Africa", "United States")
)

# Age group mapping: take ex at the start of each 5-year bin
AGE_STARTS <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
AGE_LABELS <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", ">80")

# --- Download function ---
download_lifetable <- function(loc_un, token) {
  url <- paste0(BASE_URL, "/data/indicators/", INDICATOR_ID,
                "/locations/", loc_un,
                "/start/1990/end/2100",
                "?pagingInHeader=false&format=csv")

  # Download via httr (the API returns pipe-delimited CSV with all rows)
  library(httr)
  resp <- GET(url, add_headers(Authorization = paste("Bearer", token)))

  if (status_code(resp) != 200) {
    warning(paste("API returned status", status_code(resp), "for location", loc_un))
    return(NULL)
  }

  # Parse the pipe-delimited response (skip the "sep =|" header line)
  raw_text <- content(resp, "text", encoding = "UTF-8")
  dt <- fread(text = raw_text, sep = "|", skip = 1)
  dt
}

# --- Main ---
cat("=== Downloading UN WPP Life Tables ===\n")

for (loc_un in UN_LOCATIONS) {
  loc_name <- UN_TO_GBD[un_id == loc_un, name]
  if (length(loc_name) == 0) loc_name <- paste("UN ID", loc_un)
  cat("\nDownloading:", loc_name, "(UN ID:", loc_un, ")...\n")

  raw <- download_lifetable(loc_un, TOKEN)

  if (is.null(raw) || nrow(raw) == 0) {
    cat("  No data returned, skipping\n")
    next
  }
  cat("  Raw rows:", nrow(raw), "\n")

  # Standardize column names to lowercase
  setnames(raw, tolower(names(raw)))

  # Filter: Median variant, Male + Female only
  raw <- raw[tolower(variant) == "median"]
  raw <- raw[tolower(sex) %in% c("male", "female")]
  raw[, sex_std := fifelse(tolower(sex) == "male", "M", "F")]

  # Extract year and age
  raw[, year_id := as.integer(timelabel)]
  raw[, age := as.integer(agestart)]
  raw[, ex := as.numeric(value)]

  # Take ex at the start of each 5-year age group
  out <- raw[age %in% AGE_STARTS]
  out[, age_group_id := AGE_LABELS[match(age, AGE_STARTS)]]

  # Map to GBD location ID
  gbd_id <- UN_TO_GBD[un_id == loc_un, gbd_id]
  if (length(gbd_id) == 0) gbd_id <- loc_un

  out <- out[, .(year_id, age_group_id, sex_id = sex_std, ex, location_id = gbd_id)]

  # Save
  out_file <- file.path(LIFETABLE_DIR, paste0(gbd_id, "_lifetable.csv"))
  fwrite(out, out_file)
  cat("  Saved:", nrow(out), "rows →", basename(out_file), "\n")
  cat("  Years:", paste(range(out$year_id), collapse = "–"), "\n")
  cat("  Age groups:", length(unique(out$age_group_id)), "\n")
}

cat("\n=== Download complete ===\n")
