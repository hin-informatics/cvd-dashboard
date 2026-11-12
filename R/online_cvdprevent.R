# ==============================================================================
# online_cvdprevent.R
# Minimalist, robust extractor for the official CVDPREVENT REST API.
# Fetches clinical indicators at Practice (level 5) and ICB (level 7)
# with local disk caching to prevent redundant network requests.
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(readr)
})

# API Indicator registry mapping Indicator Code to Indicator ID & System Level
CVDP_INDICATORS <- list(
  CVDP007HYP  = list(id = 32, level = 5, name = "Hypertension BP control"),
  CVDP003CHOL = list(id = 14, level = 5, name = "Cholesterol primary prevention"),
  CVDP009CHOL = list(id = 34, level = 5, name = "Cholesterol secondary prevention LLT"),
  CVDP012CHOL = list(id = 54, level = 5, name = "Cholesterol secondary target"),
  CVDP003FH   = list(id = 10, level = 7, name = "FH genetically confirmed (ICB)"),
  CVDP002CKD  = list(id = 13, level = 5, name = "CKD uncoded 2 low eGFR"),
  CVDP003CKD  = list(id = 15, level = 5, name = "CKD high risk 1 low eGFR"),
  CVDP004CKD  = list(id = 17, level = 5, name = "CKD monitoring with ACR"),
  CVDP005CKD  = list(id = 19, level = 5, name = "CKD treated with RAS antagonists"),
  CVDP006CKD  = list(id = 29, level = 5, name = "CKD monitoring with eGFR"),
  CVDP007CKD  = list(id = 31, level = 5, name = "CKD ACR < 70 treated to BP target"),
  CVDP001AF   = list(id = 1,  level = 7, name = "AF prevalence (ICB)"),
  CVDP002AF   = list(id = 7,  level = 5, name = "AF anticoagulants (practice)")
)

# Input:  indicator_code (character, e.g. "CVDP007HYP")
#         time_period_id (integer, e.g. 20 for Sept 2024, 33 for March 2026)
#         cache_dir (character, path to local cache directory)
# Output: tibble(practice_code, numerator, denominator) for practice level (level 5)
#         tibble(icb_key, numerator, denominator) for ICB level (level 7)
fetch_online_cvdprevent <- function(indicator_code, time_period_id = 20, cache_dir = if (exists("CACHE_DIR")) CACHE_DIR else "cvd-dashboard/R/cache") {
  meta <- CVDP_INDICATORS[[indicator_code]]
  if (is.null(meta)) {
    stop(sprintf("Unknown CVDPREVENT indicator code: %s", indicator_code))
  }
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cache_file <- file.path(cache_dir, sprintf("%s_tp%d_lvl%d.rds", indicator_code, time_period_id, meta$level))
  
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  
  url <- sprintf("https://api.cvdprevent.nhs.uk/indicator/%d/rawDataJSON?timePeriodID=%d&systemLevelID=%d",
                 meta$id, time_period_id, meta$level)
  
  req <- request(url)
  resp <- req_perform(req)
  json <- fromJSON(resp_body_string(resp))
  
  raw_df <- as_tibble(json[[2]]) %>%
    filter(MetricCategoryTypeName == "Sex", CategoryAttribute == "Persons")
  
  if (meta$level == 5) {
    # Practice level
    res <- raw_df %>%
      transmute(
        practice_code = AreaCode,
        numerator     = suppressWarnings(as.numeric(Numerator)),
        denominator   = suppressWarnings(as.numeric(Denominator))
      ) %>%
      filter(!is.na(practice_code))
  } else {
    # ICB level
    res <- raw_df %>%
      transmute(
        icb_key     = toupper(str_trim(gsub("^NHS | INTEGRATED CARE BOARD$| ICB - .*$", "", AreaName, ignore.case = TRUE))),
        numerator   = suppressWarnings(as.numeric(Numerator)),
        denominator = suppressWarnings(as.numeric(Denominator))
      ) %>%
      filter(!is.na(icb_key))
  }
  
  saveRDS(res, cache_file)
  return(res)
}

# Input:  indicator_code (character, e.g. "CVDP007HYP")
#         time_period_id (integer, default: 20)
#         cache_dir (character, path to local cache directory)
# Output: tibble(numerator, denominator) for national England level (level 1)
fetch_online_cvdprevent_national <- function(indicator_code, time_period_id = 20, cache_dir = if (exists("CACHE_DIR")) CACHE_DIR else "cvd-dashboard/R/cache") {
  meta <- CVDP_INDICATORS[[indicator_code]]
  if (is.null(meta)) {
    stop(sprintf("Unknown CVDPREVENT indicator code: %s", indicator_code))
  }
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cache_file <- file.path(cache_dir, sprintf("%s_tp%d_national.rds", indicator_code, time_period_id))
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  
  url <- sprintf("https://api.cvdprevent.nhs.uk/indicator/%d/rawDataJSON?timePeriodID=%d&systemLevelID=1",
                 meta$id, time_period_id)
  req <- request(url)
  resp <- req_perform(req)
  json <- fromJSON(resp_body_string(resp))
  
  raw_df <- as_tibble(json[[2]]) %>%
    filter(MetricCategoryTypeName == "Sex", CategoryAttribute == "Persons")
  
  res <- tibble(
    numerator   = suppressWarnings(as.numeric(raw_df$Numerator[1])),
    denominator = suppressWarnings(as.numeric(raw_df$Denominator[1]))
  )
  
  saveRDS(res, cache_file)
  return(res)
}
