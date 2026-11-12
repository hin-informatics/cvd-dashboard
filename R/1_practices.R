# ==============================================================================
# 1_practices.R
# Extracts and standardises the London Practice Master table.
# Input:  data/archive/GP Practice Master List.xlsx
#         data/raw/epcn - Jan 2025.xlsx
# Output: Standardised practice_master data frame (364 South London or 1,139 All-London practices)
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

# Input:  archive_dir  (character) - Path to Archive directory (data/archive)
#         all_london   (logical)   - If TRUE, includes all 1,139 practices across London
#         region_label (character) - Label for region column (default from REGION_LABEL or "London")
#         raw_dir      (character) - Path to Raw Extracts directory (data/raw)
# Output: Standardised practice_master data frame (364 South London or 1,139 All-London practices)
get_practice_master <- function(archive_dir = NULL, all_london = NULL, region_label = NULL, raw_dir = NULL) {
  if (is.null(archive_dir)) {
    archive_dir <- if (exists("ARCHIVE_DIR")) ARCHIVE_DIR else file.path("data", "archive")
  }
  if (is.null(raw_dir)) {
    raw_dir <- if (exists("RAW_DIR")) RAW_DIR else file.path("data", "raw")
  }
  if (is.null(all_london)) {
    all_london <- exists("EXPAND_TO_ALL_LONDON") && isTRUE(EXPAND_TO_ALL_LONDON)
  }
  if (is.null(region_label)) {
    region_label <- if (exists("REGION_LABEL")) REGION_LABEL else if (isTRUE(all_london)) "London" else "South London"
  }
  
  gpm_file <- file.path(archive_dir, "GP Practice Master List.xlsx")
  if (!file.exists(gpm_file)) {
    stop("Practice master file not found: ", gpm_file)
  }
  
  df_sl <- suppressWarnings(read_excel(gpm_file, sheet = "Sheet2"))
  names(df_sl) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_sl)))
  
  sl_master <- df_sl %>%
    transmute(
      practice_code = `Partner Organisation Code`,
      practice_name = `Partner Name`,
      sub_icb_code  = `Practice Parent Sub ICB Loc Code`,
      sub_icb_name  = `Practice Parent Sub ICB Loc Name`,
      pcn_code      = `PCN Code`,
      pcn_name      = `PCN Name`,
      region        = region_label,
      country       = "England"
    ) %>%
    distinct(practice_code, .keep_all = TRUE)
  
  if (!isTRUE(all_london)) {
    return(sl_master)
  }
  
  # Load active North London GP practices from ePCN partner dataset
  epcn_file <- file.path(raw_dir, "epcn - Jan 2025.xlsx")
  if (!file.exists(epcn_file)) {
    warning("ePCN file not found; falling back to South London master: ", epcn_file)
    return(sl_master)
  }
  
  df_epcn <- suppressWarnings(read_excel(epcn_file, sheet = "PCN Core Partner Details"))
  names(df_epcn) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_epcn)))
  
  nl_master <- df_epcn %>%
    filter(
      (grepl("London", `Practice Parent Sub ICB Loc Name`, ignore.case = TRUE) |
       grepl("London", `PCN Parent Sub ICB Loc Name`, ignore.case = TRUE)),
      is.na(`Practice to PCN Relationship End Date`),
      !(`Partner Organisation Code` %in% sl_master$practice_code)
    ) %>%
    transmute(
      practice_code = `Partner Organisation Code`,
      practice_name = `Partner Name`,
      sub_icb_code  = `Practice Parent Sub ICB Loc Code`,
      sub_icb_name  = `Practice Parent Sub ICB Loc Name`,
      pcn_code      = `PCN Code`,
      pcn_name      = `PCN Name`,
      region        = region_label,
      country       = "England"
    ) %>%
    distinct(practice_code, .keep_all = TRUE)
  
  master <- bind_rows(sl_master, nl_master) %>%
    distinct(practice_code, .keep_all = TRUE)
  
  return(master)
}

if (sys.nframe() == 0) {
  if (file.exists("cvd-dashboard/R/config.R")) {
    source("cvd-dashboard/R/config.R")
  } else if (file.exists("config.R")) {
    source("config.R")
  }
  pm <- get_practice_master()
  cat(sprintf("Loaded practice_master: %d practices across %d PCNs and %d ICBs (Region: %s)\n",
              nrow(pm), n_distinct(pm$pcn_code), n_distinct(pm$sub_icb_code), unique(pm$region)))
}
