# ==============================================================================
# 2_readers.R
# Minimalist, robust readers for NHS raw extracts:
# - CVDPREVENT indicator extracts
# - QOF 2023/24 sheets (CVD, Diabetes, Respiratory)
# - ePACT2 prescribing extracts (Items, Patients, List Size)
# - PHE / OHID prevalence models
# - National Diabetes Audit (NDA) care processes & treatment targets
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tibble)
})

# ------------------------------------------------------------------------------
# 1. CVDPREVENT Reader
# ------------------------------------------------------------------------------
read_cvdp <- function(file_path) {
  df <- suppressWarnings(read_excel(file_path, sheet = 2))
  code_col <- grep("^AreaCode$", names(df), value = TRUE)[1]
  num_col  <- grep("^Numerator$", names(df), value = TRUE)[1]
  den_col  <- grep("^Denominator$", names(df), value = TRUE)[1]
  
  res <- df %>%
    transmute(
      practice_code = as.character(.data[[code_col]]),
      numerator     = suppressWarnings(as.numeric(.data[[num_col]])),
      denominator   = suppressWarnings(as.numeric(.data[[den_col]]))
    ) %>%
    filter(!is.na(practice_code))
  
  return(res)
}

# Input:  file_path (character) - Path to ICB-level CVDPREVENT Excel workbook
# Output: tibble(icb_key, numerator, denominator) normalised by ICB name
read_cvdp_icb <- function(file_path) {
  df <- suppressMessages(suppressWarnings(read_excel(file_path, sheet = 2)))
  name_col <- grep("^AreaName$", names(df), value = TRUE)[1]
  num_col  <- grep("^Numerator$", names(df), value = TRUE)[1]
  den_col  <- grep("^Denominator$", names(df), value = TRUE)[1]
  
  df %>%
    transmute(
      icb_key     = toupper(str_trim(gsub("^NHS | INTEGRATED CARE BOARD$| ICB - .*$", "", .data[[name_col]], ignore.case = TRUE))),
      numerator   = suppressWarnings(as.numeric(.data[[num_col]])),
      denominator = suppressWarnings(as.numeric(.data[[den_col]]))
    ) %>%
    filter(!is.na(icb_key), !is.na(numerator)) %>%
    distinct(icb_key, .keep_all = TRUE)
}

# ------------------------------------------------------------------------------
# 2. ePACT2 Ingestion Reader
# ------------------------------------------------------------------------------
read_epact2 <- function(file_path, sheet_name = NULL) {
  sheets <- excel_sheets(file_path)
  target_sheet <- if (!is.null(sheet_name) && sheet_name %in% sheets) {
    sheet_name
  } else if ("Data" %in% sheets) {
    "Data"
  } else if ("data" %in% sheets) {
    "data"
  } else {
    sheets[1]
  }
  
  df <- suppressMessages(suppressWarnings(read_excel(file_path, sheet = target_sheet)))
  names(df) <- stringr::str_squish(gsub("[\r\n]", " ", names(df)))
  
  p_col     <- grep("Practice.*Code|Cost Centre Code", names(df), value = TRUE)[1]
  items_col <- grep("^Items$", names(df), value = TRUE)[1]
  pat_col   <- grep("Patient Count", names(df), value = TRUE)[1]
  ls_col    <- grep("List Size", names(df), value = TRUE)[1]
  
  res <- tibble(practice_code = as.character(df[[p_col]]))
  if (!is.na(items_col)) res$items     <- suppressWarnings(as.numeric(df[[items_col]]))
  if (!is.na(pat_col))   res$patients  <- suppressWarnings(as.numeric(df[[pat_col]]))
  if (!is.na(ls_col))    res$list_size <- suppressWarnings(as.numeric(df[[ls_col]]))
  
  return(res %>% filter(!is.na(practice_code)))
}

# Input:  raw_dir (character) - Directory containing ePACT2 list size and QOF CVD files
# Output: tibble(practice_code, list_size) coalescing ePACT2 list size with QOF list size
read_list_size <- function(raw_dir) {
  ls_epact <- read_epact2(file.path(raw_dir, "list size report Nov 2024.xlsx"))
  qof_hyp  <- suppressMessages(suppressWarnings(read_excel(file.path(raw_dir, "qof-2324-cvd practice level.xlsx"), sheet = "HYP", skip = 12, col_names = FALSE)))
  ls_qof   <- tibble(
    practice_code = as.character(qof_hyp[[6]]),
    qof_ls        = suppressWarnings(as.numeric(qof_hyp[[11]]))
  ) %>% filter(!is.na(practice_code))
  
  full_join(ls_epact, ls_qof, by = "practice_code") %>%
    transmute(
      practice_code = practice_code,
      list_size     = coalesce(list_size, qof_ls)
    )
}

# ------------------------------------------------------------------------------
# 3. QOF Sheet & Indicator Reader
# ------------------------------------------------------------------------------
read_qof_sheet <- function(file_path, sheet_name) {
  raw_hdr <- suppressWarnings(read_excel(file_path, sheet = sheet_name, skip = 10, n_max = 2, col_names = FALSE))
  r11 <- unlist(raw_hdr[1, ])
  r12 <- unlist(raw_hdr[2, ])
  
  data <- suppressWarnings(read_excel(file_path, sheet = sheet_name, skip = 12, col_names = FALSE))
  
  p_code    <- as.character(data[[6]])
  list_size <- suppressWarnings(as.numeric(data[[11]]))
  reg_col   <- if (sheet_name == "HYP") 14 else 12
  register  <- suppressWarnings(as.numeric(data[[reg_col]]))
  
  res <- tibble(
    practice_code = p_code,
    list_size     = list_size,
    register      = register
  )
  
  ind_idx <- which(!is.na(r11) & grepl("^[A-Z]{2,4}[0-9]{3}", r11))
  for (idx in ind_idx) {
    ind_name <- stringr::str_extract(r11[idx], "^[A-Z]{2,4}[0-9]{3}")
    num_col <- idx + 1
    den_col <- idx + 2
    res[[paste0(ind_name, "_n")]] <- suppressWarnings(as.numeric(data[[num_col]]))
    res[[paste0(ind_name, "_d")]] <- suppressWarnings(as.numeric(data[[den_col]]))
  }
  
  return(filter(res, !is.na(practice_code)))
}

read_qof_bp <- function(file_path) {
  data <- suppressWarnings(read_excel(file_path, sheet = "BP", skip = 11, col_names = FALSE))
  tibble(
    practice_code = as.character(data[[6]]),
    bp002_n       = suppressWarnings(as.numeric(data[[23]])),
    bp002_d       = suppressWarnings(as.numeric(data[[28]])) # Denominator plus PCAs
  ) %>% filter(!is.na(practice_code))
}

# ------------------------------------------------------------------------------
# 4. Modelled Prevalence Reader (PHE/OHID)
# ------------------------------------------------------------------------------
read_prevalence <- function(file_path, sheet_name = "GP_Practices") {
  df <- suppressWarnings(read_excel(file_path, sheet = sheet_name))
  names(df) <- stringr::str_squish(gsub("[\r\n]", " ", names(df)))
  
  code_col <- grep("^code$", names(df), ignore.case = TRUE, value = TRUE)[1]
  num_col  <- grep("^Count$", names(df), ignore.case = TRUE, value = TRUE)[1]
  den_col  <- grep("^Denominator$", names(df), ignore.case = TRUE, value = TRUE)[1]
  
  df %>%
    transmute(
      practice_code = as.character(.data[[code_col]]),
      prev_n        = suppressWarnings(as.numeric(.data[[num_col]])),
      prev_d        = suppressWarnings(as.numeric(.data[[den_col]]))
    ) %>%
    filter(!is.na(practice_code))
}

# ------------------------------------------------------------------------------
# 5. National Diabetes Audit (NDA) Readers
# ------------------------------------------------------------------------------
read_nda_cptt <- function(file_path) {
  df <- suppressWarnings(read_excel(file_path, sheet = "Type 2 and other CP_TT", skip = 2, col_names = FALSE))
  tibble(
    practice_code = as.character(df[[4]]),
    cp8_n         = suppressWarnings(as.numeric(df[[32]])),
    cp8_d         = suppressWarnings(as.numeric(df[[33]])),
    tthba1c_n     = suppressWarnings(as.numeric(df[[44]])),
    tthba1c_d     = suppressWarnings(as.numeric(df[[45]])),
    ttbp_n        = suppressWarnings(as.numeric(df[[53]])),
    ttbp_d        = suppressWarnings(as.numeric(df[[54]])),
    ttstatin_n    = suppressWarnings(as.numeric(df[[62]])),
    ttstatin_d    = suppressWarnings(as.numeric(df[[63]]))
  ) %>% filter(!is.na(practice_code))
}

read_nda_reg <- function(file_path) {
  df <- suppressWarnings(read_excel(file_path, sheet = "Type 2 and other registrations", skip = 2, col_names = FALSE))
  tibble(
    practice_code = as.character(df[[4]]),
    t2_prev_n     = suppressWarnings(as.numeric(df[[5]])),
    t2_prev_d     = suppressWarnings(as.numeric(df[[6]]))
  ) %>% filter(!is.na(practice_code))
}
