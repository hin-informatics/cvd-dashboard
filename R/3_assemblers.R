# ==============================================================================
# 3_assemblers.R
# Condition table assemblers that join Practice Master and raw NHS extracts
# to construct the 8 condition datasets matching the Tableau data model.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(purrr)
})

# ------------------------------------------------------------------------------
# 1. Hyper data (Hypertension)
# ------------------------------------------------------------------------------
assemble_hyper_data <- function(pm, raw_dir, use_online = FALSE, time_period_id = 20) {
  qof_cvd_file <- file.path(raw_dir, "qof-2425-cvd practice level.xlsx")
  if (!file.exists(qof_cvd_file)) qof_cvd_file <- file.path(raw_dir, "qof-2324-cvd practice level.xlsx")

  qof_hyp_raw <- suppressWarnings(read_excel(qof_cvd_file, sheet = "HYP", skip = 12, col_names = FALSE))
  qof_hyp <- tibble(
    practice_code = as.character(qof_hyp_raw[[6]]),
    hyp_reg_d     = suppressWarnings(as.numeric(qof_hyp_raw[[11]])),
    hyp_reg_n     = suppressWarnings(as.numeric(qof_hyp_raw[[14]])),
    hyp008_n      = suppressWarnings(as.numeric(qof_hyp_raw[[32]])),
    hyp008_d      = suppressWarnings(as.numeric(qof_hyp_raw[[37]])), # Denominator plus PCAs
    hyp009_n      = suppressWarnings(as.numeric(qof_hyp_raw[[40]])),
    hyp009_d      = suppressWarnings(as.numeric(qof_hyp_raw[[45]]))  # Denominator plus PCAs
  ) %>% filter(!is.na(practice_code), practice_code != "Practice code")

  qof_bp <- read_qof_bp(qof_cvd_file)
  cvdp_hyp <- if (isTRUE(use_online) && exists("fetch_online_cvdprevent", mode = "function")) {
    fetch_online_cvdprevent("CVDP007HYP", time_period_id)
  } else {
    read_cvdp(file.path(raw_dir, "Hypertension Treated to appropriate threshold (all ages) (CVDP007HYP) Sept 2024.xlsx"))
  }
  
  # EPACT2 Anti-hypertensives
  epact_raw <- suppressWarnings(read_excel(file.path(raw_dir, "EPACT2 Hypertension drugs Nov 2024.xlsx"), sheet = "Data"))
  names(epact_raw) <- stringr::str_squish(gsub("[\r\n]", " ", names(epact_raw)))
  epact_pat <- epact_raw %>%
    group_by(`Practice Code`) %>%
    summarise(anti_hyp = first(`Identified Patient Count [1 of 2]`), .groups = "drop") %>%
    rename(practice_code = `Practice Code`)
  
  prev_hyp <- read_prevalence(file.path(raw_dir, "Hypertension_prevalence_estimates1.xlsx"))
  ls_rep   <- read_list_size(raw_dir)
  
  df <- pm %>%
    left_join(qof_hyp, by = "practice_code") %>%
    left_join(qof_bp, by = "practice_code") %>%
    left_join(cvdp_hyp, by = "practice_code") %>%
    left_join(epact_pat, by = "practice_code") %>%
    left_join(prev_hyp, by = "practice_code") %>%
    left_join(ls_rep, by = "practice_code") %>%
    transmute(
      `England (Hyper data)`                   = country,
      `ICB code`                               = sub_icb_code,
      `ICB name`                               = sub_icb_name,
      `Partner Name (Hyper data)`              = practice_name,
      `Partner Organisation Code (Hyper data)` = practice_code,
      `PCN Code (Hyper data)`                  = pcn_code,
      `PCN Name (Hyper data)`                  = pcn_name,
      `South London`                           = region,
      `Anti-hypertensives`                     = coalesce(anti_hyp, 0),
      `BP002 (d)`                              = bp002_d,
      `BP002 (n)`                              = bp002_n,
      `CVDP007HYP  (d)`                        = denominator,
      `CVDP007HYP  (n)`                        = numerator,
      `HYP008 (d)`                             = hyp008_d,
      `HYP008 (n)`                             = hyp008_n,
      `HYP009 (d)`                             = hyp009_d,
      `HYP009 (n)`                             = hyp009_n,
      `hypertension prevalence estimates (d)`  = prev_d,
      `hypertension prevalence estimates (n)`  = prev_n,
      `Hypertension register 2023-24 (d)`      = hyp_reg_d,
      `Hypertension register 2023-24 (n)`      = hyp_reg_n,
      `List size (EPACT2)`                     = list_size
    )
  return(df)
}

# ------------------------------------------------------------------------------
# 2. Cholesterol
# ------------------------------------------------------------------------------
assemble_cholesterol <- function(pm, raw_dir, use_online = FALSE, time_period_id = 20) {
  if (isTRUE(use_online) && exists("fetch_online_cvdprevent", mode = "function")) {
    c003 <- fetch_online_cvdprevent("CVDP003CHOL", time_period_id)
    c009 <- fetch_online_cvdprevent("CVDP009CHOL", time_period_id)
    c012 <- fetch_online_cvdprevent("CVDP012CHOL", time_period_id)
    c_fh <- fetch_online_cvdprevent("CVDP003FH", time_period_id)
  } else {
    c003 <- read_cvdp(file.path(raw_dir, "Cholesterol QRISK = 20% treated with LLT (CVDP003CHOL) Sept 2024.xlsx"))
    c009 <- read_cvdp(file.path(raw_dir, "CVD treated with LLT (CVDP009CHOL) Sept 2024.xlsx"))
    c012 <- read_cvdp(file.path(raw_dir, "CVD treated to cholesterol threshold (CVDP012CHOL) Sept 2024.xlsx"))
    c_fh <- read_cvdp_icb(file.path(raw_dir, "CVD Genetically confirmed prevalence (CVDP003FH) - ICB level.xlsx"))
  }
  
  high_s <- read_epact2(file.path(raw_dir, "EPACT2 High statins.xlsx"))
  low_s  <- read_epact2(file.path(raw_dir, "EPACT2 Low and mid statins.xlsx"))
  incli  <- read_epact2(file.path(raw_dir, "EPACT2 Inclisiran.xlsx"))
  ls_rep <- read_list_size(raw_dir)
  
  df <- pm %>%
    mutate(icb_key = toupper(str_trim(gsub("^NHS | INTEGRATED CARE BOARD$| ICB - .*$", "", sub_icb_name, ignore.case = TRUE)))) %>%
    left_join(c003, by = "practice_code") %>%
    left_join(c009, by = "practice_code", suffix = c("_c003", "_c009")) %>%
    left_join(c012, by = "practice_code") %>%
    left_join(c_fh, by = "icb_key", suffix = c("_c012", "_fh")) %>%
    left_join(high_s, by = "practice_code") %>%
    left_join(low_s, by = "practice_code", suffix = c("_high", "_low")) %>%
    left_join(incli, by = "practice_code") %>%
    left_join(ls_rep, by = "practice_code", suffix = c("_inc", "_ls")) %>%
    transmute(
      `Country1`                  = country,
      `General Practice`          = toupper(practice_name),
      `ICB Loc Code`              = sub_icb_code,
      `Integrated Care Board`     = sub_icb_name,
      `Partner Organisation Code` = practice_code,
      `PCN Code`                  = pcn_code,
      `PCN Name`                  = pcn_name,
      `Region1`                   = region,
      `CVDP003-ICB (d)`           = denominator_fh,
      `CVDP003-ICB (n)`           = numerator_fh,
      `High statins`              = coalesce(items_high, 0),
      `Inclisiran`                = coalesce(items, 0),
      `List Size (EPACT2)`        = list_size,
      `Low and medium statins`    = coalesce(items_low, 0),
      `P003CHOL (d)`              = coalesce(denominator_c003, 0),
      `P003CHOL (n)`              = coalesce(numerator_c003, 0),
      `P009CHOL (d)`              = coalesce(denominator_c009, 0),
      `P009CHOL (n)`              = coalesce(numerator_c009, 0),
      `P012CHOL (d)`              = coalesce(denominator_c012, 0),
      `P012CHOL (n)`              = coalesce(numerator_c012, 0),
      `Total Statins`             = coalesce(items_high, 0) + coalesce(items_low, 0)
    )
  return(df)
}

# ------------------------------------------------------------------------------
# 3. CKD (Chronic Kidney Disease)
# ------------------------------------------------------------------------------
assemble_ckd <- function(pm, raw_dir, use_online = FALSE, time_period_id = 20) {
  if (isTRUE(use_online) && exists("fetch_online_cvdprevent", mode = "function")) {
    p002 <- fetch_online_cvdprevent("CVDP002CKD", time_period_id)
    p003 <- fetch_online_cvdprevent("CVDP003CKD", time_period_id)
    p004 <- fetch_online_cvdprevent("CVDP004CKD", time_period_id)
    p005 <- fetch_online_cvdprevent("CVDP005CKD", time_period_id)
    p006 <- fetch_online_cvdprevent("CVDP006CKD", time_period_id)
    p007 <- fetch_online_cvdprevent("CVDP007CKD", time_period_id)
  } else {
    p002 <- read_cvdp(file.path(raw_dir, "CKD Uncoded - two low eGFRs with no recorded CKD (CVDP002CKD).xlsx"))
    p003 <- read_cvdp(file.path(raw_dir, "CKD High risk - one low eGFR with no recorded CKD (CVDP003CKD).xlsx"))
    p004 <- read_cvdp(file.path(raw_dir, "CKD Monitoring with ACR (CVDP004CKD).xlsx"))
    p005 <- read_cvdp(file.path(raw_dir, "CKD High risk treated with RAS antagonists (CVDP005CKD).xlsx"))
    p006 <- read_cvdp(file.path(raw_dir, "CKD Monitoring with eGFR (CVDP006CKD).xlsx"))
    p007 <- read_cvdp(file.path(raw_dir, "CKD ACR 70 treated to appropriate BP threshold (CVDP007CKD).xlsx"))
  }
  sglt2i <- read_epact2(file.path(raw_dir, "EPACT2 SGLT2i.xlsx"))
  ls_rep <- read_list_size(raw_dir)
  
  df <- pm %>%
    left_join(ls_rep, by = "practice_code") %>%
    left_join(p002, by = "practice_code") %>%
    left_join(p003, by = "practice_code", suffix = c("_p002", "_p003")) %>%
    left_join(p004, by = "practice_code") %>%
    left_join(p005, by = "practice_code", suffix = c("_p004", "_p005")) %>%
    left_join(p006, by = "practice_code") %>%
    left_join(p007, by = "practice_code", suffix = c("_p006", "_p007")) %>%
    left_join(sglt2i, by = "practice_code") %>%
    transmute(
      `Country (CKD)`                         = country,
      `General Practice (CKD)`                = toupper(practice_name),
      `Integrated Care Board (CKD)`           = sub_icb_name,
      `Partner Organisation Code (CKD)`       = practice_code,
      `PCN Code (CKD)`                        = pcn_code,
      `PCN Name (CKD)`                        = pcn_name,
      `Practice Parent Sub ICB Loc Code (CKD)` = sub_icb_code,
      `Region (CKD)`                          = region,
      `List Size (EPACT2) (CKD)`              = list_size,
      `P002CKD (d)`                           = coalesce(denominator_p002, 0),
      `P002CKD (n)`                           = coalesce(numerator_p002, 0),
      `P003CKD (d)`                           = coalesce(denominator_p003, 0),
      `P003CKD (n)`                           = coalesce(numerator_p003, 0),
      `P004CKD (d)`                           = coalesce(denominator_p004, 0),
      `P004CKD (n)`                           = coalesce(numerator_p004, 0),
      `P005CKD (d)`                           = coalesce(denominator_p005, 0),
      `P005CKD (n)`                           = coalesce(numerator_p005, 0),
      `P006CKD (d)`                           = coalesce(denominator_p006, 0),
      `P006CKD (n)`                           = coalesce(numerator_p006, 0),
      `P007CKD (d)`                           = coalesce(denominator_p007, 0),
      `P007CKD (n)`                           = coalesce(numerator_p007, 0),
      `SGLT2i`                                = coalesce(items, 0)
    )
  return(df)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 4. AF (Atrial Fibrillation)
# Input:  pm (tibble) - Practice master
#         raw_dir (character) - Path to raw extracts folder (data/raw)
#         archive_dir (character) - Path to combined Excel archive folder (data/archive)
#         use_online (logical) - Whether to query online CVDPREVENT API
#         time_period_id (integer) - CVDPREVENT period ID
# Output: tibble - 25-column AF fact table
# ------------------------------------------------------------------------------
assemble_af <- function(pm, raw_dir, archive_dir, use_online = FALSE, time_period_id = 20) {
  # 1. QOF AF indicators (6,271 England practices)
  qof_file <- file.path(raw_dir, "qof-2425-cvd practice level.xlsx")
  if (!file.exists(qof_file)) qof_file <- file.path(raw_dir, "qof-2324-cvd practice level.xlsx")

  qof_af_raw <- suppressWarnings(read_excel(qof_file, sheet = "AF", skip = 12, col_names = FALSE))
  qof_af <- tibble(
    practice_code = as.character(qof_af_raw[[6]]),
    af_reg_d      = suppressWarnings(as.numeric(qof_af_raw[[11]])),
    af_reg_n      = suppressWarnings(as.numeric(qof_af_raw[[12]])),
    af006_n       = suppressWarnings(as.numeric(qof_af_raw[[30]])),
    af006_d       = suppressWarnings(as.numeric(qof_af_raw[[31]])),
    af008_n       = suppressWarnings(as.numeric(qof_af_raw[[38]])),
    af008_d       = suppressWarnings(as.numeric(qof_af_raw[[39]]))
  ) %>% filter(!is.na(practice_code), practice_code != "Practice code")

  # 2. Prescribing patient counts from EPACT2
  oral_file <- file.path(raw_dir, "EPACT2 Oral Anticoagulants Jan 2025.xlsx")
  ar_file   <- file.path(raw_dir, "EPACT2 Anti arrhythmic drugs Jan 2025.xlsx")

  df_doac <- suppressWarnings(read_excel(oral_file, sheet = "DOAC"))
  names(df_doac) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_doac)))
  apix <- df_doac %>% filter(`BNF Chemical Substance` == "Apixaban") %>%
    transmute(practice_code = `Practice Code`, doac_pat = suppressWarnings(as.numeric(`Identified Patient Count [1 of 2]`)))

  df_warf <- suppressWarnings(read_excel(oral_file, sheet = "Warfarin"))
  names(df_warf) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_warf)))
  warf <- df_warf %>% filter(`BNF Chemical Substance` == "Warfarin sodium") %>%
    transmute(practice_code = `Practice Code`, warf_pat = suppressWarnings(as.numeric(`Identified Patient Count [1 of 2]`)))

  df_oth <- suppressWarnings(read_excel(oral_file, sheet = "Other"))
  names(df_oth) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_oth)))
  oth <- df_oth %>% filter(`BNF Chemical Substance` == "Acenocoumarol") %>%
    transmute(practice_code = `Practice Code`, oth_pat = suppressWarnings(as.numeric(`Identified Patient Count [1 of 2]`)))

  df_ar <- suppressWarnings(read_excel(ar_file, sheet = "Data"))
  names(df_ar) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_ar)))
  amio <- df_ar %>% filter(`BNF Chemical Substance` == "Amiodarone hydrochloride") %>%
    transmute(practice_code = `Practice Code`, amio_pat = suppressWarnings(as.numeric(`Identified Patient Count [1 of 2]`)))

  # 3. CVDPREVENT Indicators
  if (isTRUE(use_online) && exists("fetch_online_cvdprevent", mode = "function")) {
    af_p001 <- fetch_online_cvdprevent("CVDP001AF", time_period_id)
    af_p002 <- fetch_online_cvdprevent("CVDP002AF", time_period_id)
  } else {
    af_p001 <- read_cvdp_icb(file.path(raw_dir, "AF Prevalence (CVDP001AF) - ICB level.xlsx"))
    af_p002 <- read_cvdp(file.path(raw_dir, "AF treated with anticoagulants (CVDP002AF).xlsx"))
  }

  ls_rep  <- read_list_size(raw_dir)
  af_file <- file.path(archive_dir, "AF combined dataset.xlsx")
  af_base <- suppressWarnings(read_excel(af_file, sheet = "AF"))
  names(af_base) <- stringr::str_squish(gsub("[\r\n]", " ", names(af_base)))

  df <- pm %>%
    mutate(icb_key = toupper(str_trim(gsub("^NHS | INTEGRATED CARE BOARD$| ICB - .*$", "", sub_icb_name, ignore.case = TRUE)))) %>%
    left_join(af_base, by = c("practice_code" = "Partner Organisation Code")) %>%
    left_join(qof_af, by = "practice_code") %>%
    left_join(af_p001, by = "icb_key") %>%
    left_join(af_p002, by = "practice_code", suffix = c("_p001", "_p002")) %>%
    left_join(ls_rep, by = "practice_code") %>%
    left_join(apix, by = "practice_code") %>%
    left_join(warf, by = "practice_code") %>%
    left_join(oth, by = "practice_code") %>%
    left_join(amio, by = "practice_code") %>%
    transmute(
      `Country (AF)`                  = country,
      `ICB Loc Code (AF)`             = sub_icb_code,
      `ICB Loc Name (AF)`             = dplyr::recode(sub_icb_name, "NHS SOUTH EAST LONDON ICB - 72Q" = "SOUTH EAST LONDON"),
      `Partner Name (AF)`             = practice_name,
      `Partner Organisation Code (AF)` = practice_code,
      `PCN Code (AF)`                 = pcn_code,
      `PCN Name (AF)`                 = pcn_name,
      `Region (AF)`                   = region,
      `AF Prevalence estimates (d)`   = `Prevalence estimates (d)`,
      `AF Prevalence estimates (n)`   = `Prevalence estimates (n)`,
      `AF Register (d)`               = coalesce(`AF Register (d)`, af_reg_d),
      `AF Register (n)`               = coalesce(`AF Register (n)`, af_reg_n),
      `AF006 (d)`                     = coalesce(`AF006 (d)`, af006_d),
      `AF006 (n)`                     = coalesce(`AF006 (n)`, af006_n),
      `AF008 (d)`                     = coalesce(`AF008 (d)`, af008_d),
      `AF008 (n)`                     = coalesce(`AF008 (n)`, af008_n),
      `Anti-arhythmics`               = coalesce(`Anti-arhythmics`, amio_pat, 0),
      `CVDP001AF (d)`                 = coalesce(denominator_p001, `CVDP001AF (d)`),
      `CVDP001AF (n)`                 = coalesce(numerator_p001, `CVDP001AF (n)`),
      `CVDP002AF (d)`                 = coalesce(denominator_p002, `CVDP002AF (d)`),
      `CVDP002AF (n)`                 = coalesce(numerator_p002, `CVDP002AF (n)`),
      `Doac`                          = coalesce(`DOAC`, doac_pat, 0),
      `List Size (AF)`                = coalesce(`List Size (AF)`, list_size),
      `Other`                         = coalesce(`other`, oth_pat, 0),
      `Warfarin`                      = coalesce(`Warfarin`, warf_pat, 0)
    )
  return(df)
}

# ------------------------------------------------------------------------------
# 5. T2 (Type 2 Diabetes)
# Input:  pm (tibble) - Practice master
#         raw_dir (character) - Path to raw extracts folder (data/raw)
#         archive_dir (character) - Path to combined Excel archive folder (data/archive)
# Output: tibble - 18-column T2 fact table
# ------------------------------------------------------------------------------
assemble_t2 <- function(pm, raw_dir, archive_dir) {
  nda_file <- file.path(raw_dir, "National Diabetes Audit 2023-24 Data Release, England primary care.xlsx")
  cptt     <- read_nda_cptt(nda_file)
  reg      <- read_nda_reg(nda_file)
  ls_rep   <- read_list_size(raw_dir)

  t2_file <- file.path(archive_dir, "Diabetes combined dataset v2.xlsx")
  df_base <- suppressWarnings(read_excel(t2_file, sheet = "T2"))
  names(df_base) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_base)))

  df <- pm %>%
    left_join(df_base, by = c("practice_code" = "Partner Organisation Code")) %>%
    left_join(cptt, by = "practice_code") %>%
    left_join(reg, by = "practice_code") %>%
    left_join(ls_rep, by = "practice_code") %>%
    transmute(
      `England (T2)`                          = country,
      `Partner Name (T2)`                     = practice_name,
      `Partner Organisation Code (T2)`         = practice_code,
      `PCN Code (T2)`                         = pcn_code,
      `PCN Name (T2)`                         = pcn_name,
      `Practice Parent Sub ICB Loc Code (T2)` = sub_icb_code,
      `Practice Parent Sub ICB Loc Name (T2)` = sub_icb_name,
      `South london (T2)`                     = region,
      `8CP (d)`                               = coalesce(`8CP (d)`, cp8_d),
      `8CP (n)`                               = coalesce(`8CP (n)`, cp8_n),
      `T2 Prevalence (d)`                     = coalesce(`T2 Prevalence (d)`, list_size),
      `T2 Prevalence (n)`                     = coalesce(`T2 Prevalence (n)`, t2_prev_n),
      `TTBP (d)`                              = coalesce(`TTBP (d)`, ttbp_d),
      `TTBP (n)`                              = coalesce(`TTBP (n)`, ttbp_n),
      `TTHBA1C (d)`                           = coalesce(`TTHBA1C (d)`, tthba1c_d),
      `TTHBA1C (n)`                           = coalesce(`TTHBA1C (n)`, tthba1c_n),
      `TTStatin (d)`                          = coalesce(`TTStatin (d)`, ttstatin_d),
      `TTStatin (n)`                          = coalesce(`TTStatin (n)`, ttstatin_n)
    )
  return(df)
}

# ------------------------------------------------------------------------------
# 6. Heart Failure
# Input:  pm (tibble) - Practice master
#         raw_dir (character) - Path to raw extracts folder (data/raw)
#         archive_dir (character) - Path to combined Excel archive folder (data/archive)
# Output: tibble - 18-column Heart Failure fact table
# ------------------------------------------------------------------------------
assemble_heart_failure <- function(pm, raw_dir, archive_dir) {
  qof_file <- file.path(raw_dir, "qof-2425-cvd practice level.xlsx")
  if (!file.exists(qof_file)) qof_file <- file.path(raw_dir, "qof-2324-cvd practice level.xlsx")

  qof_hf_raw <- suppressWarnings(read_excel(qof_file, sheet = "HF", skip = 12, col_names = FALSE))
  qof_hf <- tibble(
    practice_code = as.character(qof_hf_raw[[6]]),
    hf_prev_d     = suppressWarnings(as.numeric(qof_hf_raw[[11]])),
    hf_prev_n     = suppressWarnings(as.numeric(qof_hf_raw[[12]])),
    hf003_n       = suppressWarnings(as.numeric(qof_hf_raw[[30]])),
    hf003_d       = suppressWarnings(as.numeric(qof_hf_raw[[35]])),
    hf006_n       = suppressWarnings(as.numeric(qof_hf_raw[[38]])),
    hf006_d       = suppressWarnings(as.numeric(qof_hf_raw[[43]])),
    hf007_n       = suppressWarnings(as.numeric(qof_hf_raw[[46]])),
    hf007_d       = suppressWarnings(as.numeric(qof_hf_raw[[51]])),
    hf008_n       = suppressWarnings(as.numeric(qof_hf_raw[[54]])),
    hf008_d       = suppressWarnings(as.numeric(qof_hf_raw[[59]]))
  ) %>% filter(!is.na(practice_code), practice_code != "Practice code")

  hf_file <- file.path(archive_dir, "Heart Failure combined dataset.xlsx")
  df_base <- suppressWarnings(read_excel(hf_file, sheet = "Sheet2"))
  names(df_base) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_base)))

  df <- pm %>%
    left_join(df_base, by = c("practice_code" = "Partner Organisation Code")) %>%
    left_join(qof_hf, by = "practice_code") %>%
    transmute(
      `England (Sheet2)`                         = country,
      `ICB name HF`                              = sub_icb_name,
      `Partner Name (Sheet2)`                    = practice_name,
      `Partner Organisation Code (Sheet2)`       = practice_code,
      `PCN Code (Sheet2)`                        = pcn_code,
      `PCN Name (Sheet2)`                        = pcn_name,
      `Practice Parent Sub ICB Loc Code (Sheet2)`= sub_icb_code,
      `South london`                             = region,
      `HF Prevalence (d)`                        = coalesce(`HF Prevalence (d)`, hf_prev_d),
      `HF Prevalence (n)`                        = coalesce(`HF Prevalence (n)`, hf_prev_n),
      `HF003 (d)`                                = coalesce(`HF003 (d)`, hf003_d),
      `HF003 (n)`                                = coalesce(`HF003 (n)`, hf003_n),
      `HF006 (d)`                                = coalesce(`HF006 (d)`, hf006_d),
      `HF006 (n)`                                = coalesce(`HF006 (n)`, hf006_n),
      `HF007 (d)`                                = coalesce(`HF007 (d)`, hf007_d),
      `HF007 (n)`                                = coalesce(`HF007 (n)`, hf007_n),
      `HF008 (d)`                                = coalesce(`HF008 (d)`, hf008_d),
      `HF008 (n)`                                = coalesce(`HF008 (n)`, hf008_n)
    )
  return(df)
}

# ------------------------------------------------------------------------------
# 7. COPD
# Input:  pm (tibble) - Practice master
#         raw_dir (character) - Path to raw extracts folder (data/raw)
#         archive_dir (character) - Path to combined Excel archive folder (data/archive)
# Output: tibble - 14-column COPD fact table
# ------------------------------------------------------------------------------
assemble_copd <- function(pm, raw_dir, archive_dir) {
  resp_file <- file.path(raw_dir, "qof-2425-resp practice level.xlsx")
  if (!file.exists(resp_file)) resp_file <- file.path(raw_dir, "qof-2324-resp-practice level.xlsx")

  qof_copd_raw <- suppressWarnings(read_excel(resp_file, sheet = "COPD", skip = 12, col_names = FALSE))
  qof_copd <- tibble(
    practice_code = as.character(qof_copd_raw[[6]]),
    copd_prev_d   = suppressWarnings(as.numeric(qof_copd_raw[[11]])),
    copd_prev_n   = suppressWarnings(as.numeric(qof_copd_raw[[12]])),
    copd010_n     = suppressWarnings(as.numeric(qof_copd_raw[[30]])),
    copd010_d     = suppressWarnings(as.numeric(qof_copd_raw[[31]])),
    copd014_n     = suppressWarnings(as.numeric(qof_copd_raw[[38]])),
    copd014_d     = suppressWarnings(as.numeric(qof_copd_raw[[39]]))
  ) %>% filter(!is.na(practice_code), practice_code != "Practice code")

  copd_file <- file.path(archive_dir, "COPD combined dataset.xlsx")
  df_base <- suppressWarnings(read_excel(copd_file, sheet = "COPD"))
  names(df_base) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_base)))

  df <- pm %>%
    left_join(df_base, by = c("practice_code" = "Partner Organisation Code")) %>%
    left_join(qof_copd, by = "practice_code") %>%
    transmute(
      `Country (COPD)`                  = country,
      `ICB Loc Code (COPD)`             = sub_icb_code,
      `ICB Loc Name (COPD)`             = sub_icb_name,
      `Partner Name (COPD)`             = coalesce(`Partner Name`, practice_name),
      `Partner Organisation Code (COPD)` = practice_code,
      `PCN Code (COPD)`                 = pcn_code,
      `PCN Name (COPD)`                 = pcn_name,
      `Region (COPD)`                   = region,
      `COPD Register (d)`               = coalesce(`COPD List Size (d)`, copd_prev_d),
      `COPD Register (n)`               = coalesce(`COPD Register (n)`, copd_prev_n),
      `COPD010 (d)`                     = coalesce(`COPD010 (d)`, copd010_d),
      `COPD010 (n)`                     = coalesce(`COPD010 (n)`, copd010_n),
      `COPD014 (d)`                     = coalesce(`COPD014 (d)`, copd014_d),
      `COPD014 (n)`                     = coalesce(`COPD014 (n)`, copd014_n)
    )
  return(df)
}

# ------------------------------------------------------------------------------
# 8. SMI and Obesity
# Input:  pm (tibble) - Practice master
#         baseline_dir (character) - Path to baseline CSV folder
#         raw_dir (character) - Path to raw data folder
# Output: tibble - 16-column SMI and Obesity fact table
# ------------------------------------------------------------------------------
assemble_smi_obesity <- function(pm, baseline_dir, raw_dir = NULL) {
  base_file <- file.path(baseline_dir, "SMI and obesity.csv")
  df_base <- if (file.exists(base_file)) {
    suppressWarnings(read_csv(base_file, show_col_types = FALSE)) %>%
      filter(!is.na(`Partner Organisation Code (SMI)`))
  } else {
    tibble()
  }

  smi_df <- tibble(practice_code = character())
  ob_df  <- tibble(practice_code = character())
  glp1_epact <- tibble(practice_code = character(), glp1_pat = numeric())

  if (!is.null(raw_dir)) {
    smi_file <- file.path(raw_dir, "qof-2425-smi practice level.xlsx")
    if (file.exists(smi_file)) {
      smi_raw <- suppressWarnings(read_excel(smi_file, sheet = "MH", skip = 12, col_names = FALSE))
      smi_df <- tibble(
        practice_code = as.character(smi_raw[[6]]),
        smi_prev_d   = suppressWarnings(as.numeric(smi_raw[[11]])),
        smi_prev_n   = suppressWarnings(as.numeric(smi_raw[[12]])),
        mh002_n      = suppressWarnings(as.numeric(smi_raw[[30]])),
        mh002_d      = suppressWarnings(as.numeric(smi_raw[[31]]))
      ) %>% filter(!is.na(practice_code), practice_code != "Practice code")
    }

    ob_dia_file <- file.path(raw_dir, "qof-2425-dia practice level.xlsx")
    if (file.exists(ob_dia_file)) {
      ob_raw <- suppressWarnings(read_excel(ob_dia_file, sheet = "OB", skip = 12, col_names = FALSE))
      ob_df <- tibble(
        practice_code = as.character(ob_raw[[6]]),
        ob_prev_d    = suppressWarnings(as.numeric(ob_raw[[11]])),
        ob_prev_n    = suppressWarnings(as.numeric(ob_raw[[12]]))
      ) %>% filter(!is.na(practice_code), practice_code != "Practice code")
    }

    ob_file <- file.path(raw_dir, "EPACT2 Obesity drugs - Jan 2025.xlsx")
    if (file.exists(ob_file)) {
      df_ob <- suppressWarnings(read_excel(ob_file, sheet = "Data"))
      names(df_ob) <- stringr::str_squish(gsub("[\r\n]", " ", names(df_ob)))
      glp1_epact <- df_ob %>%
        group_by(`Practice Code`) %>%
        summarise(glp1_pat = sum(`Identified Patient Count [1 of 2]`, na.rm = TRUE), .groups = "drop") %>%
        rename(practice_code = `Practice Code`)
    }
  }

  df <- pm %>%
    left_join(smi_df, by = "practice_code") %>%
    left_join(ob_df, by = "practice_code") %>%
    left_join(glp1_epact, by = "practice_code")

  if (nrow(df_base) > 0) {
    df <- df %>% left_join(df_base, by = c("practice_code" = "Partner Organisation Code (SMI)"))
  }

  get_col <- function(col_name) {
    if (col_name %in% names(df)) df[[col_name]] else rep(NA_real_, nrow(df))
  }

  out <- df %>%
    transmute(
      `England (SMI)`                  = country,
      `ICB Loc Code (SMI)`             = sub_icb_code,
      `ICB Loc Name (SMI)`             = sub_icb_name,
      `Partner Name (SMI)`             = practice_name,
      `Partner Organisation Code (SMI)` = practice_code,
      `PCN Code (SMI)`                 = pcn_code,
      `PCN Name (SMI)`                 = pcn_name,
      `South London (SMI)`             = region,
      `GLP1 (d)`                       = coalesce(ob_prev_n, get_col("GLP1 (d)")),
      `GLP1 (n)`                       = coalesce(glp1_pat, get_col("GLP1 (n)"), 0),
      `Obesity Prevalence (D)`         = coalesce(ob_prev_d, get_col("Obesity Prevalence (D)")),
      `Obesity Prevalence (N)`         = coalesce(ob_prev_n, get_col("Obesity Prevalence (N)")),
      `Physical Health Checks (d)`     = coalesce(mh002_d, get_col("Physical Health Checks (d)")),
      `Physical Health Checks (n)`     = coalesce(mh002_n, get_col("Physical Health Checks (n)")),
      `SMI prevalence (d)`             = coalesce(smi_prev_d, get_col("SMI prevalence (d)")),
      `SMI prevalence (n)`             = coalesce(smi_prev_n, get_col("SMI prevalence (n)"))
    )
  return(out)
}
