# ==============================================================================
# 4_benchmarks.R
# Reconstructs and appends the Level-of-Detail (LOD) synthetic benchmark rows
# (e.g. Outside South London, England totals) to each condition dataset.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

append_benchmarks <- function(df, filename, baseline_dir, use_online = FALSE, time_period_id = 20) {
  base_file <- file.path(baseline_dir, filename)
  if (!file.exists(base_file)) {
    warning("Baseline file not found for benchmarks: ", base_file)
    return(df)
  }
  
  base_df   <- suppressWarnings(read_csv(base_file, show_col_types = FALSE))
  code_col  <- grep("Partner Organisation Code", names(base_df), value = TRUE)[1]
  synthetic <- base_df %>% filter(is.na(.data[[code_col]]))
  
  if (nrow(synthetic) == 0) {
    return(df)
  }
  
  # Ensure strict column type alignment between assembled practices and synthetic rows
  for (col in names(synthetic)) {
    if (is.numeric(df[[col]])) {
      synthetic[[col]] <- suppressWarnings(as.numeric(synthetic[[col]]))
    } else {
      synthetic[[col]] <- as.character(synthetic[[col]])
    }
  }
  
  # Update synthetic Outside South London row for updated online CVDPREVENT indicators
  if (isTRUE(use_online) && time_period_id != 20 && exists("fetch_online_cvdprevent_national", mode = "function")) {
    if (filename == "Hyper data.csv") {
      eng <- fetch_online_cvdprevent_national("CVDP007HYP", time_period_id)
      sl_n <- sum(df[["CVDP007HYP  (n)"]], na.rm = TRUE)
      sl_d <- sum(df[["CVDP007HYP  (d)"]], na.rm = TRUE)
      synthetic[["CVDP007HYP  (n)"]] <- eng$numerator - sl_n
      synthetic[["CVDP007HYP  (d)"]] <- eng$denominator - sl_d
    } else if (filename == "cholesterol.csv") {
      for (cd in c("CVDP003CHOL", "CVDP009CHOL", "CVDP012CHOL", "CVDP003FH")) {
        eng <- fetch_online_cvdprevent_national(cd, time_period_id)
        col_n <- if (cd == "CVDP003FH") "CVDP003-ICB (n)" else sub("CVDP0*", "P0", paste0(cd, " (n)"))
        col_d <- if (cd == "CVDP003FH") "CVDP003-ICB (d)" else sub("CVDP0*", "P0", paste0(cd, " (d)"))
        if (col_n %in% names(df) && col_d %in% names(df)) {
          sl_n <- sum(df[[col_n]], na.rm = TRUE)
          sl_d <- sum(df[[col_d]], na.rm = TRUE)
          synthetic[[col_n]] <- eng$numerator - sl_n
          synthetic[[col_d]] <- eng$denominator - sl_d
        }
      }
    } else if (filename == "CKD.csv") {
      for (cd in c("CVDP002CKD", "CVDP003CKD", "CVDP004CKD", "CVDP005CKD", "CVDP006CKD", "CVDP007CKD")) {
        eng <- fetch_online_cvdprevent_national(cd, time_period_id)
        col_n <- sub("CVDP0*", "P0", paste0(cd, " (n)"))
        col_d <- sub("CVDP0*", "P0", paste0(cd, " (d)"))
        if (col_n %in% names(df) && col_d %in% names(df)) {
          sl_n <- sum(df[[col_n]], na.rm = TRUE)
          sl_d <- sum(df[[col_d]], na.rm = TRUE)
          synthetic[[col_n]] <- eng$numerator - sl_n
          synthetic[[col_d]] <- eng$denominator - sl_d
        }
      }
    } else if (filename == "AF.csv") {
      for (cd in c("CVDP001AF", "CVDP002AF")) {
        eng <- fetch_online_cvdprevent_national(cd, time_period_id)
        col_n <- paste0(cd, " (n)")
        col_d <- paste0(cd, " (d)")
        if (col_n %in% names(df) && col_d %in% names(df)) {
          sl_n <- sum(df[[col_n]], na.rm = TRUE)
          sl_d <- sum(df[[col_d]], na.rm = TRUE)
          synthetic[[col_n]] <- eng$numerator - sl_n
          synthetic[[col_d]] <- eng$denominator - sl_d
      }
    }
  }
}

  # Update synthetic Outside Region labels (e.g. Outside London vs Outside South London)
  outside_lbl <- if (exists("OUTSIDE_REGION")) OUTSIDE_REGION else "Outside London"
  for (col in names(synthetic)) {
    if (is.character(synthetic[[col]])) {
      synthetic[[col]] <- gsub("Outside South London|Outside south London", outside_lbl, synthetic[[col]], ignore.case = TRUE)
    }
  }

  # Bind synthetic benchmark rows at the end
  out <- bind_rows(df, synthetic)
  return(out)
}
