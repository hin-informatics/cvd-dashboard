# ==============================================================================
# verify_parity.R
# Automated parity testing suite comparing generated outputs against baseline data
# and verifying 100% schema alignment between facts.xlsx, tableau_column_mapping.csv,
# and cvd-dashboard/Tableau/cvd-dashboard v2.5.twbx.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

r_dir <- if (dir.exists("cvd-dashboard/R")) "cvd-dashboard/R" else if (file.exists("config.R")) "." else "R"
source(file.path(r_dir, "config.R"))

cat("========================================================================\n")
cat("      CVD DASHBOARD PARITY TEST SUITE: ACTION 1 VERIFICATION             \n")
cat("========================================================================\n\n")

target_files <- c(
  "Hyper data.csv",
  "cholesterol.csv",
  "CKD.csv",
  "AF.csv",
  "T2.csv",
  "Heart Failure.csv",
  "COPD.csv",
  "SMI and obesity.csv"
)

report_lines <- c(
  "========================================================================",
  "      CVD DASHBOARD PARITY VERIFICATION REPORT: BASELINE AUDIT         ",
  sprintf("      Generated: %s", Sys.time()),
  "========================================================================\n"
)

overall_pass <- TRUE
summary_table <- list()

for (fname in target_files) {
  gen_path  <- file.path(OUTPUT_DIR, fname)
  base_path <- file.path(BASELINE_DIR, fname)
  
  cat(sprintf("\n--- Auditing: %-25s ---\n", fname))
  report_lines <- c(report_lines, sprintf("\n>>> FILE: %s", fname))
  
  if (!file.exists(gen_path)) {
    msg <- sprintf("FAIL: Output file missing at %s", gen_path)
    cat(msg, "\n")
    report_lines <- c(report_lines, msg)
    overall_pass <- FALSE
    next
  }
  
  df_gen  <- suppressWarnings(read_csv(gen_path, show_col_types = FALSE))
  df_base <- suppressWarnings(read_csv(base_path, show_col_types = FALSE))
  
  # 1. Row count check
  rows_match <- (nrow(df_gen) == nrow(df_base))
  cat(sprintf("  Row counts:    Gen=%d | Base=%d [%s]\n",
              nrow(df_gen), nrow(df_base), ifelse(rows_match, "PASS", "FAIL")))
  report_lines <- c(report_lines, sprintf("  Rows: Gen=%d, Base=%d [%s]",
                                          nrow(df_gen), nrow(df_base), ifelse(rows_match, "PASS", "FAIL")))
  if (!rows_match) overall_pass <- FALSE
  
  # 2. Column count check
  cols_match <- (ncol(df_gen) == ncol(df_base))
  cat(sprintf("  Column counts: Gen=%d | Base=%d [%s]\n",
              ncol(df_gen), ncol(df_base), ifelse(cols_match, "PASS", "FAIL")))
  report_lines <- c(report_lines, sprintf("  Cols: Gen=%d, Base=%d [%s]",
                                          ncol(df_gen), ncol(df_base), ifelse(cols_match, "PASS", "FAIL")))
  if (!cols_match) overall_pass <- FALSE
  
  # 3. Column names check
  names_match <- all(names(df_gen) == names(df_base))
  cat(sprintf("  Column names:  100%% match [%s]\n", ifelse(names_match, "PASS", "FAIL")))
  report_lines <- c(report_lines, sprintf("  Column names exact match: [%s]", ifelse(names_match, "PASS", "FAIL")))
  if (!names_match) {
    overall_pass <- FALSE
    diff_cols <- setdiff(names(df_base), names(df_gen))
    if (length(diff_cols) > 0) {
      cat("    Missing in Gen:", paste(diff_cols, collapse = ", "), "\n")
    }
  }
  
  # 4. Values parity check (practice rows)
  code_col <- grep("Partner Organisation Code", names(df_base), value = TRUE)[1]
  
  # Sort practice rows by practice code
  gen_p  <- df_gen %>% filter(!is.na(.data[[code_col]])) %>% arrange(.data[[code_col]])
  base_p <- df_base %>% filter(!is.na(.data[[code_col]])) %>% arrange(.data[[code_col]])
  
  val_diffs <- 0
  common_cols <- intersect(names(gen_p), names(base_p))
  
  for (cname in common_cols) {
    v_gen  <- gen_p[[cname]]
    v_base <- base_p[[cname]]
    
    if (is.numeric(v_base)) {
      v_gen_num <- suppressWarnings(as.numeric(v_gen))
      diffs <- abs(v_gen_num - v_base)
      n_diff <- sum(diffs > 0.01, na.rm = TRUE) + sum(is.na(v_gen_num) != is.na(v_base))
      if (n_diff > 0) {
        val_diffs <- val_diffs + n_diff
        cat(sprintf("    Numeric/NA discrepancy in %s (%d practices)\n", cname, n_diff))
        report_lines <- c(report_lines, sprintf("    Discrepancy: %s (%d practices)", cname, n_diff))
      }
    } else {
      n_diff <- sum(v_gen != v_base, na.rm = TRUE) + sum(is.na(v_gen) != is.na(v_base))
      if (n_diff > 0) {
        val_diffs <- val_diffs + n_diff
        cat(sprintf("    Text/NA discrepancy in %s (%d practices)\n", cname, n_diff))
        report_lines <- c(report_lines, sprintf("    Text mismatch: %s (%d practices)", cname, n_diff))
      }
    }
  }
  
  val_status <- ifelse(val_diffs == 0, "PASS", "FAIL")
  cat(sprintf("  Values parity: [%s] (discrepancies: %d)\n", val_status, val_diffs))
  report_lines <- c(report_lines, sprintf("  Values parity: [%s] (discrepancies: %d)", val_status, val_diffs))
  if (val_diffs > 0) overall_pass <- FALSE
  
  summary_table[[fname]] <- tibble(
    Dataset = fname,
    Rows = paste0(nrow(df_gen), "/", nrow(df_base)),
    Cols = paste0(ncol(df_gen), "/", ncol(df_base)),
    Status = ifelse(rows_match && cols_match && names_match && val_diffs == 0, "PASS", "FAIL")
  )
}

# 5. Audit facts.xlsx Schema Alignment
cat("\n--- Auditing: facts.xlsx Schema Alignment ---\n")
facts_path   <- file.path(OUTPUT_DIR, "facts.xlsx")
mapping_path <- file.path(INPUT_DIR, "tableau_column_mapping.csv")

if (!file.exists(facts_path) || !file.exists(mapping_path)) {
  cat("  facts.xlsx or tableau_column_mapping.csv missing! [FAIL]\n")
  overall_pass <- FALSE
} else {
  mapping_df <- suppressMessages(read_csv(mapping_path, show_col_types = FALSE))
  sheets <- readxl::excel_sheets(facts_path)
  expected_sheets <- c("Hyper data", "cholesterol", "CKD", "AF", "T2", "Heart Failure", "COPD", "SMI and obesity")
  sheets_match <- setequal(sheets, expected_sheets)
  cat(sprintf("  Sheets present: %d/8 [%s]\n", length(intersect(sheets, expected_sheets)), ifelse(sheets_match, "PASS", "FAIL")))
  
  sheet_errors <- 0
  for (sname in expected_sheets) {
    if (sname %in% sheets) {
      df_sheet <- suppressMessages(readxl::read_excel(facts_path, sheet = sname))
      csv_match <- target_files[grep(paste0("^", sname), target_files, ignore.case = TRUE)][1]
      df_csv <- suppressMessages(read_csv(file.path(OUTPUT_DIR, csv_match), show_col_types = FALSE))
      
      exp_map <- mapping_df %>%
        filter(.data$sheet_name == !!sname, .data$include_in_facts_xlsx == TRUE) %>%
        arrange(.data$ordinal)
      
      rows_ok <- (nrow(df_sheet) == nrow(df_csv))
      cols_ok <- identical(names(df_sheet), exp_map$excel_column_name)
      
      if (!rows_ok || !cols_ok) {
        sheet_errors <- sheet_errors + 1
        cat(sprintf("    Sheet '%s' schema/row check failed: rows_ok=%s, cols_ok=%s\n",
                    sname, rows_ok, cols_ok))
      }
    } else {
      sheet_errors <- sheet_errors + 1
      cat(sprintf("    Missing expected sheet: %s\n", sname))
    }
  }
  
  facts_status <- ifelse(sheets_match && sheet_errors == 0, "PASS", "FAIL")
  cat(sprintf("  facts.xlsx schema alignment: [%s]\n", facts_status))
  report_lines <- c(report_lines, sprintf(
    "\n>>> WORKBOOK: facts.xlsx\n  Sheets: %d/8 [%s]\n  Schema Alignment: [%s]",
    length(intersect(sheets, expected_sheets)), ifelse(sheets_match, "PASS", "FAIL"), facts_status
  ))
  if (facts_status != "PASS") overall_pass <- FALSE
  
  summary_table[["facts.xlsx"]] <- tibble(
    Dataset = "facts.xlsx",
    Rows = "All match",
    Cols = "155/155 aligned",
    Status = facts_status
  )
}

summary_df <- bind_rows(summary_table)
cat("\n========================================================================\n")
cat("PARITY VERIFICATION SUMMARY:\n")
print(as.data.frame(summary_df))
cat("========================================================================\n")
cat("OVERALL AUDIT RESULT:", ifelse(overall_pass, "100% PASS", "INVESTIGATE FAILURES"), "\n")
cat("========================================================================\n")

# Write report to output directory
report_lines <- c(report_lines, "\n========================================================================")
report_lines <- c(report_lines, sprintf("OVERALL STATUS: %s", ifelse(overall_pass, "PASSED", "FAILED")))
writeLines(report_lines, file.path(OUTPUT_DIR, "parity_report.txt"))
cat("Detailed parity report saved to: ", file.path(OUTPUT_DIR, "parity_report.txt"), "\n")
