# ==============================================================================
# 5_facts.R
# Final step in pipeline: Consolidates the 8 condition fact datasets as 
# individual sheets into a single Excel workbook ("facts.xlsx"), translating
# column headers and ordering via the lookup table in cvd-dashboard/R/input/ so
# Tableau Desktop's "Replace Data Source" matches 100% of dimensions and
# measures with zero broken references (!).
#
# Inputs:
#   - tables: (Optional) Named list of data frames for the 8 condition datasets.
#             If NULL, reads from the CSV files in output_dir.
#   - mapping_file: Path to lookup CSV mapping CSV column names to Excel
#                   headers expected by Tableau (default: cvd-dashboard/R/input/tableau_column_mapping.csv).
#   - output_dir: Destination folder for facts.xlsx (default: cvd-dashboard/R/output).
#   - output_file: Workbook filename (default: "facts.xlsx").
#
# Outputs:
#   - "cvd-dashboard/R/output/facts.xlsx" containing 8 sheets aligned to Tableau's schema
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(writexl)
  library(dplyr)
})

# Apply lookup-table column renaming and ordering for a single sheet
format_for_tableau_facts <- function(df, sheet_name, mapping_df) {
  sheet_map <- mapping_df %>%
    filter(.data$sheet_name == !!sheet_name, .data$include_in_facts_xlsx == TRUE) %>%
    arrange(.data$ordinal)
  
  missing_cols <- setdiff(sheet_map$csv_column_name, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Sheet '%s' is missing expected CSV columns: %s",
      sheet_name, paste(missing_cols, collapse = ", ")
    ))
  }
  
  # Select in exact Tableau relation ordinal order and rename to target Excel headers
  out_df <- df[, sheet_map$csv_column_name, drop = FALSE]
  names(out_df) <- sheet_map$excel_column_name
  
  return(out_df)
}

export_facts_xlsx <- function(tables = NULL,
                              output_dir = NULL,
                              output_file = "facts.xlsx",
                              mapping_file = NULL) {
  if (is.null(output_dir)) {
    output_dir <- if (exists("OUTPUT_DIR")) OUTPUT_DIR else file.path("cvd-dashboard", "R", "output")
  }
  if (is.null(mapping_file)) {
    in_dir <- if (exists("INPUT_DIR")) INPUT_DIR else file.path("cvd-dashboard", "R", "input")
    mapping_file <- file.path(in_dir, "tableau_column_mapping.csv")
  }
  
  if (!file.exists(mapping_file)) {
    stop(sprintf("Column mapping lookup table not found at: %s", mapping_file))
  }
  
  mapping_df <- suppressMessages(read_csv(mapping_file, show_col_types = FALSE))
  target_path <- file.path(output_dir, output_file)
  
  # Standard sheet order and source CSV filenames
  condition_specs <- list(
    "Hyper data"      = "Hyper data.csv",
    "cholesterol"     = "cholesterol.csv",
    "CKD"             = "CKD.csv",
    "AF"              = "AF.csv",
    "T2"              = "T2.csv",
    "Heart Failure"   = "Heart Failure.csv",
    "COPD"            = "COPD.csv",
    "SMI and obesity" = "SMI and obesity.csv"
  )
  
  sheets <- list()
  
  for (sheet_name in names(condition_specs)) {
    if (!is.null(tables) && is.list(tables) && (sheet_name %in% names(tables))) {
      raw_df <- tables[[sheet_name]]
    } else {
      csv_file <- file.path(output_dir, condition_specs[[sheet_name]])
      if (!file.exists(csv_file)) {
        stop(sprintf("Required CSV file missing: %s", csv_file))
      }
      raw_df <- suppressMessages(read_csv(csv_file, show_col_types = FALSE))
    }
    
    sheets[[sheet_name]] <- format_for_tableau_facts(raw_df, sheet_name, mapping_df)
  }
  
  # Write multi-sheet Excel file
  writexl::write_xlsx(sheets, path = target_path, col_names = TRUE, format_headers = TRUE)
  
  # Keep cvd-dashboard/facts.xlsx in sync if present
  alt_facts <- file.path("cvd-dashboard", "facts.xlsx")
  if (file.exists(alt_facts)) {
    file.copy(target_path, alt_facts, overwrite = TRUE)
  }
  
  cat(sprintf("\n[Export facts.xlsx] Successfully written %d sheets to:\n  -> %s\n",
              length(sheets), target_path))
  for (sname in names(sheets)) {
    cat(sprintf("   - Sheet '%-16s': %d rows, %d cols (Tableau v2.5 schema aligned)\n",
                sname, nrow(sheets[[sname]]), ncol(sheets[[sname]])))
  }
  
  return(invisible(target_path))
}

# Standalone execution hook
if (sys.nframe() == 0) {
  if (file.exists("cvd-dashboard/R/config.R")) {
    source("cvd-dashboard/R/config.R")
  } else if (file.exists("config.R")) {
    source("config.R")
  }
  export_facts_xlsx()
}
