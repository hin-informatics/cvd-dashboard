# ==============================================================================
# run.R
# Main pipeline orchestrator for Action 1-3: Replicating & Expanding Dashboard Data
# Ingests raw public sources, builds the 8 condition tables, appends LOD
# benchmarks, and writes production CSV extracts to cvd-dashboard/R/output/.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
})

cat("========================================================================\n")
cat("      CVD DASHBOARD PIPELINE: ALL-LONDON EXPANSION (1,139 PRACTICES)    \n")
cat("========================================================================\n\n")

# Load pipeline components
r_dir <- if (dir.exists("cvd-dashboard/R")) "cvd-dashboard/R" else if (file.exists("config.R")) "." else "R"
source(file.path(r_dir, "config.R"))
source(file.path(r_dir, "online_cvdprevent.R"))
source(file.path(r_dir, "1_practices.R"))
source(file.path(r_dir, "2_readers.R"))
source(file.path(r_dir, "3_assemblers.R"))
source(file.path(r_dir, "4_benchmarks.R"))
source(file.path(r_dir, "5_facts.R"))

archive_dir <- ARCHIVE_DIR
facts_list  <- list()

online_active <- exists("USE_ONLINE_CVDPREVENT") && isTRUE(USE_ONLINE_CVDPREVENT)
tp_id         <- if (exists("CVDPREVENT_TIME_PERIOD")) CVDPREVENT_TIME_PERIOD else 20
all_london    <- exists("EXPAND_TO_ALL_LONDON") && isTRUE(EXPAND_TO_ALL_LONDON)

if (online_active) {
  cat(sprintf(">>> ONLINE CVDPREVENT EXTRACTION ACTIVE: TimePeriodID = %d <<<\n", tp_id))
} else {
  cat(">>> OFFLINE REPLICATION MODE (BASELINE) <<<\n")
}
cat(sprintf(">>> COVERAGE MODE: %s <<<\n\n", if (all_london) "ALL LONDON (1,139 GP Practices across 5 ICBs)" else "South London (364 GP Practices)"))

# 1. Practice Master
cat("[1/8] Generating Practice Master...\n")
pm <- get_practice_master(archive_dir, all_london = all_london, raw_dir = RAW_DIR)
cat(sprintf("      Active practices loaded: %d across %d PCNs and %d ICBs\n",
            nrow(pm), n_distinct(pm$pcn_code), n_distinct(pm$sub_icb_code)))

# Helper to save table with benchmark rows
save_condition_table <- function(df, filename) {
  df_with_bench <- append_benchmarks(
    df, filename, BASELINE_DIR,
    use_online = online_active,
    time_period_id = tp_id
  )
  out_path <- file.path(OUTPUT_DIR, filename)
  write_csv(df_with_bench, out_path, na = "")
  cat(sprintf("      Exported %-25s -> %s (%d rows, %d cols)\n",
              filename, out_path, nrow(df_with_bench), ncol(df_with_bench)))
  return(df_with_bench)
}

# 2. Hyper data
cat("\n[2/8] Assembling Hyper data (Hypertension)...\n")
hyper_df <- assemble_hyper_data(pm, RAW_DIR, use_online = online_active, time_period_id = tp_id)
facts_list[["Hyper data"]] <- save_condition_table(hyper_df, "Hyper data.csv")

# 3. Cholesterol
cat("\n[3/8] Assembling cholesterol (Lipid Management & FH)...\n")
chol_df <- assemble_cholesterol(pm, RAW_DIR, use_online = online_active, time_period_id = tp_id)
facts_list[["cholesterol"]] <- save_condition_table(chol_df, "cholesterol.csv")

# 4. CKD
cat("\n[4/8] Assembling CKD (Chronic Kidney Disease)...\n")
ckd_df <- assemble_ckd(pm, RAW_DIR, use_online = online_active, time_period_id = tp_id)
facts_list[["CKD"]] <- save_condition_table(ckd_df, "CKD.csv")

# 5. AF
cat("\n[5/8] Assembling AF (Atrial Fibrillation)...\n")
af_df <- assemble_af(pm, RAW_DIR, archive_dir, use_online = online_active, time_period_id = tp_id)
facts_list[["AF"]] <- save_condition_table(af_df, "AF.csv")

# 6. T2
cat("\n[6/8] Assembling T2 (Type 2 Diabetes)...\n")
t2_df <- assemble_t2(pm, RAW_DIR, archive_dir)
facts_list[["T2"]] <- save_condition_table(t2_df, "T2.csv")

# 7. Heart Failure
cat("\n[7/8] Assembling Heart Failure...\n")
hf_df <- assemble_heart_failure(pm, RAW_DIR, archive_dir)
facts_list[["Heart Failure"]] <- save_condition_table(hf_df, "Heart Failure.csv")

# 8. COPD & SMI
cat("\n[8/8] Assembling COPD and SMI & Obesity...\n")
copd_df <- assemble_copd(pm, RAW_DIR, archive_dir)
facts_list[["COPD"]] <- save_condition_table(copd_df, "COPD.csv")

smi_df <- assemble_smi_obesity(pm, BASELINE_DIR, RAW_DIR)
facts_list[["SMI and obesity"]] <- save_condition_table(smi_df, "SMI and obesity.csv")

# Final Step: Consolidate all fact tables into a single Excel file
cat("\n========================================================================\n")
cat("FINAL STEP: Consolidating condition tables into facts.xlsx...\n")
cat("========================================================================\n")
export_facts_xlsx(tables = facts_list, output_dir = OUTPUT_DIR, output_file = "facts.xlsx")

cat("\n========================================================================\n")
cat("SUCCESS: All 8 condition datasets and facts.xlsx successfully exported!\n")
cat("Output directory: ", OUTPUT_DIR, "\n")
cat("========================================================================\n")
