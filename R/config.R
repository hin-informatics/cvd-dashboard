# ==============================================================================
# Pipeline Configuration: Paths and Core Parameters
# ==============================================================================

# Working directory anchors - self-contained within cvd-dashboard/
cur_wd <- getwd()
if (basename(cur_wd) == "R" && basename(dirname(cur_wd)) == "cvd-dashboard") {
  CVD_DIR <- dirname(cur_wd)
} else if (basename(cur_wd) == "cvd-dashboard") {
  CVD_DIR <- cur_wd
} else if (dir.exists(file.path(cur_wd, "cvd-dashboard"))) {
  CVD_DIR <- file.path(cur_wd, "cvd-dashboard")
} else {
  CVD_DIR <- cur_wd
}

BASE_DIR     <- CVD_DIR
RAW_DIR      <- file.path(CVD_DIR, "data", "raw")
ARCHIVE_DIR  <- file.path(CVD_DIR, "data", "archive")
BASELINE_DIR <- file.path(CVD_DIR, "data", "baseline")
INPUT_DIR    <- file.path(CVD_DIR, "R", "input")
OUTPUT_DIR   <- file.path(CVD_DIR, "R", "output")
CACHE_DIR    <- file.path(CVD_DIR, "R", "cache")

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Scope: Geographic Coverage
EXPAND_TO_ALL_LONDON <- TRUE
SOUTH_LONDON_ICBS    <- c("72Q", "36L")
ALL_LONDON_ICBS      <- c("72Q", "36L", "93C", "A3A8R", "W2U3Z")

# Benchmark row labels
REGION_LABEL      <- if (isTRUE(EXPAND_TO_ALL_LONDON)) "London" else "South London"
COUNTRY_LABEL     <- "England"
OUTSIDE_REGION    <- if (isTRUE(EXPAND_TO_ALL_LONDON)) "Outside London" else "Outside South London"

# Online CVDPREVENT Data Extraction Settings
USE_ONLINE_CVDPREVENT  <- TRUE
CVDPREVENT_TIME_PERIOD <- 33  # 33 = To March 2026 (Latest), 28 = To Sept 2025, 20 = Sept 2024 (Baseline)
