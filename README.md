# London Long Term Conditions (CVD) Dashboard & Data Pipeline

An automated data pipeline and interactive Tableau dashboard monitoring Cardiovascular Disease (CVD) and related long-term conditions across **All of London**.

---

## 📌 Executive Summary

This project extracts, standardizes, and visualizes clinical indicator and prevalence data across Primary Care Networks (PCNs) and GP practices in Greater London. Originally developed as a regional pilot covering 106 GP practices, the pipeline and dashboard have been expanded to cover the whole of London:

- **1,139 GP Practices** across **217 Primary Care Networks (PCNs)**
- **All 5 London Integrated Care Boards (ICBs)**:
  - NHS North Central London ICB (`108`)
  - NHS North East London ICB (`109`)
  - NHS North West London ICB (`107`)
  - NHS South East London ICB (`QM7`)
  - NHS South West London ICB (`QNX`)
- **Comparative Benchmarks**: GP Practice ↔ PCN ↔ ICB ↔ All-London ↔ England National

---

## 🩺 Clinical Conditions Tracked

The pipeline processes official Quality and Outcomes Framework (QOF) and cardiovascular indicators across eight long-term condition domains:

1. **Atrial Fibrillation (AF)** — Prevalence, treatment with anticoagulants, and risk assessment (CHADS-VASc).
2. **Chronic Kidney Disease (CKD)** — Prevalence, uncoded low eGFR detection, and blood pressure control.
3. **Chronic Obstructive Pulmonary Disease (COPD)** — Prevalence, diagnosis confirmation via spirometry, and management.
4. **Diabetes / Type 2 (DM)** — Prevalence, IFCC HbA1c control targets, and lifestyle intervention.
5. **Heart Failure (HF)** — Prevalence, diagnosis confirmation by echocardiogram/specialist, and beta-blocker/ACE-I therapies.
6. **Hypertension (HYP)** — Prevalence, blood pressure target attainment (<140/90 mmHg).
7. **Obesity (OB)** — Register of patients aged 18+ with BMI ≥ 30.
8. **Severe Mental Illness (SMI)** — Comprehensive physical health assessments, CVD risk profiling, and blood pressure monitoring.

---

## 📂 Project Architecture

The `cvd-dashboard` repository is **100% self-contained** and operates independently without external directory dependencies.

```text
cvd-dashboard/
├── data/
│   ├── raw/                  # Source NHS England QOF workbooks & ODS reference files
│   │   ├── qof-2324-cvd practice level.xlsx   # Official QOF 2023/24 CVD publication
│   │   ├── epraccur.csv                       # ODS GP Practice reference dataset
│   │   ├── eclp.csv                           # ODS PCN-to-Practice mapping
│   │   └── ...                                # Condition-specific extracts & API files
│   ├── baseline/             # Validated baseline datasets for parity auditing
│   │   └── facts.xlsx
│   └── archive/              # Historical source snapshots
│
├── R/
│   ├── config.R              # Project configuration, dynamic root discovery, ICB codes
│   ├── 1_practices.R         # Builds London GP practice master list & PCN hierarchy
│   ├── 2_extractors.R        # Extracts prevalence, numerators, denominators, & rates
│   ├── 3_assemblers.R        # Assembles condition fact tables & validates parity
│   ├── run.R                 # End-to-end master orchestrator
│   ├── input/
│   │   └── tableau_column_mapping.csv         # Schema and field mapping definitions
│   └── output/
│       ├── facts.xlsx        # Consolidated facts workbook powering Tableau
│       ├── parity_report.txt # Automated data validation report
│       └── *.csv             # Modular condition extracts (AF, CKD, COPD, etc.)
│
├── Tableau/
│   └── cvd-dashboard v2.5.twbx  # Packaged London Long Term Conditions Dashboard
│
├── .gitignore                # Git ignore configuration
└── README.md                 # Project documentation
```

---

## 🚀 Getting Started

### Prerequisites

1. **R Environment**
   - R version ≥ 4.2.0 (tested on R 4.6.1).
   - Required packages:
     ```R
     install.packages(c("readxl", "writexl", "dplyr", "purrr", "stringr", "janitor", "tidyr", "rprojroot"))
     ```

2. **Tableau Desktop / Tableau Reader**
   - Tableau Desktop 2023.1 or higher (or [Tableau Reader](https://www.tableau.com/products/reader) for view-only access).

---

### Running the Data Pipeline

To re-extract, assemble, and build the full London dataset from source:

#### Option A: Command Line / Terminal
```bash
cd cvd-dashboard
Rscript R/run.R
```

#### Option B: R / RStudio Console
```R
setwd("path/to/cvd-dashboard")
source("R/run.R")
```

The pipeline will:
1. Load environment and directory paths dynamically via `rprojroot`.
2. Filter ODS GP practice registers to all active practices across the 5 London ICBs (`1,139` practices).
3. Extract indicators for all conditions from the 2023/24 QOF workbook.
4. Harmonize schemas, assemble consolidated fact tables, and run parity checks.
5. Export `R/output/facts.xlsx` and individual condition CSVs.

---

### Opening the Tableau Dashboard

1. Launch Tableau Desktop.
2. Open [`Tableau/cvd-dashboard v2.5.twbx`](file:///C:/Users/k2481754/OneDrive%20-%20King's%20College%20London/Documents/My%20Projects/CVD%20Dashboard/cvd-dashboard/Tableau/cvd-dashboard%20v2.5.twbx).
3. The packaged workbook is pre-bundled with the London-wide dataset and narrative wording:
   - Interactive multi-tier filtering by **ICB**, **PCN**, and **GP Practice**.
   - Performance benchmarks against **PCN**, **ICB**, **London**, and **England**.
   - Individual story tabs for each condition domain.

---

## 📊 Data Sources & Governance

- **NHS England**: Quality and Outcomes Framework (QOF) 2023-24 recorded disease prevalences and achievement indicators.
- **NHS Digital Organisation Data Service (ODS)**: Practice details (`epraccur`) and Primary Care Network membership (`eclp`).
- **Licence**: Contains public sector information licensed under the [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

---

## 🛠️ Pipeline Modules Detail

| Script | Input | Output | Purpose |
| :--- | :--- | :--- | :--- |
| `config.R` | Filesystem root | Environment variables & ICB mappings | Configures paths dynamically using `rprojroot::find_root()` and defines London ICB codes. |
| `1_practices.R` | `data/raw/epraccur.csv`, `data/raw/eclp.csv` | Practice master tibble | Resolves London ICBs, joins PCN memberships, and filters to 1,139 active GP practices. |
| `2_extractors.R` | `data/raw/qof-2324-cvd practice level.xlsx` | Condition tibbles list | Standardizes column headers, extracts register sizes, numerators, denominators, and rates. |
| `3_assemblers.R` | Practice master, extracted tibbles, `tableau_column_mapping.csv` | Final joined datasets | Blends practice metadata with condition indicators; generates `facts.xlsx` and audit reports. |
| `run.R` | Modular R scripts | `R/output/facts.xlsx`, `R/output/*.csv` | Master execution entrypoint. |

---

## 📝 Changelog & Version History

- **v2.5 (Current)**:
  - Expanded pipeline and dashboard to All-London scope (1,139 practices, 217 PCNs, 5 ICBs).
  - Standardized all dashboard text, calculated field definitions, titles, and legends to reflect the London-wide comparative narrative.
  - Made repository 100% self-contained with zero external folder dependencies.
- **v2.0**: Initial regional pilot dashboard covering South East London.
