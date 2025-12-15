/*==============================================================================
   MASTER DO FILE: QP-SCIE Data Processing Pipeline (2011-2022)
   
   This file orchestrates the complete data processing workflow in a modular,
   structure.
   
   WORKFLOW STRUCTURE:
   
   Phase 1: Data Extraction & Cleaning
   ├── 1. SCIE_clean.do          → Clean balance sheet data (SCIE)
   ├── 2. firm_clean.do          → Clean firm characteristics (QP)
   └── 3. worker_skills.do       → Process workers & calculate skills (QP)
   
   Phase 2: Data Integration
   └── 4. merge_final.do         → Merge all sources
                                    Output: final_data_2011_2022.dta
   
   Phase 3: Variable Construction
   ├── 5. prepare_deflators.do   → Import & prepare deflators
   │                                Output: deflators.dta
   └── 6. construct_intangibles.do → Deflate variables & build intangible capital
                                      Output: final_data_2011_2022_intangibles.dta
   
   Phase 4: Finalization
   └── 7. data_clean_final.do    → Clean, winsorize & create derived variables
                                    Output: final_data_2011_2022_cleaned_winsorized.dta
     
==============================================================================*/

clear all
set more off
set maxvar 10000

/*==============================================================================
   CONFIGURATION
==============================================================================*/

* SET YOUR ROOT DIRECTORY HERE
global rootdir "C:\Users\alexv\OneDrive\Documentos\3-Papers\Intangible_skill_Vicente\QP_SCIE"

* Verify directory exists
capture cd "$rootdir"
if _rc != 0 {
    di as error "ERROR: Root directory not found!"
    di as error "Please update the rootdir path in this file"
    exit 601
}

cd "$rootdir/code"

* Start master log file
cap log close
log using "master_pipeline.log", replace

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║                 QP-SCIE DATA PROCESSING PIPELINE               ║"
di "║                         2011-2022                              ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""
di "Root directory: $rootdir"
di "Start date:     `c(current_date)'"
di "Start time:     `c(current_time)'"
di ""
di "This pipeline will:"
di "  1. Clean SCIE balance sheet data"
di "  2. Clean QP firm-level data"
di "  3. Process QP worker data & calculate skills"
di "  4. Merge all datasets"
di "  5. Prepare price deflators"
di "  6. Construct intangible capital stocks"
di "  7. Finalize dataset with cleaning & derived variables"
di ""

/*==============================================================================
   PHASE 1: DATA EXTRACTION & CLEANING
==============================================================================*/

di "╔════════════════════════════════════════════════════════════════╗"
di "║                    PHASE 1: DATA CLEANING                      ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""

* Module 1: Clean SCIE data
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 1/7: Cleaning SCIE balance sheet data                  │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/SCIE_clean.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"

* Module 2: Clean firm-level QP data
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 2/7: Cleaning QP firm-level data                       │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/firm_clean.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"

* Module 3: Process worker data and create skill measures
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 3/7: Processing workers & calculating skills           │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/worker_skills.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"

/*==============================================================================
   PHASE 2: DATA INTEGRATION
==============================================================================*/

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║                  PHASE 2: DATA INTEGRATION                     ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""

* Module 4: Merge all datasets
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 4/7: Merging QP + SCIE datasets                        │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/merge_final.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022.dta"

/*==============================================================================
   PHASE 3: VARIABLE CONSTRUCTION
==============================================================================*/

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║               PHASE 3: VARIABLE CONSTRUCTION                   ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""

* Module 5: Prepare deflators
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 5/7: Preparing price deflators                         │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/prepare_deflators.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"
di "  Output: deflators.dta"

* Module 6: Construct intangible capital
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 6/7: Constructing intangible capital stocks            │"
di "│ Method: Peters & Taylor (2017) - Perpetual Inventory Method   │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/construct_intangibles.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022_intangibles.dta"

/*==============================================================================
   PHASE 4: FINALIZATION
==============================================================================*/

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║                    PHASE 4: FINALIZATION                       ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""

* Module 7: Final cleaning and derived variables
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 7/7: Final cleaning, winsorization & derived variables │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/data_clean_final.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"

/*==============================================================================
   PIPELINE COMPLETION SUMMARY
==============================================================================*/

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║                  PIPELINE COMPLETED SUCCESSFULLY               ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""
di "End time: `c(current_time)'"
di ""
di "══════════════════════════════════════════════════════════════════"
di "FINAL DATASETS CREATED"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Intermediate files (for reference):"
di "  1. SCIE_2011_2022.dta"
di "  2. firm_2011_2022.dta"
di "  3. worker_skills_2011_2022.dta"
di "  4. firm_skills_wages_2011_2022.dta"
di "  5. final_data_2011_2022.dta"
di "  6. deflators.dta"
di "  7. final_data_2011_2022_intangibles.dta"
di ""
di "Final analysis-ready datasets:"
di "  → final_data_2011_2022_cleaned.dta"
di "    (All observations with quality flags for robustness checks)"
di ""
di "  → final_data_2011_2022_cleaned_winsorized.dta ⭐ RECOMMENDED"
di "    (Clean sample, winsorized, ready for analysis)"
di ""
di "  → cleaning_summary_by_year.xlsx"
di "    (Summary statistics by year)"
di ""
di "══════════════════════════════════════════════════════════════════"
di "KEY VARIABLES IN FINAL DATASET"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Intangible Capital Stocks (real 2020 prices):"
di "  • K_knowledge              Knowledge capital (from R&D)"
di "  • K_org                    Organization capital (from SG&A)"
di "  • K_intangible_total       Total intangible capital"
di "  • K_physical               Physical capital"
di "  • K_total                  Total capital (Physical + Intangible)"
di ""
di "Intensity Measures:"
di "  • intang_intensity         K_intangible / K_total"
di "  • knowledge_intensity      K_knowledge / K_intangible"
di "  • org_intensity            K_org / K_intangible"
di "  • share_skilled            Share of workers with tertiary education"
di ""
di "Investment Rates:"
di "  • inv_rate_knowledge       R&D investment / K_total"
di "  • inv_rate_org             SG&A investment / K_total"
di "  • inv_rate_physical        Physical investment / K_total"
di ""
di "Productivity Measures:"
di "  • labor_productivity       Revenue per worker"
di "  • capital_intensity        Capital per worker"
di ""
di "All monetary variables in real 2020 prices with '_real' suffix"
di "All key variables winsorized (1%-99%) with '_w' suffix"
di ""
di "══════════════════════════════════════════════════════════════════"
di "METHODOLOGICAL NOTES"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Intangible Capital Construction:"
di "  • Method: Peters & Taylor (2017) - Perpetual Inventory Method"
di "  • Knowledge capital: δ = 15% (from R&D expenditures)"
di "  • Organization capital: δ = 20% (from Advertising + Training)"
di "  • Initial stocks: K_0 = 0"
di "  • Base year: 2020 (all prices in 2020 constant prices)"
di ""
di "Deflators:"
di "  • GDP deflator: Revenue, costs, wages, R&D, SG&A"
di "  • GFCF deflator: Investment & disinvestment flows"
di "  • Capital deflator: Balance sheet stocks"
di "  • Source: FRED (GDP), EU KLEMS-INTAN (Capital, GFCF)"
di ""
di "Data Cleaning:"
di "  • Negative values set to missing where economically inappropriate"
di "  • Quality flags for robustness checks"
di "  • Winsorization at 1% and 99% percentiles"
di ""
di "══════════════════════════════════════════════════════════════════"

* Close master log
cap log close

di ""
di "Full log saved to: master_pipeline.log"
di ""
di "Ready for analysis! Use: final_data_2011_2022_cleaned_winsorized.dta"
di ""
