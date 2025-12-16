/*==============================================================================
   MASTER DO FILE: QP-SCIE Data Processing Pipeline (2011-2022)
   
   This file orchestrates the complete data processing workflow in a modular
   structure.
   
   WORKFLOW STRUCTURE:
   
   Phase 1: Data Extraction & Cleaning
   ├── 1. SCIE_clean.do          → Clean balance sheet data (SCIE)
   ├── 2. firm_clean.do          → Clean firm characteristics (QP)
   └── 3. worker_skills.do       → Process workers & calculate skills (QP)
   
   Phase 2: Data Integration
   └── 4. merge_final.do         → Merge all sources
                                    Output: final_data_2011_2022.dta
   
   Phase 3: Auxiliary Data Preparation
   ├── 5. prepare_deflators.do   → Import & prepare price deflators
   │                                Output: deflators.dta
   └── 6. prepare_rfr.do         → Import & prepare risk-free rate
                                    Output: rfr.dta
   
   Phase 4: Data Cleaning & Deflation
   └── 7. data_clean_final.do    → Deflate variables & apply sample restrictions
                                    Output: final_data_2011_2022_cleaned.dta
   
   Phase 5: Intangible Capital Construction
   └── 8. construct_intangibles.do → Build intangible capital stocks (PIM)
                                      Output: final_data_2011_2022_intangibles.dta
   
   Phase 6: Analysis Preparation
   └── 9. analysis_prep.do       → Create analytical variables & winsorize
                                    Output: final_data_2011_2022_analysis.dta
     
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
log using "$rootdir/output/master_pipeline.log", replace

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
di "  5. Prepare price deflators (GDP, GFCF, Capital)"
di "  6. Prepare risk-free rate (Portuguese 10-year bonds)"
di "  7. Deflate variables & clean data"
di "  8. Construct intangible capital stocks (sector-specific)"
di "  9. Prepare analysis-ready dataset"
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
di "│ MODULE 1/9: Cleaning SCIE balance sheet data                  │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/SCIE_clean.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"

* Module 2: Clean firm-level QP data
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 2/9: Cleaning QP firm-level data                       │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/firm_clean.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"

* Module 3: Process worker data and create skill measures
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 3/9: Processing workers & calculating skills           │"
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
di "│ MODULE 4/9: Merging QP + SCIE datasets                        │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/merge_final.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022.dta"

/*==============================================================================
   PHASE 3: AUXILIARY DATA PREPARATION
==============================================================================*/

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║            PHASE 3: AUXILIARY DATA PREPARATION                 ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""

* Module 5: Prepare deflators
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 5/9: Preparing price deflators                         │"
di "│ GDP, GFCF, Capital deflators (2020=100)                       │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/prepare_deflators.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"
di "  Output: deflators.dta"

* Module 6: Prepare risk-free rate
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 6/9: Preparing risk-free rate data                     │"
di "│ Portuguese 10-year government bonds (FRED)                    │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/prepare_rfr.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"
di "  Output: rfr.dta"

/*==============================================================================
   PHASE 4: DATA CLEANING & DEFLATION
==============================================================================*/

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║              PHASE 4: DATA CLEANING & DEFLATION                ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""

* Module 7: Clean and deflate data
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 7/9: Deflating & cleaning data                         │"
di "│ Merges deflators, converts to real 2020 prices, applies       │"
di "│ sample restrictions                                            │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/data_clean_final.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022_cleaned.dta"

/*==============================================================================
   PHASE 5: INTANGIBLE CAPITAL CONSTRUCTION
==============================================================================*/

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║          PHASE 5: INTANGIBLE CAPITAL CONSTRUCTION              ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""

* Module 8: Construct intangible capital
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 8/9: Constructing intangible capital stocks            │"
di "│ Method: Perpetual Inventory Method (PIM)                      │"
di "│ Sector-specific parameters (Ewens, Peters & Wang 2025)        │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/construct_intangibles.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022_intangibles.dta"

/*==============================================================================
   PHASE 6: ANALYSIS PREPARATION
==============================================================================*/

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║                PHASE 6: ANALYSIS PREPARATION                   ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""

* Module 9: Prepare analysis-ready dataset
di _newline(2)
di "┌────────────────────────────────────────────────────────────────┐"
di "│ MODULE 9/9: Creating analytical variables                     │"
di "│ Credit spreads, ratios, intensities, winsorization            │"
di "└────────────────────────────────────────────────────────────────┘"
local start_time = clock(c(current_time), "hms")

do "$rootdir/code/analysis_prep.do"

local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  ✓ Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022_analysis.dta"

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
di "  2. IES_age_2011_2022.dta"
di "  3. firm_2011_2022.dta"
di "  4. worker_skills_2011_2022.dta"
di "  5. firm_skills_wages_2011_2022.dta"
di "  6. final_data_2011_2022.dta"
di "  7. deflators.dta"
di "  8. rfr.dta"
di "  9. final_data_2011_2022_cleaned.dta"
di "  10. final_data_2011_2022_intangibles.dta"
di ""
di "Final analysis-ready dataset:"
di "  → final_data_2011_2022_analysis.dta ⭐ READY FOR ANALYSIS"
di ""
di "══════════════════════════════════════════════════════════════════"
di "KEY VARIABLES IN FINAL DATASET"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Intangible Capital Stocks (real 2020 prices):"
di "  • K_knowledge              Knowledge capital (from R&D, sector-specific δ)"
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
di "  • share_rnd_workers        Share of R&D workers"
di "  • rnd_intensity            R&D / Revenue"
di ""
di "Financial Variables:"
di "  • leverage                 Total debt / K_total"
di "  • interest_rate            Implicit interest rate (%)"
di "  • credit_spread            Spread over Portuguese 10-year bonds (pp)"
di "  • rfr                      Risk-free rate (10-year PT government bonds)"
di ""
di "Investment Rates (relative to K_total):"
di "  • inv_rate_physical        Physical investment rate"
di "  • inv_rate_knowledge       R&D investment rate"
di "  • inv_rate_org             SG&A investment rate"
di "  • inv_rate_bs_intangible   Balance sheet intangible investment rate"
di "  • inv_rate_intangible      Total intangible investment rate"
di ""
di "Productivity Measures:"
di "  • labor_productivity       Revenue per worker"
di ""
di "Industry Classification:"
di "  • ff5_industry             Fama-French 5 (Consumer, Manufacturing,"
di "                             High Tech, Health, Other)"
di ""
di "Winsorized versions available (suffix '_w') at 1%-99%"
di ""
di "All monetary variables in real 2020 prices"
di ""
di "══════════════════════════════════════════════════════════════════"
di "METHODOLOGICAL NOTES"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Intangible Capital Construction:"
di "  • Method: Perpetual Inventory Method (PIM)"
di "  • Sector-specific knowledge capital depreciation (EPW 2025):"
di "    - Consumer: δ = 43%"
di "    - Manufacturing: δ = 50%"
di "    - High Tech: δ = 42%"
di "    - Health: δ = 33%"
di "    - Other: δ = 35%"
di "  • Organization capital: δ = 20% (all sectors)"
di "  • Initial stocks: K_0 = 0"
di "  • Base year: 2020 (all prices in 2020 constant prices)"
di ""
di "Deflators:"
di "  • GDP deflator: Revenue,