/*==============================================================================
   MASTER DO FILE: QP-SCIE Data Processing Pipeline (2011-2022)

   Orchestrates complete data processing workflow:

   Phase 1: Data Extraction & Cleaning
     1. SCIE_clean.do          - Clean balance sheet data (SCIE)
     2. firm_clean.do          - Clean firm characteristics (QP)
     3. worker_skills.do       - Process workers & calculate skills (QP)

   Phase 2: Data Integration
     4. merge_final.do         - Merge all sources

   Phase 3: Auxiliary Data Preparation
     5. prepare_deflators.do   - Import & prepare price deflators
     5.5. prepare_2010_debt.do - Prepare 2010 debt (for 2011 lags)
     6. prepare_rfr.do         - Import & prepare risk-free rate

   Phase 4: Data Cleaning & Deflation
     7. data_clean_final.do    - Deflate variables & apply sample restrictions

   Phase 5: Intangible Capital Construction
     8. construct_intangibles.do - Build intangible capital stocks (PIM)

   Phase 6: Analysis Preparation
     9. analysis_prep.do       - Create analytical variables

   OUTPUT: final_data_2011_2022_analysis.dta

==============================================================================*/

clear all
set more off
set maxvar 10000

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

cap log close
log using "$rootdir/output/master_pipeline.log", replace

di ""
di "QP-SCIE DATA PROCESSING PIPELINE (2011-2022)"
di "Root directory: $rootdir"
di "Start: `c(current_date)' `c(current_time)'"
di ""

/*==============================================================================
   PHASE 1: DATA EXTRACTION & CLEANING
==============================================================================*/

di "PHASE 1: Data Cleaning"
di ""

* Module 1: Clean SCIE data
di "MODULE 1/9: Cleaning SCIE balance sheet data"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/SCIE_clean.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di ""

* Module 2: Clean firm-level QP data
di "MODULE 2/9: Cleaning QP firm-level data"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/firm_clean.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di ""

* Module 3: Process worker data and create skill measures
di "MODULE 3/9: Processing workers & calculating skills"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/worker_skills.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di ""

/*==============================================================================
   PHASE 2: DATA INTEGRATION
==============================================================================*/

di "PHASE 2: Data Integration"
di ""

* Module 4: Merge all datasets
di "MODULE 4/9: Merging QP + SCIE datasets"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/merge_final.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022.dta"
di ""

/*==============================================================================
   PHASE 3: AUXILIARY DATA PREPARATION
==============================================================================*/

di "PHASE 3: Auxiliary Data Preparation"
di ""

* Module 5: Prepare deflators
di "MODULE 5/9: Preparing price deflators"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/prepare_deflators.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di "  Output: deflators.dta"
di ""

* Module 5.5: Prepare 2010 debt for lag structure
di "MODULE 5.5/9: Preparing 2010 debt (for 2011 lags)"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/prepare_2010_debt.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di "  Output: debt_2010.dta"
di ""

* Module 6: Prepare risk-free rate
di "MODULE 6/9: Preparing risk-free rate data"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/prepare_rfr.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di "  Output: rfr.dta"
di ""

/*==============================================================================
   PHASE 4: DATA CLEANING & DEFLATION
==============================================================================*/

di "PHASE 4: Data Cleaning & Deflation"
di ""

* Module 7: Clean and deflate data
di "MODULE 7/9: Deflating & cleaning data"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/data_clean_final.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022_cleaned.dta"
di ""

/*==============================================================================
   PHASE 5: INTANGIBLE CAPITAL CONSTRUCTION
==============================================================================*/

di "PHASE 5: Intangible Capital Construction"
di ""

* Module 8: Construct intangible capital
di "MODULE 8/9: Constructing intangible capital stocks (PIM)"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/construct_intangibles.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022_intangibles.dta"
di ""

/*==============================================================================
   PHASE 6: ANALYSIS PREPARATION
==============================================================================*/

di "PHASE 6: Analysis Preparation"
di ""

* Module 9: Prepare analysis-ready dataset
di "MODULE 9/9: Creating analytical variables"
local start_time = clock(c(current_time), "hms")
do "$rootdir/code/analysis_prep.do"
local end_time = clock(c(current_time), "hms")
local elapsed = round((`end_time' - `start_time') / 1000 / 60, 0.1)
di "  Completed in `elapsed' minutes"
di "  Output: final_data_2011_2022_analysis.dta"
di ""

/*==============================================================================
   COMPLETION SUMMARY
==============================================================================*/

di "PIPELINE COMPLETED SUCCESSFULLY"
di "End: `c(current_time)'"
di ""
di "Final analysis-ready dataset:"
di "  final_data_2011_2022_analysis.dta"
di ""
di "Key variables available:"
di "  - Intangible capital stocks (K_knowledge, K_org, K_intangible_pt/bs_rd)"
di "  - Financial variables (leverage, interest_rate, credit_spread)"
di "  - Skill measures (share_skilled, share_rd_workers)"
di "  - Investment rates (inv_rate_physical, inv_rate_intang)"
di "  - All monetary variables in real 2020 prices"
di ""
di "Ready for analysis. Next step: run facts.do"

cap log close
