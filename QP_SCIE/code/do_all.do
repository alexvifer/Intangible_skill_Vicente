/*==============================================================================
   MAIN DO FILE: Process QP and SCIE Data (2011-2022)
   
   This file runs all cleaning scripts in the correct order.
   Set your root directory below and run this file.
   
==============================================================================*/

clear all
set more off
set maxvar 10000

* SET YOUR ROOT DIRECTORY HERE
global rootdir "C:\Users\alexv\OneDrive\Documentos\3-Papers\Intangible_skill_Vicente\QP_SCIE"

* Example: global rootdir "C:/Users/YourName/Research/QP_SCIE"

cd "$rootdir/code"

* Start log file
cap log close
log using "data_cleaning_2011plus.log", replace

di _newline(2)
di "=========================================="
di "STARTING DATA CLEANING PROCESS"
di "=========================================="
di "Root directory: $rootdir"
di "Date: `c(current_date)'"
di "Time: `c(current_time)'"
di "=========================================="

/*==============================================================================
   RUN CLEANING SCRIPTS IN ORDER
==============================================================================*/

* 1. Clean SCIE data (balance sheet)
di _newline(2)
di "STEP 1/4: Cleaning SCIE data..."
do "$rootdir/code/SCIE_clean.do"

* 2. Clean firm-level QP data
di _newline(2)
di "STEP 2/4: Cleaning firm-level QP data..."
do "$rootdir/code/firm_clean.do"

* 3. Process worker-level data and create skill measures
di _newline(2)
di "STEP 3/4: Processing worker data and calculating skills..."
do "$rootdir/code/worker_skills.do"

* 4. Merge everything together
di _newline(2)
di "STEP 4/4: Merging all datasets..."
do "$rootdir/code/merge_final.do"

/*==============================================================================
   SUMMARY
==============================================================================*/

di _newline(2)
di "=========================================="
di "DATA CLEANING COMPLETE!"
di "=========================================="
di "Final datasets created:"
di "  - SCIE_2011_2022.dta"
di "  - firm_2011_2022.dta"
di "  - worker_skills_2011_2022.dta"
di "  - firm_skills_wages_2011_2022.dta (aggregated)"
di "  - final_data_2011_2022.dta (ALL MERGED)"
di "=========================================="

* Close log
cap log close

