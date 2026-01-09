/*==============================================================================
   DATA CLEANING - CLEAN SOURCE VARIABLES BEFORE INTANGIBLES CONSTRUCTION
   
   Prerequisites: prepare_deflators.do (creates deflators.dta)
   Input:  final_data_2011_2022.dta (from merge_final.do)
           deflators.dta (from prepare_deflators.do)
   Output: final_data_2011_2022_cleaned.dta
   
   This file:
   1. Merges deflators
   2. Deflates all variables to real 2020 prices
   3. Cleans source variables (removes missing/negative balance sheet items)
   4. Applies basic sample restrictions
   
   Next step: construct_intangibles.do builds capital stocks from clean data
   
==============================================================================*/

clear all
set more off

cd "$rootdir/output"

cap log close
log using "data_cleaning_final.log", replace

di "=========================================="
di "DATA CLEANING & DEFLATION"
di "=========================================="
di "Date: `c(current_date)'"
di ""

/*==============================================================================
   STEP 1: LOAD DATA AND MERGE DEFLATORS
==============================================================================*/

di "STEP 1: Loading merged dataset and deflators..."

use "final_data_2011_2022.dta", clear

* Keep only SCIE-matched observations
keep if scie_match == 1

* Keep only private, non-incorporated businesses
keep if public == 0
keep if EFJR0 == "soc" 

local n_start = _N
di "  Starting observations: " %12.0fc `n_start'

* Merge deflators (created by prepare_deflators.do)
merge m:1 ano using "$rootdir/input/deflators.dta", keep(match master)

* Verify all observations matched
count if _merge != 3
if r(N) > 0 {
    di as error "ERROR: Some observations did not match with deflators!"
    di as error "Make sure prepare_deflators.do has been run."
    exit 111
}
drop _merge

di "  → Deflators merged successfully"
di ""

/*==============================================================================
   STEP 1.5: MERGE 2010 DEBT FOR LAG STRUCTURE
==============================================================================*/

di "STEP 1.5: Merging 2010 debt data for lagged interest rate calculation..."

* Merge 2010 debt (needed for 2011 interest rates)
merge m:1 NPC_FIC using "$rootdir/output/debt_2010.dta", keep(match master)

count if _merge == 3
local n_matched = r(N)
di "  → Matched " %8.0fc `n_matched' " observations with 2010 debt"

drop _merge

di ""

/*==============================================================================
   STEP 2: DEFLATE VARIABLES 
==============================================================================*/

di "STEP 2: Deflating variables to real 2020 prices..."
di "  Base year: 2020 (deflators = 100)"
di "  NOTE: Debt variables kept NOMINAL for interest rate calculations"
di ""

*------------------------------------------------------------------------------
* A. Revenue and sales (GDP deflator)
*------------------------------------------------------------------------------

foreach var in SV500101 SD000002 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

*------------------------------------------------------------------------------
* B. Cost variables (GDP deflator)
*------------------------------------------------------------------------------

foreach var in SV500601 SV500801 SD000102 SV602500 SV602700 SV603900 ///
               SV804000 SV804100 SV804200 SV804400 SV804600 SV804700 ///
               SV804800 SV806700 SV807100 SV569305 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

*------------------------------------------------------------------------------
* C. Result variables (GDP deflator)
*------------------------------------------------------------------------------

foreach var in SV501701 SV501801 SV502001 SD000011 SD000012 ///
               SD000014 SD000016 SD000018 SD000019 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

*------------------------------------------------------------------------------
* C2. Interest expenses
*------------------------------------------------------------------------------

* Real interest expenses
cap confirm variable SV502201
if _rc == 0 {
    gen SV502201_real = (SV502201 / gdp_deflator) * 100
    label var SV502201_real "Interest expenses (real 2020 prices)"
}

*------------------------------------------------------------------------------
* D. Investment flows (GFCF deflator)
*------------------------------------------------------------------------------

foreach var in SD000031 SD000032 SD000036 SD000037 SD000038 SD000113 ///
               SD000046 SD000047 SD000118 SD000119 SD000120 SD000121 ///
               SD000122 SD000123 SD000124 SD000125 SD000126 SD000127 ///
               SD000128 SD557602 SD557603 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gfcf_deflator) * 100
    }
}

*------------------------------------------------------------------------------
* E. Disinvestment flows (GFCF deflator)
*------------------------------------------------------------------------------

foreach var in SV558201 SD558202 SD558203 SD558204 SV558205 SD563301 ///
               SD563302 SD563303 SD563304 SD563305 SD563306 SV563307 ///
               SD000044 SD000115 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gfcf_deflator) * 100
    }
}

*------------------------------------------------------------------------------
* F. R&D expenditures (GDP deflator)
*------------------------------------------------------------------------------

cap confirm variable IM_RND_EXPN
if _rc == 0 {
    gen RD_real = (IM_RND_EXPN / gdp_deflator) * 100
}

*------------------------------------------------------------------------------
* G. Balance sheet CAPITAL variables (Capital deflator)
*------------------------------------------------------------------------------

foreach var in SV510101 SV510201 SV510301 SV510401 SV511201 SV512501 ///
               SV512601 SV512701 SD000005 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / capital_deflator) * 100
    }
}

*------------------------------------------------------------------------------
* H. Debt variables (GDP deflator)
*------------------------------------------------------------------------------

foreach var in SV514301 SV515201 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

*------------------------------------------------------------------------------
* I. Equity (GDP deflator)
*------------------------------------------------------------------------------

cap confirm variable SV514101
if _rc == 0 {
    gen SV514101_real = (SV514101 / gdp_deflator) * 100
    label var SV514101_real "Equity"
}

*------------------------------------------------------------------------------
* J. Wages from QP (GDP deflator)
*------------------------------------------------------------------------------

foreach var in avg_wage_all avg_wage_skilled avg_wage_unskilled {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

di "  → All variables deflated"
di ""

/*==============================================================================
   STEP 3: REMOVE STRUCTURAL PROBLEMS
==============================================================================*/

di "STEP 3: Removing structural problems..."

* Remove missing identifiers
drop if firm_id == . | ano == .

* Remove duplicates
duplicates drop firm_id ano, force

local n_after_step3 = _N
local n_dropped_step3 = `n_start' - `n_after_step3'
di "  Dropped: " %8.0fc `n_dropped_step3' " (missing IDs, duplicates)"
di "  Remaining: " %12.0fc `n_after_step3'
di ""

/*==============================================================================
   STEP 4: DROP OBSERVATIONS WITH MISSING OR NEGATIVE VALUES
==============================================================================*/

di "STEP 4: Dropping observations with missing or negative values..."
di ""

* Count observations before this step
local n_before_step4 = _N

* List of variables to check (must exist and be non-negative)
* SV510101_real: Tangible fixed assets (physical capital)
* SV510401_real: Intangible assets (excluding goodwill)
* SV500101_real: Revenue
* SD000011_real: Production value
* SD000002_real: Sales
* SD000014_real: GVA
* SV500801_real: Wagebill
* SV514301_real: Long-term debt
* SV515201_real: Short-term debt
* SV502201_real: Interest expenses
* SD000032_real: Tangible investment
* RD_real: R&D expenditures
* SV804400_real: Advertising expenses
* SV603900_real: Training expenses
* SV512701_real: Total assets  

local vars_check "SV510101_real SV510401_real SV500101_real SD000011_real SD000002_real SV500801_real SD000014_real SV516001_nom SV514301_real SV515201_real SV502201_real SD000032_real SV512701_real"

* Add intangible construction inputs if they exist
cap confirm variable RD_real
if _rc == 0 {
    local vars_check "`vars_check' RD_real"
}

cap confirm variable SV804400_real
if _rc == 0 {
    local vars_check "`vars_check' SV804400_real"
}

cap confirm variable SV603900_real
if _rc == 0 {
    local vars_check "`vars_check' SV603900_real"
}

* Drop observations with missing or negative values
local n_dropped = 0
foreach var of local vars_check {
    cap confirm variable `var'
    if _rc == 0 {
        count if (`var' == . | `var' < 0)
        local n_bad = r(N)
        if `n_bad' > 0 {
            qui drop if (`var' == . | `var' < 0)
            local n_dropped = `n_dropped' + `n_bad'
        }
    }
}

local n_after_step4 = _N
local n_dropped_step4 = `n_before_step4' - `n_after_step4'

di ""
di "  Total dropped: " %8.0fc `n_dropped_step4'
di "  Remaining: " %12.0fc `n_after_step4'
di ""

/*==============================================================================
   STEP 5: DROP OBSERVATIONS WITHOUT MEANINGFUL ECONOMIC ACTIVITY
==============================================================================*/

di "STEP 5: Dropping observations without meaningful economic activity..."
di ""

gen flag_no_activity = 0

* No workers
replace flag_no_activity = 1 if n_workers == 0 | n_workers == .

* Less than 1000 euros in tangible fixed assets
replace flag_no_activity = 1 if SV510101_real < 1000 | SV510101_real == .

* Less than 1000 euros in revenue
replace flag_no_activity = 1 if SV500101_real < 1000 | SV500101_real == .

* Less than 1000 euros in production
replace flag_no_activity = 1 if SD000011_real < 1000 | SD000011_real == .

* Less than 500 euros in wagebill
replace flag_no_activity = 1 if SV500801_real < 500 | SV500801_real == .

count if flag_no_activity == 1
local n_no_activity = r(N)
di "  No economic activity: " %8.0fc `n_no_activity'
di "    (Missing/zero: workers, tangible capital, revenue, production, or wagebill)"

drop if flag_no_activity == 1
drop flag_no_activity

local n_after_step5 = _N
di ""
di "  Total dropped: " %8.0fc `n_no_activity'
di "  Remaining: " %12.0fc `n_after_step5'
di ""

/*==============================================================================
   STEP 6: REQUIRE 2+ CONSECUTIVE OBSERVATIONS
==============================================================================*/

di "STEP 6: Requiring 2+ consecutive observations..."
di ""

* Declare panel structure
xtset firm_id ano

* Check if next observation exists and is consecutive
gen temp_consecutive = (F.firm_id == firm_id & !missing(F.ano))

* Flag ALL observations for firms that have at least one consecutive pair
bysort firm_id: egen has_2consecutive = max(temp_consecutive)

drop temp_consecutive

count if has_2consecutive == 0
local n_no_consecutive = r(N)
di "  Firms without consecutive obs: " %8.0fc `n_no_consecutive'

keep if has_2consecutive == 1
drop has_2consecutive

local n_after_step6 = _N
di "  Remaining: " %12.0fc `n_after_step6'
di ""

/*==============================================================================
   STEP 7: SAVE CLEANED DATASET
==============================================================================*/

di "STEP 7: Saving cleaned dataset..."
di ""

compress
save "final_data_2011_2022_cleaned.dta", replace

di "  → Saved: final_data_2011_2022_cleaned.dta"
di "     Observations: " %12.0fc `n_after_step6'
di ""

/*==============================================================================
   CLEANING SUMMARY
==============================================================================*/

di "=========================================="
di "CLEANING SUMMARY"
di "=========================================="
di ""
di "Initial observations:                  " %12.0fc `n_start'
di ""
di "Step 3 - Structural problems:          " %12.0fc `n_dropped_step3'
di "Step 4 - Missing/negative values:      " %12.0fc `n_dropped_step4'
di "Step 5 - No economic activity:         " %12.0fc `n_no_activity'
di "Step 6 - No consecutive obs:           " %12.0fc `n_no_consecutive'
di "                                        " "─────────────"
local total_dropped = `n_start' - `n_after_step6'
di "Total excluded:                        " %12.0fc `total_dropped'
di ""
di "Final clean sample:                    " %12.0fc `n_after_step6'
di ""
di "Retention rate:                        " %6.2f (100 * `n_after_step6' / `n_start') "%"
di ""
di "=========================================="
di "CLEANING CHECKS PERFORMED"
di "=========================================="
di ""
di "Step 4: Missing or negative values (dropped):"
di 
di ""
di "Step 5: Missing economic activity (dropped if zero/missing):"
di
di ""
di "Step 6: Panel structure:"
di "   Required 2+ consecutive observations per firm"
di ""
di 
di ""
di "=========================================="
di "NEXT STEP"
di "=========================================="
di ""
di "Run: construct_intangibles.do"
di ""
di "This will build capital stocks from the"
di "clean source variables created here."
di ""
di "Note: Missing R&D, advertising, training"
di "      will be set to 0 in the PIM (standard"
di "      assumption: missing flow = no investment)"
di ""
di "=========================================="

cap log close
