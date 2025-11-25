/*==============================================================================
   DATA CLEANING PROCEDURE FOR FINAL MERGED DATASET
   
   Parsimonious approach following standard practices:
   1. Check for logical inconsistencies
   2. Remove negative values where inappropriate
   3. Identify and handle outliers (winsorization at 1% and 99%)
   4. Create cleaned variables
   5. Document all changes
   
   Based on SCIE variables 2011-2022
==============================================================================*/

clear all
set more off

cd "$rootdir/output"

* Start log
cap log close
log using "data_cleaning_final.log", replace

di "=========================================="
di "STARTING DATA CLEANING"
di "Date: `c(current_date)'"
di "=========================================="

use "final_data_2011_2022.dta", clear

* Keep original number of observations
local n_start = _N
di "Starting observations: `n_start'"

/*==============================================================================
   STEP 1: REMOVE STRUCTURAL PROBLEMS
==============================================================================*/

di _newline(2)
di "STEP 1: Removing structural issues..."

* A. Remove observations with missing key identifiers
drop if firm_id == . | ano == .

* B. Check for duplicate firm-year observations
duplicates report firm_id ano
duplicates tag firm_id ano, gen(dup)
tab dup
drop if dup > 0  // Keep only first occurrence
drop dup

local n_after_dup = _N
di "  After removing duplicates: `n_after_dup' (dropped " `n_start' - `n_after_dup' ")"

/*==============================================================================
   STEP 2: VARIABLE-SPECIFIC CLEANING
==============================================================================*/

di _newline(2)
di "STEP 2: Cleaning individual variables..."

* -----------------------------------------------------------------------------
* A. PEOPLE/EMPLOYMENT VARIABLES (should be >= 0)
* -----------------------------------------------------------------------------
di "  A. Employment variables..."

foreach var in SV601201 SV601301 SV601501 SV601701 {
    * Set negative values to missing
    count if `var' < 0 & `var' != .
    if r(N) > 0 {
        di "    `var': Setting " r(N) " negative values to missing"
        replace `var' = . if `var' < 0
    }
}

* Logical check: full-time + part-time should equal total employment
gen check_employ = abs((SV601501 + SV601701) - SV601201) if SV601201 != . & SV601501 != . & SV601701 != .
count if check_employ > 1  // Allow 1 person rounding error
if r(N) > 0 {
    di "    Warning: " r(N) " observations with employment inconsistency (FT+PT != Total)"
    * Set these to missing for safety
    foreach var in SV601201 SV601301 SV601501 SV601701 {
        replace `var' = . if check_employ > 1 & check_employ != .
    }
}
drop check_employ

* -----------------------------------------------------------------------------
* B. REVENUE AND SALES VARIABLES (should be >= 0)
* -----------------------------------------------------------------------------
di "  B. Revenue variables..."

foreach var in SV500101 SD000002 {
    count if `var' < 0 & `var' != .
    if r(N) > 0 {
        di "    `var': Setting " r(N) " negative values to missing"
        replace `var' = . if `var' < 0
    }
}

* -----------------------------------------------------------------------------
* C. COST VARIABLES (should be >= 0)
* -----------------------------------------------------------------------------
di "  C. Cost variables (COGS, materials, labor)..."

foreach var in SV500601 SV500801 SD000102 SV602500 SV602700 SV603900 ///
               SV804000 SV804100 SV804200 SV804400 SV804600 SV804700 ///
               SV804800 SV806700 SV807100 SV569305 {
    count if `var' < 0 & `var' != .
    if r(N) > 0 {
        di "    `var': Setting " r(N) " negative values to missing"
        replace `var' = . if `var' < 0
    }
}

* -----------------------------------------------------------------------------
* D. BALANCE SHEET - ASSETS (should be >= 0)
* -----------------------------------------------------------------------------
di "  D. Asset variables..."

foreach var in SV510101 SV510201 SV510301 SV510401 SV511201 SV512501 ///
               SV512601 SV512701 SD000005 {
    count if `var' < 0 & `var' != .
    if r(N) > 0 {
        di "    `var': Setting " r(N) " negative values to missing"
        replace `var' = . if `var' < 0
    }
}

* Logical check: Total assets = Non-current + Current
gen check_assets = abs((SV511201 + SV512601) - SV512701) if SV512701 != . & SV511201 != . & SV512601 != .
sum check_assets, d
* Allow 1% deviation due to rounding
count if check_assets/SV512701 > 0.01 & check_assets != .
if r(N) > 0 {
    di "    Warning: " r(N) " observations with asset structure inconsistency"
    * Flag but don't drop (could be legitimate differences in some cases)
    gen flag_asset_check = (check_assets/SV512701 > 0.01 & check_assets != .)
}
drop check_assets

* -----------------------------------------------------------------------------
* E. BALANCE SHEET - LIABILITIES AND EQUITY (can be negative for equity)
* -----------------------------------------------------------------------------
di "  E. Liability and equity variables..."

* Liabilities should be >= 0
foreach var in SV514701 SV515901 SV516001 {
    count if `var' < 0 & `var' != .
    if r(N) > 0 {
        di "    `var': Setting " r(N) " negative values to missing"
        replace `var' = . if `var' < 0
    }
}

* Equity (SV514101) CAN be negative (firms in distress)
* But check if total equity + liabilities = total assets
gen check_balance = abs((SV514101 + SV516001) - SV516101) if SV514101 != . & SV516001 != . & SV516101 != .
sum check_balance, d
count if check_balance/SV516101 > 0.01 & check_balance != .
if r(N) > 0 {
    di "    Warning: " r(N) " observations with balance sheet inconsistency"
    gen flag_balance_check = (check_balance/SV516101 > 0.01 & check_balance != .)
}
drop check_balance

* -----------------------------------------------------------------------------
* F. INVESTMENT VARIABLES (should be >= 0)
* -----------------------------------------------------------------------------
di "  F. Investment variables..."

foreach var in SD000031 SD000032 SD000036 SD000037 SD000038 SD000113 ///
               SD000046 SD000047 SD000118 SD000119 SD000120 SD000121 ///
               SD000122 SD000123 SD000124 SD000125 SD000126 SD000127 ///
               SD000128 SD557602 SD557603 {
    count if `var' < 0 & `var' != .
    if r(N) > 0 {
        di "    `var': Setting " r(N) " negative values to missing"
        replace `var' = . if `var' < 0
    }
}

* -----------------------------------------------------------------------------
* G. DISINVESTMENT VARIABLES (should be >= 0)
* -----------------------------------------------------------------------------
di "  G. Disinvestment variables..."

foreach var in SV558201 SD558202 SD558203 SD558204 SV558205 SD563301 ///
               SD563302 SD563303 SD563304 SD563305 SD563306 SV563307 ///
               SD000044 SD000115 {
    count if `var' < 0 & `var' != .
    if r(N) > 0 {
        di "    `var': Setting " r(N) " negative values to missing"
        replace `var' = . if `var' < 0
    }
}

* -----------------------------------------------------------------------------
* H. RESULT/PROFIT VARIABLES (CAN be negative - losses are valid)
* -----------------------------------------------------------------------------
di "  H. Result variables (allowing negative values)..."

* These variables are kept as-is since negative values represent losses:
* SV501701 (EBITDA), SV501801 (Depreciation - can be reversal), 
* SV502001 (Operating result), SV502201 (Interest expenses)
* SD000011 (Production), SD000012 (Intermediate consumption)
* SD000014 (GVA), SD000016 (GOS)
* SD000018, SD000019 (Inventory variation)

* No cleaning needed - losses are economically meaningful

* -----------------------------------------------------------------------------
* I. ROYALTY VARIABLES (should be >= 0)
* -----------------------------------------------------------------------------
di "  I. Royalty variables..."

foreach var in SV807100 SV607604 {
    count if `var' < 0 & `var' != .
    if r(N) > 0 {
        di "    `var': Setting " r(N) " negative values to missing"
        replace `var' = . if `var' < 0
    }
}

/*==============================================================================
   STEP 3: LOGICAL CONSISTENCY CHECKS
==============================================================================*/

di _newline(2)
di "STEP 3: Logical consistency checks..."

* A. Employment should be positive for active firms
count if n_workers == 0 | n_workers == .
di "  Firms with 0 or missing workers: " r(N)
* Flag these but don't automatically drop
gen flag_no_workers = (n_workers == 0 | n_workers == .)

* B. Total assets should be positive for active firms
count if SV512701 <= 0 | SV512701 == .
di "  Firms with 0, negative, or missing total assets: " r(N)
gen flag_no_assets = (SV512701 <= 0 | SV512701 == .)

* C. Revenue should be positive for active firms
count if SV500101 <= 0 | SV500101 == .
di "  Firms with 0, negative, or missing revenue: " r(N)
gen flag_no_revenue = (SV500101 <= 0 | SV500101 == .)

* D. Firms should have some employees if they report wages
count if SV500801 > 0 & SV500801 != . & n_workers == 0
di "  Firms with labor costs but no workers: " r(N)
gen flag_wages_no_workers = (SV500801 > 0 & SV500801 != . & n_workers == 0)

/*==============================================================================
   STEP 4: CREATE CLEANED SAMPLE
==============================================================================*/

di _newline(2)
di "STEP 4: Creating cleaned sample..."

* Mark observations for potential exclusion
gen drop_flag = 0

* Drop if missing critical financial data
replace drop_flag = 1 if flag_no_assets == 1
replace drop_flag = 1 if flag_no_revenue == 1 & flag_no_workers == 1

* Count flagged observations
count if drop_flag == 1
local n_flagged = r(N)
di "  Observations flagged for removal: `n_flagged'"

* Create cleaned dataset
preserve
drop if drop_flag == 1
local n_clean = _N
di "  Clean sample size: `n_clean'"
save "final_data_2011_2022_cleaned.dta", replace
restore

/*==============================================================================
   STEP 5: WINSORIZATION OF KEY VARIABLES (1% and 99%)
==============================================================================*/

di _newline(2)
di "STEP 5: Winsorizing key continuous variables at 1% and 99%..."

* Keep only clean sample
keep if drop_flag == 0

* List of variables to winsorize
local winsor_vars "SV500101 SV500601 SV500801 SV512701 SV514101 SV516001 ///
                   SV501701 SV502001 SV502201 SD000014 SD000016 ///
                   avg_wage_all avg_wage_skilled avg_wage_unskilled ///
                   share_skilled firm_age"

* Winsorize each variable
foreach var of local winsor_vars {
    cap confirm variable `var'
    if _rc == 0 {
        qui sum `var', d
        if r(N) > 0 {
            * Store percentiles
            local p1 = r(p1)
            local p99 = r(p99)
            
            * Create winsorized version
            gen `var'_w = `var'
            qui replace `var'_w = `p1' if `var' < `p1' & `var' != .
            qui replace `var'_w = `p99' if `var' > `p99' & `var' != .
            
            * Count changes
            qui count if `var'_w != `var' & `var' != . & `var'_w != .
            if r(N) > 0 {
                di "    `var': Winsorized " r(N) " observations"
            }
            
            label var `var'_w "`var' (winsorized 1%-99%)"
        }
    }
}

/*==============================================================================
   STEP 6: CREATE USEFUL DERIVED VARIABLES
==============================================================================*/

di _newline(2)
di "STEP 6: Creating derived variables..."

* A. Log transformations (for winsorized variables where appropriate)
foreach var in SV500101 SV512701 n_workers {
    cap confirm variable `var'_w
    if _rc == 0 {
        gen ln_`var' = ln(`var'_w) if `var'_w > 0
        label var ln_`var' "Log of `var' (winsorized)"
    }
}

* B. Productivity measures (using winsorized values)
gen labor_productivity = SV500101_w / n_workers if n_workers > 0
label var labor_productivity "Revenue per worker (winsorized)"

gen capital_intensity = SV512701_w / n_workers if n_workers > 0
label var capital_intensity "Assets per worker (winsorized)"

* C. Financial ratios (using winsorized values where appropriate)
gen debt_to_assets = SV516001_w / SV512701_w if SV512701_w > 0
label var debt_to_assets "Total debt / Total assets"

gen equity_to_assets = SV514101_w / SV512701_w if SV512701_w > 0
label var equity_to_assets "Equity / Total assets"

gen profit_margin = SV502001_w / SV500101_w if SV500101_w > 0
label var profit_margin "Operating profit / Revenue"

gen roa = SV502001_w / SV512701_w if SV512701_w > 0
label var roa "Return on assets (Operating profit / Assets)"

* D. Labor share
gen labor_share = SV500801_w / SV500101_w if SV500101_w > 0
label var labor_share "Labor costs / Revenue"

* E. Intangible intensity
gen intangible_ratio = SV510401 / SV512701_w if SV512701_w > 0 & SV510401 != .
label var intangible_ratio "Intangible assets / Total assets"

/*==============================================================================
   STEP 7: FINAL SAVE AND SUMMARY
==============================================================================*/

di _newline(2)
di "STEP 7: Saving final dataset..."

* Order variables logically
order firm_id ano NPC_FIC n_workers share_skilled ///
      SV500101 SV512701 SV514101 SV516001 firm_age

compress
save "final_data_2011_2022_cleaned_winsorized.dta", replace

local n_final = _N
di "  Final dataset saved: `n_final' observations"

/*==============================================================================
   SUMMARY STATISTICS
==============================================================================*/

di _newline(2)
di "=========================================="
di "CLEANING SUMMARY"
di "=========================================="
di "Initial observations:        `n_start'"
di "After removing duplicates:   `n_after_dup'"
di "Flagged for removal:         `n_flagged'"
di "Final cleaned sample:        `n_final'"
di "Cleaning rate:               " round(100*`n_final'/`n_start', 0.1) "%"
di "=========================================="

* Show distribution of flags
di _newline
di "Quality flags distribution:"
tab flag_no_workers if drop_flag == 0
tab flag_no_assets if drop_flag == 0
tab flag_no_revenue if drop_flag == 0

* Summary statistics for key variables (winsorized)
di _newline
di "Summary statistics (winsorized variables):"
di "==========================================="
sum SV500101_w SV512701_w n_workers share_skilled ///
    labor_productivity capital_intensity roa, d

* Export summary table
preserve
collapse (count) n_obs = firm_id ///
         (mean) mean_workers = n_workers ///
                mean_revenue = SV500101_w ///
                mean_assets = SV512701_w ///
                mean_skilled_share = share_skilled ///
         (p50) median_workers = n_workers ///
               median_revenue = SV500101_w ///
               median_assets = SV512701_w ///
         , by(ano)

export excel using "cleaning_summary_by_year.xlsx", firstrow(variables) replace
di "Summary by year exported to: cleaning_summary_by_year.xlsx"
restore

di _newline(2)
di "=========================================="
di "CLEANING COMPLETE"
di "=========================================="
di "Files created:"
di "  1. final_data_2011_2022_cleaned.dta"
di "     (Cleaned, all flags included)"
di "  2. final_data_2011_2022_cleaned_winsorized.dta"
di "     (Cleaned + winsorized, ready for analysis)"
di "  3. cleaning_summary_by_year.xlsx"
di "     (Summary statistics by year)"
di "=========================================="

cap log close
