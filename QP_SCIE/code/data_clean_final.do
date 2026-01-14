/*==============================================================================
   DATA CLEANING - DEFLATION AND SAMPLE RESTRICTIONS

   Deflates all monetary variables to 2020 prices, removes missing/negative
   balance sheet items, applies sample restrictions.

   Input:  final_data_2011_2022.dta, deflators.dta, debt_2010.dta
   Output: final_data_2011_2022_cleaned.dta

==============================================================================*/

clear all
set more off
cd "$rootdir/output"

cap log close
log using "data_cleaning_final.log", replace

use "final_data_2011_2022.dta", clear

* Keep SCIE-matched, private incorporated firms only
keep if scie_match == 1
keep if public == 0
keep if EFJR0 == "soc"

local n_start = _N
di "Starting observations: " %12.0fc `n_start'

* Merge deflators
merge m:1 ano using "$rootdir/input/deflators.dta", keep(match master)
assert _merge == 3
drop _merge

* Merge 2010 debt (for 2011 interest rate calculations)
merge m:1 NPC_FIC using "$rootdir/output/debt_2010.dta", keep(match master)
drop _merge

di "Deflators merged"

* Deflate variables to real 2020 prices

* Revenue and sales (GDP deflator)
foreach var in SV500101 SD000002 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

* Cost variables (GDP deflator)
foreach var in SV500601 SV500801 SD000102 SV602500 SV602700 SV603900 ///
               SV804000 SV804100 SV804200 SV804400 SV804600 SV804700 ///
               SV804800 SV806700 SV807100 SV569305 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

* Result variables (GDP deflator)
foreach var in SV501701 SV501801 SV502001 SV502201 SD000011 SD000012 ///
               SD000014 SD000016 SD000018 SD000019 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

* Investment flows (GFCF deflator)
foreach var in SD000031 SD000032 SD000036 SD000037 SD000038 SD000113 ///
               SD000046 SD000047 SD000118 SD000119 SD000120 SD000121 ///
               SD000122 SD000123 SD000124 SD000125 SD000126 SD000127 ///
               SD000128 SD557602 SD557603 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gfcf_deflator) * 100
    }
}

* Disinvestment flows (GFCF deflator)
foreach var in SV558201 SD558202 SD558203 SD558204 SV558205 SD563301 ///
               SD563302 SD563303 SD563304 SD563305 SD563306 SV563307 ///
               SD000044 SD000115 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gfcf_deflator) * 100
    }
}

* R&D expenditures (GDP deflator)
cap confirm variable IM_RND_EXPN
if _rc == 0 {
    gen RD_real = (IM_RND_EXPN / gdp_deflator) * 100
}

* Balance sheet capital variables (Capital deflator)
foreach var in SV510101 SV510201 SV510301 SV510401 SV511201 SV512501 ///
               SV512601 SV512701 SD000005 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / capital_deflator) * 100
    }
}

* Debt variables (GDP deflator)
foreach var in SV514301 SV515201 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

* Equity (GDP deflator)
cap confirm variable SV514101
if _rc == 0 {
    gen SV514101_real = (SV514101 / gdp_deflator) * 100
}

* Wages from QP (GDP deflator)
foreach var in avg_wage_all avg_wage_skilled avg_wage_unskilled {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
    }
}

di "All variables deflated to 2020 prices"
di ""

* Remove structural problems (missing IDs, duplicates)
drop if firm_id == . | ano == .
duplicates drop firm_id ano, force

local n_after_step3 = _N
local n_dropped_step3 = `n_start' - `n_after_step3'
di "Removed missing IDs and duplicates"
di "  Dropped: " %8.0fc `n_dropped_step3'
di "  Remaining: " %12.0fc `n_after_step3'
di ""

* Drop observations with missing or negative values in key variables
local vars_check "SV510101_real SV510401_real SV500101_real SD000011_real SD000002_real SV500801_real SD000014_real SV516001_nom SV514301_real SV515201_real SV502201_real SD000032_real SV512701_real"

* Add intangible construction inputs if available
cap confirm variable RD_real
if _rc == 0 local vars_check "`vars_check' RD_real"
cap confirm variable SV804400_real
if _rc == 0 local vars_check "`vars_check' SV804400_real"
cap confirm variable SV603900_real
if _rc == 0 local vars_check "`vars_check' SV603900_real"

foreach var of local vars_check {
    cap confirm variable `var'
    if _rc == 0 {
        qui drop if (`var' == . | `var' < 0)
    }
}

local n_after_step4 = _N
local n_dropped_step4 = `n_after_step3' - `n_after_step4'
di "Removed missing or negative values in key variables"
di "  Dropped: " %8.0fc `n_dropped_step4'
di "  Remaining: " %12.0fc `n_after_step4'
di ""

* Drop observations without meaningful economic activity
gen flag_no_activity = 0
replace flag_no_activity = 1 if n_workers == 0 | n_workers == .
replace flag_no_activity = 1 if SV510101_real < 1000 | SV510101_real == .
replace flag_no_activity = 1 if SV500101_real < 1000 | SV500101_real == .
replace flag_no_activity = 1 if SD000011_real < 1000 | SD000011_real == .
replace flag_no_activity = 1 if SV500801_real < 500 | SV500801_real == .

count if flag_no_activity == 1
local n_no_activity = r(N)
drop if flag_no_activity == 1
drop flag_no_activity

local n_after_step5 = _N
di "Removed observations without meaningful economic activity"
di "  Dropped: " %8.0fc `n_no_activity'
di "  Remaining: " %12.0fc `n_after_step5'
di ""

* Require 2+ consecutive observations for PIM
xtset firm_id ano
gen temp_consecutive = (F.firm_id == firm_id & !missing(F.ano))
bysort firm_id: egen has_2consecutive = max(temp_consecutive)
drop temp_consecutive

count if has_2consecutive == 0
local n_no_consecutive = r(N)
keep if has_2consecutive == 1
drop has_2consecutive

local n_after_step6 = _N
di "Required 2+ consecutive observations"
di "  Dropped: " %8.0fc `n_no_consecutive'
di "  Remaining: " %12.0fc `n_after_step6'
di ""

compress
save "final_data_2011_2022_cleaned.dta", replace

* Summary
di "CLEANING SUMMARY"
di "  Initial observations: " %12.0fc `n_start'
di "  Structural problems: " %8.0fc `n_dropped_step3'
di "  Missing/negative values: " %8.0fc `n_dropped_step4'
di "  No economic activity: " %8.0fc `n_no_activity'
di "  No consecutive obs: " %8.0fc `n_no_consecutive'
local total_dropped = `n_start' - `n_after_step6'
di "  Total dropped: " %12.0fc `total_dropped'
di "  Final sample: " %12.0fc `n_after_step6'
local retention = 100 * `n_after_step6' / `n_start'
di "  Retention rate: " %6.2f `retention' "%"

cap log close
