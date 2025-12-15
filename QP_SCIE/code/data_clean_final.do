/*==============================================================================
   DATA CLEANING AND FINALIZATION
   
   Input:  final_data_2011_2022_intangibles.dta
   Output: final_data_2011_2022_cleaned.dta
           final_data_2011_2022_cleaned_winsorized.dta
   
   Cleaning steps:
   1. Remove structural problems (missing IDs, duplicates)
   2. Clean variable-specific issues (negatives where inappropriate)
   3. Flag missing activity (no workers/capital/revenue/production/wagebill)
   4. Identify extreme incoherence (top 1% vs bottom 1% in key variables)
   5. Winsorize at 99.5th percentile (top 0.5% only)
   
==============================================================================*/

clear all
set more off

cd "$rootdir/output"

cap log close
log using "data_cleaning_final.log", replace

di "=========================================="
di "DATA CLEANING & FINALIZATION"
di "=========================================="
di "Date: `c(current_date)'"
di ""

use "final_data_2011_2022_intangibles.dta", clear

keep if scie_match == 1

local n_start = _N
di "Starting observations: " %12.0fc `n_start'
di ""

/*==============================================================================
   STEP 1: STRUCTURAL PROBLEMS
==============================================================================*/

di "STEP 1: Removing structural problems..."

* Remove missing identifiers
drop if firm_id == . | ano == .

* Remove duplicates
duplicates drop firm_id ano, force

local n_after_step1 = _N
local n_dropped_step1 = `n_start' - `n_after_step1'
di "  Dropped: " %8.0fc `n_dropped_step1' " (missing IDs, duplicates)"
di "  Remaining: " %12.0fc `n_after_step1'
di ""

/*==============================================================================
   STEP 2: FLAG INCORRECT VALUES (NEGATIVES IN REAL VARIABLES)
==============================================================================*/

di "STEP 2: Flagging incorrect values (negatives in real variables)..."

/*------------------------------------------------------------------------------
   A. Production variables
------------------------------------------------------------------------------*/

* Revenue
gen flag_bad_revenue = (SV500101_real < 0) if SV500101_real != .
qui replace SV500101_real = . if flag_bad_revenue == 1

* Production
gen flag_bad_production = (SD000011_real < 0) if SD000011_real != .
qui replace SD000011_real = . if flag_bad_production == 1

* Wagebill
gen flag_bad_wagebill = (SV500801_real < 0) if SV500801_real != .
qui replace SV500801_real = . if flag_bad_wagebill == 1

/*------------------------------------------------------------------------------
   B. Capital stocks (already in real terms from construct_intangibles.do)
------------------------------------------------------------------------------*/

* Physical capital (K_physical already real)
gen flag_bad_capital = (K_physical < 0) if K_physical != .
qui replace K_physical = . if flag_bad_capital == 1

* Balance sheet intangibles
gen flag_bad_BS_intang = (SV510401_real < 0) if SV510401_real != .
qui replace SV510401_real = . if flag_bad_BS_intang == 1

/*------------------------------------------------------------------------------
   C. Financial variables
------------------------------------------------------------------------------*/

* Total debt
gen flag_bad_debt = (SV516001_real < 0) if SV516001_real != .
qui replace SV516001_real = . if flag_bad_debt == 1

* Long-term debt
gen flag_bad_LTdebt = (SV514701_real < 0) if SV514701_real != .
qui replace SV514701_real = . if flag_bad_LTdebt == 1

* Short-term debt
gen flag_bad_STdebt = (SV515901_real < 0) if SV515901_real != .
qui replace SV515901_real = . if flag_bad_STdebt == 1

* Interest expenses
gen flag_bad_interest = (SV502201_real < 0) if SV502201_real != .
qui replace SV502201_real = . if flag_bad_interest == 1

* Note: Equity (SV514101_real) CAN be negative - not flagged

/*------------------------------------------------------------------------------
   D. Investment variables (already in real terms)
------------------------------------------------------------------------------*/

* Tangible investment
gen flag_bad_tang_inv = (SD000032_real < 0) if SD000032_real != .
qui replace SD000032_real = . if flag_bad_tang_inv == 1

* R&D expenditures (RD_real created in construct_intangibles.do)
cap confirm variable RD_real
if _rc == 0 {
    gen flag_bad_RD = (RD_real < 0) if RD_real != .
    qui replace RD_real = . if flag_bad_RD == 1
}

* Advertising
gen flag_bad_adv = (SV804400_real < 0) if SV804400_real != .
qui replace SV804400_real = . if flag_bad_adv == 1

* Training
gen flag_bad_train = (SV603900_real < 0) if SV603900_real != .
qui replace SV603900_real = . if flag_bad_train == 1

/*------------------------------------------------------------------------------
   E. Recalculate capital stocks if components were flagged
------------------------------------------------------------------------------*/

* If RD, advertising, or training were bad, we need to recalculate K_knowledge and K_org
* This requires re-running the perpetual inventory method

* Flag firms that need K_knowledge recalculated
cap confirm variable flag_bad_RD
if _rc == 0 {
    gen needs_K_knowledge_fix = (flag_bad_RD == 1)
}
else {
    gen needs_K_knowledge_fix = 0
}

* Flag firms that need K_org recalculated
gen needs_K_org_fix = (flag_bad_adv == 1 | flag_bad_train == 1)

* For simplicity, set these capital stocks to missing if source data was bad
* (Proper fix would require re-running PIM, but that's complex)
qui replace K_knowledge = . if needs_K_knowledge_fix == 1
qui replace K_org = . if needs_K_org_fix == 1

* Recalculate aggregates
qui replace K_intangible_external = . if flag_bad_BS_intang == 1
qui replace K_intangible_internal = K_knowledge + K_org
qui replace K_intangible_total = K_intangible_internal + K_intangible_external
qui replace K_total = K_physical + K_intangible_total

drop needs_K_knowledge_fix needs_K_org_fix

/*------------------------------------------------------------------------------
   F. Create overall "incorrect values" flag
------------------------------------------------------------------------------*/

gen flag_incorrect_values = 0
qui replace flag_incorrect_values = 1 if flag_bad_revenue == 1 | ///
                                          flag_bad_production == 1 | ///
                                          flag_bad_wagebill == 1 | ///
                                          flag_bad_capital == 1 | ///
                                          flag_bad_BS_intang == 1 | ///
                                          flag_bad_debt == 1 | ///
                                          flag_bad_LTdebt == 1 | ///
                                          flag_bad_STdebt == 1 | ///
                                          flag_bad_interest == 1 | ///
                                          flag_bad_tang_inv == 1

cap confirm variable flag_bad_RD
if _rc == 0 {
    qui replace flag_incorrect_values = 1 if flag_bad_RD == 1
}

qui replace flag_incorrect_values = 1 if flag_bad_adv == 1 | flag_bad_train == 1

count if flag_incorrect_values == 1
local n_incorrect = r(N)
di "  Flagged: " %8.0fc `n_incorrect' " observations (negative values in real variables)"
di ""

/*==============================================================================
   STEP 3: FLAG MISSING ACTIVITY (ZEROS IN REAL OUTCOME VARIABLES)
==============================================================================*/

di "STEP 3: Flagging missing activity (zeros/missing in real outcome variables)..."

* Core outcome variables (all in real terms)
gen flag_no_workers = (n_workers == 0 | n_workers == .)
gen flag_no_capital = (K_physical <= 0 | K_physical == .)
gen flag_no_revenue = (SV500101_real <= 0 | SV500101_real == .)
gen flag_no_production = (SD000011_real <= 0 | SD000011_real == .)
gen flag_no_wagebill = (SV500801_real <= 0 | SV500801_real == .)

* Overall missing activity flag
gen flag_missing_activity = 0
qui replace flag_missing_activity = 1 if flag_no_workers == 1 | ///
                                          flag_no_capital == 1 | ///
                                          flag_no_revenue == 1 | ///
                                          flag_no_production == 1 | ///
                                          flag_no_wagebill == 1

count if flag_missing_activity == 1
local n_no_activity = r(N)
di "  Flagged: " %8.0fc `n_no_activity' " observations (no economic activity)"
di ""

/*==============================================================================
   STEP 4: IDENTIFY EXTREME INCOHERENCE (1% vs 99%)
==============================================================================*/

di "STEP 4: Identifying extreme incoherence (top 1% vs bottom 1%)..."

* Calculate percentiles on clean observations (not flagged in Steps 2-3)
gen sample_for_percentiles = (flag_incorrect_values == 0 & flag_missing_activity == 0)

* Key real variables for coherence checks
foreach var in SD000011_real K_total SV500101_real n_workers {
    cap confirm variable `var'
    if _rc == 0 {
        qui sum `var' if sample_for_percentiles == 1, d
        scalar p1_`var' = r(p1)
        scalar p99_`var' = r(p99)
    }
}

* Incoherence patterns: top 1% of one variable, bottom 1% of another
qui gen flag_high_prod_low_K = (SD000011_real > p99_SD000011_real & ///
                                 K_total < p1_K_total & ///
                                 SD000011_real != . & K_total != .) ///
                                 if sample_for_percentiles == 1

qui gen flag_high_prod_low_L = (SD000011_real > p99_SD000011_real & ///
                                 n_workers < p1_n_workers & ///
                                 SD000011_real != . & n_workers != .) ///
                                 if sample_for_percentiles == 1

qui gen flag_high_K_low_prod = (K_total > p99_K_total & ///
                                 SD000011_real < p1_SD000011_real & ///
                                 K_total != . & SD000011_real != .) ///
                                 if sample_for_percentiles == 1

qui gen flag_high_K_low_rev = (K_total > p99_K_total & ///
                                SV500101_real < p1_SV500101_real & ///
                                K_total != . & SV500101_real != .) ///
                                if sample_for_percentiles == 1

qui gen flag_high_L_low_prod = (n_workers > p99_n_workers & ///
                                 SD000011_real < p1_SD000011_real & ///
                                 n_workers != . & SD000011_real != .) ///
                                 if sample_for_percentiles == 1

qui gen flag_high_L_low_rev = (n_workers > p99_n_workers & ///
                                SV500101_real < p1_SV500101_real & ///
                                n_workers != . & SV500101_real != .) ///
                                if sample_for_percentiles == 1

qui gen flag_high_rev_low_K = (SV500101_real > p99_SV500101_real & ///
                                K_total < p1_K_total & ///
                                SV500101_real != . & K_total != .) ///
                                if sample_for_percentiles == 1

qui gen flag_high_rev_low_L = (SV500101_real > p99_SV500101_real & ///
                                n_workers < p1_n_workers & ///
                                SV500101_real != . & n_workers != .) ///
                                if sample_for_percentiles == 1

* Overall incoherence flag
gen flag_incoherent = 0
qui foreach pattern in high_prod_low_K high_prod_low_L high_K_low_prod ///
                       high_K_low_rev high_L_low_prod high_L_low_rev ///
                       high_rev_low_K high_rev_low_L {
    replace flag_incoherent = 1 if flag_`pattern' == 1
}

count if flag_incoherent == 1
local n_incoherent = r(N)
di "  Flagged: " %8.0fc `n_incoherent' " observations (extreme incoherence)"
di ""

drop sample_for_percentiles

/*==============================================================================
   COMBINE FLAGS AND CREATE DROP_FLAG
==============================================================================*/

* Combine all flags for final sample restriction
gen drop_flag = 0
qui replace drop_flag = 1 if flag_incorrect_values == 1 | ///
                             flag_missing_activity == 1 | ///
                             flag_incoherent == 1

count if drop_flag == 1
local n_total_dropped = r(N)

di "Combined sample restriction:"
di "  Total flagged: " %8.0fc `n_total_dropped'
di ""

/*==============================================================================
   STEP 5: ASYMMETRIC WINSORIZATION (TOP 0.5%) FOR ROBUSTNESS
==============================================================================*/

di "STEP 5: Winsorizing at 99.5th percentile (top 0.5% only)..."
di ""

* Real variables to winsorize (all in 2020 prices)
local winsor_vars "K_total K_physical K_intangible_total                 SV500101_real SD000011_real SV500801_real SV516001_real SV514701_real SV515901_real SD000032_real n_workers"

* Variable descriptions for clean output
local lab_K_total "Total capital"
local lab_K_physical "Physical capital"
local lab_K_intangible_total "Intangible capital"
local lab_SV500101_real "Revenue"
local lab_SD000011_real "Production"
local lab_SV500801_real "Wagebill"
local lab_SV516001_real "Total debt"
local lab_SV514701_real "Long-term debt"
local lab_SV515901_real "Short-term debt"
local lab_SD000032_real "Tangible investment"
local lab_n_workers "Workers"

local n_winsorized = 0

* Winsorize each variable
qui foreach var of local winsor_vars {
    cap confirm variable `var'
    if _rc == 0 {
        * Calculate 99.5th percentile on clean sample
        _pctile `var' if drop_flag == 0, p(99.5)
        local p995 = r(r1)
        
        count if `var' != . & drop_flag == 0
        local n_valid = r(N)
        
        if `n_valid' > 0 & `p995' != . {
            gen `var'_w = `var'
            replace `var'_w = `p995' if `var' > `p995' & `var' != .
            label var `var'_w "`var' (winsorized top 0.5%)"
            local n_winsorized = `n_winsorized' + 1
        }
    }
}

di "  Completed: " `n_winsorized' " real variables winsorized at top 0.5%"
di ""

/*==============================================================================
   STEP 6: CREATE FLAG TO IDENTIFY FIRMS WITH AT LEAST 2 CONSECUTIVE OBSERVATIONS
==============================================================================*/

* Declare panel structure
xtset firm_id ano

* Check if next observation exists and is consecutive
gen temp_consecutive = (F.firm_id == firm_id & !missing(F.ano))

* Flag ALL observations for firms that have at least one consecutive pair
bysort firm_id: egen flag_2consecutive = max(temp_consecutive)

drop temp_consecutive

label var flag_2consecutive "Firm has at least 2 consecutive years"


/*==============================================================================
   STEP 7: SAVE DATASETS
==============================================================================*/

di "STEP 7: Saving final datasets..."
di ""

* Save full dataset with all flags
compress
save "final_data_2011_2022_flag.dta", replace
local n_all = _N
di "  → Saved: final_data_2011_2022_flag.dta"
di "     All observations: " %12.0fc `n_all'
di "     (includes flagged observations for robustness checks)"
di ""

* Save clean sample only (main analysis dataset)
keep if drop_flag == 0 & flag_2consecutive == 1
compress
save "final_data_2011_2022_clean.dta", replace
local n_final = _N
di "  → Saved: final_data_2011_2022_clean.dta"
di "     Clean sample: " %12.0fc `n_final'
di "     (ready for analysis)"
di ""

/*==============================================================================
   CLEANING SUMMARY
==============================================================================*/

di "=========================================="
di "CLEANING SUMMARY"
di "=========================================="
di ""
di "Initial observations:              " %12.0fc `n_start'
di ""
di "Step 1 - Structural problems:      " %12.0fc `n_dropped_step1'
di "Step 2 - Incorrect values:         " %12.0fc `n_incorrect'
di "Step 3 - Missing activity:         " %12.0fc `n_no_activity'
di "Step 4 - Extreme incoherence:      " %12.0fc `n_incoherent'
di "                                    " "─────────────"
di "Total excluded:                    " %12.0fc `n_total_dropped'
di ""
di "Final clean sample:                " %12.0fc `n_final'
di ""
di "Retention rate:                    " %6.2f (100 * `n_final' / `n_start') "%"
di ""
di "Variables checked: REAL (2020 prices)"
di "Winsorization: TOP 0.5% only (asymmetric)"
di ""
di "=========================================="
di "FILES CREATED"
di "=========================================="
di ""
di "1. final_data_2011_2022_flag.dta"
di "   → All observations with quality flags"
di "   → Use for robustness checks"
di ""
di "2. final_data_2011_2022_clean.dta"
di "   → Clean sample (drop_flag == 0)"
di "   → Winsorized variables (*_w suffix)"
di "   → READY FOR ANALYSIS"
di ""
di "=========================================="

cap log close