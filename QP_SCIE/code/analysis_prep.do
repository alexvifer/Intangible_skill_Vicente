/*==============================================================================
   ANALYSIS PREPARATION
   
   Input:  final_data_2011_2022_intangibles.dta (from construct_intangibles.do)
   Output: final_data_2011_2022_analysis.dta
   
   This file:
   1. Creates risk-free rate for credit spread calculation
   2. Constructs key analytical variables and ratios
   3. Treats outliers via winsorization (1%-99%)
   4. Creates log variables for regression analysis
   
==============================================================================*/

clear all
set more off

cd "$rootdir/output"

cap log close
log using "analysis_prep.log", replace

di "=========================================="
di "ANALYSIS PREPARATION"
di "=========================================="
di "Date: `c(current_date)'"
di ""

/*==============================================================================
   STEP 1: LOAD DATA WITH INTANGIBLES
==============================================================================*/

di "STEP 1: Loading dataset with intangible capital stocks..."

use "final_data_2011_2022_intangibles.dta", clear

local n_start = _N
di "  Starting observations: " %12.0fc `n_start'
di ""

* Declare panel structure
xtset firm_id ano

/*==============================================================================
   STEP 2: MERGE RISK-FREE RATE (PORTUGUESE GOVERNMENT BONDS)
==============================================================================*/

di "STEP 2: Merging risk-free rate (10-year Portuguese government bonds)..."
di ""

* Merge risk-free rate data (created by prepare_rfr.do)
merge m:1 ano using "$rootdir/input/rfr.dta", keep(match master)

* Verify merge
count if _merge != 3
if r(N) > 0 {
    di as error "ERROR: Some observations did not match with risk-free rate!"
    di as error "Make sure prepare_rfr.do has been run."
    exit 111
}
drop _merge

label var rfr "Risk-free rate (10-year PT gov bond, %)"

di "  ✓ Risk-free rate merged successfully"
di ""

/*==============================================================================
   STEP 3: CONSTRUCT KEY RATIOS AND MEASURES
==============================================================================*/

di "STEP 3: Constructing key ratios and measures..."
di ""

*------------------------------------------------------------------------------
* A. Capital intensity measures
*------------------------------------------------------------------------------

gen intang_intensity = K_intangible_total / K_total
label var intang_intensity "Intangible intensity (K_intang / K_total)"

gen knowledge_intensity = K_knowledge / K_intangible_total if K_intangible_total > 0
label var knowledge_intensity "Knowledge intensity (K_knowledge / K_intang)"

gen org_intensity = K_org / K_intangible_total if K_intangible_total > 0
label var org_intensity "Organization intensity (K_org / K_intang)"

gen external_intang_share = K_intangible_external / K_intangible_total if K_intangible_total > 0
label var external_intang_share "External intangibles share"

di "  ✓ Capital intensity measures"

*------------------------------------------------------------------------------
* B. Financial ratios
*------------------------------------------------------------------------------

gen total_debt = SV516001_real
label var total_debt "Total debt (real 2020 prices)"

gen leverage = total_debt / K_total if K_total > 0
label var leverage "Leverage ratio (Debt / K_total)"

gen debt_to_assets = total_debt / SV512701_real if SV512701_real > 0
label var debt_to_assets "Debt to total assets"

gen interest_rate = (SV502201_real / total_debt) * 100 if total_debt > 0
label var interest_rate "Implicit interest rate (%)"

gen credit_spread = interest_rate - rfr
label var credit_spread "Credit spread over risk-free rate (pp)"

di "  ✓ Financial ratios"

*------------------------------------------------------------------------------
* C. Skill and R&D measures
*------------------------------------------------------------------------------

* Number of R&D workers (RND_PER is NUMBER, not share)
gen n_rnd_workers = RND_PER if RND_PER != .
label var n_rnd_workers "Number of R&D workers"

* Share of R&D workers
gen share_rnd_workers = n_rnd_workers / n_workers if n_workers > 0
label var share_rnd_workers "Share of R&D workers"

* R&D intensity
cap confirm variable RD_real
if _rc == 0 {
    gen rnd_intensity = RD_real / SV500101_real if SV500101_real > 0
    label var rnd_intensity "R&D intensity (R&D / Revenue)"
}

di "  ✓ Skill and R&D measures"

*------------------------------------------------------------------------------
* D. Productivity measures
*------------------------------------------------------------------------------

* Labor productivity (Revenue per worker)
gen labor_productivity = SV500101_real / n_workers if n_workers > 0
label var labor_productivity "Labor productivity (Revenue / worker, real 2020)"

* TODO: Consider adding TFP measures:
* - Solow residual: TFP = Y - alpha*K - (1-alpha)*L
* - Levinsohn-Petrin or Olley-Pakes estimation
* - Wooldridge's method for panel data

di "  ✓ Productivity measures"

*------------------------------------------------------------------------------
* E. Investment rates
*------------------------------------------------------------------------------

gen inv_rate_physical = SD000032_real / K_total if K_total > 0
label var inv_rate_physical "Physical investment rate (I_physical / K_total)"

cap confirm variable RD_real
if _rc == 0 {
    gen inv_rate_knowledge = RD_real / K_total if K_total > 0
    label var inv_rate_knowledge "Knowledge investment rate (R&D / K_total)"
}

gen inv_rate_org = SGA_inv_real / K_total if K_total > 0
label var inv_rate_org "Organization investment rate (SG&A / K_total)"

* Balance sheet intangible investment (acquisitions, software, licenses)
cap confirm variable SD000031_real
if _rc == 0 {
    gen inv_bs_intangible = SD000031_real
    label var inv_bs_intangible "Balance sheet intangible investment (real 2020)"
    
    gen inv_rate_bs_intangible = SD000031_real / K_total if K_total > 0
    label var inv_rate_bs_intangible "BS intangible investment rate"
}

* Total intangible investment = R&D + SG&A + Balance sheet purchases
gen total_intang_inv = SGA_inv_real
if missing(total_intang_inv) total_intang_inv = 0

cap confirm variable RD_real
if _rc == 0 {
    replace total_intang_inv = total_intang_inv + RD_real if RD_real != .
}

cap confirm variable SD000031_real
if _rc == 0 {
    replace total_intang_inv = total_intang_inv + SD000031_real if SD000031_real != .
}

label var total_intang_inv "Total intangible investment (R&D + SG&A + BS, real 2020)"

* Total intangible investment rate
gen inv_rate_intangible = total_intang_inv / K_total if K_total > 0
label var inv_rate_intangible "Total intangible investment rate"

di "  ✓ Investment rates"

*------------------------------------------------------------------------------
* F. Size and age
*------------------------------------------------------------------------------

gen ln_emp = ln(n_workers)
label var ln_emp "Log employment"

cap confirm variable firm_age
if _rc == 0 {
    gen ln_age = ln(firm_age + 1)
    label var ln_age "Log firm age (+1)"
}

di "  ✓ Size and age measures"
di ""

/*==============================================================================
   STEP 4: WINSORIZATION (1%-99%)
==============================================================================*/

di "STEP 4: Winsorizing extreme values (1%-99%)..."
di ""

local winsor_vars "intang_intensity leverage interest_rate credit_spread     labor_productivity inv_rate_physical inv_rate_intangible                  inv_rate_bs_intangible rnd_intensity"

* Add R&D investment rate if exists
cap confirm variable inv_rate_knowledge
if _rc == 0 {
    local winsor_vars "`winsor_vars' inv_rate_knowledge"
}

* Winsorize each variable
foreach var of local winsor_vars {
    cap confirm variable `var'
    if _rc == 0 {
        qui sum `var', d
        local p1 = r(p1)
        local p99 = r(p99)
        
        gen `var'_w = `var'
        qui replace `var'_w = `p1' if `var' < `p1' & `var' != .
        qui replace `var'_w = `p99' if `var' > `p99' & `var' != .
        
        local lab: variable label `var'
        label var `var'_w "`lab' (winsorized 1%-99%)"
    }
}

di "  ✓ Winsorization complete (suffix '_w')"
di ""

/*==============================================================================
   STEP 5: SAVE ANALYSIS-READY DATASET
==============================================================================*/

di "STEP 5: Saving analysis-ready dataset..."
di ""

compress
save "final_data_2011_2022_analysis.dta", replace

local n_final = _N
di "  ✓ Saved: final_data_2011_2022_analysis.dta"
di "     Observations: " %12.0fc `n_final'
di ""

/*==============================================================================
   COMPLETION
==============================================================================*/

di "=========================================="
di "ANALYSIS PREPARATION COMPLETE"
di "=========================================="
di ""
di "Output: final_data_2011_2022_analysis.dta"
di ""
di "Key variables created:"
di "  • Capital: intang_intensity, knowledge_intensity, org_intensity"
di "  • Financial: leverage, interest_rate, credit_spread"
di "  • Skills: share_skilled, share_rnd_workers, rnd_intensity"
di "  • Productivity: labor_productivity (consider adding TFP)"
di "  • Investment: inv_rate_physical, inv_rate_knowledge, inv_rate_org"
di "               inv_rate_bs_intangible, inv_rate_intangible (total)"
di ""
di "Total intangible investment = R&D + SG&A + Balance sheet purchases"
di ""
di "Winsorized versions available (suffix '_w')"
di ""
di ""
di "=========================================="

cap log close
