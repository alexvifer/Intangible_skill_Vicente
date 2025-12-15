/*==============================================================================
   CONSTRUCT INTANGIBLE CAPITAL
   
   This module:
   1. Merges deflators with merged dataset
   2. Deflates all variables to real 2020 prices
   3. Constructs intangible capital stocks using perpetual inventory method
   4. Saves enhanced dataset with intangible capital variables
   
   Input:  final_data_2011_2022.dta (from merge_final.do)
           deflators.dta (from prepare_deflators.do)
   
   Output: final_data_2011_2022_intangibles.dta
   
   Method: Peters & Taylor (2017) with two intangible capital stocks:
           - Knowledge Capital (from R&D): δ = 15%
           - Organization Capital (from SG&A): δ = 20%
           - Initial stocks: K_0 = 0
   
==============================================================================*/

clear all
set more off

cd "$rootdir/output"

* Start log
cap log close
log using "construct_intangibles.log", replace

di "=========================================="
di "CONSTRUCTING INTANGIBLE CAPITAL"
di "Date: `c(current_date)'"
di "=========================================="

/*==============================================================================
   STEP 1: LOAD DATA AND MERGE DEFLATORS
==============================================================================*/

di _newline(2)
di "STEP 1: Loading merged dataset and deflators..."

use "final_data_2011_2022.dta", clear
local n_start = _N
di "  Observations in merged dataset: `n_start'"

* Merge deflators
merge m:1 ano using "$rootdir/input/deflators.dta", keep(match master)

tab _merge
count if _merge != 3
if r(N) > 0 {
    di "  WARNING: " r(N) " observations without deflators"
}
drop _merge

* Verify deflators exist
count if gdp_deflator == .
if r(N) > 0 {
    di "  ERROR: Missing GDP deflator for " r(N) " observations"
    exit
}

di "  → Deflators merged successfully"

/*==============================================================================
   STEP 2: DEFLATE ALL VARIABLES
==============================================================================*/

di _newline(2)
di "STEP 2: Deflating variables to real 2020 prices..."
di "  Base year: 2020 (deflators = 100)"

*------------------------------------------------------------------------------
* A. DEFLATE REVENUE AND SALES (GDP deflator)
*------------------------------------------------------------------------------
di "  A. Revenue and sales..."

foreach var in SV500101 SD000002 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
        label var `var'_real "`var' (real, 2020 prices)"
    }
}

*------------------------------------------------------------------------------
* B. DEFLATE COST VARIABLES (GDP deflator)
*------------------------------------------------------------------------------
di "  B. Cost variables..."

foreach var in SV500601 SV500801 SD000102 SV602500 SV602700 SV603900 ///
               SV804000 SV804100 SV804200 SV804400 SV804600 SV804700 ///
               SV804800 SV806700 SV807100 SV569305 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
        label var `var'_real "`var' (real, 2020 prices)"
    }
}

*------------------------------------------------------------------------------
* C. DEFLATE RESULT VARIABLES (GDP deflator)
*------------------------------------------------------------------------------
di "  C. Result variables (profits, EBITDA, etc.)..."

foreach var in SV501701 SV501801 SV502001 SV502201 SD000011 SD000012 ///
               SD000014 SD000016 SD000018 SD000019 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
        label var `var'_real "`var' (real, 2020 prices)"
    }
}

*------------------------------------------------------------------------------
* D. DEFLATE INVESTMENT FLOWS (GFCF deflator)
*------------------------------------------------------------------------------
di "  D. Investment flows..."

foreach var in SD000031 SD000032 SD000036 SD000037 SD000038 SD000113 ///
               SD000046 SD000047 SD000118 SD000119 SD000120 SD000121 ///
               SD000122 SD000123 SD000124 SD000125 SD000126 SD000127 ///
               SD000128 SD557602 SD557603 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gfcf_deflator) * 100
        label var `var'_real "`var' (real, 2020 prices)"
    }
}

*------------------------------------------------------------------------------
* E. DEFLATE DISINVESTMENT FLOWS (GFCF deflator)
*------------------------------------------------------------------------------
di "  E. Disinvestment flows..."

foreach var in SV558201 SD558202 SD558203 SD558204 SV558205 SD563301 ///
               SD563302 SD563303 SD563304 SD563305 SD563306 SV563307 ///
               SD000044 SD000115 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gfcf_deflator) * 100
        label var `var'_real "`var' (real, 2020 prices)"
    }
}

*------------------------------------------------------------------------------
* F. DEFLATE R&D EXPENDITURES (GDP deflator)
*------------------------------------------------------------------------------
di "  F. R&D expenditures..."

cap confirm variable IM_RND_EXPN
if _rc == 0 {
    gen RD_real = (IM_RND_EXPN / gdp_deflator) * 100
    label var RD_real "R&D expenditures (real, 2020 prices)"
    di "    → RD_real created from IM_RND_EXPN"
}
else {
    di "    WARNING: IM_RND_EXPN not found - R&D capital cannot be constructed"
}

*------------------------------------------------------------------------------
* G. DEFLATE BALANCE SHEET VARIABLES (Capital deflator)
*------------------------------------------------------------------------------
di "  G. Balance sheet variables (assets, liabilities, equity)..."

foreach var in SV510101 SV510201 SV510301 SV510401 SV511201 SV512501 ///
               SV512601 SV512701 SD000005 SV514101 SV514701 SV515901 ///
               SV516001 SV516101 {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / capital_deflator) * 100
        label var `var'_real "`var' (real, 2020 prices)"
    }
}

*------------------------------------------------------------------------------
* H. DEFLATE WAGES (GDP deflator)
*------------------------------------------------------------------------------
di "  H. Wages from QP..."

foreach var in avg_wage_all avg_wage_skilled avg_wage_unskilled {
    cap confirm variable `var'
    if _rc == 0 {
        gen `var'_real = (`var' / gdp_deflator) * 100
        label var `var'_real "`var' (real, 2020 prices)"
    }
}

di "  → All variables deflated successfully"

/*==============================================================================
   STEP 3: CONSTRUCT INTANGIBLE CAPITAL STOCKS
==============================================================================*/

di _newline(2)
di "=========================================="
di "CONSTRUCTING INTANGIBLE CAPITAL STOCKS"
di "=========================================="
di "Method: Perpetual Inventory Method (PIM)"
di "Following Peters & Taylor (2017)"
di ""
di "Parameters:"
di "  - Knowledge capital (R&D): δ = 15%"
di "  - Organization capital (SG&A): δ = 20%"
di "  - Initial stocks: K_0 = 0"
di "=========================================="

* Sort panel data
sort firm_id ano
xtset firm_id ano

*------------------------------------------------------------------------------
* STEP 3A: Create SG&A Investment Measure
*------------------------------------------------------------------------------
di _newline
di "STEP 3A: Creating SG&A investment measure..."
di "  SG&A = Advertising (SV804400) + Training (SV603900)"

* Generate SG&A investment = Advertising + Training (both in real terms)
gen SGA_inv_real = 0

* Add advertising if available
cap confirm variable SV804400_real
if _rc == 0 {
    replace SGA_inv_real = SV804400_real if SV804400_real != .
    di "    → Added advertising (SV804400_real)"
}
else {
    di "    WARNING: SV804400 not found - advertising excluded from SG&A"
}

* Add training if available
cap confirm variable SV603900_real
if _rc == 0 {
    replace SGA_inv_real = SGA_inv_real + SV603900_real if SV603900_real != .
    di "    → Added training (SV603900_real)"
}
else {
    di "    WARNING: SV603900 not found - training excluded from SG&A"
}

* Set to missing if both components are missing
replace SGA_inv_real = . if SV804400_real == . & SV603900_real == .

label var SGA_inv_real "SG&A (Advertising + Training, real 2020 prices)"

sum SGA_inv_real, d
di "    → SG&A investment summary:"
di "       Mean:   " %12.0f r(mean)
di "       Median: " %12.0f r(p50)
di "       N:      " r(N)

*------------------------------------------------------------------------------
* STEP 3B: Construct Knowledge Capital Stock (from R&D)
*------------------------------------------------------------------------------
di _newline
di "STEP 3B: Constructing Knowledge Capital (from R&D)..."
di "  Formula: K_t = (1 - δ) × K_{t-1} + I_t"
di "  where δ = 0.15, I_t = RD_real"

cap confirm variable RD_real
if _rc == 0 {
    * Set R&D to zero if missing (standard approach)
    gen RD_flow = RD_real
    replace RD_flow = 0 if RD_flow == .
    
    * Generate Knowledge Capital stock
    gen K_knowledge = 0
    by firm_id: replace K_knowledge = 0 if _n == 1  // Initial stock = 0
    
    * Apply perpetual inventory method
    * δ_R&D = 0.15 → (1-δ) = 0.85
    by firm_id: replace K_knowledge = 0.85 * K_knowledge[_n-1] + RD_flow if _n > 1
    
    label var K_knowledge "Knowledge capital stock (from R&D, real 2020 prices)"
    
    sum K_knowledge, d
    di "    → Knowledge capital constructed:"
    di "       Mean:   " %12.0f r(mean)
    di "       Median: " %12.0f r(p50)
    di "       Max:    " %12.0f r(max)
    di "       N:      " r(N)
}
else {
    gen K_knowledge = .
    di "    WARNING: Cannot construct knowledge capital - RD_real not available"
}

*------------------------------------------------------------------------------
* STEP 3C: Construct Organization Capital Stock (from SG&A)
*------------------------------------------------------------------------------
di _newline
di "STEP 3C: Constructing Organization Capital (from SG&A)..."
di "  Formula: K_t = (1 - δ) × K_{t-1} + I_t"
di "  where δ = 0.20, I_t = SGA_inv_real"

* Set SG&A investment to zero if missing
gen SGA_flow = SGA_inv_real
replace SGA_flow = 0 if SGA_flow == .

* Generate Organization Capital stock
gen K_org = 0
by firm_id: replace K_org = 0 if _n == 1  // Initial stock = 0

* Apply perpetual inventory method
* δ_SGA = 0.20 → (1-δ) = 0.80
by firm_id: replace K_org = 0.80 * K_org[_n-1] + SGA_flow if _n > 1

label var K_org "Organization capital stock (from SG&A, real 2020 prices)"

sum K_org, d
di "    → Organization capital constructed:"
di "       Mean:   " %12.0f r(mean)
di "       Median: " %12.0f r(p50)
di "       Max:    " %12.0f r(max)
di "       N:      " r(N)

*------------------------------------------------------------------------------
* STEP 3D: Create Total Intangible Capital
*------------------------------------------------------------------------------
di _newline
di "STEP 3D: Creating total intangible capital..."

* Internally created intangible capital
gen K_intangible_internal = K_knowledge + K_org
label var K_intangible_internal "Internally created intangible capital (real 2020 prices)"

* Add balance sheet intangibles (externally acquired)
* Note: SV510401_real is already deflated with capital deflator
cap confirm variable SV510401_real
if _rc == 0 {
    gen K_intangible_external = SV510401_real
    replace K_intangible_external = 0 if K_intangible_external == .
    label var K_intangible_external "Balance sheet intangibles (real 2020 prices)"
    di "    → Added balance sheet intangibles (SV510401_real)"
}
else {
    gen K_intangible_external = 0
    label var K_intangible_external "Balance sheet intangibles (real 2020 prices)"
    di "    WARNING: SV510401 not found - balance sheet intangibles set to 0"
}

* Total intangible capital
gen K_intangible_total = K_intangible_internal + K_intangible_external
label var K_intangible_total "Total intangible capital (real 2020 prices)"

sum K_intangible_total, d
di "    → Total intangible capital:"
di "       Mean:   " %12.0f r(mean)
di "       Median: " %12.0f r(p50)
di "       Max:    " %12.0f r(max)

*------------------------------------------------------------------------------
* STEP 3E: Create Total Capital
*------------------------------------------------------------------------------
di _newline
di "STEP 3E: Creating capital aggregates..."

* Physical capital (use real value)
cap confirm variable SV512701_real
if _rc == 0 {
    gen K_physical = SV512701_real
    replace K_physical = 0 if K_physical == .
    label var K_physical "Physical capital stock (real 2020 prices)"
}
else {
    gen K_physical = 0
    label var K_physical "Physical capital stock (real 2020 prices)"
    di "    WARNING: SV512701 not found - physical capital set to 0"
}

* Total capital
gen K_total = K_physical + K_intangible_total
label var K_total "Total capital (Physical + Intangible, real 2020 prices)"

di "    → Capital aggregates"

/*==============================================================================
   STEP 4: SAVE ENHANCED DATASET
==============================================================================*/

di _newline(2)
di "STEP 5: Saving dataset with intangible capital..."

compress
save "final_data_2011_2022_intangibles.dta", replace

local n_final = _N
di "  → Dataset saved: final_data_2011_2022_intangibles.dta"
di "    Observations: `n_final'"

/*==============================================================================
   COMPLETION MESSAGE
==============================================================================*/

di _newline(2)
di "=========================================="
di "INTANGIBLE CAPITAL CONSTRUCTION COMPLETE"
di "=========================================="
di ""
di "Output file:"
di "  → final_data_2011_2022_intangibles.dta"
di ""
di "Key variables created:"
di "  • K_knowledge           - Knowledge capital (from R&D)"
di "  • K_org                 - Organization capital (from SG&A)"
di "  • K_intangible_total    - Total intangible capital"
di "  • K_physical            - Physical capital"
di "  • K_total               - Total capital"
di "  • All original variables with '_real' suffix"
di ""
di "Next step:"
di "  → Run data_clean_final.do for cleaning and winsorization"
di "=========================================="

cap log close
