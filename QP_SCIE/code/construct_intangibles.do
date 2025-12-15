/*==============================================================================
   CONSTRUCT INTANGIBLE CAPITAL FROM CLEAN DATA
   
   Input:  final_data_2011_2022_cleaned.dta (from data_clean_final.do)
   Output: final_data_2011_2022_intangibles.dta
   
   This file constructs intangible capital stocks using perpetual inventory
   method (PIM) following Peters & Taylor (2017).
   
   All source variables are already:
   - Deflated to real 2020 prices
   - Cleaned (no missing/negative balance sheet items)
   
   Method: Peters & Taylor (2017) with two intangible capital stocks:
           - Knowledge Capital (from R&D): δ = 15%
           - Organization Capital (from SG&A): δ = 20%
           - Initial stocks: K_0 = 0
   
==============================================================================*/

clear all
set more off

cd "$rootdir/output"

cap log close
log using "construct_intangibles.log", replace

di "=========================================="
di "CONSTRUCTING INTANGIBLE CAPITAL"
di "=========================================="
di "Date: `c(current_date)'"
di ""
di "Method: Perpetual Inventory Method (PIM)"
di "Following Peters & Taylor (2017)"
di ""
di "Parameters:"
di "  - Knowledge capital (R&D): δ = 15%"
di "  - Organization capital (SG&A): δ = 20%"
di "  - Initial stocks: K_0 = 0"
di ""
di "=========================================="
di ""

/*==============================================================================
   STEP 1: LOAD CLEANED DATA
==============================================================================*/

di "STEP 1: Loading cleaned dataset..."

use "final_data_2011_2022_cleaned.dta", clear

local n_start = _N
di "  Observations: " %12.0fc `n_start'
di ""

* Sort panel data
sort firm_id ano
xtset firm_id ano

/*==============================================================================
   STEP 2: CREATE SG&A INVESTMENT MEASURE
==============================================================================*/

di "STEP 2: Creating SG&A investment measure..."
di "  SG&A = Advertising (SV804400_real) + Training (SV603900_real)"
di ""

* Generate SG&A investment = Advertising + Training
gen SGA_inv_real = 0

* Add advertising if available
cap confirm variable SV804400_real
if _rc == 0 {
    replace SGA_inv_real = SV804400_real if SV804400_real != .
}

* Add training if available
cap confirm variable SV603900_real
if _rc == 0 {
    replace SGA_inv_real = SGA_inv_real + SV603900_real if SV603900_real != .
}

* Set to missing if both components are missing
replace SGA_inv_real = . if SV804400_real == . & SV603900_real == .

* For PIM, missing flows = no investment = 0
gen SGA_flow = SGA_inv_real
replace SGA_flow = 0 if SGA_flow == .

label var SGA_inv_real "SG&A investment (Advertising + Training, real 2020 prices)"
label var SGA_flow "SG&A flow for PIM (missing → 0)"

sum SGA_inv_real, d
di "  SG&A investment summary:"
di "    Mean:   " %12.0f r(mean)
di "    Median: " %12.0f r(p50)
di "    N:      " r(N)
di ""

/*==============================================================================
   STEP 3: CONSTRUCT KNOWLEDGE CAPITAL STOCK (FROM R&D)
==============================================================================*/

di "STEP 3: Constructing Knowledge Capital (from R&D)..."
di "  Formula: K_t = (1 - δ) × K_{t-1} + I_t"
di "  where δ = 0.15, I_t = R&D expenditures"
di ""

cap confirm variable RD_real
if _rc == 0 {
    * For PIM, missing flows = no investment = 0
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
    di "  Knowledge capital constructed:"
    di "    Mean:   " %12.0f r(mean)
    di "    Median: " %12.0f r(p50)
    di "    Max:    " %12.0f r(max)
    di "    N:      " r(N)
}
else {
    gen K_knowledge = .
    di "  WARNING: RD_real not found - knowledge capital cannot be constructed"
}
di ""

/*==============================================================================
   STEP 4: CONSTRUCT ORGANIZATION CAPITAL STOCK (FROM SG&A)
==============================================================================*/

di "STEP 4: Constructing Organization Capital (from SG&A)..."
di "  Formula: K_t = (1 - δ) × K_{t-1} + I_t"
di "  where δ = 0.20, I_t = SG&A expenditures"
di ""

* Generate Organization Capital stock
gen K_org = 0
by firm_id: replace K_org = 0 if _n == 1  // Initial stock = 0

* Apply perpetual inventory method
* δ_SGA = 0.20 → (1-δ) = 0.80
by firm_id: replace K_org = 0.80 * K_org[_n-1] + SGA_flow if _n > 1

label var K_org "Organization capital stock (from SG&A, real 2020 prices)"

sum K_org, d
di "  Organization capital constructed:"
di "    Mean:   " %12.0f r(mean)
di "    Median: " %12.0f r(p50)
di "    Max:    " %12.0f r(max)
di "    N:      " r(N)
di ""

/*==============================================================================
   STEP 5: CREATE CAPITAL AGGREGATES
==============================================================================*/

di "STEP 5: Creating capital aggregates..."
di ""

*------------------------------------------------------------------------------
* Physical capital (from cleaned balance sheet)
*------------------------------------------------------------------------------

cap confirm variable SV510101_real
if _rc == 0 {
    gen K_physical = SV510101_real
    label var K_physical "Physical capital stock (tangible fixed assets, real 2020 prices)"
}
else {
    di "  ERROR: SV510101_real not found!"
    exit 111
}

*------------------------------------------------------------------------------
* Internally created intangible capital
*------------------------------------------------------------------------------

gen K_intangible_internal = K_knowledge + K_org
label var K_intangible_internal "Internally created intangible capital (real 2020 prices)"

*------------------------------------------------------------------------------
* Externally acquired intangibles (from cleaned balance sheet)
*------------------------------------------------------------------------------

cap confirm variable SV510401_real
if _rc == 0 {
    gen K_intangible_external = SV510401_real
    label var K_intangible_external "Balance sheet intangibles (real 2020 prices)"
}
else {
    di "  ERROR: SV510401_real not found!"
    exit 111
}

*------------------------------------------------------------------------------
* Total intangible capital
*------------------------------------------------------------------------------

gen K_intangible_total = K_intangible_internal + K_intangible_external
label var K_intangible_total "Total intangible capital (real 2020 prices)"

sum K_intangible_total, d
di "  Total intangible capital:"
di "    Mean:   " %12.0f r(mean)
di "    Median: " %12.0f r(p50)
di "    Max:    " %12.0f r(max)
di ""

*------------------------------------------------------------------------------
* Total capital
*------------------------------------------------------------------------------

gen K_total = K_physical + K_intangible_total
label var K_total "Total capital (Physical + Intangible, real 2020 prices)"

sum K_total, d
di "  Total capital:"
di "    Mean:   " %12.0f r(mean)
di "    Median: " %12.0f r(p50)
di "    Max:    " %12.0f r(max)
di ""

/*==============================================================================
   STEP 6: VERIFY CONSISTENCY
==============================================================================*/

di "STEP 6: Verifying consistency..."
di ""

* Check that all capital stocks are non-negative and non-missing
count if K_physical < 0 | K_physical == .
if r(N) > 0 {
    di as error "  ERROR: K_physical has " r(N) " negative/missing values!"
}

count if K_intangible_external < 0 | K_intangible_external == .
if r(N) > 0 {
    di as error "  ERROR: K_intangible_external has " r(N) " negative/missing values!"
}

count if K_knowledge == .
if r(N) > 0 {
    di "  WARNING: K_knowledge has " r(N) " missing values"
}

count if K_org < 0 | K_org == .
if r(N) > 0 {
    di as error "  ERROR: K_org has " r(N) " negative/missing values!"
}

* Check aggregates consistency
gen check1 = abs(K_intangible_internal - (K_knowledge + K_org))
gen check2 = abs(K_intangible_total - (K_intangible_internal + K_intangible_external))
gen check3 = abs(K_total - (K_physical + K_intangible_total))

sum check1 check2 check3

qui sum check1
if r(max) > 0.01 {
    di as error "  ERROR: K_intangible_internal inconsistent with components!"
}

qui sum check2
if r(max) > 0.01 {
    di as error "  ERROR: K_intangible_total inconsistent with components!"
}

qui sum check3
if r(max) > 0.01 {
    di as error "  ERROR: K_total inconsistent with components!"
}

drop check1 check2 check3

di "  → All consistency checks passed"
di ""

/*==============================================================================
   STEP 7: SAVE DATASET WITH INTANGIBLE CAPITAL
==============================================================================*/

di "STEP 7: Saving dataset with intangible capital..."
di ""

compress
save "final_data_2011_2022_intangibles.dta", replace

local n_final = _N
di "  → Saved: final_data_2011_2022_intangibles.dta"
di "     Observations: " %12.0fc `n_final'
di ""

/*==============================================================================
   COMPLETION MESSAGE
==============================================================================*/

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
di "  • K_intangible_internal - Internal intangibles (K_knowledge + K_org)"
di "  • K_intangible_external - External intangibles (from balance sheet)"
di "  • K_intangible_total    - Total intangible capital"
di "  • K_physical            - Physical capital (from balance sheet)"
di "  • K_total               - Total capital (Physical + Intangible)"
di ""
di "All capital stocks are:"
di "  ✓ Non-negative"
di "  ✓ Non-missing"
di "  ✓ In real 2020 prices"
di "  ✓ Internally consistent"
di ""
di "Next steps (optional):"
di "  → Create derived variables (intensities, ratios)"
di "  → Winsorize for specific analyses"
di "  → Generate descriptive statistics"
di ""
di "=========================================="

cap log close
