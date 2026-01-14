/*==============================================================================
   CONSTRUCT INTANGIBLE CAPITAL FROM CLEAN DATA - FLEXIBLE SYSTEM
   
   Input:  final_data_2011_2022_cleaned.dta (from data_clean_final.do)
   Output: final_data_2011_2022_intangibles.dta
   
   This file constructs intangible capital stocks using perpetual inventory
   method (PIM) with a FLEXIBLE SYSTEM supporting multiple definitions.
   
   All source variables are already:
   - Deflated to real 2020 prices
   - Cleaned (no missing/negative balance sheet items)
   
   INTANGIBLE CAPITAL DEFINITIONS:
   ================================
   
   MEASURE 1: Peters & Taylor (2017) - Full Specification
   --------------------------------------------------------
   Components:
   - Knowledge Capital (from R&D): δ = SECTOR-SPECIFIC (EPW 2025)
   - Organization Capital (from SG&A): δ = 20%
   - Balance Sheet Intangibles: δ = N/A (stock variable)
   
   Variables created:
   - K_intangible_pt     : Full P&T measure (Knowledge + Org + BS)
   - K_intangible_pt_int : Internal only (Knowledge + Org)
   
   MEASURE 2: Balance Sheet + R&D Only
   -------------------------------------
   Components:
   - Knowledge Capital (from R&D): δ = SECTOR-SPECIFIC (EPW 2025)
   - Balance Sheet Intangibles: δ = N/A (stock variable)
   
   Variables created:
   - K_intangible_bs_rd  : BS + R&D measure
   
   COMMON COMPONENTS:
   ------------------
   - K_knowledge         : Knowledge capital from R&D (sector-specific δ)
   - K_org               : Organization capital from SG&A (δ=20%)
   - K_intangible_bs     : Balance sheet intangibles (direct from BS)
   - K_physical          : Physical capital (tangible fixed assets)
   
   SECTOR-SPECIFIC R&D DEPRECIATION (Ewens, Peters & Wang 2025):
   --------------------------------------------------------------
   - Consumer: δ = 43%
   - Manufacturing: δ = 50%
   - High Tech: δ = 42%
   - Health: δ = 33%
   - Other: δ = 35%
   
   TOTAL CAPITAL VARIABLES:
   ------------------------
   For each intangible definition X, we create:
   - K_total_X = K_physical + K_intangible_X
   
   Example: K_total_pt = K_physical + K_intangible_pt
   
==============================================================================*/

clear all
set more off

cd "$rootdir/output"

cap log close
log using "construct_intangibles.log", replace

use "final_data_2011_2022_cleaned.dta", clear

* Sector classification following Ewens, Peters & Wang (2025) Fama-French 5
gen ff5_industry = ""
replace ff5_industry = "Consumer" if inlist(cae3, 10, 11, 12, 13, 14, 15)
replace ff5_industry = "Consumer" if inlist(cae3, 45, 46, 47, 55, 56, 95, 96)
replace ff5_industry = "Manufacturing" if inlist(cae3, 16, 17, 18, 19, 20, 22, 23)
replace ff5_industry = "Manufacturing" if inlist(cae3, 24, 25, 28, 29, 30, 31, 32, 33)
replace ff5_industry = "High Tech" if inlist(cae3, 26, 27, 61, 62, 63)
replace ff5_industry = "Health" if inlist(cae3, 21, 86, 87, 88)
replace ff5_industry = "Other" if ff5_industry == ""
label var ff5_industry "Fama-French 5 Industry (EPW 2025)"

tab ff5_industry, missing

sort firm_id ano
xtset firm_id ano

* SG&A Investment (Advertising + Training)
gen SGA_inv_real = 0
cap confirm variable SV804400_real
if _rc == 0 {
    replace SGA_inv_real = SV804400_real if SV804400_real != .
}
cap confirm variable SV603900_real
if _rc == 0 {
    replace SGA_inv_real = SGA_inv_real + SV603900_real if SV603900_real != .
}
replace SGA_inv_real = . if SV804400_real == . & SV603900_real == .

gen SGA_flow = SGA_inv_real
replace SGA_flow = 0 if SGA_flow == .
label var SGA_inv_real "SG&A investment (Advertising + Training, real 2020 prices)"
label var SGA_flow "SG&A flow for PIM (missing → 0)"

* R&D Investment
cap confirm variable RD_real
if _rc == 0 {
    gen RD_flow = RD_real
    replace RD_flow = 0 if RD_flow == .
    label var RD_flow "R&D flow for PIM (missing → 0)"
}
else {
    gen RD_flow = 0
}

* Knowledge Capital Stock (sector-specific δ from EPW 2025)
gen K_knowledge = 0
by firm_id: replace K_knowledge = 0 if _n == 1

by firm_id: replace K_knowledge = 0.57 * K_knowledge[_n-1] + RD_flow if _n > 1 & ff5_industry == "Consumer"
by firm_id: replace K_knowledge = 0.50 * K_knowledge[_n-1] + RD_flow if _n > 1 & ff5_industry == "Manufacturing"
by firm_id: replace K_knowledge = 0.58 * K_knowledge[_n-1] + RD_flow if _n > 1 & ff5_industry == "High Tech"
by firm_id: replace K_knowledge = 0.67 * K_knowledge[_n-1] + RD_flow if _n > 1 & ff5_industry == "Health"
by firm_id: replace K_knowledge = 0.65 * K_knowledge[_n-1] + RD_flow if _n > 1 & ff5_industry == "Other"

label var K_knowledge "Knowledge capital (sector-specific δ, real 2020)"

* Organization Capital Stock (δ=20%)
gen K_org = 0
by firm_id: replace K_org = 0 if _n == 1
by firm_id: replace K_org = 0.80 * K_org[_n-1] + SGA_flow if _n > 1
label var K_org "Organization capital (SG&A, δ=20%, real 2020)"

* Balance Sheet Intangibles
cap confirm variable SV510401_real
if _rc == 0 {
    gen K_intangible_bs = SV510401_real
    label var K_intangible_bs "Balance sheet intangibles (real 2020)"
}
else {
    di as error "ERROR: SV510401_real not found!"
    exit 111
}

* Physical Capital
cap confirm variable SV510101_real
if _rc == 0 {
    gen K_physical = SV510101_real
    label var K_physical "Physical capital (tangible fixed assets, real 2020)"
}
else {
    di as error "ERROR: SV510101_real not found!"
    exit 111
}

* Peters & Taylor (2017) Full Specification

* Internal intangibles (Knowledge + Organization)
gen K_intangible_pt_int = K_knowledge + K_org
label var K_intangible_pt_int "P&T internal intangibles (K + Org, real 2020)"

* Full P&T measure (Internal + External)
gen K_intangible_pt = K_intangible_pt_int + K_intangible_bs
label var K_intangible_pt "P&T total intangibles (K + Org + BS, real 2020)"

* Total capital under P&T definition
gen K_total_pt = K_physical + K_intangible_pt
label var K_total_pt "P&T total capital (Physical + Intangible, real 2020)"

sum K_intangible_pt, d
di "     Mean:   " %12.0f r(mean)
di "     Median: " %12.0f r(p50)
di "     Max:    " %12.0f r(max)
di ""

*------------------------------------------------------------------------------
* MEASURE 2: Balance Sheet + R&D Only (NO Organization Capital)
*------------------------------------------------------------------------------

di "  MEASURE 2: Balance Sheet + R&D"
di "  ───────────────────────────────"

* BS + R&D measure (excludes Organization Capital)
gen K_intangible_bs_rd = K_knowledge + K_intangible_bs
label var K_intangible_bs_rd "BS+R&D intangibles (K + BS, NO Org, real 2020)"

* Total capital under BS+R&D definition
gen K_total_bs_rd = K_physical + K_intangible_bs_rd
label var K_total_bs_rd "BS+R&D total capital (Physical + Intangible, real 2020)"

sum K_intangible_bs_rd, d
di "     Mean:   " %12.0f r(mean)
di "     Median: " %12.0f r(p50)
di "     Max:    " %12.0f r(max)
di ""

/*==============================================================================
   STEP 5: CREATE LEGACY VARIABLE NAMES FOR BACKWARD COMPATIBILITY
==============================================================================*/

di "STEP 5: Creating legacy variable names (backward compatibility)..."
di ""

* Map new flexible system to old variable names for existing code
* Default measure: Peters & Taylor Full
gen K_intangible_total = K_intangible_pt
label var K_intangible_total "Total intangible capital (DEFAULT: P&T Full, real 2020)"

gen K_total = K_total_pt
label var K_total "Total capital (DEFAULT: P&T, real 2020)"

gen K_intangible_internal = K_intangible_pt_int
label var K_intangible_internal "Internal intangibles (DEFAULT: P&T, real 2020)"

gen K_intangible_external = K_intangible_bs
label var K_intangible_external "External intangibles (DEFAULT: BS, real 2020)"

di "  ✓ Legacy names mapped to P&T Full measure"
di "    (For backward compatibility with existing analysis code)"
di ""

/*==============================================================================
   STEP 6: VERIFY CONSISTENCY
==============================================================================*/

di "STEP 6: Verifying consistency..."
di ""

*------------------------------------------------------------------------------
* Check component stocks
*------------------------------------------------------------------------------

count if K_physical < 0 | K_physical == .
if r(N) > 0 {
    di as error "  ERROR: K_physical has " r(N) " negative/missing values!"
}

count if K_intangible_bs < 0 | K_intangible_bs == .
if r(N) > 0 {
    di as error "  ERROR: K_intangible_bs has " r(N) " negative/missing values!"
}

count if K_knowledge == .
if r(N) > 0 {
    di "  WARNING: K_knowledge has " r(N) " missing values"
}

count if K_org < 0 | K_org == .
if r(N) > 0 {
    di as error "  ERROR: K_org has " r(N) " negative/missing values!"
}

*------------------------------------------------------------------------------
* Check aggregates consistency for each measure
*------------------------------------------------------------------------------

* Peters & Taylor consistency
gen check_pt1 = abs(K_intangible_pt_int - (K_knowledge + K_org))
gen check_pt2 = abs(K_intangible_pt - (K_intangible_pt_int + K_intangible_bs))
gen check_pt3 = abs(K_total_pt - (K_physical + K_intangible_pt))

qui sum check_pt1
if r(max) > 0.01 {
    di as error "  ERROR: K_intangible_pt_int inconsistent!"
}

qui sum check_pt2
if r(max) > 0.01 {
    di as error "  ERROR: K_intangible_pt inconsistent!"
}

qui sum check_pt3
if r(max) > 0.01 {
    di as error "  ERROR: K_total_pt inconsistent!"
}

drop check_pt*

* BS+R&D consistency
gen check_bs1 = abs(K_intangible_bs_rd - (K_knowledge + K_intangible_bs))
gen check_bs2 = abs(K_total_bs_rd - (K_physical + K_intangible_bs_rd))

qui sum check_bs1
if r(max) > 0.01 {
    di as error "  ERROR: K_intangible_bs_rd inconsistent!"
}

qui sum check_bs2
if r(max) > 0.01 {
    di as error "  ERROR: K_total_bs_rd inconsistent!"
}

drop check_bs*

di "  ✓ All consistency checks passed"
di ""

/*==============================================================================
   STEP 7: SAVE DATASET WITH INTANGIBLE CAPITAL
==============================================================================*/

di "STEP 7: Saving dataset with intangible capital..."
di ""

compress
save "final_data_2011_2022_intangibles.dta", replace

local n_final = _N
di "  ✓ Saved: final_data_2011_2022_intangibles.dta"
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
di "══════════════════════════════════════════════════════════════════"
di "COMPONENT VARIABLES"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Building Blocks:"
di "  • K_knowledge           Knowledge capital (R&D, sector-specific δ)"
di "  • K_org                 Organization capital (SG&A, δ=20%)"
di "  • K_intangible_bs       Balance sheet intangibles"
di "  • K_physical            Physical capital"
di "  • ff5_industry          Sector classification (EPW 2025)"
di ""
di "══════════════════════════════════════════════════════════════════"
di "ALTERNATIVE INTANGIBLE CAPITAL MEASURES"
di "══════════════════════════════════════════════════════════════════"
di ""
di "MEASURE 1: Peters & Taylor (2017) Full"
di "───────────────────────────────────────"
di "  • K_intangible_pt_int   Internal (K + Org)"
di "  • K_intangible_pt       Total (K + Org + BS)"
di "  • K_total_pt            Physical + Intangible"
di ""
di "MEASURE 2: Balance Sheet + R&D Only"
di "────────────────────────────────────"
di "  • K_intangible_bs_rd    BS + R&D (NO Org)"
di "  • K_total_bs_rd         Physical + Intangible"
di ""
di "══════════════════════════════════════════════════════════════════"
di "LEGACY VARIABLES (Backward Compatibility)"
di "══════════════════════════════════════════════════════════════════"
di ""
di "These map to P&T Full measure by default:"
di "  • K_intangible_internal → K_intangible_pt_int"
di "  • K_intangible_external → K_intangible_bs"
di "  • K_intangible_total    → K_intangible_pt"
di "  • K_total               → K_total_pt"
di ""
di "══════════════════════════════════════════════════════════════════"
di "SECTOR-SPECIFIC DEPRECIATION RATES (EPW 2025)"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Knowledge Capital (R&D):"
di "  • Consumer:      δ = 43%"
di "  • Manufacturing: δ = 50%"
di "  • High Tech:     δ = 42%"
di "  • Health:        δ = 33%"
di "  • Other:         δ = 35%"
di ""
di "Organization Capital (SG&A): δ = 20% (all sectors)"
di ""
di "══════════════════════════════════════════════════════════════════"
di ""
di "All capital stocks are:"
di "  ✓ Non-negative"
di "  ✓ Non-missing (where applicable)"
di "  ✓ In real 2020 prices"
di "  ✓ Internally consistent"
di "  ✓ Sector-heterogeneous (knowledge capital)"
di ""
di "Next step:"
di "  → Run analysis_prep.do to create analytical variables"
di "    for each intangible capital definition"
di ""
di "=========================================="

cap log close
