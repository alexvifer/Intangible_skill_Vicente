/*==============================================================================
   ANALYSIS PREPARATION - FLEXIBLE MULTI-MEASURE SYSTEM
   
   Input:  final_data_2011_2022_intangibles.dta (from construct_intangibles.do)
   Output: final_data_2011_2022_analysis.dta
   
   This file:
   1. Merges risk-free rate for credit spread calculation
   2. Constructs base variables (totals, aggregates)
   3. Creates derived variables FOR EACH INTANGIBLE CAPITAL DEFINITION
   
   INTANGIBLE CAPITAL MEASURES SUPPORTED:
   ======================================
   
   MEASURE 1: Peters & Taylor (2017) Full
   - Variables use suffix: _pt
   - Components: K_knowledge + K_org + K_intangible_bs
   
   MEASURE 2: Balance Sheet + R&D Only
   - Variables use suffix: _bs_rd
   - Components: K_knowledge + K_intangible_bs (NO Org)
   
   LEGACY (No Suffix):
   - Default variables map to P&T Full measure
   - For backward compatibility with existing code
   
==============================================================================*/

clear all
set more off

cd "$rootdir/output"

cap log close
log using "analysis_prep.log", replace

di "=========================================="
di "ANALYSIS PREPARATION"
di "FLEXIBLE MULTI-MEASURE SYSTEM"
di "=========================================="
di "Date: `c(current_date)'"
di ""
di "This creates analytical variables for:"
di "  • Peters & Taylor (2017) Full"
di "  • Balance Sheet + R&D Only"
di "  • Legacy (default = P&T Full)"
di ""
di "=========================================="
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
   STEP 3: CONSTRUCT BASE VARIABLES (MEASURE-INDEPENDENT)
==============================================================================*/

di "STEP 3: Constructing base variables (measure-independent)..."
di ""

*------------------------------------------------------------------------------
* A. Label capital stock components
*------------------------------------------------------------------------------

label var K_physical "Physical capital (tangible fixed assets, real 2020)"
label var K_knowledge "Knowledge capital from R&D (real 2020)"
label var K_org "Organization capital from SG&A (real 2020)"
label var K_intangible_bs "Balance sheet intangibles (real 2020)"

*------------------------------------------------------------------------------
* B. Financial variables 
*------------------------------------------------------------------------------

* Long-term and short-term debt
gen debt_lt = SV514301_real
label var debt_lt "Long-term debt (real 2020)"

gen debt_st = SV515201_real
label var debt_st "Short-term debt (real 2020)"

* Total debt
gen total_debt = debt_lt + debt_st
label var total_debt "Total debt (real 2020)"

* Interest expenses
gen interest_expense = SV502201_real
label var interest_expense "Interest expenses (real 2020)"

* Total assets (real)
gen total_assets = SV512701_real
label var total_assets "Total assets from balance sheet (real 2020)"

* Equity 
gen equity = SV514101_real
label var equity "Equity (real 2020)"

*------------------------------------------------------------------------------
* C. Production, revenue, sales and GVA
*------------------------------------------------------------------------------

gen revenue = SV500101_real
label var revenue "Revenue (real 2020)"

gen production = SD000011_real
label var production "Production value (real 2020)"

gen wagebill = SV500801_real
label var wagebill "Total wagebill (real 2020)"

gen sales = SD000002_real
label var sales "Total sales (real 2020)"

gen GVA = SD000014_real
label var GVA "Gross value added (real 2020)"

*------------------------------------------------------------------------------
* D. Investment flows
*------------------------------------------------------------------------------

gen inv_physical = SD000032_real
label var inv_physical "Physical investment (real 2020)"

* R&D investment (if available)
cap confirm variable RD_real
if _rc == 0 {
    gen inv_rd = RD_real
    label var inv_rd "R&D investment (real 2020)"
}

* SG&A investment (already constructed in construct_intangibles.do)
gen inv_sga = SGA_inv_real
label var inv_sga "SG&A investment (Advertising + Training, real 2020)"

* Balance sheet intangible investment (acquisitions, software, etc.)
cap confirm variable SD000031_real
if _rc == 0 {
    gen inv_intang_bs = SD000031_real
    label var inv_intang_bs "Balance sheet intangible investment (real 2020)"
}

* Total intangible investment = R&D + SG&A + BS purchases
gen inv_intang_total = inv_sga
replace inv_intang_total = 0 if missing(inv_intang_total)

cap confirm variable inv_rd
if _rc == 0 {
    replace inv_intang_total = inv_intang_total + inv_rd if !missing(inv_rd)
}

cap confirm variable inv_intang_bs
if _rc == 0 {
    replace inv_intang_total = inv_intang_total + inv_intang_bs if !missing(inv_intang_bs)
}

label var inv_intang_total "Total intangible investment (R&D + SG&A + BS, real 2020)"

*------------------------------------------------------------------------------
* E. Labor variables
*------------------------------------------------------------------------------

* Number of workers (already exists as n_workers)
label var n_workers "Total number of workers"

* Number of R&D workers
gen n_rd_workers = RND_PER if RND_PER != .
label var n_rd_workers "Number of R&D workers"

* Share of skilled workers (already calculated, bounded [0,1])
label var share_skilled "Share of workers with tertiary education"

di "  ✓ Base variables constructed"
di ""

/*==============================================================================
   STEP 4: CONSTRUCT DERIVED VARIABLES - PETERS & TAYLOR FULL
==============================================================================*/

di "STEP 4: Constructing derived variables for P&T Full measure..."
di ""

*------------------------------------------------------------------------------
* A. Capital intensity measures
*------------------------------------------------------------------------------

gen intang_intensity_pt = K_intangible_pt / K_total_pt if K_total_pt > 0
label var intang_intensity_pt "P&T: Intangible intensity (K_intang / K_total)"

gen knowledge_intensity_pt = K_knowledge / K_intangible_pt if K_intangible_pt > 0
label var knowledge_intensity_pt "P&T: Knowledge intensity (K_knowledge / K_intang)"

gen org_intensity_pt = K_org / K_intangible_pt if K_intangible_pt > 0
label var org_intensity_pt "P&T: Organization intensity (K_org / K_intang)"

* Truncate at theoretical bounds [0,1]
foreach var in intang_intensity_pt knowledge_intensity_pt org_intensity_pt {
    qui replace `var' = 0 if `var' < 0 & !missing(`var')
    qui replace `var' = 1 if `var' > 1 & !missing(`var')
}

*------------------------------------------------------------------------------
* B. Financial ratios (using P&T total capital)
*------------------------------------------------------------------------------

gen leverage_pt = total_debt / K_total_pt if K_total_pt > 0
label var leverage_pt "P&T: Leverage (Debt / K_total)"

gen leverage_assets = total_debt / total_assets if total_assets > 0
label var leverage_assets "Leverage (Debt / Assets)"

* Interest rate using LAGGED debt
sort firm_id ano
by firm_id: gen debt_lag = cond(ano == 2011, debt_2010, L.total_debt)
gen interest_rate = (interest_expense / debt_lag) * 100 if debt_lag > 0 & interest_expense > 0
label var interest_rate "Implicit interest rate (%, interest_t / debt_{t-1})"
label var debt_lag "Lagged total debt (nominal, for interest rate calculation)"

gen credit_spread = interest_rate - rfr if !missing(interest_rate) & !missing(rfr)
label var credit_spread "Credit spread over risk-free rate (pp)"

*------------------------------------------------------------------------------
* C. Productivity measures (measure-independent)
*------------------------------------------------------------------------------

gen labor_productivity_rev = revenue / n_workers if n_workers > 0
label var labor_productivity_rev "Labor productivity (Revenue / worker)"

gen labor_productivity_sales = sales / n_workers if n_workers > 0
label var labor_productivity_sales "Labor productivity (Sales / worker)"

gen labor_productivity_GVA = GVA / n_workers if n_workers > 0
label var labor_productivity_GVA "Labor productivity (GVA / worker)"

*------------------------------------------------------------------------------
* D. Investment rates (using P&T lagged capital)
*------------------------------------------------------------------------------

* Generate lagged capital for investment rate denominators
sort firm_id ano
by firm_id: gen K_total_pt_lag = L.K_total_pt

* Investment rates: I_t / K_{t-1} (standard practice)
gen inv_rate_physical_pt = inv_physical / K_total_pt_lag if K_total_pt_lag > 0 & inv_physical >= 0
label var inv_rate_physical_pt "P&T: Physical investment rate (I_t / K_{t-1})"

cap confirm variable inv_rd
if _rc == 0 {
    gen inv_rate_rd_pt = inv_rd / K_total_pt_lag if K_total_pt_lag > 0 & inv_rd >= 0
    label var inv_rate_rd_pt "P&T: R&D investment rate (I_t / K_{t-1})"
}

gen inv_rate_sga_pt = inv_sga / K_total_pt_lag if K_total_pt_lag > 0 & inv_sga >= 0
label var inv_rate_sga_pt "P&T: SG&A investment rate (I_t / K_{t-1})"

gen inv_rate_intang_pt = inv_intang_total / K_total_pt_lag if K_total_pt_lag > 0 & inv_intang_total >= 0
label var inv_rate_intang_pt "P&T: Total intangible investment rate (I_t / K_{t-1})"

*------------------------------------------------------------------------------
* E. Log transforms (P&T measure)
*------------------------------------------------------------------------------

gen ln_K_total_pt = ln(K_total_pt) if K_total_pt > 0
label var ln_K_total_pt "P&T: Log Total capital"

gen ln_K_intangible_pt = ln(K_intangible_pt) if K_intangible_pt > 0
label var ln_K_intangible_pt "P&T: Log Intangible capital"

di "  ✓ P&T Full measure variables constructed"
di ""

/*==============================================================================
   STEP 5: CONSTRUCT DERIVED VARIABLES - BALANCE SHEET + R&D
==============================================================================*/

di "STEP 5: Constructing derived variables for BS+R&D measure..."
di ""

*------------------------------------------------------------------------------
* A. Capital intensity measures
*------------------------------------------------------------------------------

gen intang_intensity_bs_rd = K_intangible_bs_rd / K_total_bs_rd if K_total_bs_rd > 0
label var intang_intensity_bs_rd "BS+R&D: Intangible intensity (K_intang / K_total)"

gen knowledge_intensity_bs_rd = K_knowledge / K_intangible_bs_rd if K_intangible_bs_rd > 0
label var knowledge_intensity_bs_rd "BS+R&D: Knowledge intensity (K_knowledge / K_intang)"

* Note: No org_intensity for BS+R&D since it excludes organization capital

* Truncate at theoretical bounds [0,1]
foreach var in intang_intensity_bs_rd knowledge_intensity_bs_rd {
    qui replace `var' = 0 if `var' < 0 & !missing(`var')
    qui replace `var' = 1 if `var' > 1 & !missing(`var')
}

*------------------------------------------------------------------------------
* B. Financial ratios (using BS+R&D total capital)
*------------------------------------------------------------------------------

gen leverage_bs_rd = total_debt / K_total_bs_rd if K_total_bs_rd > 0
label var leverage_bs_rd "BS+R&D: Leverage (Debt / K_total)"

*------------------------------------------------------------------------------
* C. Investment rates (using BS+R&D lagged capital)
*------------------------------------------------------------------------------

* Generate lagged capital for investment rate denominators
sort firm_id ano
by firm_id: gen K_total_bs_rd_lag = L.K_total_bs_rd

* Investment rates: I_t / K_{t-1}
gen inv_rate_physical_bs_rd = inv_physical / K_total_bs_rd_lag if K_total_bs_rd_lag > 0 & inv_physical >= 0
label var inv_rate_physical_bs_rd "BS+R&D: Physical investment rate (I_t / K_{t-1})"

cap confirm variable inv_rd
if _rc == 0 {
    gen inv_rate_rd_bs_rd = inv_rd / K_total_bs_rd_lag if K_total_bs_rd_lag > 0 & inv_rd >= 0
    label var inv_rate_rd_bs_rd "BS+R&D: R&D investment rate (I_t / K_{t-1})"
}

* For BS+R&D, total intangible investment excludes SG&A
gen inv_intang_bs_rd = 0
replace inv_intang_bs_rd = inv_rd if !missing(inv_rd)
replace inv_intang_bs_rd = inv_intang_bs_rd + inv_intang_bs if !missing(inv_intang_bs)
label var inv_intang_bs_rd "BS+R&D: Intangible investment (R&D + BS, NO SG&A)"

gen inv_rate_intang_bs_rd = inv_intang_bs_rd / K_total_bs_rd_lag if K_total_bs_rd_lag > 0 & inv_intang_bs_rd >= 0
label var inv_rate_intang_bs_rd "BS+R&D: Intangible investment rate (I_t / K_{t-1})"

*------------------------------------------------------------------------------
* D. Log transforms (BS+R&D measure)
*------------------------------------------------------------------------------

gen ln_K_total_bs_rd = ln(K_total_bs_rd) if K_total_bs_rd > 0
label var ln_K_total_bs_rd "BS+R&D: Log Total capital"

gen ln_K_intangible_bs_rd = ln(K_intangible_bs_rd) if K_intangible_bs_rd > 0
label var ln_K_intangible_bs_rd "BS+R&D: Log Intangible capital"

di "  ✓ BS+R&D measure variables constructed"
di ""

/*==============================================================================
   STEP 6: CREATE LEGACY VARIABLES (BACKWARD COMPATIBILITY)
==============================================================================*/

di "STEP 6: Creating legacy variables (backward compatibility)..."
di ""

* Map P&T Full to legacy variable names (no suffix)
* This ensures existing analysis code continues to work

* Capital intensities (map to P&T)
gen intang_intensity = intang_intensity_pt
label var intang_intensity "Intangible intensity (DEFAULT: P&T)"

gen knowledge_intensity = knowledge_intensity_pt
label var knowledge_intensity "Knowledge intensity (DEFAULT: P&T)"

gen org_intensity = org_intensity_pt
label var org_intensity "Organization intensity (DEFAULT: P&T)"

* Financial ratios (map to P&T)
gen leverage = leverage_pt
label var leverage "Leverage (DEFAULT: P&T)"

* Investment rates (map to P&T)
gen inv_rate_physical = inv_rate_physical_pt
label var inv_rate_physical "Physical investment rate (DEFAULT: P&T)"

cap confirm variable inv_rate_rd_pt
if _rc == 0 {
    gen inv_rate_rd = inv_rate_rd_pt
    label var inv_rate_rd "R&D investment rate (DEFAULT: P&T)"
}

gen inv_rate_sga = inv_rate_sga_pt
label var inv_rate_sga "SG&A investment rate (DEFAULT: P&T)"

gen inv_rate_intang = inv_rate_intang_pt
label var inv_rate_intang "Intangible investment rate (DEFAULT: P&T)"

* Log transforms (measure-independent except capital)
gen ln_emp = ln(n_workers) if n_workers > 0
label var ln_emp "Log employment"

gen ln_revenue = ln(revenue) if revenue > 0
label var ln_revenue "Log revenue"

gen ln_sales = ln(sales) if sales > 0
label var ln_sales "Log sales"

gen ln_K_total = ln_K_total_pt
label var ln_K_total "Log Total capital (DEFAULT: P&T)"

gen ln_K_physical = ln(K_physical) if K_physical > 0
label var ln_K_physical "Log Physical capital"

gen ln_K_intangible = ln_K_intangible_pt
label var ln_K_intangible "Log Intangible capital (DEFAULT: P&T)"

gen ln_production = ln(production) if production > 0
label var ln_production "Log Production"

gen ln_GVA = ln(GVA) if GVA > 0
label var ln_GVA "Log Gross value added"

cap confirm variable firm_age
if _rc == 0 {
    gen ln_age = ln(firm_age + 1) if firm_age >= 0
    label var ln_age "Log firm age (+1)"
}

* R&D and skill intensity measures (measure-independent)
cap confirm variable inv_rd
if _rc == 0 {
    gen rd_intensity = inv_rd / revenue if revenue > 0 
    label var rd_intensity "R&D intensity (R&D / Revenue)"
}

gen share_rd_workers = n_rd_workers / n_workers if n_workers > 0
label var share_rd_workers "Share of R&D workers"

* Truncate at [0,1]
qui replace share_rd_workers = 0 if share_rd_workers < 0 & !missing(share_rd_workers)
qui replace share_rd_workers = 1 if share_rd_workers > 1 & !missing(share_rd_workers)

di "  ✓ Legacy variables created (mapped to P&T Full)"
di ""

/*==============================================================================
   STEP 7: SAVE ANALYSIS-READY DATASET
==============================================================================*/

di "STEP 7: Saving analysis-ready dataset..."
di ""

compress
save "final_data_2011_2022_analysis.dta", replace

local n_final = _N
di "  ✓ Saved: final_data_2011_2022_analysis.dta"
di "     Observations: " %12.0fc `n_final'
di ""

/*==============================================================================
   COMPLETION MESSAGE
==============================================================================*/

di "=========================================="
di "ANALYSIS PREPARATION COMPLETE"
di "=========================================="
di ""
di "Output: final_data_2011_2022_analysis.dta"
di ""
di "══════════════════════════════════════════════════════════════════"
di "VARIABLE NAMING CONVENTION"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Suffix System:"
di "  _pt        → Peters & Taylor Full measure"
di "  _bs_rd     → Balance Sheet + R&D measure"
di "  (no suffix)→ Legacy/default (maps to P&T Full)"
di ""
di "══════════════════════════════════════════════════════════════════"
di "PETERS & TAYLOR (P&T) FULL MEASURE VARIABLES"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Capital Stocks:"
di "  • K_intangible_pt        Total intangible (K + Org + BS)"
di "  • K_intangible_pt_int    Internal only (K + Org)"
di "  • K_total_pt             Physical + Intangible"
di ""
di "Intensities:"
di "  • intang_intensity_pt    K_intangible / K_total"
di "  • knowledge_intensity_pt K_knowledge / K_intangible"
di "  • org_intensity_pt       K_org / K_intangible"
di ""
di "Financial:"
di "  • leverage_pt            Debt / K_total"
di ""
di "Investment Rates (I_t / K_{t-1}):"
di "  • inv_rate_physical_pt   Physical investment rate"
di "  • inv_rate_rd_pt         R&D investment rate"
di "  • inv_rate_sga_pt        SG&A investment rate"
di "  • inv_rate_intang_pt     Total intangible investment rate"
di ""
di "Logs:"
di "  • ln_K_total_pt          Log total capital"
di "  • ln_K_intangible_pt     Log intangible capital"
di ""
di "══════════════════════════════════════════════════════════════════"
di "BALANCE SHEET + R&D (BS+R&D) MEASURE VARIABLES"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Capital Stocks:"
di "  • K_intangible_bs_rd     Total intangible (K + BS, NO Org)"
di "  • K_total_bs_rd          Physical + Intangible"
di ""
di "Intensities:"
di "  • intang_intensity_bs_rd K_intangible / K_total"
di "  • knowledge_intensity_bs_rd K_knowledge / K_intangible"
di ""
di "Financial:"
di "  • leverage_bs_rd         Debt / K_total"
di ""
di "Investment Rates (I_t / K_{t-1}):"
di "  • inv_rate_physical_bs_rd Physical investment rate"
di "  • inv_rate_rd_bs_rd      R&D investment rate"
di "  • inv_rate_intang_bs_rd  Intangible investment rate (NO SG&A)"
di ""
di "Logs:"
di "  • ln_K_total_bs_rd       Log total capital"
di "  • ln_K_intangible_bs_rd  Log intangible capital"
di ""
di "══════════════════════════════════════════════════════════════════"
di "MEASURE-INDEPENDENT VARIABLES"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Financial (constant across measures):"
di "  • leverage_assets        Debt / Assets"
di "  • interest_rate          Implicit interest rate (%)"
di "  • credit_spread          Spread over risk-free rate (pp)"
di "  • rfr                    Risk-free rate"
di ""
di "Productivity:"
di "  • labor_productivity_rev Revenue / worker"
di "  • labor_productivity_sales Sales / worker"
di "  • labor_productivity_GVA GVA / worker"
di ""
di "Skills:"
di "  • share_skilled          Share tertiary educated workers"
di "  • share_rd_workers       Share R&D workers"
di "  • rd_intensity           R&D / Revenue"
di ""
di "Logs (measure-independent):"
di "  • ln_emp                 Log employment"
di "  • ln_revenue             Log revenue"
di "  • ln_sales               Log sales"
di "  • ln_production          Log production"
di "  • ln_GVA                 Log GVA"
di "  • ln_age                 Log firm age (+1)"
di "  • ln_K_physical          Log physical capital"
di ""
di "══════════════════════════════════════════════════════════════════"
di "METHODOLOGICAL NOTES"
di "══════════════════════════════════════════════════════════════════"
di ""
di "Interest Rates:"
di "  • Interest rate: interest_expense / lagged_debt"
di "  • Credit spread: Firm rate - Portuguese 10Y bonds"
di ""
di "Investment Rates:"
di "  • All use I_t / K_{t-1} formulation"
di "  • K_{t-1} differs by measure (K_total_pt_lag vs K_total_bs_rd_lag)"
di ""
di "Legacy Variables:"
di "  • Variables without suffix map to P&T Full"
di "  • Ensures backward compatibility with existing code"
di ""
di "══════════════════════════════════════════════════════════════════"

cap log close
