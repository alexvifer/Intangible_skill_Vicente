/*==============================================================================
   MOTIVATING EVIDENCE: Financial Frictions, Intangibles, and Skill Complementarity
   
   This do-file generates the empirical motivation for the theoretical model.
   
   KEY PATTERNS TO DOCUMENT:
   1. Complementarity between intangibles and skilled labor
   2. Pecking-order distortion: constrained firms tilt toward tangibles
   3. Underexploitation: constraints weaken the skill-intangible relationship
   
   CONSTRAINT PROXIES:
   - Leverage ratio (debt / assets)
   - Credit spread (implicit interest rate)
   - Firm age (younger = more constrained)
   - Firm size (smaller = more constrained)
   
   INPUT:  final_data_2011_2022_cleaned.dta
   OUTPUT: Figures (.png) and Tables (.tex) in $rootdir/output/figures/
   
==============================================================================*/
cls
clear all
set more off
set scheme s2color

/*==============================================================================
   CONFIGURATION
==============================================================================*/

* SET YOUR ROOT DIRECTORY HERE (same as do_all.do)
global rootdir "C:\Users\alexv\OneDrive\Documentos\3-Papers\Intangible_skill_Vicente\QP_SCIE"

* Create output directories
cap mkdir "$rootdir/output/figures"
cap mkdir "$rootdir/output/tables"

cd "$rootdir/output"

* Start log
cap log close
log using "motivating_evidence.log", replace

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║           MOTIVATING EVIDENCE FOR THEORETICAL MODEL            ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""
di "Date: `c(current_date)'"
di "Time: `c(current_time)'"
di ""

/*==============================================================================
   STEP 1: LOAD DATA AND CONSTRUCT VARIABLES
==============================================================================*/

di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "STEP 1: Loading data and constructing analysis variables"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""

use "final_data_2011_2022_cleaned.dta", clear

di "  Observations loaded: " %12.0fc _N
di ""

*------------------------------------------------------------------------------
* A. KEY OUTCOME VARIABLES
*------------------------------------------------------------------------------

di "  A. Creating key outcome variables..."

* Intangible intensity (share of intangibles in total capital)
gen intang_intensity = K_intangible_total / K_total if K_total > 0
label var intang_intensity "Intangible capital / Total capital"

* Intangible-to-tangible ratio (capital composition)
gen intang_tang_ratio = K_intangible_total / K_physical if K_physical > 0
label var intang_tang_ratio "Intangible capital / Physical capital"

* Log of intangible-to-tangible ratio (for regressions)
gen ln_intang_tang = ln(intang_tang_ratio) if intang_tang_ratio > 0
label var ln_intang_tang "Log(Intangible / Physical capital)"

* Knowledge capital intensity
gen knowledge_intensity = K_knowledge / K_total if K_total > 0
label var knowledge_intensity "Knowledge capital / Total capital"

* Organization capital intensity
gen org_intensity = K_org / K_total if K_total > 0
label var org_intensity "Organization capital / Total capital"

* Internal intangible intensity (excluding balance sheet)
gen internal_intang_intensity = K_intangible_internal / K_total if K_total > 0
label var internal_intang_intensity "Internal intangibles / Total capital"

*------------------------------------------------------------------------------
* B. FINANCIAL CONSTRAINT PROXIES
*------------------------------------------------------------------------------

di "  B. Constructing financial constraint proxies..."

* --- 1. LEVERAGE RATIO ---
* Total debt / Total assets
gen leverage = SV516001_real / SV512701_real if SV512701_real > 0
label var leverage "Leverage ratio (Total debt / Total assets)"

* Winsorize leverage at 1% and 99%
_pctile leverage, p(1 99)
gen leverage_w = leverage
replace leverage_w = r(r1) if leverage < r(r1) & leverage != .
replace leverage_w = r(r2) if leverage > r(r2) & leverage != .
label var leverage_w "Leverage ratio (winsorized 1-99%)"

* --- 2. CREDIT SPREAD (Implicit interest rate) ---
* Interest expenses / Total financing obtained
* First, create total financing = LT debt (SV514301) + ST debt (SV515201)
cap confirm variable SV514301_real
if _rc == 0 {
    gen total_financing = SV514301_real + SV515201_real
}
else {
    * Use total liabilities if detailed breakdown unavailable
    gen total_financing = SV516001_real
}
replace total_financing = . if total_financing <= 0

* Implicit interest rate
gen implicit_rate = SV502201_real / total_financing if total_financing > 0
label var implicit_rate "Implicit interest rate (Interest exp / Debt)"

* Winsorize at 1% and 99%
_pctile implicit_rate, p(1 99)
gen implicit_rate_w = implicit_rate
replace implicit_rate_w = r(r1) if implicit_rate < r(r1) & implicit_rate != .
replace implicit_rate_w = r(r2) if implicit_rate > r(r2) & implicit_rate != .
label var implicit_rate_w "Implicit interest rate (winsorized 1-99%)"

* Credit spread = implicit rate - risk-free rate
* Approximate risk-free rate for Portugal (average ~2% over period)
gen credit_spread = implicit_rate_w - 0.02
replace credit_spread = 0 if credit_spread < 0
label var credit_spread "Credit spread (implicit rate - risk-free)"

* --- 3. FIRM AGE ---
* Already have firm_age from firm_clean.do
cap confirm variable firm_age
if _rc == 0 {
    label var firm_age "Firm age (years)"
}
else {
    di "  WARNING: firm_age not found"
    gen firm_age = .
}

* --- 4. FIRM SIZE ---
* Log of total assets
gen ln_assets = ln(SV512701_real) if SV512701_real > 0
label var ln_assets "Log(Total assets)"

* Log of revenue (alternative)
gen ln_revenue = ln(SV500101_real) if SV500101_real > 0
label var ln_revenue "Log(Revenue)"

* Log of employment (alternative)
gen ln_emp = ln(n_workers) if n_workers > 0
label var ln_emp "Log(Employment)"

*------------------------------------------------------------------------------
* C. CREATE CONSTRAINT INDICATORS (BINARY)
*------------------------------------------------------------------------------

di "  C. Creating constraint indicator variables..."

* Calculate medians for splitting sample
qui sum leverage_w, d
local med_leverage = r(p50)

qui sum credit_spread, d
local med_spread = r(p50)

qui sum firm_age, d
local med_age = r(p50)

qui sum ln_assets, d
local med_size = r(p50)

* Binary constraint indicators (1 = constrained)
gen constrained_leverage = (leverage_w > `med_leverage') if leverage_w != .
gen constrained_spread = (credit_spread > `med_spread') if credit_spread != .
gen constrained_age = (firm_age < `med_age') if firm_age != .
gen constrained_size = (ln_assets < `med_size') if ln_assets != .

label var constrained_leverage "High leverage (above median)"
label var constrained_spread "High credit spread (above median)"
label var constrained_age "Young firm (below median age)"
label var constrained_size "Small firm (below median assets)"

* Composite constraint index (sum of indicators)
egen n_constraints = rowtotal(constrained_leverage constrained_spread ///
                               constrained_age constrained_size)
gen constrained_composite = (n_constraints >= 2) if n_constraints != .
label var constrained_composite "Constrained (2+ indicators)"
label var n_constraints "Number of constraint indicators (0-4)"

*------------------------------------------------------------------------------
* D. SECTOR FIXED EFFECTS
*------------------------------------------------------------------------------

di "  D. Preparing fixed effects..."

* Create 2-digit sector code if not already present
cap confirm variable cae2
if _rc != 0 {
    cap confirm variable caeies
    if _rc == 0 {
        gen cae2 = floor(caeies / 100)
    }
    else {
        cap confirm variable cae3
        if _rc == 0 {
            gen cae2 = cae3
        }
        else {
            di "  WARNING: No sector variable found"
            gen cae2 = .
        }
    }
}
label var cae2 "2-digit sector code"

* Encode sector for fixed effects
cap encode cae2, gen(sector_fe)
if _rc != 0 {
    egen sector_fe = group(cae2)
}

*------------------------------------------------------------------------------
* E. ANALYSIS SAMPLE RESTRICTIONS
*------------------------------------------------------------------------------

di "  E. Defining analysis sample..."

* Mark observations with all key variables non-missing
gen analysis_sample = 1
replace analysis_sample = 0 if intang_intensity == .
replace analysis_sample = 0 if share_skilled == .
replace analysis_sample = 0 if leverage_w == .
replace analysis_sample = 0 if ln_assets == .
replace analysis_sample = 0 if sector_fe == .

count if analysis_sample == 1
local n_analysis = r(N)
di "  Analysis sample: " %12.0fc `n_analysis' " observations"
di ""

*------------------------------------------------------------------------------
* F. SUMMARY STATISTICS
*------------------------------------------------------------------------------

di "  F. Summary statistics for key variables..."
di ""

preserve
keep if analysis_sample == 1

di "  Key Variables - Analysis Sample:"
di "  ─────────────────────────────────────────────────────────────────"

sum intang_intensity share_skilled leverage_w credit_spread firm_age ln_assets, d

di ""
di "  Constraint Indicators:"
di "  ─────────────────────────────────────────────────────────────────"
tab constrained_composite

restore

compress
save "analysis_data.dta", replace

di ""
di "  → Analysis dataset saved: analysis_data.dta"
di ""

/*==============================================================================
   STEP 2: STYLIZED FACT 1 - COMPLEMENTARITY
==============================================================================*/

di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "STEP 2: Stylized Fact 1 - Complementarity (Skills & Intangibles)"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""

use "analysis_data.dta", clear
keep if analysis_sample == 1

*------------------------------------------------------------------------------
* FIGURE 1: Binned Scatter - Intangible Intensity vs Skill Share
*------------------------------------------------------------------------------

di "  Creating Figure 1: Intangible Intensity vs Skill Share..."

* Residualize variables (control for year and sector FE)
qui reghdfe intang_intensity, absorb(ano sector_fe) resid(intang_resid)
qui reghdfe share_skilled, absorb(ano sector_fe) resid(skill_resid)

* Create binned scatter
* Using 20 bins for clean visualization
xtile skill_bin = skill_resid, nq(20)
bysort skill_bin: egen mean_intang = mean(intang_resid)
bysort skill_bin: egen mean_skill = mean(skill_resid)

* Plot
preserve
duplicates drop skill_bin, force

twoway (scatter mean_intang mean_skill, mcolor(navy) msize(medium)) ///
       (lfit mean_intang mean_skill, lcolor(cranberry) lwidth(medthick)), ///
       ytitle("Intangible Intensity (residualized)") ///
       xtitle("Share Skilled Workers (residualized)") ///
       title("Complementarity: Intangibles and Skills", size(medium)) ///
       subtitle("Controlling for year and sector fixed effects", size(small)) ///
       legend(off) ///
       note("Note: Each point represents a vingtile of the skill distribution." ///
            "Variables residualized on year and 2-digit sector fixed effects.", size(vsmall))

graph export "figures/fig1_complementarity_binscatter.png", replace width(2000)
graph export "figures/fig1_complementarity_binscatter.pdf", replace

restore

di "  → Figure 1 saved: fig1_complementarity_binscatter.png"

*------------------------------------------------------------------------------
* TABLE 1: Regression - Complementarity
*------------------------------------------------------------------------------

di "  Creating Table 1: Complementarity Regressions..."

* Column 1: No controls
qui reg intang_intensity share_skilled, robust
estadd local yearFE "No"
estadd local sectorFE "No"
estadd local firmFE "No"
estimates store comp_1

* Column 2: Year FE
qui reghdfe intang_intensity share_skilled, absorb(ano) vce(cluster firm_id)
estadd local yearFE "Yes"
estadd local sectorFE "No"
estadd local firmFE "No"
estimates store comp_2

* Column 3: Year + Sector FE
qui reghdfe intang_intensity share_skilled, absorb(ano sector_fe) vce(cluster firm_id)
estadd local yearFE "Yes"
estadd local sectorFE "Yes"
estadd local firmFE "No"
estimates store comp_3

* Column 4: Year + Sector + Size control
qui reghdfe intang_intensity share_skilled ln_assets, absorb(ano sector_fe) vce(cluster firm_id)
estadd local yearFE "Yes"
estadd local sectorFE "Yes"
estadd local firmFE "No"
estimates store comp_4

* Column 5: Firm FE (within-firm variation)
qui reghdfe intang_intensity share_skilled, absorb(firm_id ano) vce(cluster firm_id)
estadd local yearFE "Yes"
estadd local sectorFE "No"
estadd local firmFE "Yes"
estimates store comp_5

* Export table
esttab comp_1 comp_2 comp_3 comp_4 comp_5 using "tables/tab1_complementarity.tex", ///
    replace booktabs ///
    title("Complementarity between Intangible Capital and Skilled Labor") ///
    mtitles("(1)" "(2)" "(3)" "(4)" "(5)") ///
    keep(share_skilled ln_assets) ///
    label ///
    b(%9.4f) se(%9.4f) ///
    stats(N r2_a yearFE sectorFE firmFE, ///
          labels("Observations" "Adjusted R\$^2\$" "Year FE" "Sector FE" "Firm FE") ///
          fmt(%12.0fc %9.3f)) ///
    addnotes("Standard errors clustered at the firm level in parentheses." ///
             "Dependent variable: Intangible capital / Total capital.")

di "  → Table 1 saved: tab1_complementarity.tex"
di ""

/*==============================================================================
   STEP 3: STYLIZED FACT 2 - PECKING ORDER DISTORTION
==============================================================================*/

di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "STEP 3: Stylized Fact 2 - Pecking Order (Constraints & Composition)"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""

use "analysis_data.dta", clear
keep if analysis_sample == 1

*------------------------------------------------------------------------------
* FIGURE 2: Binned Scatter - Capital Composition vs Leverage
*------------------------------------------------------------------------------

di "  Creating Figure 2: Capital Composition vs Leverage..."

* Residualize
qui reghdfe intang_intensity, absorb(ano sector_fe) resid(intang_resid)
qui reghdfe leverage_w, absorb(ano sector_fe) resid(lev_resid)

* Create bins
xtile lev_bin = lev_resid, nq(20)
bysort lev_bin: egen mean_intang = mean(intang_resid)
bysort lev_bin: egen mean_lev = mean(lev_resid)

preserve
duplicates drop lev_bin, force

twoway (scatter mean_intang mean_lev, mcolor(navy) msize(medium)) ///
       (lfit mean_intang mean_lev, lcolor(cranberry) lwidth(medthick)), ///
       ytitle("Intangible Intensity (residualized)") ///
       xtitle("Leverage Ratio (residualized)") ///
       title("Pecking Order: Leverage and Capital Composition", size(medium)) ///
       subtitle("Controlling for year and sector fixed effects", size(small)) ///
       legend(off) ///
       note("Note: Each point represents a vingtile of the leverage distribution." ///
            "Higher leverage associated with lower intangible intensity.", size(vsmall))

graph export "figures/fig2_pecking_order_binscatter.png", replace width(2000)
graph export "figures/fig2_pecking_order_binscatter.pdf", replace

restore

di "  → Figure 2 saved: fig2_pecking_order_binscatter.png"

*------------------------------------------------------------------------------
* TABLE 2: Regression - Pecking Order (All Constraint Measures)
*------------------------------------------------------------------------------

di "  Creating Table 2: Pecking Order Regressions..."

* Column 1: Leverage
qui reghdfe intang_intensity leverage_w ln_assets share_skilled, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store po_lev

* Column 2: Credit spread
qui reghdfe intang_intensity credit_spread ln_assets share_skilled, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store po_spread

* Column 3: Firm age
qui reghdfe intang_intensity firm_age ln_assets share_skilled, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store po_age

* Column 4: Firm size (drop ln_assets from RHS)
qui reghdfe intang_intensity ln_assets share_skilled, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store po_size

* Column 5: All together
qui reghdfe intang_intensity leverage_w credit_spread firm_age ln_assets share_skilled, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store po_all

* Export table
esttab po_lev po_spread po_age po_size po_all using "tables/tab2_pecking_order.tex", ///
    replace booktabs ///
    title("Pecking Order: Financial Constraints and Capital Composition") ///
    mtitles("Leverage" "Spread" "Age" "Size" "All") ///
    keep(leverage_w credit_spread firm_age ln_assets share_skilled) ///
    order(leverage_w credit_spread firm_age ln_assets share_skilled) ///
    label ///
    b(%9.4f) se(%9.4f) ///
    stats(N r2_a, labels("Observations" "Adjusted R$^2$") fmt(%12.0fc %9.3f)) ///
    addnotes("All regressions include year and sector fixed effects." ///
             "Standard errors clustered at the firm level." ///
             "Dependent variable: Intangible capital / Total capital.")

di "  → Table 2 saved: tab2_pecking_order.tex"
di ""

/*==============================================================================
   STEP 4: KEY RESULT - UNDEREXPLOITATION OF COMPLEMENTARITY
==============================================================================*/

di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "STEP 4: Key Result - Underexploitation of Complementarity"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""

use "analysis_data.dta", clear
keep if analysis_sample == 1

*------------------------------------------------------------------------------
* FIGURE 3: Split Binscatter by Constraint Status (MAIN FIGURE)
*------------------------------------------------------------------------------

di "  Creating Figure 3: Split Binscatter (Main Result)..."

* --- LEVERAGE ---

* Residualize within each group
qui reghdfe intang_intensity if constrained_leverage == 0, absorb(ano sector_fe) resid(intang_resid_unc)
qui reghdfe share_skilled if constrained_leverage == 0, absorb(ano sector_fe) resid(skill_resid_unc)
qui reghdfe intang_intensity if constrained_leverage == 1, absorb(ano sector_fe) resid(intang_resid_con)
qui reghdfe share_skilled if constrained_leverage == 1, absorb(ano sector_fe) resid(skill_resid_con)

* Create bins for unconstrained
xtile skill_bin_unc = skill_resid_unc if constrained_leverage == 0, nq(15)
bysort skill_bin_unc: egen mean_intang_unc = mean(intang_resid_unc) if constrained_leverage == 0
bysort skill_bin_unc: egen mean_skill_unc = mean(skill_resid_unc) if constrained_leverage == 0

* Create bins for constrained
xtile skill_bin_con = skill_resid_con if constrained_leverage == 1, nq(15)
bysort skill_bin_con: egen mean_intang_con = mean(intang_resid_con) if constrained_leverage == 1
bysort skill_bin_con: egen mean_skill_con = mean(skill_resid_con) if constrained_leverage == 1

preserve

* Keep unique bins
keep if (skill_bin_unc != . & constrained_leverage == 0) | ///
        (skill_bin_con != . & constrained_leverage == 1)

* Collapse
gen bin = skill_bin_unc if constrained_leverage == 0
replace bin = skill_bin_con if constrained_leverage == 1
gen mean_intang = mean_intang_unc if constrained_leverage == 0
replace mean_intang = mean_intang_con if constrained_leverage == 1
gen mean_skill = mean_skill_unc if constrained_leverage == 0
replace mean_skill = mean_skill_con if constrained_leverage == 1

bysort constrained_leverage bin: keep if _n == 1

twoway (scatter mean_intang mean_skill if constrained_leverage == 0, ///
            mcolor(navy) msize(medium) msymbol(circle)) ///
       (lfit mean_intang mean_skill if constrained_leverage == 0, ///
            lcolor(navy) lwidth(medthick) lpattern(solid)) ///
       (scatter mean_intang mean_skill if constrained_leverage == 1, ///
            mcolor(cranberry) msize(medium) msymbol(triangle)) ///
       (lfit mean_intang mean_skill if constrained_leverage == 1, ///
            lcolor(cranberry) lwidth(medthick) lpattern(dash)), ///
       ytitle("Intangible Intensity (residualized)") ///
       xtitle("Share Skilled Workers (residualized)") ///
       title("Underexploitation: Constraints Weaken Skill-Intangible Link", size(medium)) ///
       subtitle("Split by leverage (above/below median)", size(small)) ///
       legend(order(1 "Low leverage (unconstrained)" 3 "High leverage (constrained)") ///
              rows(1) position(6) size(small)) ///
       note("Note: Constrained firms show flatter slope, indicating inability to" ///
            "convert skilled labor into intangible investment.", size(vsmall))

graph export "figures/fig3_underexploitation_leverage.png", replace width(2000)
graph export "figures/fig3_underexploitation_leverage.pdf", replace

restore

di "  → Figure 3 saved: fig3_underexploitation_leverage.png"

*------------------------------------------------------------------------------
* FIGURE 3B-D: Alternative constraint measures
*------------------------------------------------------------------------------

di "  Creating Figures 3b-3d: Alternative constraint measures..."

foreach constraint in size age spread {
    
    if "`constraint'" == "size" local constvar "constrained_size"
    if "`constraint'" == "age" local constvar "constrained_age"  
    if "`constraint'" == "spread" local constvar "constrained_spread"
    
    if "`constraint'" == "size" local subtitle "Split by size (above/below median assets)"
    if "`constraint'" == "age" local subtitle "Split by age (above/below median)"
    if "`constraint'" == "spread" local subtitle "Split by credit spread (above/below median)"
    
    if "`constraint'" == "size" local leg1 "Large firms (unconstrained)"
    if "`constraint'" == "size" local leg2 "Small firms (constrained)"
    if "`constraint'" == "age" local leg1 "Old firms (unconstrained)"
    if "`constraint'" == "age" local leg2 "Young firms (constrained)"
    if "`constraint'" == "spread" local leg1 "Low spread (unconstrained)"
    if "`constraint'" == "spread" local leg2 "High spread (constrained)"
    
    preserve
    
    * Drop previous residuals
    cap drop *_resid* mean_* skill_bin*
    
    * Residualize
    qui reghdfe intang_intensity if `constvar' == 0, absorb(ano sector_fe) resid(intang_resid_unc)
    qui reghdfe share_skilled if `constvar' == 0, absorb(ano sector_fe) resid(skill_resid_unc)
    qui reghdfe intang_intensity if `constvar' == 1, absorb(ano sector_fe) resid(intang_resid_con)
    qui reghdfe share_skilled if `constvar' == 1, absorb(ano sector_fe) resid(skill_resid_con)
    
    * Create bins
    xtile skill_bin_unc = skill_resid_unc if `constvar' == 0, nq(15)
    bysort skill_bin_unc: egen mean_intang_unc = mean(intang_resid_unc) if `constvar' == 0
    bysort skill_bin_unc: egen mean_skill_unc = mean(skill_resid_unc) if `constvar' == 0
    
    xtile skill_bin_con = skill_resid_con if `constvar' == 1, nq(15)
    bysort skill_bin_con: egen mean_intang_con = mean(intang_resid_con) if `constvar' == 1
    bysort skill_bin_con: egen mean_skill_con = mean(skill_resid_con) if `constvar' == 1
    
    * Keep unique bins
    keep if (skill_bin_unc != . & `constvar' == 0) | (skill_bin_con != . & `constvar' == 1)
    
    gen bin = skill_bin_unc if `constvar' == 0
    replace bin = skill_bin_con if `constvar' == 1
    gen mean_intang = mean_intang_unc if `constvar' == 0
    replace mean_intang = mean_intang_con if `constvar' == 1
    gen mean_skill = mean_skill_unc if `constvar' == 0
    replace mean_skill = mean_skill_con if `constvar' == 1
    
    bysort `constvar' bin: keep if _n == 1
    
    twoway (scatter mean_intang mean_skill if `constvar' == 0, ///
                mcolor(navy) msize(medium) msymbol(circle)) ///
           (lfit mean_intang mean_skill if `constvar' == 0, ///
                lcolor(navy) lwidth(medthick) lpattern(solid)) ///
           (scatter mean_intang mean_skill if `constvar' == 1, ///
                mcolor(cranberry) msize(medium) msymbol(triangle)) ///
           (lfit mean_intang mean_skill if `constvar' == 1, ///
                lcolor(cranberry) lwidth(medthick) lpattern(dash)), ///
           ytitle("Intangible Intensity (residualized)") ///
           xtitle("Share Skilled Workers (residualized)") ///
           title("Underexploitation: Constraints Weaken Skill-Intangible Link", size(medium)) ///
           subtitle("`subtitle'", size(small)) ///
           legend(order(1 "`leg1'" 3 "`leg2'") rows(1) position(6) size(small)) ///
           note("Note: Constrained firms show flatter slope.", size(vsmall))
    
    graph export "figures/fig3_underexploitation_`constraint'.png", replace width(2000)
    graph export "figures/fig3_underexploitation_`constraint'.pdf", replace
    
    restore
    
    di "  → Figure 3 (`constraint') saved"
}

*------------------------------------------------------------------------------
* TABLE 3: Interaction Regressions (MAIN TABLE)
*------------------------------------------------------------------------------

di ""
di "  Creating Table 3: Interaction Regressions (Main Result)..."

use "analysis_data.dta", clear
keep if analysis_sample == 1

* Create interaction terms
gen skill_x_lev = share_skilled * constrained_leverage
gen skill_x_spread = share_skilled * constrained_spread
gen skill_x_age = share_skilled * constrained_age
gen skill_x_size = share_skilled * constrained_size
gen skill_x_composite = share_skilled * constrained_composite

label var skill_x_lev "Skilled × High Leverage"
label var skill_x_spread "Skilled × High Spread"
label var skill_x_age "Skilled × Young"
label var skill_x_size "Skilled × Small"
label var skill_x_composite "Skilled × Constrained"

* Column 1: Leverage interaction
qui reghdfe intang_intensity share_skilled constrained_leverage skill_x_lev ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store int_lev

* Column 2: Credit spread interaction
qui reghdfe intang_intensity share_skilled constrained_spread skill_x_spread ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store int_spread

* Column 3: Age interaction
qui reghdfe intang_intensity share_skilled constrained_age skill_x_age ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store int_age

* Column 4: Size interaction
qui reghdfe intang_intensity share_skilled constrained_size skill_x_size, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store int_size

* Column 5: Composite constraint
qui reghdfe intang_intensity share_skilled constrained_composite skill_x_composite ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store int_composite

* Export main table
esttab int_lev int_spread int_age int_size int_composite ///
    using "tables/tab3_underexploitation.tex", ///
    replace booktabs ///
    title("Underexploitation: Constraints Weaken Skill-Intangible Complementarity") ///
    mtitles("Leverage" "Spread" "Age" "Size" "Composite") ///
    keep(share_skilled constrained_leverage skill_x_lev ///
         constrained_spread skill_x_spread ///
         constrained_age skill_x_age ///
         constrained_size skill_x_size ///
         constrained_composite skill_x_composite ln_assets) ///
    order(share_skilled constrained_* skill_x_* ln_assets) ///
    label ///
    b(%9.4f) se(%9.4f) ///
    stats(N r2_a, labels("Observations" "Adjusted R$^2$") fmt(%12.0fc %9.3f)) ///
    addnotes("All regressions include year and sector fixed effects." ///
             "Standard errors clustered at the firm level." ///
             "Dependent variable: Intangible capital / Total capital." ///
             "Negative interaction indicates constrained firms cannot convert" ///
             "skilled labor into intangible investment (underexploitation).")

di "  → Table 3 saved: tab3_underexploitation.tex"

*------------------------------------------------------------------------------
* TABLE 3B: Firm Fixed Effects Specification
*------------------------------------------------------------------------------

di "  Creating Table 3b: Firm Fixed Effects Specification..."

* With firm FE (within-firm variation)
qui reghdfe intang_intensity share_skilled constrained_leverage skill_x_lev, ///
    absorb(firm_id ano) vce(cluster firm_id)
estimates store fe_lev

qui reghdfe intang_intensity share_skilled constrained_spread skill_x_spread, ///
    absorb(firm_id ano) vce(cluster firm_id)
estimates store fe_spread

qui reghdfe intang_intensity share_skilled constrained_age skill_x_age, ///
    absorb(firm_id ano) vce(cluster firm_id)
estimates store fe_age

qui reghdfe intang_intensity share_skilled constrained_size skill_x_size, ///
    absorb(firm_id ano) vce(cluster firm_id)
estimates store fe_size

qui reghdfe intang_intensity share_skilled constrained_composite skill_x_composite, ///
    absorb(firm_id ano) vce(cluster firm_id)
estimates store fe_composite

esttab fe_lev fe_spread fe_age fe_size fe_composite ///
    using "tables/tab3b_underexploitation_firmFE.tex", ///
    replace booktabs ///
    title("Underexploitation with Firm Fixed Effects") ///
    mtitles("Leverage" "Spread" "Age" "Size" "Composite") ///
    keep(share_skilled constrained_leverage skill_x_lev ///
         constrained_spread skill_x_spread ///
         constrained_age skill_x_age ///
         constrained_size skill_x_size ///
         constrained_composite skill_x_composite) ///
    order(share_skilled constrained_* skill_x_*) ///
    label ///
    b(%9.4f) se(%9.4f) ///
    stats(N r2_a, labels("Observations" "Adjusted R$^2$") fmt(%12.0fc %9.3f)) ///
    addnotes("All regressions include firm and year fixed effects." ///
             "Standard errors clustered at the firm level." ///
             "Exploits within-firm variation over time.")

di "  → Table 3b saved: tab3b_underexploitation_firmFE.tex"
di ""

/*==============================================================================
   STEP 5: ADDITIONAL RESULTS - DECOMPOSITION BY INTANGIBLE TYPE
==============================================================================*/

di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "STEP 5: Decomposition by Intangible Type"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""

use "analysis_data.dta", clear
keep if analysis_sample == 1

* Recreate interactions
gen skill_x_lev = share_skilled * constrained_leverage
gen skill_x_composite = share_skilled * constrained_composite

*------------------------------------------------------------------------------
* TABLE 4: Decomposition - Knowledge vs Organization Capital
*------------------------------------------------------------------------------

di "  Creating Table 4: Decomposition by Intangible Type..."

* Knowledge capital
qui reghdfe knowledge_intensity share_skilled constrained_leverage skill_x_lev ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store decomp_know_lev

qui reghdfe knowledge_intensity share_skilled constrained_composite skill_x_composite ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store decomp_know_comp

* Organization capital
qui reghdfe org_intensity share_skilled constrained_leverage skill_x_lev ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store decomp_org_lev

qui reghdfe org_intensity share_skilled constrained_composite skill_x_composite ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store decomp_org_comp

* Internal intangibles (total)
qui reghdfe internal_intang_intensity share_skilled constrained_leverage skill_x_lev ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store decomp_int_lev

qui reghdfe internal_intang_intensity share_skilled constrained_composite skill_x_composite ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store decomp_int_comp

esttab decomp_know_lev decomp_know_comp decomp_org_lev decomp_org_comp ///
       decomp_int_lev decomp_int_comp ///
    using "tables/tab4_decomposition.tex", ///
    replace booktabs ///
    title("Decomposition: Knowledge vs Organization Capital") ///
    mgroups("Knowledge Capital" "Organization Capital" "Internal Intangibles", ///
            pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span) ///
    mtitles("Leverage" "Composite" "Leverage" "Composite" "Leverage" "Composite") ///
    keep(share_skilled constrained_leverage skill_x_lev ///
         constrained_composite skill_x_composite ln_assets) ///
    order(share_skilled constrained_* skill_x_* ln_assets) ///
    label ///
    b(%9.4f) se(%9.4f) ///
    stats(N r2_a, labels("Observations" "Adjusted R$^2$") fmt(%12.0fc %9.3f)) ///
    addnotes("All regressions include year and sector fixed effects." ///
             "Knowledge capital from R\&D; Organization capital from SG\&A.")

di "  → Table 4 saved: tab4_decomposition.tex"
di ""

/*==============================================================================
   STEP 6: SUMMARY STATISTICS TABLE
==============================================================================*/

di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "STEP 6: Summary Statistics"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""

use "analysis_data.dta", clear
keep if analysis_sample == 1

* Summary statistics for paper
estpost summarize intang_intensity knowledge_intensity org_intensity ///
    share_skilled n_workers K_total K_intangible_total K_physical ///
    leverage_w credit_spread firm_age ln_assets, d

esttab using "tables/tab0_summary_stats.tex", ///
    replace booktabs ///
    title("Summary Statistics") ///
    cells("mean(fmt(%9.3f)) sd(fmt(%9.3f)) p25(fmt(%9.3f)) p50(fmt(%9.3f)) p75(fmt(%9.3f)) count(fmt(%12.0fc))") ///
    noobs nonumber nomtitle ///
    collabels("Mean" "SD" "P25" "Median" "P75" "N") ///
    addnotes("Sample: Firm-year observations with non-missing key variables, 2011-2022." ///
             "Capital variables in real 2020 prices (thousands of euros).")

di "  → Summary statistics saved: tab0_summary_stats.tex"

*------------------------------------------------------------------------------
* Summary statistics by constraint status
*------------------------------------------------------------------------------

di "  Creating comparison by constraint status..."

* Means by constraint status
estpost tabstat intang_intensity knowledge_intensity org_intensity ///
    share_skilled n_workers K_total leverage_w, ///
    by(constrained_composite) statistics(mean sd n) columns(statistics)

esttab using "tables/tab0b_summary_by_constraint.tex", ///
    replace booktabs ///
    title("Summary Statistics by Constraint Status") ///
    main(mean %9.3f) aux(sd %9.3f) ///
    nostar noobs nonumber nomtitle ///
    addnotes("Constrained = firms with 2+ constraint indicators." ///
             "Standard deviations in parentheses.")

di "  → Summary by constraint saved: tab0b_summary_by_constraint.tex"
di ""

/*==============================================================================
   STEP 7: ROBUSTNESS - CONTINUOUS MEASURES
==============================================================================*/

di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "STEP 7: Robustness - Continuous Constraint Measures"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""

use "analysis_data.dta", clear
keep if analysis_sample == 1

* Create continuous interactions
gen skill_x_lev_cont = share_skilled * leverage_w
gen skill_x_spread_cont = share_skilled * credit_spread
gen skill_x_age_cont = share_skilled * firm_age
gen skill_x_size_cont = share_skilled * ln_assets

label var skill_x_lev_cont "Skilled × Leverage"
label var skill_x_spread_cont "Skilled × Spread"
label var skill_x_age_cont "Skilled × Age"
label var skill_x_size_cont "Skilled × Size"

* Standardize continuous measures for comparability
foreach var in leverage_w credit_spread firm_age ln_assets {
    qui sum `var'
    gen `var'_std = (`var' - r(mean)) / r(sd)
}

gen skill_x_lev_std = share_skilled * leverage_w_std
gen skill_x_spread_std = share_skilled * credit_spread_std
gen skill_x_age_std = share_skilled * firm_age_std
gen skill_x_size_std = share_skilled * ln_assets_std

* Continuous specifications
qui reghdfe intang_intensity share_skilled leverage_w_std skill_x_lev_std ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store rob_lev

qui reghdfe intang_intensity share_skilled credit_spread_std skill_x_spread_std ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store rob_spread

qui reghdfe intang_intensity share_skilled firm_age_std skill_x_age_std ln_assets, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store rob_age

qui reghdfe intang_intensity share_skilled ln_assets_std skill_x_size_std, ///
    absorb(ano sector_fe) vce(cluster firm_id)
estimates store rob_size

esttab rob_lev rob_spread rob_age rob_size ///
    using "tables/tab5_robustness_continuous.tex", ///
    replace booktabs ///
    title("Robustness: Continuous Constraint Measures (Standardized)") ///
    mtitles("Leverage" "Spread" "Age" "Size") ///
    keep(share_skilled leverage_w_std skill_x_lev_std ///
         credit_spread_std skill_x_spread_std ///
         firm_age_std skill_x_age_std ///
         ln_assets_std skill_x_size_std) ///
    order(share_skilled *_std skill_x_*) ///
    label ///
    b(%9.4f) se(%9.4f) ///
    stats(N r2_a, labels("Observations" "Adjusted R$^2$") fmt(%12.0fc %9.3f)) ///
    addnotes("Constraint measures standardized (mean=0, sd=1)." ///
             "Negative interaction on leverage/spread, positive on age/size" ///
             "all indicate constraints weaken complementarity.")

di "  → Robustness table saved: tab5_robustness_continuous.tex"
di ""

/*==============================================================================
   COMPLETION
==============================================================================*/

di _newline(2)
di "╔════════════════════════════════════════════════════════════════╗"
di "║              MOTIVATING EVIDENCE COMPLETE                      ║"
di "╚════════════════════════════════════════════════════════════════╝"
di ""
di "Output saved in: $rootdir/output/"
di ""
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "FIGURES (in figures/ folder)"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""
di "  fig1_complementarity_binscatter.png"
di "    → Stylized Fact 1: Positive correlation skills & intangibles"
di ""
di "  fig2_pecking_order_binscatter.png"
di "    → Stylized Fact 2: Leverage reduces intangible intensity"
di ""
di "  fig3_underexploitation_leverage.png  ⭐ MAIN FIGURE"
di "    → Key Result: Constraints flatten skill-intangible relationship"
di ""
di "  fig3_underexploitation_[size/age/spread].png"
di "    → Robustness with alternative constraint measures"
di ""
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "TABLES (in tables/ folder)"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""
di "  tab0_summary_stats.tex"
di "    → Summary statistics"
di ""
di "  tab0b_summary_by_constraint.tex"
di "    → Summary by constraint status"
di ""
di "  tab1_complementarity.tex"
di "    → Complementarity regressions"
di ""
di "  tab2_pecking_order.tex"
di "    → Pecking order (constraints & composition)"
di ""
di "  tab3_underexploitation.tex  ⭐ MAIN TABLE"
di "    → Interaction regressions (underexploitation)"
di ""
di "  tab3b_underexploitation_firmFE.tex"
di "    → Firm FE specification"
di ""
di "  tab4_decomposition.tex"
di "    → Knowledge vs Organization capital"
di ""
di "  tab5_robustness_continuous.tex"
di "    → Continuous constraint measures"
di ""
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di "INTERPRETATION FOR PAPER"
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
di ""
di "The key prediction of the model is that constrained firms cannot"
di "fully exploit the complementarity between skills and intangibles."
di ""
di "In Table 3, the interaction coefficient (Skilled × Constrained)"
di "should be NEGATIVE, indicating:"
di "  • Unconstrained firms: steep positive skill-intangible slope"
di "  • Constrained firms: flatter slope (underexploitation)"
di ""
di "This is the 'skill-biased stagnation' mechanism: higher human"
di "capital delivers muted TFP gains because constrained firms cannot"
di "convert skilled labor into intangible investment."
di ""
di "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

cap log close

di ""
di "Log saved: motivating_evidence.log"
di ""
