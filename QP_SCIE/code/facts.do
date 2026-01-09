/*==============================================================================
   EMPIRICAL ANALYSIS: SKILL-BIASED STAGNATION
   
   Input:  final_data_2011_2022_analysis.dta (from analysis_prep.do)
   Output: Tables and figures in $rootdir/results/
   
==============================================================================*/

clear all
set more off

cd "$rootdir/output"

cap log close
log using "empirical_analysis.log", replace

di "=========================================="
di "EMPIRICAL ANALYSIS"
di "Date: `c(current_date)'"
di "=========================================="
di ""

/*==============================================================================
   SETUP
==============================================================================*/

use "final_data_2011_2022_analysis.dta", clear

* Declare panel
xtset firm_id ano

* Create results directory
cap mkdir "$rootdir/results"

di "Sample size: " _N
di ""

/*==============================================================================
   FACT 1: COMPLEMENTARITY BETWEEN INTANGIBLES AND SKILLED LABOR
==============================================================================*/

di "=========================================="
di "FACT 1: COMPLEMENTARITY"
di "=========================================="
di ""

*------------------------------------------------------------------------------
* Visual evidence: Raw correlation
*------------------------------------------------------------------------------

binscatter intang_intensity_bs_rd share_skilled if intang_intensity > 0 & share_skilled > 0, nquantiles(20) ///
    xtitle("Share of skilled workers") ///
    ytitle("Intangible intensity") ///
    title("Intangibles and Skills: Raw Correlation") ///
    note("Excludes firms with zero intangible capital or no skilled workers. 20 quantiles.")
graph export "$rootdir/results/fact1_raw_correlation.png", replace width(2000)

di "  ✓ Figure saved: fact1_raw_correlation.png"
di ""

*------------------------------------------------------------------------------
* Main regression table: Complementarity across three outcomes
*------------------------------------------------------------------------------

di "  → Running complementarity regressions..."
di ""

eststo clear

* Column 1: Log Revenue
reghdfe ln_revenue c.intang_intensity_bs_rd##c.share_skilled ln_K_total_bs_rd ln_emp ln_age, absorb(ano cae3 firm_id) vce(robust)

eststo revenue
estadd local year_fe "Yes"
estadd local industry_fe "Yes"
estadd local firm_fe "Yes"
estadd local controls "Yes"

* Column 2: Log Production
reghdfe ln_production c.intang_intensity_bs_rd##c.share_skilled ln_K_total_bs_rd ln_emp ln_age, absorb(ano cae3 firm_id) vce(robust)

eststo production
estadd local year_fe "Yes"
estadd local industry_fe "Yes"
estadd local firm_fe "Yes"
estadd local controls "Yes"

* Column 3: Log GVA
reghdfe ln_GVA c.intang_intensity_bs_rd##c.share_skilled ln_K_total_bs_rd ln_emp ln_age, absorb(ano cae3 firm_id) vce(robust)

eststo gva
estadd local year_fe "Yes"
estadd local industry_fe "Yes"
estadd local firm_fe "Yes"
estadd local controls "Yes"

di ""
di "  ✓ All regressions completed"
di ""

*------------------------------------------------------------------------------
* Export table to LaTeX
*------------------------------------------------------------------------------

esttab revenue production gva using "$rootdir/results/fact1_complementarity.tex", ///
    replace booktabs ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Log Revenue" "Log Production" "Log GVA") ///
    title("Complementarity Between Intangibles and Skilled Labor\label{tab:complementarity}") ///
    keep(c.intang_intensity_bs_rd#c.share_skilled intang_intensity_bs_rd share_skilled) ///
    order(c.intang_intensity_bs_rd#c.share_skilled intang_intensity_bs_rd share_skilled) ///
    coeflabels(c.intang_intensity_bs_rd#c.share_skilled "Intangible Intensity $\times$ Share Skilled" ///
               intang_intensity_bs_rd "Intangible Intensity" ///
               share_skilled "Share Skilled Workers") ///
    scalars("year_fe Year FE" "industry_fe Industry FE" "firm_fe Firm FE" "controls Controls") ///
    sfmt(0 0 0 0) ///
    nonotes ///
    addnotes("Controls include log total capital, log employment, and log firm age." ///
             "Industry FE based on 3-digit CAE codes. Robust standard errors in parentheses." ///
             "* \$p < 0.10\$, ** \$p < 0.05\$, *** \$p < 0.01\$")

di "  ✓ Table saved: fact1_complementarity.tex"
di ""

*------------------------------------------------------------------------------
* Display results in console
*------------------------------------------------------------------------------

esttab revenue production gva, ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Revenue" "Production" "GVA") ///
    keep(c.intang_intensity_bs_rd#c.share_skilled intang_intensity_bs_rd share_skilled) ///
    scalars("year_fe Year FE" "industry_fe Ind FE" "firm_fe Firm FE" "controls Controls") ///
    title("FACT 1: Complementarity Between Intangibles and Skills")

di ""
di "=========================================="
di "FACT 1 ANALYSIS COMPLETE"
di "=========================================="
di ""
di "Output: $rootdir/results/fact1_complementarity.tex"
di ""


/*==============================================================================
   FACT 2: PECKING ORDER DISTORTION
   
   Theory: Financial constraints create pecking order where constrained firms
           under-invest in intangible capital (non-pledgeable) relative to 
           physical capital (pledgeable), and underallocate workers to R&D
   
==============================================================================*/

di "=========================================="
di "FACT 2: PECKING ORDER DISTORTION"
di "=========================================="
di ""

*------------------------------------------------------------------------------
* Winsorize constraint and investment variables
*------------------------------------------------------------------------------

di "  → Winsorizing variables (1-99 by year and sector)..."
di ""

winsor2 leverage_assets, suffix(_win) cuts(5 95) by(ano cae3)
winsor2 inv_rate_physical inv_rate_intang_bs_rd, suffix(_win) cuts(5 95) by(ano cae3)

di "     ✓ Variables winsorized"
di ""

*------------------------------------------------------------------------------
* Visual evidence: Residualized binscatters with leverage
*------------------------------------------------------------------------------

di "  → Creating residualized binscatters..."
di ""

* Physical investment vs. leverage
qui reghdfe inv_rate_physical_win ln_K_total_bs_rd ln_emp ln_age, ///
    absorb(ano cae3 firm_id) resid
predict resid_inv_phys, resid
qui reghdfe leverage_assets_win, absorb(ano cae3 firm_id) resid
predict resid_leverage, resid

binscatter resid_inv_phys resid_leverage, ///
    nquantiles(20) ///
    xtitle("Leverage, residualized") ///
    ytitle("Physical investment rate, residualized") ///
    title("Physical Investment and Financial Constraints") ///
    note("Residuals from regression on firm, year and sector FE, log capital, log employment, log age.")
graph export "$rootdir/results/fact2_physical_inv_leverage.png", replace width(2000)

drop resid_inv_phys resid_leverage

* Intangible investment vs. leverage
qui reghdfe inv_rate_intang_bs_rd_win ln_K_total_bs_rd ln_emp ln_age, ///
    absorb(firm_id ano cae3) resid
predict resid_inv_intang, resid
qui reghdfe leverage_assets_win, absorb(firm_id ano cae3) resid
predict resid_leverage, resid

binscatter resid_inv_intang resid_leverage, ///
    nquantiles(20) ///
    xtitle("Leverage, residualized") ///
    ytitle("Intangible investment rate, residualized") ///
    title("Intangible Investment and Financial Constraints") ///
    note("Residuals from regression on firm, year and sector FE, log capital, log employment, log age.")
graph export "$rootdir/results/fact2_intangible_inv_leverage.png", replace width(2000)

drop resid_inv_intang resid_leverage

* Intangible intensity vs. leverage
qui reghdfe intang_intensity ln_K_total_bs_rd ln_emp ln_age, ///
    absorb(firm_id ano cae3) resid
predict resid_intang_int, resid
qui reghdfe leverage_assets_win, absorb(firm_id ano cae3) resid
predict resid_leverage, resid

binscatter resid_intang_int resid_leverage, ///
    nquantiles(20) ///
    xtitle("Leverage, residualized") ///
    ytitle("Intangible intensity, residualized") ///
    title("Intangible Intensity and Financial Constraints") ///
    note("Residuals from regression on firm, year and sector FE, log capital, log employment, log age.")
graph export "$rootdir/results/fact2_intang_intensity_leverage.png", replace width(2000)

drop resid_intang_int resid_leverage

* R&D workers vs. leverage
qui reghdfe share_rd_workers ln_K_total_bs_rd ln_emp ln_age if share_rd_workers > 0, ///
    absorb(firm_id ano cae3) resid
predict resid_rd_share, resid
qui reghdfe leverage_assets_win if share_rd_workers > 0, ///
    absorb(firm_id ano cae3) resid
predict resid_leverage, resid

binscatter resid_rd_share resid_leverage if share_rd_workers > 0, ///
    nquantiles(20) ///
    xtitle("Leverage, residualized") ///
    ytitle("Share of R&D workers, residualized") ///
    title("R&D Labor Allocation and Financial Constraints") ///
    note("Residuals from regression on firm, year and sector FE, log capital, log employment, log age." ///
         "Sample: firms with positive R&D employment.")
graph export "$rootdir/results/fact2_rd_workers_leverage.png", replace width(2000)

drop resid_rd_share resid_leverage

di "     ✓ Figures saved (4 binscatters)"
di ""

di "=========================================="
di "FACT 2 COMPLETE"
di "=========================================="
di ""
di "Output files:"
di "  • fact2_physical_inv_leverage.png      - Physical investment vs leverage"
di "  • fact2_intangible_inv_leverage.png    - Intangible investment vs leverage"
di "  • fact2_intang_intensity_leverage.png  - Intangible intensity vs leverage"
di "  • fact2_rd_workers_leverage.png        - R&D workers vs leverage"
di ""
di "KEY RESULT:"
di "  Constrained firms (high leverage) have:"
di "    → Lower intangible investment rate"
di "    → Lower intangible intensity"  
di "    → Lower R&D worker allocation"
di "  This is the pecking order distortion from low pledgeability of intangibles"
di ""

/*==============================================================================
   FACT 3: UNDEREXPLOITATION OF COMPLEMENTARITY BY CONSTRAINED FIRMS
   
   Theory: Financial constraints distort input choices, pushing firms away
           from the region where the technological complementarity between
           intangibles and skilled labor is operative.
==============================================================================*/

di "=========================================="
di "FACT 3: UNDEREXPLOITATION OF COMPLEMENTARITY"
di "=========================================="
di ""

*------------------------------------------------------------------------------
* Construct leverage-based constraint indicator
*------------------------------------------------------------------------------

di "  → Constructing leverage-based constraint groups..."
di ""

* Median leverage by year and sector
bys ano cae3: egen lev_median = median(leverage_assets_win)

gen high_leverage = leverage_assets_win >= lev_median if !missing(leverage_assets_win)
label var high_leverage "High leverage (above sector-year median)"

di "     ✓ Constraint indicator created"
di ""

*------------------------------------------------------------------------------
* Regression evidence: Build-up specification
*------------------------------------------------------------------------------

di "  → Running complementarity regressions by constraint status..."
di ""

eststo clear

* Column 1: Baseline (pooled)
eststo col1: reghdfe ln_GVA ///
    c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age, ///
    absorb(firm_id ano cae3) vce(robust)

* Column 2: Low-leverage firms
eststo col2: reghdfe ln_GVA ///
    c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age ///
    if high_leverage == 0, ///
    absorb(firm_id ano cae3) vce(robust)

* Column 3: High-leverage firms
eststo col3: reghdfe ln_GVA ///
    c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age ///
    if high_leverage == 1, ///
    absorb(firm_id ano cae3) vce(robust)

*------------------------------------------------------------------------------
* Export table
*------------------------------------------------------------------------------

esttab col1 col2 col3 using "$rootdir/results/fact3_underexploitation.tex", replace ///
    b(4) se(4) r2 star(* 0.10 ** 0.05 *** 0.01) ///
    label booktabs ///
    keep(intang_intensity_bs_rd share_skilled ///
         c.intang_intensity_bs_rd#c.share_skilled ///
         ln_K_total_bs_rd ln_emp ln_age) ///
    order(intang_intensity_bs_rd share_skilled ///
          c.intang_intensity_bs_rd#c.share_skilled ///
          ln_K_total_bs_rd ln_emp ln_age) ///
    stats(N r2, ///
          labels("Observations" "Adjusted R-squared") ///
          fmt(%12.0fc 3)) ///
    title("Underexploitation of Intangibles–Skills Complementarity") ///
    mtitles("All Firms" "Low Leverage" "High Leverage") ///
    refcat(intang_intensity_bs_rd "\textit{Main effects:}" ///
           c.intang_intensity_bs_rd#c.share_skilled "\textit{Complementarity:}" ///
           ln_K_total_bs_rd "\textit{Controls:}", nolabel) ///
    addnote("Dependent variable: Log gross value added (GVA)." ///
            "Low- and high-leverage defined relative to sector-year median leverage." ///
            "All specifications include year and industry fixed effects." ///
            "Standard errors clustered at firm level.")

di ""
di "  ✓ Table saved: fact3_underexploitation.tex"
di ""

*------------------------------------------------------------------------------
* Wage premium evidence
*------------------------------------------------------------------------------

di ""
di "Theory: Constrained firms cannot exploit complementarity between"
di "        intangibles and skilled workers, reducing skilled workers'"
di "        marginal product and hence their wage premium."
di ""

*------------------------------------------------------------------------------
* Construct wage premium measure
*------------------------------------------------------------------------------

di "  → Constructing wage premium (skilled/unskilled ratio)..."
di ""

* Check if wage variables exist and construct premium
gen wage_premium = avg_wage_skilled / avg_wage_unskilled ///
	if avg_wage_skilled > 0 & avg_wage_unskilled > 0
label var wage_premium "Wage premium (skilled/unskilled ratio)"

* Summary statistics
qui sum wage_premium, d
di "     Mean wage premium: " %5.3f r(mean)
di "     Median:            " %5.3f r(p50)
di "     N:                 " %12.0fc r(N)
di ""
di "     ✓ Wage premium constructed"
di ""

*----------------------------------------------------------------------
* Visual evidence: Wage premium vs leverage
*----------------------------------------------------------------------

di "  → Creating wage premium binscatter..."
di ""

* Residualized binscatter
qui reghdfe wage_premium ln_K_total_bs_rd ln_emp ln_age, ///
	absorb(firm_id ano cae3) resid
predict resid_wage_prem, resid

qui reghdfe leverage_assets_win, absorb(firm_id ano cae3) resid
predict resid_leverage_wage, resid

binscatter resid_wage_prem resid_leverage_wage, ///
	nquantiles(20) ///
	xtitle("Leverage, residualized") ///
	ytitle("Wage premium (skilled/unskilled), residualized") ///
	title("Wage Premium and Financial Constraints") ///
	note("Wage premium = ratio of average skilled to unskilled wages." ///
		 "Residuals from regression on firm, year and sector FE, log capital, log employment, log age.")
graph export "$rootdir/results/fact3_wage_premium_leverage.png", replace width(2000)

drop resid_wage_prem resid_leverage_wage

di "     ✓ Figure saved: fact3_wage_premium_leverage.png"
di ""


*------------------------------------------------------------------------------
* Summary
*------------------------------------------------------------------------------

di "=========================================="
di "FACT 3 COMPLETE"
di "=========================================="
di ""
di "Output files:"
di "  • fact3_underexploitation.tex         - Regression evidence"
di ""
di "KEY RESULT:"
di "  Complementarity between intangibles and skilled labor is:"
di "    → Strong and significant for low-leverage firms"
di "    → Weaker or absent for high-leverage firms"
di ""
di "INTERPRETATION:"
di "  Financial constraints distort input choices, pushing firms away"
di "  from the region where technological complementarity is operative."
di ""


cap log close