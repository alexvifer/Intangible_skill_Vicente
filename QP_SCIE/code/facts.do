/*==============================================================================
   EMPIRICAL ANALYSIS: SKILL-BIASED STAGNATION

   Input:  final_data_2011_2022_analysis.dta
   Output: Tables and figures in $rootdir/results/

   Three main facts:
   1. Complementarity between intangibles and skilled labor
   2. Financially constrained firms underinvest in intangibles
   3. Constrained firms underexploit complementarity (cross-section + dynamics)

==============================================================================*/

clear all
set more off
cd "$rootdir/output"

cap log close
log using "empirical_analysis.log", replace

use "final_data_2011_2022_analysis.dta", clear

xtset firm_id ano

* Set white background for all graphs
set scheme s1color

cap mkdir "$rootdir/results"


/*==============================================================================
   FACT 1: COMPLEMENTARITY BETWEEN INTANGIBLES AND SKILLED LABOR
==============================================================================*/

* Raw correlation
preserve
collapse (mean) intang_intensity_bs_rd share_skilled, by(firm_id)
binscatter intang_intensity_bs_rd share_skilled, nquantiles(20) ///
    xtitle("Share of Skilled Workers") ///
    ytitle("Intangible Intensity (K_intangible / K_total)") ///
    title("Intangibles and Skills: Raw Correlation")
graph export "$rootdir/results/fact1_raw_correlation.png", replace width(2000)
restore

*-------------------------------------------------------------------------------
* Main table: GVA with nested controls (no controls, FE only, FE + controls)
*-------------------------------------------------------------------------------
eststo clear

* Column 1: No controls, no fixed effects
eststo gva_noctrls: reg ln_GVA c.intang_intensity_bs_rd##c.share_skilled, vce(robust)
estadd local firmfe "No"
estadd local yearfe "No"
estadd local indfe "No"
estadd local controls "No"

* Column 2: Fixed effects only (no additional controls)
eststo gva_fe: reghdfe ln_GVA c.intang_intensity_bs_rd##c.share_skilled, ///
    absorb(firm_id ano cae3) vce(robust)
estadd local firmfe "Yes"
estadd local yearfe "Yes"
estadd local indfe "Yes"
estadd local controls "No"

* Column 3: Fixed effects + controls
eststo gva_full: reghdfe ln_GVA c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age, absorb(firm_id ano cae3) vce(robust)
estadd local firmfe "Yes"
estadd local yearfe "Yes"
estadd local indfe "Yes"
estadd local controls "Yes"

esttab gva_noctrls gva_fe gva_full using "$rootdir/results/fact1_complementarity.tex", replace ///
    b(2) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) booktabs ///
    keep(intang_intensity_bs_rd share_skilled c.intang_intensity_bs_rd#c.share_skilled) ///
    coeflabels(intang_intensity_bs_rd "Intangible Intensity" ///
               share_skilled "Share Skilled Workers" ///
               c.intang_intensity_bs_rd#c.share_skilled "Intangible Intensity x Share Skilled") ///
    stats(N r2 firmfe yearfe indfe controls, ///
        labels("Observations" "Adjusted R-squared" "Firm FE" "Year FE" "Industry FE" "Controls") ///
        fmt(%12.0fc 3 0 0 0 0)) ///
    title("Complementarity Between Intangibles and Skilled Labor\label{tab:complementarity}") ///
    mtitles("(1)" "(2)" "(3)") ///
    mgroups("Log Gross Value Added", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    nonotes ///
    addnote("Dependent variable: Log gross value added (GVA)." ///
            "Intangible intensity defined as intangible capital divided by total capital." ///
            "Controls include log total capital, log employment, and log firm age." ///
            "Robust standard errors in parentheses.")

*-------------------------------------------------------------------------------
* Robustness table: Revenue and Production with nested controls (Appendix)
*-------------------------------------------------------------------------------
eststo clear

* Revenue: No controls
eststo rev_noctrls: reg ln_revenue c.intang_intensity_bs_rd##c.share_skilled, vce(robust)
estadd local firmfe "No"
estadd local yearfe "No"
estadd local indfe "No"
estadd local controls "No"

* Revenue: Fixed effects only
eststo rev_fe: reghdfe ln_revenue c.intang_intensity_bs_rd##c.share_skilled, ///
    absorb(firm_id ano cae3) vce(robust)
estadd local firmfe "Yes"
estadd local yearfe "Yes"
estadd local indfe "Yes"
estadd local controls "No"

* Revenue: Fixed effects + controls
eststo rev_full: reghdfe ln_revenue c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age, absorb(firm_id ano cae3) vce(robust)
estadd local firmfe "Yes"
estadd local yearfe "Yes"
estadd local indfe "Yes"
estadd local controls "Yes"

* Production: No controls
eststo prod_noctrls: reg ln_production c.intang_intensity_bs_rd##c.share_skilled, vce(robust)
estadd local firmfe "No"
estadd local yearfe "No"
estadd local indfe "No"
estadd local controls "No"

* Production: Fixed effects only
eststo prod_fe: reghdfe ln_production c.intang_intensity_bs_rd##c.share_skilled, ///
    absorb(firm_id ano cae3) vce(robust)
estadd local firmfe "Yes"
estadd local yearfe "Yes"
estadd local indfe "Yes"
estadd local controls "No"

* Production: Fixed effects + controls
eststo prod_full: reghdfe ln_production c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age, absorb(firm_id ano cae3) vce(robust)
estadd local firmfe "Yes"
estadd local yearfe "Yes"
estadd local indfe "Yes"
estadd local controls "Yes"

esttab rev_noctrls rev_fe rev_full prod_noctrls prod_fe prod_full ///
    using "$rootdir/results/fact1_complementarity_robustness.tex", replace ///
    b(2) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) booktabs ///
    keep(intang_intensity_bs_rd share_skilled c.intang_intensity_bs_rd#c.share_skilled) ///
    coeflabels(intang_intensity_bs_rd "Intangible Intensity" ///
               share_skilled "Share Skilled Workers" ///
               c.intang_intensity_bs_rd#c.share_skilled "Intangible Intensity x Share Skilled") ///
    stats(N r2 firmfe yearfe indfe controls, ///
        labels("Observations" "Adjusted R-squared" "Firm FE" "Year FE" "Industry FE" "Controls") ///
        fmt(%12.0fc 3 0 0 0 0)) ///
    title("Complementarity Between Intangibles and Skilled Labor: Alternative Outcomes\label{tab:complementarityrobust}") ///
    mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)") ///
    mgroups("Log Revenue" "Log Production", pattern(1 0 0 1 0 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    substitute(\centering \centering\small) ///
    nonotes ///
    addnote("Intangible intensity defined as intangible capital divided by total capital." ///
            "Controls include log total capital, log employment, and log firm age." ///
            "Robust standard errors in parentheses.")


/*==============================================================================
   FACT 2: FINANCIAL CONSTRAINTS AND THE PECKING ORDER
==============================================================================*/

* Winsorize variables for Fact 2
winsor2 inv_rate_physical_bs_rd, cuts(5 95) by(ano cae3) suffix(_w)
label var inv_rate_physical_bs_rd_w "Physical investment rate (winsorized 5-95)"

winsor2 inv_rate_intang_bs_rd, cuts(5 95) by(ano cae3) suffix(_w)
label var inv_rate_intang_bs_rd_w "Intangible investment rate (winsorized 5-95)"

winsor2 leverage_assets, cuts(5 95) by(ano cae3) suffix(_w)
label var leverage_assets_w "Leverage (Debt/Assets, winsorized 5-95)"

* Define leverage constraint (sector-year median)
bysort cae3 ano: egen leverage_median = median(leverage_assets_w)
gen high_leverage = (leverage_assets_w > leverage_median) if !missing(leverage_assets_w, leverage_median)
label var high_leverage "Above sector-year median leverage"

* Residualized binscatters
foreach var in inv_rate_physical_bs_rd_w inv_rate_intang_bs_rd_w intang_intensity_bs_rd {
    qui reghdfe `var' ln_K_total_bs_rd ln_emp ln_age, absorb(firm_id ano cae3) resid
    predict resid_`var', resid
}

* Physical investment vs leverage
binscatter resid_inv_rate_physical_bs_rd_w leverage_assets_w, nquantiles(20) ///
    xtitle("Leverage (Debt / Assets)") ytitle("Physical Investment Rate (residualized)") ///
    title("Physical Investment and Leverage")
graph export "$rootdir/results/fact2_physical_inv_leverage.png", replace width(2000)

* Intangible investment vs leverage
binscatter resid_inv_rate_intang_bs_rd_w leverage_assets_w, nquantiles(20) ///
    xtitle("Leverage (Debt / Assets)") ytitle("Intangible Investment Rate (residualized)") ///
    title("Intangible Investment and Leverage")
graph export "$rootdir/results/fact2_intangible_inv_leverage.png", replace width(2000)

* Intangible intensity vs leverage
binscatter resid_intang_intensity_bs_rd leverage_assets_w, nquantiles(20) ///
    xtitle("Leverage (Debt / Assets)") ytitle("Intangible Intensity (residualized)") ///
    title("Intangible Intensity and Leverage")
graph export "$rootdir/results/fact2_intang_intensity_leverage.png", replace width(2000)

* R&D workers vs leverage
preserve
keep if share_rd_workers > 0
qui reghdfe share_rd_workers ln_K_total_bs_rd ln_emp ln_age, absorb(firm_id ano cae3) resid
predict resid_rd_workers, resid
binscatter resid_rd_workers leverage_assets_w, nquantiles(20) ///
    xtitle("Leverage (Debt / Assets)") ytitle("Share of R&D Workers (residualized)") ///
    title("R&D Employment and Leverage")
graph export "$rootdir/results/fact2_rd_workers_leverage.png", replace width(2000)
restore

drop resid_* 


/*==============================================================================
   FACT 3: UNDEREXPLOITATION OF COMPLEMENTARITY BY CONSTRAINED FIRMS
==============================================================================*/

*-------------------------------------------------------------------------------
* Main table: GVA by leverage group
*-------------------------------------------------------------------------------
eststo clear

eststo gva_pooled: reghdfe ln_GVA c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age, absorb(firm_id ano cae3) vce(robust)

eststo gva_low: reghdfe ln_GVA c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age if high_leverage == 0, ///
    absorb(firm_id ano cae3) vce(robust)

eststo gva_high: reghdfe ln_GVA c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age if high_leverage == 1, ///
    absorb(firm_id ano cae3) vce(robust)

esttab gva_pooled gva_low gva_high using "$rootdir/results/fact3_underexploitation.tex", replace ///
    b(2) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) booktabs ///
    keep(intang_intensity_bs_rd share_skilled c.intang_intensity_bs_rd#c.share_skilled) ///
    coeflabels(intang_intensity_bs_rd "Intangible Intensity" ///
               share_skilled "Share Skilled Workers" ///
               c.intang_intensity_bs_rd#c.share_skilled "Intangible Intensity x Share Skilled") ///
    stats(N r2, labels("Observations" "Adjusted R-squared") fmt(%12.0fc 3)) ///
    title("Underexploitation of Intangibles-Skills Complementarity\label{tab:underexploitation}") ///
    mtitles("All Firms" "Low Leverage" "High Leverage") ///
    mgroups("Log Gross Value Added", pattern(1 0 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    refcat(intang_intensity_bs_rd "\textit{Main effects:}" ///
           c.intang_intensity_bs_rd#c.share_skilled "\textit{Complementarity:}", nolabel) ///
    nonotes ///
    addnote("Dependent variable: Log gross value added (GVA)." ///
            "Low- and high-leverage defined relative to sector-year median leverage." ///
            "All specifications include firm, year, and industry fixed effects." ///
            "Controls include log total capital, log employment, and log firm age." ///
            "Robust standard errors in parentheses.")

*-------------------------------------------------------------------------------
* Robustness table: Revenue and Production by leverage group (Appendix)
*-------------------------------------------------------------------------------
eststo clear

* Revenue regressions
eststo rev_pooled: reghdfe ln_revenue c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age, absorb(firm_id ano cae3) vce(robust)

eststo rev_low: reghdfe ln_revenue c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age if high_leverage == 0, ///
    absorb(firm_id ano cae3) vce(robust)

eststo rev_high: reghdfe ln_revenue c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age if high_leverage == 1, ///
    absorb(firm_id ano cae3) vce(robust)

* Production regressions
eststo prod_pooled: reghdfe ln_production c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age, absorb(firm_id ano cae3) vce(robust)

eststo prod_low: reghdfe ln_production c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age if high_leverage == 0, ///
    absorb(firm_id ano cae3) vce(robust)

eststo prod_high: reghdfe ln_production c.intang_intensity_bs_rd##c.share_skilled ///
    ln_K_total_bs_rd ln_emp ln_age if high_leverage == 1, ///
    absorb(firm_id ano cae3) vce(robust)

esttab rev_pooled rev_low rev_high prod_pooled prod_low prod_high ///
    using "$rootdir/results/fact3_underexploitation_robustness.tex", replace ///
    b(2) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) booktabs ///
    keep(intang_intensity_bs_rd share_skilled c.intang_intensity_bs_rd#c.share_skilled) ///
    coeflabels(intang_intensity_bs_rd "Intangible Intensity" ///
               share_skilled "Share Skilled Workers" ///
               c.intang_intensity_bs_rd#c.share_skilled "Intangible Intensity x Share Skilled") ///
    stats(N r2, labels("Observations" "Adjusted R-squared") fmt(%12.0fc 3)) ///
    title("Underexploitation of Complementarity: Alternative Outcomes\label{tab:underexploitationrobust}") ///
    mtitles("All" "Low Lev." "High Lev." "All" "Low Lev." "High Lev.") ///
    mgroups("Log Revenue" "Log Production", pattern(1 0 0 1 0 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    refcat(intang_intensity_bs_rd "\textit{Main effects:}" ///
           c.intang_intensity_bs_rd#c.share_skilled "\textit{Complementarity:}", nolabel) ///
    substitute(\centering \centering\small) ///
    nonotes ///
    addnote("Low- and high-leverage defined relative to sector-year median leverage." ///
            "All specifications include firm, year, and industry fixed effects." ///
            "Controls include log total capital, log employment, and log firm age." ///
            "Robust standard errors in parentheses.")

* Dynamic evidence: Wage premium and skill share evolution (2011-2022)
gen wage_premium = avg_wage_skilled / avg_wage_unskilled if avg_wage_skilled > 0 & avg_wage_unskilled > 0
label var wage_premium "Wage premium (skilled/unskilled)"

* Residualize to control for size/age/industry (not year FE, to preserve time trends)
* This abstracts from selection effects (e.g., larger firms having different skill composition)
qui reghdfe share_skilled ln_K_total_bs_rd ln_emp ln_age, absorb(cae3) resid
predict resid_share_skilled, resid

qui reghdfe wage_premium ln_K_total_bs_rd ln_emp ln_age, absorb(cae3) resid
predict resid_wage_premium, resid

preserve

* Aggregate trends (raw, for reference)
collapse (mean) share_skilled wage_premium, by(ano)

twoway (connected share_skilled ano, yaxis(1) lcolor(navy) mcolor(navy) msymbol(O) ///
        ytitle("Share of Skilled Workers", axis(1))) ///
       (connected wage_premium ano, yaxis(2) lcolor(cranberry) mcolor(cranberry) msymbol(D) ///
        ytitle("Wage Premium (Skilled/Unskilled)", axis(2))), ///
    xlabel(2011(2)2022) xtitle("") ///
    legend(order(1 "Share Skilled (left)" 2 "Wage Premium (right)") pos(6) rows(1)) ///
    title("Skill Supply Shock and Declining Wage Premium")
graph export "$rootdir/results/fact3_skill_premium_trends.png", replace width(2000)

restore

* Share of skilled workers by leverage (residualized)
preserve

collapse (mean) resid_share_skilled, by(ano high_leverage)
reshape wide resid_share_skilled, i(ano) j(high_leverage)

twoway (connected resid_share_skilled0 ano, lcolor(forest_green) mcolor(forest_green) msymbol(O) lwidth(medium)) ///
       (connected resid_share_skilled1 ano, lcolor(maroon) mcolor(maroon) msymbol(D) lwidth(medium)), ///
    xlabel(2011(2)2022) xtitle("") ylabel(, format(%4.3f)) ///
    ytitle("Share of Skilled Workers (residualized)") ///
    legend(order(1 "Low Leverage" 2 "High Leverage") pos(6) rows(1)) ///
    title("Skill Share by Leverage Status")
graph export "$rootdir/results/fact3_skills_by_leverage.png", replace width(2000)

restore

* Wage premium trends by leverage (residualized)
preserve

collapse (mean) resid_wage_premium, by(ano high_leverage)
reshape wide resid_wage_premium, i(ano) j(high_leverage)

twoway (connected resid_wage_premium0 ano, lcolor(forest_green) mcolor(forest_green) msymbol(O) lwidth(medium)) ///
       (connected resid_wage_premium1 ano, lcolor(maroon) mcolor(maroon) msymbol(D) lwidth(medium)), ///
    xlabel(2011(2)2022) xtitle("") ylabel(, format(%4.2f)) ///
    ytitle("Wage Premium (residualized)") ///
    legend(order(1 "Low Leverage" 2 "High Leverage") pos(6) rows(1)) ///
    title("Wage Premium Decline: Driven by Constrained Firms")
graph export "$rootdir/results/fact3_premium_by_leverage.png", replace width(2000)

restore

drop resid_share_skilled resid_wage_premium

cap log close
