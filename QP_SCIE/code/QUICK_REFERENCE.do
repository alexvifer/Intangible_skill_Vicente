/*==============================================================================
   QUICK REFERENCE GUIDE: Data Cleaning Decisions
   
   Copy-paste examples for common questions
==============================================================================*/

/*------------------------------------------------------------------------------
   Q1: How do I run the cleaning?
------------------------------------------------------------------------------*/

* From your main do-file, add:
do "$rootdir/code/data_clean_final.do"

* Or run directly:
global rootdir "your_path_here"
cd "$rootdir/code"
do "data_clean_final.do"


/*------------------------------------------------------------------------------
   Q2: Which dataset should I use for analysis?
------------------------------------------------------------------------------*/

* For main analysis, use the WINSORIZED dataset:
use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* For robustness checks, use the FLAGGED dataset:
use "$rootdir/output/final_data_2011_2022_cleaned.dta", clear


/*------------------------------------------------------------------------------
   Q3: How do I check data quality?
------------------------------------------------------------------------------*/

* Check the flags:
use "$rootdir/output/final_data_2011_2022_cleaned.dta", clear

tab flag_no_workers    // Firms with 0/missing workers
tab flag_no_assets     // Firms with 0/negative/missing assets
tab flag_no_revenue    // Firms with 0/negative/missing revenue

* Check balance sheet consistency:
sum flag_asset_check if drop_flag == 0     // Assets don't add up
sum flag_balance_check if drop_flag == 0   // Balance sheet doesn't balance


/*------------------------------------------------------------------------------
   Q4: What if I want to be more/less restrictive?
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* MORE RESTRICTIVE - Add additional filters:

* Require positive revenue AND positive assets:
keep if SV500101_w > 0 & SV512701_w > 0

* Require at least 5 employees:
keep if n_workers >= 5

* Drop firms with data quality flags:
keep if flag_asset_check == 0 & flag_balance_check == 0

* LESS RESTRICTIVE - Use cleaned but not winsorized:
use "$rootdir/output/final_data_2011_2022_cleaned.dta", clear
keep if drop_flag == 0  // Keep all clean observations


/*------------------------------------------------------------------------------
   Q5: How do I handle negative equity?
------------------------------------------------------------------------------*/

* Negative equity is KEPT in the dataset (firms in distress)
* If you want to analyze separately:

gen negative_equity = (SV514101 < 0 & SV514101 != .)
tab negative_equity

* Analyze healthy vs. distressed firms:
bysort negative_equity: sum SV502001 roa profit_margin


/*------------------------------------------------------------------------------
   Q6: How do I check winsorization impact?
------------------------------------------------------------------------------*/

* Compare original vs. winsorized:
use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* Merge with original (cleaned but not winsorized):
merge 1:1 firm_id ano using "final_data_2011_2022_cleaned.dta", ///
    keep(match) keepusing(SV500101 SV512701) nogen

* Compare distributions:
sum SV500101 SV500101_w, d
scatter SV500101 SV500101_w

* Count observations affected:
gen winsor_revenue = (SV500101 != SV500101_w & SV500101 != . & SV500101_w != .)
tab winsor_revenue


/*------------------------------------------------------------------------------
   Q7: How do I create industry-specific samples?
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* Manufacturing (CAE codes 10-33):
keep if cae3 >= 10 & cae3 <= 33

* Services (CAE codes 45-99):
keep if cae3 >= 45 & cae3 <= 99

* High-tech manufacturing (common definition):
keep if inlist(cae3, 21, 26, 27, 28, 29, 30)


/*------------------------------------------------------------------------------
   Q8: How do I create a balanced panel?
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* Count years per firm:
bysort firm_id: gen n_years = _N

* Keep only firms present in all 12 years (2011-2022):
keep if n_years == 12

* Or keep firms present at least 5 years:
keep if n_years >= 5


/*------------------------------------------------------------------------------
   Q9: How do I handle missing values in regressions?
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* Create a complete-case indicator:
egen complete_case = rowmiss(SV500101_w SV512701_w n_workers share_skilled)
keep if complete_case == 0  // No missing values

* Or use multiple imputation (requires mi command):
mi set wide
mi register imputed SV500101_w SV512701_w share_skilled
mi impute chained (regress) SV500101_w SV512701_w share_skilled = ///
    i.ano i.cae3, add(5) rseed(12345)


/*------------------------------------------------------------------------------
   Q10: Descriptive statistics table for paper
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* Overall descriptives:
estpost summarize SV500101_w SV512701_w n_workers share_skilled ///
    labor_productivity roa debt_to_assets, d
esttab using "descriptives.tex", cells("count mean sd p50 min max") ///
    noobs label replace

* By year:
preserve
collapse (mean) mean_revenue=SV500101_w mean_assets=SV512701_w ///
         mean_workers=n_workers mean_skilled=share_skilled ///
         (sd) sd_revenue=SV500101_w sd_assets=SV512701_w ///
         (count) n_firms=firm_id, by(ano)
list
export excel using "descriptives_by_year.xlsx", firstrow(variables) replace
restore

* By industry:
preserve
gen industry = .
replace industry = 1 if cae3 >= 10 & cae3 <= 33  // Manufacturing
replace industry = 2 if cae3 >= 45 & cae3 <= 99  // Services
replace industry = 3 if cae3 >= 1 & cae3 <= 9    // Primary

label define ind_lbl 1 "Manufacturing" 2 "Services" 3 "Primary"
label values industry ind_lbl

bysort industry: estpost summarize SV500101_w n_workers share_skilled
esttab using "descriptives_by_industry.tex", cells("count mean sd") ///
    noobs label replace
restore


/*------------------------------------------------------------------------------
   Q11: How do I identify outliers manually?
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022_cleaned.dta", clear
keep if drop_flag == 0

* Look at extreme values:
gsort -SV500101
list firm_id ano SV500101 n_workers in 1/20  // Top 20 revenue

* Identify observations in top/bottom 1%:
foreach var in SV500101 SV512701 n_workers {
    egen p1_`var' = pctile(`var'), p(1)
    egen p99_`var' = pctile(`var'), p(99)
    gen extreme_`var' = (`var' < p1_`var' | `var' > p99_`var') if `var' != .
}

* Create overall outlier flag:
egen any_extreme = rowtotal(extreme_*)
tab any_extreme


/*------------------------------------------------------------------------------
   Q12: Sample selection table for paper
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022.dta", clear

* Document sample construction:
count
local n1 = r(N)
di "Initial merged dataset: `n1'"

duplicates drop firm_id ano, force
count
local n2 = r(N)
di "After removing duplicates: `n2'"

drop if firm_id == . | ano == .
count
local n3 = r(N)
di "With valid identifiers: `n3'"

drop if SV512701 <= 0 | SV512701 == .
count
local n4 = r(N)
di "With positive assets: `n4'"

drop if SV500101 <= 0 & (n_workers == 0 | n_workers == .)
count
local n5 = r(N)
di "Final sample: `n5'"

* Export to table:
clear
input str50 step count
"1. Initial merged dataset" `n1'
"2. After removing duplicates" `n2'
"3. With valid identifiers" `n3'
"4. With positive assets" `n4'
"5. Final sample" `n5'
end

gen pct_retained = 100 * count / `n1'
list
export excel using "sample_selection.xlsx", firstrow(variables) replace


/*------------------------------------------------------------------------------
   Q13: Correlation matrix for key variables
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* Pearson correlations:
pwcorr SV500101_w SV512701_w n_workers share_skilled ///
    labor_productivity capital_intensity roa, star(0.05)

* Export correlation matrix:
estpost correlate SV500101_w SV512701_w n_workers share_skilled ///
    labor_productivity roa, matrix listwise
esttab using "correlations.tex", unstack not noobs compress replace


/*------------------------------------------------------------------------------
   Q14: Visual inspection of data quality
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* Distribution of key variables:
hist SV500101_w, name(revenue, replace) title("Revenue distribution")
hist n_workers, name(workers, replace) title("Employment distribution")
hist share_skilled, name(skilled, replace) title("Skill share distribution")

* Evolution over time:
preserve
collapse (mean) avg_revenue=SV500101_w avg_workers=n_workers ///
         avg_skilled=share_skilled (count) n_firms=firm_id, by(ano)
         
twoway (connected avg_revenue ano, yaxis(1)) ///
       (connected n_firms ano, yaxis(2)), ///
       legend(label(1 "Avg Revenue") label(2 "N Firms")) ///
       name(evolution, replace)
restore

* Scatter plots for relationships:
scatter labor_productivity share_skilled, name(prod_skill, replace)
scatter roa share_skilled, name(roa_skill, replace)


/*------------------------------------------------------------------------------
   Q15: Quick regression example
------------------------------------------------------------------------------*/

use "$rootdir/output/final_data_2011_2022_cleaned_winsorized.dta", clear

* Basic regression with controls:
reghdfe ln_SV500101 share_skilled ln_n_workers firm_age ///
    i.cae3, absorb(ano) vce(cluster firm_id)

* Export to table:
eststo m1: reghdfe ln_SV500101 share_skilled, absorb(ano) vce(cluster firm_id)
eststo m2: reghdfe ln_SV500101 share_skilled ln_n_workers, ///
    absorb(ano) vce(cluster firm_id)
eststo m3: reghdfe ln_SV500101 share_skilled ln_n_workers firm_age, ///
    absorb(ano firm_id) vce(cluster firm_id)
    
esttab m1 m2 m3 using "regression_table.tex", ///
    se star(* 0.10 ** 0.05 *** 0.01) label replace


/*==============================================================================
   COMMON GOTCHAS AND SOLUTIONS
==============================================================================*/

* GOTCHA 1: "Variable not found" after cleaning
* SOLUTION: Check if you're using the winsorized dataset

* GOTCHA 2: Different sample sizes across specifications
* SOLUTION: Create a complete-case sample first, then run all regressions

* GOTCHA 3: Extreme coefficients in regressions
* SOLUTION: Verify you're using winsorized variables (_w suffix)

* GOTCHA 4: Missing years for some firms
* SOLUTION: This is normal - not all firms survive all years
*            Use panel data techniques (fixed effects, etc.)

* GOTCHA 5: Flags showing many issues
* SOLUTION: This is informative, not necessarily problematic
*            Document in paper, do robustness checks

*/
