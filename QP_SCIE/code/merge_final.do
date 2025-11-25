/*==============================================================================
   MODULE 4: MERGE ALL DATASETS (2011-2022)
   
   Combines firm characteristics, SCIE data, and worker skill measures
   Creates: final_data_2011_2022.dta (complete merged dataset)
   
==============================================================================*/

cd "$rootdir/output"

di "  → Starting final merge..."

/*==============================================================================
   START WITH FIRM-LEVEL SKILL DATA
==============================================================================*/

use "firm_skills_wages_2011_2022.dta", clear

di "  Starting with firm skills data: " _N " observations"

/*==============================================================================
   MERGE WITH FIRM CHARACTERISTICS
==============================================================================*/

merge 1:1 firm_id ano using "firm_2011_2022.dta", keep(match master)

tab _merge
gen has_firm_data = (_merge == 3)
drop _merge

di "  After merging firm characteristics: " _N " observations"

/*==============================================================================
   CLEAN AND FINALIZE
==============================================================================*/

* Label key variables
label var has_firm_data "Matched with firm characteristics"
label var scie_match "Matched with SCIE balance sheet data"

compress
save "final_data_2011_2022.dta", replace

di "  → Final merged dataset created: final_data_2011_2022.dta"
di "  → Total observations: " _N

/*==============================================================================
   SUMMARY STATISTICS
==============================================================================*/

di _newline(2)
di "=========================================="
di "FINAL DATASET SUMMARY"
di "=========================================="

* Coverage
di _newline
di "Data coverage:"
tab ano
di _newline
sum has_firm_data scie_match

di _newline
di "=========================================="
di "Files created in $rootdir/output:"
di "  - SCIE_2011_2022.dta"
di "  - IES_age_2011_2022.dta"
di "  - firm_2011_2022.dta"
di "  - worker_skills_2011_2022.dta"
di "  - firm_skills_wages_2011_2022.dta"
di "  - final_data_2011_2022.dta <-- USE THIS"
di "=========================================="


Attached in the project are the do files for Stata I used to merge my data sources and have the preliminar final dataset of firms with balance sheet and income statements  (SCIE) + skill composition (QP). I want to clean the dataset following standard procedures: remove incorrect values (negatives for variables that should be =>0, etc), extreme observations, etc. I want to follow a parsimonious approach, give me suggestions on the cleaning procedure, given the variables I kept from SCIE (you can find the meaning in the attached excel file).
