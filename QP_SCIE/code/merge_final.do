/*==============================================================================
   MERGE ALL DATASETS (2011-2022)

   Combines firm characteristics, SCIE balance sheet data, and worker skills.
   Creates final merged dataset ready for cleaning and variable construction.

   Input:  firm_skills_wages_2011_2022.dta, firm_2011_2022.dta
   Output: final_data_2011_2022.dta

==============================================================================*/

clear all
set more off
cd "$rootdir/output"

cap log close
log using "merge_final.log", replace

* Start with firm-level skill data
use "firm_skills_wages_2011_2022.dta", clear

* Merge firm characteristics
merge 1:1 firm_id ano using "firm_2011_2022.dta", keep(match master)
gen has_firm_data = (_merge == 3)
drop _merge

label var has_firm_data "Matched with firm characteristics"
label var scie_match "Matched with SCIE balance sheet data"

compress
save "final_data_2011_2022.dta", replace

cap log close
