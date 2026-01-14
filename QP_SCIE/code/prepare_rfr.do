/*==============================================================================
   PREPARE RISK-FREE RATE DATA

   Imports Portuguese 10-year government bond yields from FRED.
   Used to construct credit spreads for financial constraint analysis.

   Input:  IRLTLT01PTM156N.csv (FRED)
   Output: rfr.dta (2011-2022)

==============================================================================*/

clear all
set more off
cd "$rootdir/input"

cap log close
log using "rfr_preparation.log", replace

import delimited "IRLTLT01PTM156N.csv", clear varnames(1) encoding("UTF-8")

* Extract year
gen ano = real(substr(observation_date, 1, 4))
drop observation_date

rename irltlt01ptm156n rfr
destring rfr, replace force

keep ano rfr
keep if ano >= 2011 & ano <= 2022

sort ano

label var rfr "Risk-free rate (10-year PT gov bond, %)"
label var ano "Year"

save "rfr.dta", replace

cap log close
