/*==============================================================================
   PREPARE RISK-FREE RATE DATA
   
   This file imports Portuguese 10-year government bond yields from FRED
   and prepares them for merging with the main dataset.
   
   Source: FRED series IRLTLT01PTM156N
   Output: rfr.dta (ready to merge with main dataset)
   
==============================================================================*/

clear all
set more off

cd "$rootdir/input"

* Start log
cap log close
log using "rfr_preparation.log", replace

di "=========================================="
di "PREPARING RISK-FREE RATE DATA"
di "Date: `c(current_date)'"
di "=========================================="

/*==============================================================================
   IMPORT PORTUGUESE 10-YEAR GOVERNMENT BOND YIELDS FROM FRED
==============================================================================*/

di _newline
di "Importing Portuguese 10-year bond yields from FRED..."
di "  Series: IRLTLT01PTM156N"
di ""

* Import the CSV file
import delimited "IRLTLT01PTM156N.csv", clear varnames(1) encoding("UTF-8")

* Extract year from date string (format: YYYY-MM-DD)
gen year_str = substr(observation_date, 1, 4)
gen ano = real(year_str)
drop observation_date year_str

* Rename interest rate variable to rfr
* Adjust the variable name to match your CSV column header
rename irltlt01ptm156n rfr

* Keep only relevant years
keep ano rfr
keep if ano >= 2011 & ano <= 2022

* Ensure rfr is numeric
destring rfr, replace force

* Check for missing values
count if missing(rfr)
if r(N) > 0 {
    di as error "  WARNING: " r(N) " missing values in rfr"
    di as error "  Years with missing rfr:"
    list ano rfr if missing(rfr), clean noobs
}

* Sort by year
sort ano

label var rfr "Risk-free rate (10-year PT gov bond, %)"
label var ano "Year"

di "  ✓ Risk-free rate imported (2011-2022)"
di ""
di "  Portuguese 10-year bond yields (%):"
list ano rfr, sep(0) clean noobs

* Save dataset
save "rfr.dta", replace

di ""
di "=========================================="
di "RISK-FREE RATE DATA READY"
di "=========================================="
di "  Saved: rfr.dta"
di "  Years: 2011-2022"
di "=========================================="

cap log close