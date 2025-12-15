/*==============================================================================
   PREPARE DEFLATORS FOR CLEANING
   
   This file imports and processes all deflators needed for the analysis:
   1. GDP deflator (from FRED)   
   2. Capital deflators (from EU KLEMS-INTAN)
   
   Output: deflators.dta (ready to merge with main dataset)
   
==============================================================================*/

clear all
set more off

cd "$rootdir/input"

* Start log
cap log close
log using "deflator_preparation.log", replace

di "=========================================="
di "PREPARING DEFLATORS"
di "Date: `c(current_date)'"
di "=========================================="

/*==============================================================================
   STEP 1: IMPORT GDP DEFLATOR (.CSV FROM FRED)
==============================================================================*/

di _newline
di "STEP 1: Importing GDP deflator from FRED..."

* Import the CSV file
import delimited "PRTGDPDEFQISMEI_NBD20200101.csv", clear varnames(1)

* Rename variables to standard names
rename observation_date date_string
rename prtgdpdefqismei_nbd20200101 gdp_deflator

* Extract year from date string
gen year_str = substr(date_string, 1, 4)
gen ano = real(year_str)
drop date_string year_str

keep ano gdp_deflator
keep if ano >= 2011 & ano <= 2022
sort ano

label var gdp_deflator "GDP deflator (2020=100)"
label var ano "Year"

di "  ✓ GDP deflator imported (2011-2022)"
list, sep(0)

* Save temporary file
save "deflators_temp.dta", replace

/*==============================================================================
   STEP 2: IMPORT DATA FOR CAPITAL (.XLSX FROM EUKLEMS-INTANPROD)
==============================================================================*/

* Import GFCF tangible (Ip_Tang, row 57, columns V-AF for 2011-2021)
import excel "PT_intangible analytical.xlsx", sheet("Ip_Tang") cellrange(V57:AF57) clear
xpose, clear varname
gen ano = 2010 + _n
rename v1 gfcf_deflator
destring gfcf_deflator, replace
keep ano gfcf_deflator
label var gfcf_deflator "GFCF deflator (2020=100)"
tempfile gfcf
save `gfcf'

* Import Capital nominal (K_Tang, row 57, columns V-AF for 2011-2021)
import excel "PT_intangible analytical.xlsx", sheet("K_Tang") cellrange(V57:AF57) clear
xpose, clear varname
gen ano = 2010 + _n
rename v1 k_nom
destring k_nom, replace
tempfile k_nom
save `k_nom'

* Import Capital volume (Kq_Tang, row 57, columns V-AF for 2011-2021)
import excel "PT_intangible analytical.xlsx", sheet("Kq_Tang") cellrange(V57:AF57) clear
xpose, clear varname
gen ano = 2010 + _n
rename v1 k_vol
destring k_vol, replace

* Merge and calculate capital deflator
merge 1:1 ano using `k_nom', nogen
gen capital_deflator = (k_nom / k_vol) * 100
keep ano capital_deflator
label var capital_deflator "Capital deflator (2020=100)"

/*==============================================================================
   STEP 3: MERGE ALL
==============================================================================*/

merge 1:1 ano using `gfcf', nogen
merge 1:1 ano using "deflators_temp.dta"
keep if _merge == 2 | _merge == 3
drop _merge
sort ano

order ano gdp_deflator gfcf_deflator capital_deflator

/*------------------------------------------------------------------------------
   EXTRAPOLATE 2022 for GFCF and Capital deflators
------------------------------------------------------------------------------*/

di _newline
di "Extrapolating 2022 deflators..."

* Calculate 2021-2022 growth rate from GDP deflator
qui sum gdp_deflator if ano == 2021
local gdp_2021 = r(mean)
qui sum gdp_deflator if ano == 2022
local gdp_2022 = r(mean)
local gdp_growth = (`gdp_2022' / `gdp_2021') - 1

di "  GDP growth 2021-2022: " %5.2f (100*`gdp_growth') "%"

* Extrapolate GFCF deflator
qui sum gfcf_deflator if ano == 2021
local gfcf_2021 = r(mean)
replace gfcf_deflator = `gfcf_2021' * (1 + `gdp_growth') if ano == 2022
qui sum gfcf_deflator if ano == 2022
di "  GFCF 2022 (extrapolated): " %6.2f r(mean)

* Extrapolate Capital deflator
qui sum capital_deflator if ano == 2021
local cap_2021 = r(mean)
replace capital_deflator = `cap_2021' * (1 + `gdp_growth') if ano == 2022
qui sum capital_deflator if ano == 2022

order ano gdp_deflator gfcf_deflator capital_deflator

save "deflators.dta", replace

di _newline
di "=========================================="
di "DEFLATORS READY (2020=100)"
di "=========================================="
list, sep(0)

cap log close

* Clean up temporary files
cap erase "deflators_temp.dta"
