/*==============================================================================
   PREPARE DEFLATORS

   Imports GDP deflator (FRED) and capital deflators (EU KLEMS-INTAN).
   Extrapolates 2022 values using GDP deflator growth.

   Input:  PRTGDPDEFQISMEI_NBD20200101.csv, PT_intangible analytical.xlsx
   Output: deflators.dta (2011-2022, base year 2020=100)

==============================================================================*/

clear all
set more off
cd "$rootdir/input"

cap log close
log using "deflator_preparation.log", replace

* GDP deflator from FRED
import delimited "PRTGDPDEFQISMEI_NBD20200101.csv", clear varnames(1)
rename observation_date date_string
rename prtgdpdefqismei_nbd20200101 gdp_deflator
gen ano = real(substr(date_string, 1, 4))
keep ano gdp_deflator
keep if ano >= 2011 & ano <= 2022
sort ano
label var gdp_deflator "GDP deflator (2020=100)"
save "deflators_temp.dta", replace

* GFCF deflator from EU KLEMS
import excel "PT_intangible analytical.xlsx", sheet("Ip_Tang") cellrange(V57:AF57) clear
xpose, clear varname
gen ano = 2010 + _n
rename v1 gfcf_deflator
destring gfcf_deflator, replace
keep ano gfcf_deflator
label var gfcf_deflator "GFCF deflator (2020=100)"
tempfile gfcf
save `gfcf'

* Capital deflator from EU KLEMS (nominal/volume)
import excel "PT_intangible analytical.xlsx", sheet("K_Tang") cellrange(V57:AF57) clear
xpose, clear varname
gen ano = 2010 + _n
rename v1 k_nom
destring k_nom, replace
tempfile k_nom
save `k_nom'

import excel "PT_intangible analytical.xlsx", sheet("Kq_Tang") cellrange(V57:AF57) clear
xpose, clear varname
gen ano = 2010 + _n
rename v1 k_vol
destring k_vol, replace
merge 1:1 ano using `k_nom', nogen
gen capital_deflator = (k_nom / k_vol) * 100
keep ano capital_deflator
label var capital_deflator "Capital deflator (2020=100)"

* Merge all deflators
merge 1:1 ano using `gfcf', nogen
merge 1:1 ano using "deflators_temp.dta", keep(2 3) nogen
sort ano
order ano gdp_deflator gfcf_deflator capital_deflator

* Extrapolate 2022 using GDP deflator growth
qui sum gdp_deflator if ano == 2021
local gdp_2021 = r(mean)
qui sum gdp_deflator if ano == 2022
local gdp_2022 = r(mean)
local gdp_growth = (`gdp_2022' / `gdp_2021') - 1

qui sum gfcf_deflator if ano == 2021
replace gfcf_deflator = r(mean) * (1 + `gdp_growth') if ano == 2022

qui sum capital_deflator if ano == 2021
replace capital_deflator = r(mean) * (1 + `gdp_growth') if ano == 2022

save "deflators.dta", replace

cap log close
cap erase "deflators_temp.dta"
