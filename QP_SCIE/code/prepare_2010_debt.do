/*==============================================================================
   PREPARE 2010 DEBT FOR LAG STRUCTURE
   
   Extracts debt variables from 2010 SCIE to enable interest rate calculation
   in 2011 (which requires lagged debt).
   
   Output: debt_2010.dta
   
==============================================================================*/

clear all
set more off

cd "$rootdir/input/SCIE_data"

di "=========================================="
di "EXTRACTING 2010 DEBT DATA"
di "=========================================="
di ""

* Find and load SCIE 2010
local f : dir "." files "SCIE2010*"
local fpath = "./" + `f'

di "Loading SCIE 2010..."
usespss `fpath', clear

* Keep only what we need: identifier + debt variables
keep NPC_FIC SV514301 SV515201

* Remove missing identifiers
drop if NPC_FIC == . | NPC_FIC == 0

* Create total debt for 2010
gen debt_2010 = SV514301 + SV515201 if !missing(SV514301) & !missing(SV515201)

* Keep only firm ID and total debt
keep NPC_FIC debt_2010

* Label
label var debt_2010 "Total debt in 2010 (for 2011 lag, nominal)"

* Remove duplicates
duplicates drop NPC_FIC, force

compress
save "$rootdir/output/debt_2010.dta", replace

di ""
di "  ✓ 2010 debt data saved: debt_2010.dta"
di "     Firms: " _N
di ""
di "This will be merged to provide lagged debt for 2011 interest rate calculation"
di "=========================================="