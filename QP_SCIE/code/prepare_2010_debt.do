/*==============================================================================
   EXTRACT 2010 DEBT FOR LAG STRUCTURES

   Extracts debt from 2010 SCIE to enable interest rate calculation in 2011.

   Input:  SCIE_data/SCIE2010_SEC2010_*.sav
   Output: debt_2010.dta

==============================================================================*/

clear all
set more off
cd "$rootdir/input/SCIE_data"

cap log close
log using "$rootdir/output/debt_2010_prep.log", replace

* Find 2010 SCIE file
local f : dir "." files "SCIE2010*"
local fpath = "./" + `f'

usespss `fpath', clear

* Extract debt variables
keep NPC_FIC SV514301 SV515201
drop if NPC_FIC == . | NPC_FIC == 0

* Total debt for 2010
gen debt_2010 = SV514301 + SV515201 if !missing(SV514301, SV515201)
keep NPC_FIC debt_2010

label var debt_2010 "Total debt 2010 (for 2011 lag, nominal)"

duplicates drop NPC_FIC, force
compress

save "$rootdir/output/debt_2010.dta", replace

cap log close
