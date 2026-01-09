/*==============================================================================
   MODULE 1: CLEAN SCIE DATA (2011-2022)
   
   Processes balance sheet data from SCIE/IES
   Creates: SCIE_2011_2022.dta, IES_age_2011_2022.dta
   
==============================================================================*/

cd "$rootdir/input/SCIE_data"

* Define variables to keep from SCIE
local vars = "ANO NPC_FIC EFJR0 CAE_COD I_NASC SV601201 SV601301 SV601501 SV601701 SV500101 SD000002 SV500601 SV804000 SV804100 SV804200 SV804400 SV804600 SV804700 SV804800 SV806700 SV807100 SV569305 SV500801 SV602500 SV602700 SV603900 SD000102 SV607604 SV501701 SV501801 SV502001 SV502201 SV510101 SV510201 SV510301 SV510401 SV511201 SV512501 SV512601 SV512701 SV514101 SV514301 SV515201 SV516001 SV516101 SD557602 SD557603 SD000031 SD000118 SD000119 SD000047 SD000120 SD000046 SD000121 SD000113 SD000122 SD000123 SD000124 SD000125 SD000126 SD000127 SD000128 SD000032 SD000036 SD000037 SD000038 SV558201 SD558202 SD558203 SD558204 SV558205 SD563301 SD563302 SD563303 SD563304 SD563305 SD563306 SV563307 SD000044 SD000115 SD000011 SD000012 SD000014 SD000016 SD000018 SD000019 SD000005 RND_PER IM_RND_EXPN"

* Load and append all years
forvalues ano=2011(1)2022 {
    local f : dir "." files "SCIE`ano'*"
	local fpath = "./" + `f'
    
    di "  Processing SCIE `ano'..."
    usespss `fpath', clear
    
    keep `vars'
    compress
    
    if `ano' != 2011 append using "$rootdir/output/SCIE_2011_2022.dta"
    save "$rootdir/output/SCIE_2011_2022.dta", replace
}

cd "$rootdir/output"

* Create clean variables
gen ano = real(ANO)
gen caeies = real(CAE_COD)
drop ANO CAE_COD

* Remove missing identifiers
drop if NPC_FIC == . | NPC_FIC == 0

* Remove duplicates (keep first occurrence)
duplicates tag NPC_FIC ano, gen(dup)
tab dup
drop if dup > 0
drop dup

sort NPC_FIC ano
compress
save "SCIE_2011_2022.dta", replace

di "  → SCIE data cleaned: SCIE_2011_2022.dta"

* Create firm age variable from SCIE
use "SCIE_2011_2022.dta", clear
keep NPC_FIC ano I_NASC
gen anoconst = ano if I_NASC == 1
sort NPC_FIC ano
by NPC_FIC: egen ano_constIES = min(anoconst)
drop anoconst
compress
save "IES_age_2011_2022.dta", replace

di "  → Firm age data created: IES_age_2011_2022.dta"
