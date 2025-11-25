/*==============================================================================
   MODULE 2: CLEAN FIRM-LEVEL QP DATA (2011-2022)
   
   Processes firm characteristics from Quadros de Pessoal
   Creates: firm_2011_2022.dta
   
==============================================================================*/

cd "$rootdir/input/QP_data"

* Load and append all years
forvalues ano=2011(1)2022 {
    di "  Processing QP Firms `ano'..."
    
    * Check for .dta first (2011 and 2022), then .sav
    local f : dir "." files "*Empresas_`ano'*.dta"
    if `"`f'"' != "" {
        use `f', clear
    }
    else {
        local f : dir "." files "*Empresas_`ano'*.sav"
        usespss `f', clear
    }
    
    * Standardize variable names (may vary by year)
    cap ren ANO ano
	cap ren ANO_CONST ano_const
    cap ren NUEMP nuemp
    cap ren N_EMP nuemp    
    cap ren natju nat_jur
    cap ren NAT_JURIDICA nat_jur
    cap ren cspub cap_soc_pub 
    cap ren PERC_CAP_PUBL cap_soc_pub 
	cap ren CAE_REV3_2DIG CAE2
    
    * Calculate firm age
    cap gen ano_const = ano - antiguidade if antiguidade != 999
    
    * Get sector code
    cap gen cae3 = real(CAE2)
    
    * Keep relevant variables
    keep ano nuemp EMP_ID ano_const nat_jur ///
         cap_soc_pub NPC_FIC cae3
    
    compress
    
    if `ano' != 2011 append using "$rootdir/output/firm_2011_2022.dta"
    save "$rootdir/output/firm_2011_2022.dta", replace
}

cd "$rootdir/output"

* Create public sector indicator
gen public = 1 if (nat_jur < 20 | nat_jur == 61 | nat_jur == 72 | nat_jur == 73) ///
                  | (cap_soc_pub >= 50 & cap_soc_pub != .)
replace public = 0 if public == .

* Create firm identifier
gen firm_id = EMP_ID
label var firm_id "Firm identifier (EMP_ID)"

* Check for duplicates
duplicates report firm_id ano

* Merge with SCIE data using NPC_FIC
merge 1:1 NPC_FIC ano using "SCIE_2011_2022.dta", keep(match master)
gen scie_match = (_merge == 3)
drop _merge

* Merge with firm age from SCIE
merge m:1 NPC_FIC ano using "IES_age_2011_2022.dta", keep(match master) keepusing(ano_constIES)
drop _merge

* Calculate firm age (use earliest available date)
replace ano_const = . if ano_const == 0
bysort firm_id: egen ano_const_QP = min(ano_const)
bysort firm_id: egen ano_constIES_corr = min(ano_constIES)
bysort firm_id (ano): replace ano_constIES_corr = . if ano_constIES_corr > ano[1]
egen cohort = rowmin(ano_const_QP ano_constIES_corr)
gen firm_age = ano - cohort

drop ano_const_QP ano_constIES_corr I_NASC ano_constIES

* Order variables
order firm_id ano NPC_FIC EMP_ID

compress
save "firm_2011_2022.dta", replace

di "  → Firm data cleaned: firm_2011_2022.dta"
di "  → Observations: " _N
sum scie_match
