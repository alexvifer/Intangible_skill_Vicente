/*==============================================================================
   MODULE 3: PROCESS WORKER DATA AND CALCULATE SKILLS (2011-2022)
   
   Processes worker-level data and creates skill composition measures
   
   EDUCATION LEVELS (habil1):
   1 = No schooling
   2 = Primary (4 years)
   3 = Lower secondary (6 years)
   4 = Upper secondary (9 years)
   5 = Secondary (12 years)
   6 = Bacharelato (tertiary)
   7 = Licenciatura (tertiary)
   8 = Masters/PhD (tertiary)
   9 = IGNORADO (unknown) → treated as missing
   
   SKILL DEFINITION: Skilled = habil1 >= 6 (Tertiary education)
   TO CHANGE: Modify "gen skilled = (habil1 >= 6)" in collapse section
   
   Creates: worker_skills_2011_2022.dta, firm_skills_wages_2011_2022.dta
   
==============================================================================*/

cd "$rootdir/input/QP_data"

* Initialize output file
clear
gen ano = .
save "$rootdir/output/worker_skills_2011_2022.dta", replace

* Load and append all years
forvalues ano=2011(1)2022 {
    di "  Processing QP Workers `ano'..."
    
    * Check for .dta first (2011 and 2022), then .sav
    local f : dir "." files "*Trabalhadores_`ano'*.dta"
    if `"`f'"' != "" {
        use `f', clear
    }
    else {
        local f : dir "." files "*Trabalhadores_`ano'*.sav"
        usespss `f', clear
    }
    
    * Standardize variable names
    cap ren ANO ano
    cap ren NUEMP nuemp
    cap ren NUEST nuest
    
    * Age (handle top/bottom coding)
    cap gen age = real(idade) 
    cap replace age = 68 if idade == ">=68"
    cap replace age = 17 if idade == "<=17"
    cap drop if age == . | age == 0
    
    * Keep only relevant variables for skill analysis
    keep ano nuemp nuest age NPC_FIC EMP_ID ntrab habil1 rbase prest_reg
    
    append using "$rootdir/output/worker_skills_2011_2022.dta"
    compress
    save "$rootdir/output/worker_skills_2011_2022.dta", replace
}

cd "$rootdir/output"

drop if ano == .

/*==============================================================================
   CALCULATE WAGES
==============================================================================*/

* Total nominal wage (base + regular benefits)
egen nominal_wage = rowtotal(rbase prest_reg)
replace nominal_wage = . if nominal_wage == 0

* Drop observations with missing wage or education
drop if nominal_wage == . | nominal_wage <= 0

* Treat habil1 = 9 as missing (IGNORADO)
replace habil1 = . if habil1 == 9
drop if habil1 == . | habil1 == 0

/*==============================================================================
   GET FIRM IDENTIFIER
==============================================================================*/

* If EMP_ID missing, try to get from firm data
cap confirm variable EMP_ID
if _rc == 0 {
    gen has_empid = (EMP_ID != .)
    sum has_empid
    if r(mean) < 0.5 {
        di "  Warning: Many missing EMP_ID, attempting merge with firm data..."
        merge m:1 nuemp ano using "firm_2011_2022.dta", keepusing(EMP_ID) keep(match master) nogen
    }
}
else {
    di "  EMP_ID not found, merging with firm data..."
    merge m:1 nuemp ano using "firm_2011_2022.dta", keepusing(EMP_ID) keep(match master) nogen
}

* Create firm identifier
gen firm_id = EMP_ID
label var firm_id "Firm identifier"

* Drop if missing firm ID
drop if firm_id == .

compress
save "worker_skills_2011_2022.dta", replace

di "  → Worker data cleaned: worker_skills_2011_2022.dta"
di "  → Observations: " _N

/*==============================================================================
   COLLAPSE TO FIRM-YEAR LEVEL
==============================================================================*/

di "  → Aggregating to firm-year level..."

* Generate skill dummies from habil1 (tertiary = 6,7,8,9)
gen skilled = (habil1 >= 6) if habil1 != .
gen unskilled = (habil1 < 6) if habil1 != .

* Create wage variables by education level
gen wage_skilled = nominal_wage if skilled == 1
gen wage_unskilled = nominal_wage if unskilled == 1

* Collapse to firm-year level
collapse (count) n_workers = ntrab ///
         (sum) n_skilled = skilled ///
               n_unskilled = unskilled ///
         (mean) avg_wage_all = nominal_wage ///
                avg_wage_skilled = wage_skilled ///
                avg_wage_unskilled = wage_unskilled ///         
         , by(firm_id ano)

* Calculate shares
gen share_skilled = n_skilled / n_workers
gen share_unskilled = n_unskilled / n_workers

* Label variables
label var n_workers "Total number of workers"
label var n_skilled "Number of workers with tertiary education (habil1>=6)"
label var n_unskilled "Number of workers without tertiary education (habil1<6)"
label var avg_wage_all "Average wage (all workers)"
label var avg_wage_skilled "Average wage (tertiary education)"
label var avg_wage_unskilled "Average wage (non-tertiary)"
label var share_skilled "Share of workers with tertiary education"
label var share_unskilled "Share of workers without tertiary education"

compress
save "firm_skills_wages_2011_2022.dta", replace

di "  → Firm-level skills data created: firm_skills_wages_2011_2022.dta"
di "  → Firm-year observations: " _N

