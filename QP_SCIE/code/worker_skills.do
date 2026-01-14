/*==============================================================================
   PROCESS WORKER DATA AND CALCULATE SKILLS (2011-2022)

   Aggregates worker-level data to firm-year level.
   Constructs skill composition measures and average wages by skill group.

   SKILL DEFINITION: Skilled = tertiary education (habil1 >= 6)
   - habil1: 1-5 (non-tertiary), 6-8 (tertiary), 9 (missing)

   Input:  QP_data/QP_Trabalhadores_{year}_*.{dta,sav}, firm_2011_2022.dta
   Output: worker_skills_2011_2022.dta, firm_skills_wages_2011_2022.dta

==============================================================================*/

clear all
set more off
cd "$rootdir/input/QP_data"

cap log close
log using "$rootdir/output/worker_skills.log", replace

* Initialize
clear
gen ano = .
save "$rootdir/output/worker_skills_2011_2022.dta", replace

* Append all years
forvalues ano = 2011/2022 {
    * Check for .dta first (2011, 2022), then .sav
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

    keep ano nuemp nuest age NPC_FIC EMP_ID ntrab habil1 rbase prest_reg

    append using "$rootdir/output/worker_skills_2011_2022.dta"
    compress
    save "$rootdir/output/worker_skills_2011_2022.dta", replace
}

cd "$rootdir/output"
drop if ano == .

* Calculate wages
egen nominal_wage = rowtotal(rbase prest_reg)
replace nominal_wage = . if nominal_wage == 0
drop if nominal_wage == . | nominal_wage <= 0

* Treat habil1 = 9 as missing
replace habil1 = . if habil1 == 9
drop if habil1 == . | habil1 == 0

* Get firm identifier (merge with firm data if needed)
cap confirm variable EMP_ID
if _rc == 0 {
    gen has_empid = (EMP_ID != .)
    qui sum has_empid
    if r(mean) < 0.5 {
        merge m:1 nuemp ano using "firm_2011_2022.dta", keepusing(EMP_ID) keep(match master) nogen
    }
    drop has_empid
}
else {
    merge m:1 nuemp ano using "firm_2011_2022.dta", keepusing(EMP_ID) keep(match master) nogen
}

gen firm_id = EMP_ID
label var firm_id "Firm identifier"
drop if firm_id == .

compress
save "worker_skills_2011_2022.dta", replace

* Aggregate to firm-year level
gen skilled = (habil1 >= 6) if habil1 != .
gen unskilled = (habil1 < 6) if habil1 != .
gen wage_skilled = nominal_wage if skilled == 1
gen wage_unskilled = nominal_wage if unskilled == 1

collapse (count) n_workers = ntrab ///
         (sum) n_skilled = skilled n_unskilled = unskilled ///
         (mean) avg_wage_all = nominal_wage ///
                avg_wage_skilled = wage_skilled ///
                avg_wage_unskilled = wage_unskilled, ///
         by(firm_id ano)

gen share_skilled = n_skilled / n_workers
gen share_unskilled = n_unskilled / n_workers

label var n_workers "Total workers"
label var n_skilled "Workers with tertiary education (habil1>=6)"
label var n_unskilled "Workers without tertiary education"
label var avg_wage_all "Average wage (all)"
label var avg_wage_skilled "Average wage (tertiary)"
label var avg_wage_unskilled "Average wage (non-tertiary)"
label var share_skilled "Share of workers with tertiary education"
label var share_unskilled "Share of workers without tertiary"

compress
save "firm_skills_wages_2011_2022.dta", replace

cap log close
