*******************************************************
* Elegant Time Series Plots for Portugal and Peers
* Author: Alejandro Vicente
* Date: 2025-11-11
*******************************************************

*--- Setup
clear all
set more off
cd "C:\Users\alexv\OneDrive\Documentos\3-Papers\Intangible_skill_Vicente"  

* Optional: install if missing
cap which grstyle
if _rc ssc install grstyle, replace

* Use a clean minimalistic style
grstyle init
grstyle set plain, horizontal
set scheme s1color

*******************************************************
* 1. Plot Wage Skill Premium (Portugal)
*******************************************************
import excel "C:\Users\alexv\OneDrive\Documentos\3-Papers\Intangible_skill_Vicente\wage_skill_premium_Portugal.xlsx", firstrow clear

* Variable names assumed: year, wage_skill_premium, college_rate
* If they have spaces, rename:

tsset year, yearly

* Plot 1: Wage Skill Premium
twoway (line wage_skill_premium year, lcolor(navy) lwidth(medthick)), ///
     ///
     ///
    ytitle("Premium") xtitle("Year") ///
    graphregion(color(white)) bgcolor(white) ///
    legend(off)
graph export "plot_wage_skill_premium.png", replace width(1600)

*******************************************************
* 2. Plot College Rate (Portugal)
*******************************************************
twoway (line college_rate year, lcolor(maroon) lwidth(medthick)), ///
     ///
     ///
    ytitle("College Rate") xtitle("Year") ///
    graphregion(color(white)) bgcolor(white) ///
    legend(off)
graph export "plot_college_rate.png", replace width(1600)

*******************************************************
* 3. Plot Intangible Capital Across Countries
*******************************************************
import excel "C:\Users\alexv\OneDrive\Documentos\3-Papers\Intangible_skill_Vicente\Intangibles_EUKLEMS_INTANPROD.xlsx", sheet("Hoja2") firstrow clear

tsset year, yearly

* Check variable list
describe

* Plot all countries together
local countries PORTUGAL SPAIN US UK FRANCE GERMANY

twoway ///
    (line PORTUGAL year, lcolor(navy) lwidth(medthick)) ///
    (line SPAIN year, lcolor(maroon) lwidth(medthick) lpattern(dash)) ///
    (line US year, lcolor(forest_green) lwidth(medthick)) ///
    (line UK year, lcolor(orange) lwidth(medthick) lpattern(shortdash)) ///
    (line FRANCE year, lcolor(black) lwidth(medthick) lpattern(dot)) ///
    (line GERMANY year, lcolor(gs6) lwidth(medthick) lpattern(dash_dot)), ///
     ///
    ///
    ytitle("S/K") xtitle("Year") ///
    legend(order(1 "Portugal" 2 "Spain" 3 "US" 4 "UK" 5 "France" 6 "Germany") ///
           ring(0) pos(11) rows(2)) ///
    graphregion(color(white)) bgcolor(white)
graph export "plot_intangibles_comparison.png", replace width(1600)

*******************************************************
* End of file
*******************************************************
display "All three plots have been successfully exported as PNG files."
