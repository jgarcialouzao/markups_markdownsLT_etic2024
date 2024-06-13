






**********************************************************************************
********************         Regression analysis                ********************

** Decomposition of markups and markdowns across sectors, time, and firms.


use ${path}\Data_stata\finaldata_TL_LL_purch.dta, clear


keep markup markdown NACE2 year

replace markup = ln(markup)	
replace markdown = ln(markdown)

** Fixed-effect regressions

** Markups 
* Over the whole sample
	sum markup 
    gen sdmu = `r(Var)'

* Within/Between firms
    reghdfe markup, abs(sector = NACE2 time = year) resid
    predict mu_fwithin if e(sample), res	
    qui sum sector 
    gen sdmu_fbetween_sector = `r(Var)'
	qui sum time 
    gen sdmu_fbetween_year = `r(Var)'
	qui sum mu_fwithin
    gen sdmu_fwithin = `r(Var)'
	drop sector time

sum sdmu*

** Markdowns 
* Over the whole sample
	sum markdown 
    gen sdnu = `r(Var)'

* Within/Between firms
    reghdfe markdown, abs(sector = NACE2 time = year) resid
    predict nu_fwithin if e(sample), res
    qui sum sector 
    gen sdnu_fbetween_sector = `r(Var)'
	qui sum time 
    gen sdnu_fbetween_year = `r(Var)'
	qui sum nu_fwithin
    gen sdnu_fwithin = `r(Var)'
	drop sector time

sum sdnu*

keep sd*
order sdmu sdmu_fbetween* sdmu_fwithin sdnu sdnu_fbetween* sdnu_fwithin
keep if _n == 1

save ${path}\tables\between_within_decomposition.dta, replace


** Firm-level regressions program
program define prog_het_regression

							syntax [, filename(string)]

use ${path}\Data_stata\finaldata_`filename'.dta, clear


*Winsorize markup and markdown distribution within industries by winsorizing components, keep consistency for decomposition
foreach v in e_varinp_y e_l_y alpha_varinp alpha_l {
qui bys NACE2: egen `v'_p1   = pctile(`v'), p(2)
replace `v' = `v'_p1 if `v'<`v'_p1
qui bys NACE2: egen `v'_p99  = pctile(`v'), p(98)
replace `v' = `v'_p99 if `v'>`v'_p99
}

*  Mark-up
gen markup_varinp = e_varinp_y/alpha_varinp

*  Mark-down labor
gen markdown_l = (1/markup_varinp)*(e_l_y/alpha_l)


** Keep relevant variables and label them
keep  year id NACE2 nace_r2_code y y_c tfp l k varinp e_varinp_y e_l_y e_k_y alpha_l alpha_varinp alpha_k alpha_o markup_varinp markdown_l pirate* EXP IMP empl year_birth year_death type own beta* sales_share emp_share
order year id NACE2 nace_r2_code y y_c tfp l k varinp e_varinp_y e_l_y e_k_y alpha_l alpha_varinp alpha_k alpha_o markup_varinp markdown_l  pirate* EXP IMP empl year_birth year_death type own beta*

label var y                "Sales"
label var y_c              "Sales (corr. measurement error)"
label var tfp              "Productivity"
label var l                "Cost of employment (wL)"
label var k                "Fixed tangible assets"
label var varinp           "Variable input"
label var e_varinp_y       "Variable input-Sales Elasticity"
label var e_l_y            "Labor-Sales Elasticity"
label var e_k_y            "Capital-Sales Elasticity"
label var alpha_l          "Labor cost share of Sales (corr)"
label var alpha_varinp     "Variable input cost share of Sales (corr)"
label var alpha_k          "Capital cost share of Sales (corr)"
label var alpha_o          "Operational cost share of Sales (corr)"
label var markup_varinp    "Markup based on varinp"
label var markdown_l       "Markdown labor"
label var pirate_gross     "Profit rate (Gross PI/Sales)"
label var pirate_ebitda    "Profit rate (Ebitda/Sale)"
*/
	
replace markup = ln(markup)	
replace markdown = ln(markdown)


* Age
qui gen age = year - year_birth
gen young = age<5
*Firms with at least one-fifth of the market in sales -- proxy of product market power
gen pmpower = sales_share>0.1

*Firms with at least one-fifth of the market of employment -- proxy of labor market power
gen lmpower = emp_share>0.1

* Labor costs represents at least 50% of sales
gen l_share = alpha_l>0.5


*Export share of sales
gen exp_share = (EXP)/exp(y) if EXP>0
replace exp_share = 0 if exp_share==.
*replace exp_share = . if exp_share>1
gen exporter = exp_share>0.1 & exp_share<.

*Export share of sales
gen imp_share = (IMP)/exp(y) if IMP>0
replace imp_share = 0 if imp_share==.
*replace imp_share = . if exp_share>1
gen importer  = imp_share>0.1 & imp_share<.

* Foreign ownership 
gen foreign = own>=22


** Relationship between markups and markdowns 
*ssc install binscatter, replace
binscatterhist markup markdown, controls(i.year) absorb(NACE2) xtitle("(log) Markdowns") ytitle("(log) Markups") n(100) 

qui graph export ${path}\figures\nu_mu_corr.pdf, as(pdf) replace

binscatterhist markup tfp , controls(i.year) absorb(NACE2) ytitle("(log) Markups") xtitle("(log) TFP") n(100) 

qui graph export ${path}\figures\mu_tfp_corr.pdf, as(pdf) replace

binscatterhist markdown tfp  , controls(i.year) absorb(NACE2) ytitle("(log) Markdowns") xtitle("(log) TFP")  n(100) 

qui graph export ${path}\figures\nu_tfp_corr.pdf, as(pdf) replace


*** Markups regression
foreach x in pmpower lmpower l_share young exporter importer foreign {
preserve 
gen var = "`x'"

gen bmu_corr_2  = . 
gen semu_corr_2 = .
gen r2a_corr_2  = .
reghdfe markup `x'              	, absorb(NACE2#year) cluster(id) keepsing
replace bmu_corr_2  = _b[`x' ]  
replace semu_corr_2 = _se[`x' ] 
replace r2a_corr_2  = `e(r2_a)'

gen bmu_tfp = . 
gen semu_tfp = .
gen r2a_tfp = .	
*wildbootstrap areg markup `x' tfp	         , absorb(group) cluster(id) rseed(12345) reps(100)
reghdfe markup `x' tfp	         , absorb(NACE2#year) cluster(id) keepsing
replace bmu_tfp  = _b[`x' ]  
replace semu_tfp = _se[`x' ] 
replace r2a_tfp  = `e(r2_a)'

keep if bmu_corr_2 !=.
keep if _n == 1
keep var bmu* semu* r2*
tempfile data`x'
save `data`x''
restore

}


preserve 
clear
foreach x in pmpower lmpower l_share young exporter importer foreign {
append using `data`x''
}
order var
save ${path}\tables\regestimatesMU_`filename'.dta, replace
restore




foreach x in pmpower lmpower l_share young exporter importer foreign  {
preserve 
gen var = "`x'"

gen bnu_corr_2  = . 
gen senu_corr_2 = .
gen r2a_corr_2  = .
reghdfe markdown `x'              	, absorb(NACE2#year) cluster(id) keepsing
replace bnu_corr_2  = _b[`x' ]  
replace senu_corr_2 = _se[`x' ] 
replace r2a_corr_2  = `e(r2_a)'

gen bnu_tfp = . 
gen senu_tfp = .
gen r2a_tfp = .	
*wildbootstrap areg markdown  `x' tfp	         , absorb(group) cluster(id) rseed(12345) reps(100)
reghdfe markdown `x' tfp	           , absorb(NACE2#year) cluster(id) keepsing
replace bnu_tfp  = _b[`x' ]  
replace senu_tfp = _se[`x' ] 
replace r2a_tfp  = `e(r2_a)'


gen bnu_tfp_mu = . 
gen senu_tfp_mu = .
gen r2a_tfp_mu = .	
*wildbootstrap areg markdown  `x' tfp markup	         , absorb(group) cluster(id) rseed(12345) reps(100)
reghdfe markdown `x' tfp markup	           , absorb(NACE2#year) cluster(id) keepsing
replace bnu_tfp_mu  = _b[`x' ]  
replace senu_tfp_mu = _se[`x' ] 
replace r2a_tfp_mu  = `e(r2_a)'


keep if bnu_corr_2 !=.
keep if _n == 1
keep var bnu* senu* r2*
tempfile data`x'
save `data`x''
restore

}


preserve 
clear
foreach x in pmpower lmpower l_share young exporter importer  foreign {
append using `data`x''
}
order var
save ${path}\tables\regestimatesNU_`filename'.dta, replace
restore



end 

