


**********************************************************************************
********************          Markups Evidence                ********************



************** Cross-section dispersion

use ${path}\Data_stata\finaldata_TL_LL_purch.dta, clear


keep id year markup*
rename markup_varinp markup_tl 

qui grstyle init
qui grstyle set plain, compact grid dotted /*no grid*/
qui grstyle color major_grid gs13
qui grstyle set symbol
qui grstyle set lpattern
qui grstyle set color Dark2, n(2) 
qui grstyle set color Dark2, n(2)  opacity(34): p#markfill


tw (kdensity markup_tl if year==2004) (kdensity markup_tl if year==2018) if markup<=4, ytitle("Density") xtitle("Markups") ylabel(0(1)5) xline(1.10, lcolor(green%80)) xline(1.14, lcolor(orange%80) lpattern(dash))  legend(label(1 "2004") label( 2 "2018"))

qui graph export ${path}\figures\mu_dispersion.pdf, as(pdf) replace




        ** Dispersion of marups over time

use ${path}\Data_stata\finaldata_TL_LL_purch.dta, clear

forvalues t = 2004(4)2016 {
preserve 
keep if year>=`t' & year<=`t'+3
	** Variance decomposition - by sub-periods
* log terms 	
foreach v in e_varinp_y alpha_varinp markup_varinp  {
gen ln`v' = ln(`v')	
}


* variance terms 
foreach v in  e_varinp_y alpha_varinp markup_varinp  {
qui sum ln`v'
gen v_ln`v' = `r(Var)'	
}

label var v_lne_varinp_y "var(e_c)"
label var v_lnalpha_varinp "var(alpha_c)"
label var v_lnmarkup_varinp "var(mu)"


* covariance terms
* cov e_c_y alpha_c
qui corr lne_varinp_y lnalpha_varinp, covariance
gen cov_ec_alphac = -2*`r(cov_12)'
label var cov_ec_alphac  "-2 * cov(e_c,alpha_c)"

keep v_* cov_*
keep if _n == 1

** contribution of markups terms: var(mu) = var(e_c) + var(alpha_c) - 2*cov(e_c,alpha_c)
foreach x in v_lnmarkup_varinp v_lne_varinp_y v_lnalpha_varinp cov_ec_alphac {
gen con_`x' = `x'/v_lnmarkup_varinp
}
keep v_lnmarkup_varinp v_lne_varinp_y v_lnalpha_varinp cov_ec_alphac con_*
order  v_lnmarkup_varinp v_lne_varinp_y v_lnalpha_varinp cov_ec_alphac  con_v_lnmarkup_varinp con_v_lne_varinp_y con_v_lnalpha_varinp con_cov_ec_alphac 

save ${path}\tables\decomposition_mu_`t'.dta, replace
restore 
}





