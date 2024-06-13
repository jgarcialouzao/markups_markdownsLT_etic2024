


**********************************************************************************
********************          Markdowns Evidence                ********************

************** Cross-section dispersion

use ${path}\Data_stata\finaldata_TL_LL_purch.dta, clear


qui grstyle init
qui grstyle set plain, compact grid dotted /*no grid*/
qui grstyle color major_grid gs13
qui grstyle set symbol
qui grstyle set lpattern
qui grstyle set color Dark2, n(2) 
qui grstyle set color Dark2, n(2)  opacity(34): p#markfill

tw (kdensity markdown if year==2004) (kdensity markdown if year==2018) if markdown<=6, ytitle("Density") xtitle("Markdowns") ylabel(0(0.2)1.6)  xline(1.35, lcolor(green%80)) xline(1.33, lcolor(orange%80) lpattern(dash))  legend(label(1 "2004") label( 2 "2018")) xlabel(0(1)6)

qui graph export ${path}\figures\nu_dispersion.pdf, as(pdf) replace


        ** Dispersion of marups over time

use ${path}\Data_stata\finaldata_TL_LL_purch.dta, clear

forvalues t = 2004(4)2016 {
preserve 
keep if year>=`t' & year<=`t'+3
	** Variance decomposition - by sub-periods
foreach v in e_l_y e_varinp_y alpha_l alpha_varinp markup_varinp markdown_l {
gen ln`v' = ln(`v')	
}

* variance terms 
foreach v in e_l_y e_varinp_y alpha_l alpha_varinp markup_varinp markdown_l {
qui sum ln`v'
gen v_ln`v' = `r(Var)'	
}
label var v_lne_l_y "var(e_l)"
label var v_lne_varinp_y "var(e_c)"
label var v_lnalpha_varinp "var(alpha_c)"
label var v_lnalpha_l "var(alpha_l"
label var v_lnmarkup_varinp "var(mu)"
label var v_lnmarkdown_l "var(nu)"

* covariance terms
*cov e_l_y alpha_l
qui corr lne_l_y lnalpha_l, covariance
gen cov_el_alphal = -2*`r(cov_12)'
label var cov_el_alphal "-2 * cov(e_l,alpha_l)"

*cov e_l_y mu
qui corr lne_l_y lnmarkup_varinp, covariance
gen cov_el_mu = -2*`r(cov_12)'
label var cov_el_mu "-2 * cov(e_l,mu)"

*cov alpha_l mu
qui corr lnalpha_l lnmarkup_varinp, covariance
gen cov_mu_alphal = 2*`r(cov_12)'
label var cov_mu_alphal   "2 * cov(mu,alpha_l)"
	
keep v_* cov_*
keep if _n == 1


** contribution of markdown terms: var(nu) = var(e_l) + var(alpha_l) + var(mu) - 2*cov(e_l,alpha_l) - 2*cov(e_l,mu) + 2*cov(alpha_l,mu)
foreach x in v_lnmarkdown_l v_lne_l_y v_lnalpha_l v_lnmarkup_varinp cov_el_alphal cov_el_mu cov_mu_alphal {
 gen con_`x' = `x'/v_lnmarkdown_l
}	
keep v_lnmarkdown_l v_lne_l_y v_lnalpha_l v_lnmarkup_varinp cov_el_alphal cov_el_mu cov_mu_alphal con_*
order  v_lnmarkdown_l v_lne_l_y v_lnalpha_l v_lnmarkup_varinp cov_el_alphal cov_el_mu cov_mu_alphal  con_v_lnmarkdown_l con_v_lne_l_y con_v_lnalpha_l con_v_lnmarkup_varinp con_cov_el_alphal con_cov_el_mu con_cov_mu_alphal
save ${path}\tables\decomposition_nu_`t'.dta, replace
restore 
}

