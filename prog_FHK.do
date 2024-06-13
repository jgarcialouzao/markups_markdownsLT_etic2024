program define prog_FHK

							syntax [, filename(string)]
*******************************************
*** CALCULATON OF MARKUPS AND MARKDOWNS ***
*******************************************

	use ${path}\Data_stata\finaldata_`filename'.dta, clear

	
	qui gen filename="`filename'"	
	
	use ${path}\Data_stata\finaldata_TL_LL_purch.dta, clear

    
	* Create industry-level elasticities
	bys NACE2 year: egen kagg2 = sum(exp(k))
	bys NACE2 year: egen lagg2 = sum(exp(l))
	bys NACE2 year: egen varinpagg2 = sum(exp(varinp))
	
	gen kagg      = ln(kagg2)
	gen lagg      = ln(lagg2)
	gen varinpagg = ln(varinpagg2)
	
	drop *agg2
		
	gen e_varinp_y_agg = betavarinp + 2*betavarinp2*varinpagg + betakvarinp*kagg + betalvarinp*lagg	
	gen e_l_y_agg      = betal + 2*betal2*lagg + betalk*kagg + betalvarinp*varinpagg	
	
	drop kagg lagg varinpagg
	
	* Create weights
	rename varinp vp
	rename markup_varinp markup_vp
	rename alpha_varinp alpha_vp

	 * Sales revenue
	qui bys year: egen tot_sales = sum(exp(y_c))
	qui bys NACE2 year: egen tot_salesN2 = sum(exp(y_c))
	qui gen wsales_f    = exp(y_c)  / tot_sales
	qui gen wsales_N2_f = exp(y_c)  / tot_salesN2
	qui gen wsales_N2   = tot_salesN2 / tot_sales
	
   * Varinp weights
	qui bys year: egen tot_cost_vp = sum(exp(vp))
	qui bys NACE2 year: egen tot_cost_vpN2 =  sum(exp(vp))
	qui gen wcost_vpN2_f = exp(vp) / tot_cost_vpN2
	qui gen wcost_vpN2 = tot_cost_vpN2 / tot_cost_vp 
	

	* Employment costs weights
	qui bys year: egen tot_wage = sum(exp(l))
	qui bys NACE2 year: egen tot_wageN2 =  sum(exp(l))
	qui gen wage_N2_f = exp(l) / tot_wageN2
	qui gen wage_N2 = tot_wageN2 / tot_wage 


	* Create aggregate measure based on Edmond, Midrigan and Xu (2018) in a given cindustry
	* Use alternative weights
	gen lwedge= (e_l_y/alpha_l)
	foreach w in wsales_N2_f {
	gen upp_sh_`w'  = (e_l_y/e_l_y_agg)*`w'*(1/lwedge)
	gen down_sh_`w' = (e_varinp_y/e_varinp_y_agg)*`w'*(1/markup_vp)
	}
	
	
	* Generate industry upp and down for the decomposition based on sales for each industry
	bys NACE2 year: egen agg_munu_inv_NACE2 = total(upp_sh_wsales_N2_f)
	bys NACE2 year: egen agg_mu_inv_NACE2 = total(down_sh_wsales_N2_f)

	
	* Rename variables that are relevant for decompositions according to Yeh et al (2022)
	rename e_l_y theta_l_tl 
	rename e_l_y_agg theta_l_tl_agg // industry-level elasticities
	rename lwedge lwedge_TL
	rename wsales_N2_f firmshare_sls // now firm share is within each industry
	rename e_varinp_y theta_m_tl
	rename e_varinp_y_agg theta_m_tl_agg // industry-level elasticities
	rename markup_vp mu_TL
	
	
	* Set aside industry weights for aggregation later
	preserve 
	    keep year NACE2 wsales_N2
		bys year NACE2: keep if _n==1		
		save ${path}\Data_temp\wsales_N2.dta, replace
	restore
	
	preserve 
	    keep year NACE2 wcost_vpN2
		bys year NACE2: keep if _n==1		
		save ${path}\Data_temp\wcost_vpN2.dta, replace
	restore
	
	preserve 
	    keep year NACE2 wage_N2
		bys year NACE2: keep if _n==1		
		save ${path}\Data_temp\wage_N2.dta, replace
	restore
	
	* Correct input errors for firms with year_birth later than its first appearance
	bys id: egen minyear=min(year)
	bys id: egen maxyear=max(year)
	replace year_birth = minyear if year_birth>minyear
	drop minyear maxyear


	save ${path}\Data_temp\fhk_base.dta, replace



***********************************************************************************
*** DECOMPOSITON ON AGGREGATE MARKDOWN BY FOSTER, HALTIWANGER AND KRIZAN (2001) ***
***********************************************************************************

* Delta X_t = \sum_{i in C_t} s_{it-1} \Delta x_{it} 			 "incumbent - within"
*     	    + \sum_{i in C_t} \Delta s_{it} (x_{it-1} - X_{t-1}) "incumbent - across"
*	        + \sum_{i in C_t} \Delta s_{it} \Delta x_{it}		 "incumbent - covariance"
*	        + \sum_{i in N_t} s_{it} (x_{it} - X_{t-1})			 "entrants"
*	        - \sum_{i in X_t} s_{it-1} (x_{it-1}-X_{t-1})		 "exiters"

*******************************************
*** FHK decomposition on "agg_munu_inv" ***
*******************************************

forval i = 2005/2018  {
	
	local j = `i' - 1
	
    use ${path}\Data_temp\fhk_base.dta, clear
	
	keep if year==`i'|year==`j'
	sort NACE2 id year	

	* Identify incumbent firms, entrants and exiters for a given industry (over 2 year window)
	qui bys NACE2 id: gen d_firm = _N
	
	qui gen d_incumbent = 0
	qui replace d_incumbent = 1 if d_firm==2 & year_birth <=`j' & year_death >=`i'
	
	qui gen d_entrant = 0
	qui replace d_entrant = 1 if d_firm==1 & year_birth >`j'
	
	qui gen d_exiter = 0
	qui replace  d_exiter = 1 if d_firm==1 & year_death <`i'
	
	*drop if  d_firm == 1 & d_exiter == 0 & d_entrant == 0 

	count
	sum d_incumbent d_entrant d_exiter

	sort NACE2 id year	
	
	save ${path}\Data_temp\fhk_`i'-`j'_temp.dta, replace
	
	* Incumbent components
	* \sum_{i in C_t} s_{it-1} \Delta x_{it} 		              "incumbent - within"
	* + \sum_{i in C_t} \Delta s_{it} (x_{it-1} - X_{t-1})        "incumbent - across"
	* + \sum_{i in C_t} \Delta s_{it} \Delta x_{it}               "incumbent - covariance"
	
	disp "Incumbent components"
	qui keep if d_incumbent==1
	
	* might need to trim xvar here
	qui gen xvar = (theta_l_tl/theta_l_tl_agg)*(1/lwedge_TL)
	qui bys NACE2 id: gen theta_l_tl_lag = theta_l_tl[_n-1]
	qui bys NACE2 id: gen theta_l_tl_agg_lag = theta_l_tl_agg[_n-1]
	qui bys NACE2 id: gen lwedge_TL_lag = lwedge_TL[_n-1]
	qui gen xvar_lag   = (theta_l_tl_lag/theta_l_tl_agg_lag)*(1/lwedge_TL_lag)
	qui gen xvar_delta = (xvar - xvar_lag)
		
	qui bys NACE2 id: gen firmshare_sls_lag = firmshare_sls[_n-1]
	qui gen firmshare_sls_delta = (firmshare_sls - firmshare_sls_lag)
	qui bys NACE2 id: gen agg_munu_inv_NACE2_lag = agg_munu_inv_NACE2[_n-1]
		
	qui gen within_temp = firmshare_sls_lag*xvar_delta
	qui gen across_temp = firmshare_sls_delta*(xvar_lag-agg_munu_inv_NACE2_lag)
	qui gen covar_temp  = firmshare_sls_delta*xvar_delta
		
	qui keep if year==`i'

	qui bys NACE2:  egen inc_within = total(within_temp)
	qui bys NACE2:  egen inc_across = total(across_temp)
	qui bys NACE2:  egen inc_covar  = total(covar_temp)
	
	qui keep year NACE2 inc* 
	qui bys NACE2: keep if _n==1

	sort year NACE2
	save ${path}\Data_temp\fhk_inc_`i'.dta, replace

	
	* Entrant component
	* \sum_{i in N_t} s_{it} (x_{it} - X_{t-1})		      "entrants"
	use ${path}\Data_temp\fhk_`i'-`j'_temp.dta, clear
	sort NACE2 id year	
    
	qui gen x=agg_munu_inv_NACE2 if year==`j'
	egen xx=mean(x)
	rename xx agg_munu_inv_NACE2_lag
    qui drop x 

	disp "Entry components"
	qui keep if d_entrant==1
	qui keep if year==`i'
		
	qui gen xvar = (theta_l_tl/theta_l_tl_agg)*(1/lwedge_TL)
	qui gen entrant_temp = firmshare_sls*(xvar - agg_munu_inv_NACE2_lag)
		
	qui bys NACE2: egen entrant = total(entrant_temp)
		
	qui keep year NACE2 entrant
	qui bys NACE2: keep if _n==1

	sort year NACE2
	save ${path}\Data_temp\fhk_entrant_`i'.dta, replace
	

	* Exiter component
	* \sum_{i in X_t} s_{it-1} (x_{it-1}-X_{t-1})		       "exiters"
	use ${path}\Data_temp\fhk_`i'-`j'_temp.dta, clear
	sort NACE2 id year	
	
	disp "Exit components"
	qui keep if d_exiter==1
	qui keep if year==`j'
		
	qui gen xvar = (theta_l_tl/theta_l_tl_agg)*(1/lwedge_TL)
	qui gen exiter_temp = firmshare_sls*(xvar - agg_munu_inv_NACE2)
		
	qui bys NACE2: egen exiter = total(exiter_temp)
		
	qui keep NACE2 exiter
	gen year = `i'
	qui bys NACE2: keep if _n==1

	sort year NACE2
	save ${path}\Data_temp\fhk_exiter_`i'.dta, replace
	
	*** Append incumbent, entrant and exiter components for a given year
	use ${path}\Data_temp\fhk_inc_`i'.dta, clear	
	*	- Merge entry components
	qui merge 1:1 year NACE2 using ${path}\Data_temp\fhk_entrant_`i'.dta, nogen	
	*	- Merge exit components
	qui merge 1:1 year NACE2 using ${path}\Data_temp\fhk_exiter_`i'.dta, nogen 

	save ${path}\Data_temp\fhk_munu_`i'.dta, replace
	
}


*****************************************
*** FHK decomposition on "agg_mu_inv" ***
*****************************************

forval i = 2005/2018  {
	
	local j = `i' - 1
	
    use ${path}\Data_temp\fhk_base.dta, clear
	
	qui keep if year==`i'|year==`j'
	sort NACE2 id year	

	* Identify incumbent firms, entrants and exiters for a given industry (over 2 year window)
	qui bys NACE2 id: gen d_firm = _N
	
	qui gen d_incumbent = 0
	qui replace d_incumbent = 1 if d_firm==2 & year_birth <=`j' & year_death >=`i'
	
	qui gen d_entrant = 0
	qui replace d_entrant = 1 if d_firm==1 & year_birth >`j'
	
	qui gen d_exiter = 0
	qui replace  d_exiter = 1 if d_firm==1 & year_death <`i'

	*drop if  d_firm == 1 & d_exiter == 0 & d_entrant == 0 
	
	count
	sum d_incumbent d_entrant d_exiter
	
	sort NACE2 id year	
	
	save ${path}\Data_temp\fhk_`i'-`j'_temp.dta, replace
	
	* Incumbent components
	* \sum_{i in C_t} s_{it-1} \Delta x_{it} 		              "incumbent - within"
	* + \sum_{i in C_t} \Delta s_{it} (x_{it-1} - X_{t-1})        "incumbent - across"
	* + \sum_{i in C_t} \Delta s_{it} \Delta x_{it}               "incumbent - covariance"
	
	disp "Incumbent components"
	qui keep if d_incumbent==1
		
	qui gen xvar = (theta_m_tl/theta_m_tl_agg)*(1/mu_TL)
	qui bys NACE2 id: gen theta_m_tl_lag = theta_m_tl[_n-1]
	qui bys NACE2 id: gen theta_m_tl_agg_lag = theta_m_tl_agg[_n-1]
	qui bys NACE2 id: gen mu_TL_lag = mu_TL[_n-1]
	qui gen xvar_lag = (theta_m_tl_lag/theta_m_tl_agg_lag)*(1/mu_TL_lag)
	qui gen xvar_delta = (xvar - xvar_lag)
		
	qui bys NACE2 id: gen firmshare_sls_lag = firmshare_sls[_n-1]
	qui gen firmshare_sls_delta = (firmshare_sls - firmshare_sls_lag)
	qui bys NACE2 id: gen agg_mu_inv_NACE2_lag = agg_mu_inv_NACE2[_n-1]
		
	qui gen within_temp = firmshare_sls_lag*xvar_delta
	qui gen across_temp = firmshare_sls_delta*(xvar_lag-agg_mu_inv_NACE2_lag)
	qui gen covar_temp = firmshare_sls_delta*xvar_delta
		
	qui keep if year==`i'
	
	qui bys NACE2: egen inc_within = total(within_temp)
	qui bys NACE2: egen inc_across = total(across_temp)
	qui bys NACE2: egen inc_covar = total(covar_temp)
	
	qui keep year NACE2 inc* 
	qui bys NACE2: keep if _n==1

	sort year NACE2
	save ${path}\Data_temp\fhk_inc_`i'.dta, replace
	
	* Entrant component
	* \sum_{i in N_t} s_{it} (x_{it} - X_{t-1})		      "entrants"
	use ${path}\Data_temp\fhk_`i'-`j'_temp.dta, clear
	sort NACE2 id year	

	qui gen x=agg_mu_inv_NACE2 if year==`j'
	egen xx=mean(x)
	rename xx agg_mu_inv_NACE2_lag
    qui drop x 

	disp "Entry components"
	qui keep if d_entrant==1
	qui keep if year==`i'
			
	qui gen xvar = (theta_m_tl/theta_m_tl_agg)*(1/mu_TL)
	qui gen entrant_temp = firmshare_sls*(xvar - agg_mu_inv_NACE2_lag)
		
	qui bys NACE2: egen entrant = total(entrant_temp)
		
	qui keep year NACE2 entrant
	qui bys NACE2: keep if _n==1

	sort year NACE2
	save ${path}\Data_temp\fhk_entrant_`i'.dta, replace
	
	* Exiter component
	* \sum_{i in X_t} s_{it-1} (x_{it-1}-X_{t-1})		       "exiters"
	use ${path}\Data_temp\fhk_`i'-`j'_temp.dta, clear
	sort NACE2 id year	
	
	disp "Exit components"
	qui keep if d_exiter==1
	qui keep if year==`j'
		
	qui gen xvar = (theta_m_tl/theta_m_tl_agg)*(1/mu_TL)
	qui gen exiter_temp = firmshare_sls*(xvar - agg_mu_inv_NACE2)
		
	qui bys NACE2: egen exiter = total(exiter_temp)
		
	qui keep NACE2 exiter
	gen year = `i'
	qui bys NACE2: keep if _n==1

    sort year NACE2
	save ${path}\Data_temp\fhk_exiter_`i'.dta, replace
	
	
	*** Append incumbent, entrant and exiter components for a given year
	use ${path}\Data_temp\fhk_inc_`i'.dta, clear	
	*	- Merge entry components
	qui merge 1:1 year NACE2 using ${path}\Data_temp\fhk_entrant_`i'.dta, nogen	
	*	- Merge exit components
	qui merge 1:1 year NACE2 using ${path}\Data_temp\fhk_exiter_`i'.dta, nogen 

	save ${path}\Data_temp\fhk_mu_`i'.dta, replace
	
}

***************************************************************
*** Appending the decompositions over years and normalizing ***
***************************************************************

    * Append over years and normalizing for mu
    use ${path}\Data_temp\fhk_mu_2005.dta, clear
	
	      qui replace inc_within = 0 if inc_within==.
		  qui replace inc_across = 0 if inc_across==.
		  qui replace inc_covar = 0 if inc_covar==.
		  qui replace entrant = 0 if entrant==.
		  qui replace exiter = 0 if exiter==.
		  qui replace year = 2005 if year==.
			
	
    forv i=2006/2018{
		qui append using ${path}\Data_temp\fhk_mu_`i'.dta
		
		  qui replace inc_within = 0 if inc_within==.
	      qui replace inc_across = 0 if inc_across==.
	      qui replace inc_covar = 0 if inc_covar==.
		  qui replace entrant = 0 if entrant==.
		  qui replace exiter = 0 if exiter==.
		  qui replace year = `i' if year==.
			
		}
    
	sort year NACE2
	qui	gen agg_mu_inv_delta = inc_within + inc_across + inc_covar + entrant - exiter
	qui gen agg_mu_inv_delta_abs = abs(inc_within) + abs(inc_across) + abs(inc_covar) + abs(entrant) + abs(exiter)

	* normalizing to get the percentage contribution of each part
	qui gen inc_within_norm1 = abs(inc_within) / agg_mu_inv_delta_abs
	qui gen inc_across_norm1 = abs(inc_across) / agg_mu_inv_delta_abs
	qui gen inc_covar_norm1 = abs(inc_covar) / agg_mu_inv_delta_abs
	qui gen entrant_norm1 = abs(entrant) / agg_mu_inv_delta_abs
	qui gen exiter_norm1 = abs(exiter) / agg_mu_inv_delta_abs

	qui ds
	local a `r(varlist)'
	local b "year agg_mu_inv_delta agg_mu_inv_delta_abs NACE2"
	local c : list a - b
	foreach v of local c {
		qui rename `v' mu_`v'
		}
	sort year NACE2
	save ${path}\Data_temp\fhk_mu.dta, replace

	* Append over years and normalizing for munu

	use ${path}\Data_temp\fhk_munu_2005.dta, clear
	
	      qui replace inc_within = 0 if inc_within==.
		  qui replace inc_across = 0 if inc_across==.
		  qui replace inc_covar = 0 if inc_covar==.
		  qui replace entrant = 0 if entrant==.
		  qui replace exiter = 0 if exiter==.
		  qui replace year = 2005 if year==.
			
	
	forv i=2006/2018{
		qui append using ${path}\Data_temp\fhk_munu_`i'.dta
		
		  qui replace inc_within = 0 if inc_within==.
	      qui replace inc_across = 0 if inc_across==.
		  qui replace inc_covar = 0 if inc_covar==.
		  qui replace entrant = 0 if entrant==.
		  qui replace exiter = 0 if exiter==.
		  qui replace year = `i' if year==.
			
		}
	
	sort year NACE2
	qui	gen agg_munu_inv_delta = inc_within + inc_across + inc_covar + entrant - exiter
	qui gen agg_munu_inv_delta_abs = abs(inc_within) + abs(inc_across) + abs(inc_covar) + abs(entrant) + abs(exiter)
	
	* normalizing to get the percentage contribution of each part	
	qui gen inc_within_norm1 = abs(inc_within) / agg_munu_inv_delta_abs
    qui gen inc_across_norm1 = abs(inc_across) / agg_munu_inv_delta_abs
	qui gen inc_covar_norm1 = abs(inc_covar) / agg_munu_inv_delta_abs
	qui gen entrant_norm1 = abs(entrant) / agg_munu_inv_delta_abs
	qui gen exiter_norm1 = abs(exiter) / agg_munu_inv_delta_abs
	
	qui ds
	local a `r(varlist)'
	local b "year agg_munu_inv_delta agg_munu_inv_delta_abs NACE2"
	local c : list a - b
	foreach v of local c {
		qui rename `v' munu_`v'
		}
	sort year NACE2
	save ${path}\Data_temp\fhk_munu.dta, replace

	
***********************************************
*** FHK decomposition by graph and by table ***
***********************************************
	
	* Compute md decomposition based on the approximation in YEH et al (2022)
	use ${path}\Data_temp\fhk_mu.dta, clear
	qui merge 1:1 year NACE2 using ${path}\Data_temp\fhk_munu.dta, nogen keep (3)
    qui merge 1:1 year NACE2 using ${path}\Data_temp\wsales_N2.dta, nogen keep(3)
    qui merge 1:1 year NACE2 using ${path}\Data_temp\wcost_vpN2.dta, nogen keep(3)
    qui merge 1:1 year NACE2 using ${path}\Data_temp\wage_N2.dta, nogen keep(3)

			
	foreach v in inc_within inc_across inc_covar entrant exiter {
	    gen md_`v'=mu_`v'-munu_`v'
	}
	
	qui	gen agg_md_inv_delta = md_inc_within + md_inc_across + md_inc_covar + md_entrant - md_exiter
	qui gen agg_md_inv_delta_abs = abs(md_inc_within) + abs(md_inc_across) + abs(md_inc_covar) + abs(md_entrant) + abs(md_exiter)
	
	qui gen md_inc_within_norm1 = abs(md_inc_within) / agg_md_inv_delta_abs
    qui gen md_inc_across_norm1 = abs(md_inc_across) / agg_md_inv_delta_abs
	qui gen md_inc_covar_norm1 = abs(md_inc_covar) / agg_md_inv_delta_abs
	qui gen md_entrant_norm1 = abs(md_entrant) / agg_md_inv_delta_abs
	qui gen md_exiter_norm1 = abs(md_exiter) / agg_md_inv_delta_abs
	
	* weight markups by varinp cost 
	foreach v in mu_inc_within mu_inc_across mu_inc_covar mu_entrant mu_exiter agg_mu_inv_delta agg_mu_inv_delta_abs mu_inc_within_norm1 mu_inc_across_norm1 mu_inc_covar_norm1 mu_entrant_norm1 mu_exiter_norm1{
		bys year: egen `v'_agg_mu_wvp = total(`v'*wcost_vpN2)
	}
	* weight markdowns by labor cost
	foreach v in md_inc_within md_inc_across md_inc_covar md_entrant md_exiter agg_md_inv_delta agg_md_inv_delta_abs md_inc_within_norm1 md_inc_across_norm1 md_inc_covar_norm1 md_entrant_norm1 md_exiter_norm1{
		bys year: egen `v'_agg_md_wl =  total(`v'*wage_N2)
	}
	bys year: keep if _n==1
	
	
	** generate counterfactuals of decomposition over time (for plotting)
	sort year 
	qui merge 1:1 year using ${path}\figures\markups_markdowns_EMX_TL_LL_purch.dta
	drop _merge
	*drop if year>2018
	keep year *_agg_mu_wvp *_agg_md_wl mu_EMX_agg_wc mu_norm_EMX_wc md_EMX_agg_wl md_norm_EMX_wl
	sort year
	
	foreach v in mu {
		egen `v'_base  = mean(`v'_EMX_agg_wc) if year==2004
        egen `v'_base1 = mean(`v'_base)
		drop `v'_base
		}
	foreach v in md {
		egen `v'_base  = mean(`v'_EMX_agg_wl) if year==2004
        egen `v'_base1 = mean(`v'_base)
		drop `v'_base
		}
	
	qui gen mu_netentry_agg_mu_wvp = mu_entrant_agg_mu_wvp - mu_exiter_agg_mu_wvp
	qui gen md_netentry_agg_md_wl = md_entrant_agg_md_wl - md_exiter_agg_md_wl
	
	qui gen mu_realloc_agg_mu_wvp = mu_inc_across_agg_mu_wvp + mu_inc_covar_agg_mu_wvp
	qui gen md_realloc_agg_md_wl = md_inc_across_agg_md_wl + md_inc_covar_agg_md_wl

	* initialize everything to 2004
	foreach v in mu{
	foreach x in `v'_inc_within_agg_mu_wvp `v'_inc_across_agg_mu_wvp `v'_inc_covar_agg_mu_wvp `v'_netentry_agg_mu_wvp `v'_realloc_agg_mu_wvp {
		qui replace `x' = `v'_base1 if `x'==.
		qui replace `x' = `x' + `x'[_n-1] if year>2004
		}
	}
	
	foreach v in md{
	foreach x in `v'_inc_within_agg_md_wl `v'_inc_across_agg_md_wl `v'_inc_covar_agg_md_wl `v'_netentry_agg_md_wl `v'_realloc_agg_md_wl {
		qui replace `x' = `v'_base1 if `x'==.
		qui replace `x' = `x' + `x'[_n-1] if year>2004
		}
	}
	preserve
	   drop *norm1_agg*  *norm_EMX_* *base1
	   drop if year>2018
	   drop *_entrant_* *_exiter_*
	   save ${path}\figures\markups_markdowns_EMX_decompositions_FHK.dta, replace

	   foreach v in mu_inc_within_agg_mu_wvp mu_inc_across_agg_mu_wvp mu_inc_covar_agg_mu_wvp md_inc_within_agg_md_wl md_inc_across_agg_md_wl md_inc_covar_agg_md_wl mu_EMX_agg_wc md_EMX_agg_wl mu_netentry_agg_mu_wvp md_netentry_agg_md_wl mu_realloc_agg_mu_wvp md_realloc_agg_md_wl{
	 	qui gen `v'_norm = `v'/`v'[1]
		
	   }
	   
	   #delimit; 
	   qui graph tw 
					(connected mu_EMX_agg_wc_norm year)
					(connected mu_inc_within_agg_mu_wvp_norm year)
					(connected mu_inc_across_agg_mu_wvp_norm year)
					(connected mu_inc_covar_agg_mu_wvp_norm year)
					(connected mu_netentry_agg_mu_wvp_norm year),
				    xtitle("Year") /*ytitle("Aggregate markup")*/ xsize(4) ysize(3)
					legend(order(1 "Aggregate Markup" 2 "Incumbent-Within" 3 "Incumbent-Between" 4 "Incumbent-Covariance" 5   "Net Entry") cols(2) position(6) ) 
					ylabel(0.9(0.05)1.15) xlabel(2004(2)2018);		
	   #delimit cr	
	   graph export ${path}\figures\mu_FHK_decomposition_wc.pdf, as(pdf) replace 


       #delimit;
       qui graph tw 
					(connected md_EMX_agg_wl_norm year)
					(connected md_inc_within_agg_md_wl_norm year)
					(connected md_inc_across_agg_md_wl_norm year)
					(connected md_inc_covar_agg_md_wl_norm year)
					(connected md_netentry_agg_md_wl_norm year),
					xtitle("Year") /*ytitle("Aggregate markdown")*/ xsize(4) ysize(3)
					legend(order(1 "Aggregate Markdown" 2 "Incumbent-Within" 3 "Incumbent-Between" 4 "Incumbent-Covariance" 5   "Net Entry") cols(2)  position(6)) 
					ylabel(0.8(0.1)1.3) xlabel(2004(2)2018);		
       #delimit cr	
       graph export ${path}\figures\md_FHK_decomposition_wl.pdf, as(pdf) replace    
	restore

	** generate the decompositions in absolute terms (for table)
	keep year *norm1_agg*
	drop if year==2004 | year>2018
	qui gen mu_netentry_norm1_agg_mu_wvp = mu_entrant_norm1_agg_mu_wvp + mu_exiter_norm1_agg_mu_wvp
	qui gen md_netentry_norm1_agg_md_wl = md_entrant_norm1_agg_md_wl + md_exiter_norm1_agg_md_wl
	
	keep year *_inc_within_* *_inc_across_* *_inc_covar_* *_netentry_*
	order year *_inc_within_* *_inc_across_* *_inc_covar_* *_netentry_*

    save ${path}\figures\markups_markdowns_EMX_decompositions_FHK_abs.dta, replace
	
	cd ${path}\tables
    * MU component 
    mat muresult1 = J(14,6,0)
	local year = 1
	forv i = 2005/2018  {
	
	local j = `i'-1
	
	local column = 1
	mat muresult1[`year',`column'] =`j'`i'
	
	local column = `column' + 1
	count
	mat muresult1[`year',`column'] = `r(N)'	
	
	local column = `column' + 1
	sum mu_inc_within_norm1_agg_mu_wvp if year ==`i', de
	mat muresult1[`year',`column'] = `r(mean)'
	
	local column = `column' + 1
	sum mu_inc_across_norm1_agg_mu_wvp if year ==`i', de
	mat muresult1[`year',`column'] = `r(mean)'
	
	local column = `column' + 1
	sum mu_inc_covar_norm1_agg_mu_wvp if year ==`i', de
	mat muresult1[`year',`column'] = `r(mean)'
	
	local column = `column' + 1
	sum mu_netentry_norm1_agg_mu_wvp if year ==`i', de
	mat muresult1[`year',`column'] = `r(mean)'

	
	local year = `year' + 1
	
}


    * MD component
	mat mdresult1 = J(14,6,0)
	local year = 1
	forv i = 2005/2018  {
	
	local j = `i'-1
	
	local column = 1
	mat mdresult1[`year',`column'] = `j'`i'
	
	local column = `column' + 1
	count
	mat mdresult1[`year',`column'] = `r(N)'
	
	
	local column = `column' + 1
	sum md_inc_within_norm1_agg_md_wl if year ==`i', de 
	mat mdresult1[`year',`column'] = `r(mean)' 

	
	local column = `column' + 1
	sum md_inc_across_norm1_agg_md_wl if year ==`i', de
	mat mdresult1[`year',`column'] = `r(mean)'
	
	
	local column = `column' + 1
	sum md_inc_covar_norm1_agg_md_wl if year ==`i', de
	mat mdresult1[`year',`column'] = `r(mean)'

	
	local column = `column' + 1
	sum md_netentry_norm1_agg_md_wl if year ==`i', de
	mat mdresult1[`year',`column'] = `r(mean)'
	
	
	
	local year = `year' + 1
	
}
    mat list muresult1 
    mat list mdresult1 
	
	esttab matrix(muresult1) using mu_FHK.tex, replace 
	esttab matrix(mdresult1) using md_FHK.tex, replace

	
	* Erase temp files
	cd ${path}\Data_temp
	local list : dir . files "*.dta"
	foreach f of local list {
		erase "`f'"
		}


end
