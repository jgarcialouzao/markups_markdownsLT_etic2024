

***********************************************************************************************************
********************         Aggregate Markup and Markdown Implications                ********************
program define prog_mumd_implications 

							syntax [, filename(string)]

	use ${path}\Data_stata\finaldata_`filename'.dta, clear
	
	qui gen filename="`filename'"
		
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
	
	* Drop if negative elasticities (if there is)
	drop if e_l_y_agg<0
	drop if e_varinp_y_agg<0
	
	* Create weights
	rename varinp vp
	rename markup_varinp markup_vp
	rename alpha_varinp alpha_vp
	
	* No weight 
	qui bys year: gen nobs = _N
	qui bys NACE2 year: gen nobs_n2 = _N
	qui gen now      = 1 / nobs 
	qui gen now_N2_f = 1 / nobs_n2
	qui gen now_N2   = nobs_n2 / nobs

	* Sales revenue
	qui bys year: egen tot_sales = sum(exp(y_c))
	qui bys NACE2 year: egen tot_salesN2 = sum(exp(y_c))
	qui gen wsales_f    = exp(y_c)  / tot_sales
	qui gen wsales_N2_f = exp(y_c)  / tot_salesN2
	qui gen wsales_N2   = tot_salesN2 / tot_sales
	
	* Number of employees weights
	qui bys year: egen tot_emp = sum(empl)
	qui bys NACE2 year: egen tot_empN2 =  sum(empl)
	qui gen wemp_f    = empl  / tot_emp
	qui gen wemp_N2_f = empl / tot_empN2
	qui gen wemp_N2 = tot_empN2 / tot_emp  
	
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
	
	preserve 
	    keep year NACE2 wemp_N2
		bys year NACE2: keep if _n==1		
		save ${path}\Data_temp\wemp_N2.dta, replace
	restore
	* Create aggregate measure based on Edmond, Midrigan and Xu (2019) in a given cindustry
	* Use alternative weights
	gen lwedge= (e_l_y/alpha_l)
	
	foreach w in  wsales_N2_f {
	gen upp_sh = (e_l_y/e_l_y_agg)*`w'*(1/lwedge)
	gen down_sh = (e_varinp_y/e_varinp_y_agg)*`w'*(1/markup_vp)

	bys NACE2 year: egen down_avg=total(down_sh)
	bys NACE2 year: egen upp_avg=total(upp_sh)
	
	gen md_EMX_nace2_`w' = down_avg/upp_avg
	gen mu_EMX_nace2_`w' = 1/down_avg
	drop down_* upp*
	}
	drop e_l_y_agg e_varinp_y_agg


    * Following measures all use simple average to calculate industry-mean
	bys year NACE2: egen mu_nace2_mean = mean(markup_vp)
	bys year NACE2: egen md_nace2_mean = mean(markdown_l)

	* Theil index for each industry	
	bys year NACE2: egen theil_mu = mean((markup_vp/mu_nace2_mean)*ln(markup_vp/mu_nace2_mean))
	bys year NACE2: egen theil_md = mean((markdown_l/md_nace2_mean)*ln(markdown_l/md_nace2_mean))
	
	* Mean log deviation for each industry
	bys year NACE2: egen mld_mu = mean(ln((mu_nace2_mean/markup_vp)))
	bys year NACE2: egen mld_md = mean(ln((md_nace2_mean/markdown_l)))
	
	* Theil and MLD for each industry 
	preserve 
	  qui bys year NACE2: keep if _n==1
	  keep year NACE2 theil* mld*
	  save ${path}\Data_temp\Theil.dta, replace
	restore
	
	* Sales HHI and HHI_top6 for each industry 
	preserve 
	  qui bys year NACE2: egen HHI = sum(wsales_N2_f^2)
	  qui bys year NACE2 (wsales_N2_f): keep if _n > nobs_n2 - 6
	  qui bys year NACE2: egen HHI_top6 = sum(wsales_N2_f^2)
	  qui bys year NACE2: keep if _n==1
	  keep year NACE2 HHI*
	  save ${path}\Data_temp\HHI.dta, replace
	restore
	
	* Varinp HHI and HHI_top6 for each industry 
	preserve 
	  qui bys year NACE2: egen HHI_varinp = sum(wcost_vpN2_f^2)
	  qui bys year NACE2 (wcost_vpN2_f): keep if _n > nobs_n2 - 6
	  qui bys year NACE2: egen HHI_varinp_top6 = sum(wcost_vpN2_f^2)
	  qui bys year NACE2: keep if _n==1
	  keep year NACE2 HHI*
	  save ${path}\Data_temp\HHI_varinp.dta, replace
	restore
	
	* Employment HHI and HHI_top6 for each industry 
	preserve 
	  qui bys year NACE2: egen HHI_emp = sum(wemp_N2_f^2)
	  qui bys year NACE2 (wemp_N2_f): keep if _n > nobs_n2 - 6
	  qui bys year NACE2: egen HHI_emp_top6 = sum(wemp_N2_f^2)
	  qui bys year NACE2: keep if _n==1
	  keep year NACE2 HHI*
	  save ${path}\Data_temp\HHI_emp.dta, replace
	restore
	
	* Wage HHI and HHI_top6 for each industry 
	preserve 
	  qui bys year NACE2: egen HHI_wage = sum(wage_N2_f^2)
	  qui bys year NACE2 (wage_N2_f): keep if _n > nobs_n2 - 6
	  qui bys year NACE2: egen HHI_wage_top6 = sum(wage_N2_f^2)
	  qui bys year NACE2: keep if _n==1
	  keep year NACE2 HHI*
	  save ${path}\Data_temp\HHI_wage.dta, replace
	restore
    
	* VA growth rate for each industry from EU Klems
	preserve
	  use ${path}\Data_aux\growth_accounts_ALL.dta, clear
	  keep if geo_code=="LT" & var=="VA_G"
	  keep year nace_r2_code value
	  rename value va_gr_nace2
	  qui destring, replace 
	  sort year nace_r2_code 
	  save ${path}\Data_temp\va_gr_N2.dta, replace
	restore
	
	* Merge all the indicies together
	sort year nace_r2_code
	qui merge m:1 year nace_r2_code using ${path}\Data_temp\va_gr_N2.dta, nogen keep(3)
	qui bys year NACE2: keep if _n==1
	keep year NACE2 theil* mld* va_gr_nace2 mu_EMX* md_EMX* 
	sort year NACE2 
    qui merge 1:1 year NACE2 using ${path}\Data_temp\wsales_N2.dta, nogen keep(3)
    qui merge 1:1 year NACE2 using ${path}\Data_temp\wcost_vpN2.dta, nogen keep(3)
    qui merge 1:1 year NACE2 using ${path}\Data_temp\wage_N2.dta, nogen keep(3)
    qui merge 1:1 year NACE2 using ${path}\Data_temp\wemp_N2.dta, nogen keep(3)
	qui merge 1:1 year NACE2 using ${path}\Data_temp\HHI.dta, nogen keep(3)
	qui merge 1:1 year NACE2 using ${path}\Data_temp\HHI_emp.dta, nogen keep(3)
	qui merge 1:1 year NACE2 using ${path}\Data_temp\HHI_wage.dta, nogen keep(3)
	qui merge 1:1 year NACE2 using ${path}\Data_temp\HHI_varinp.dta, nogen keep(3)	
	* Aggregate each industry-level index by industry share
	rename mu_EMX_nace2_wsales_N2_f mu_EMX_nace2_wsales
	rename md_EMX_nace2_wsales_N2_f md_EMX_nace2_wsales
	
	foreach v in theil_mu mld_mu mu_EMX_nace2_wsales {   
	qui bys year: egen agg_`v'=total(`v'*wcost_vpN2)
	qui gen agg_`v'_norm = agg_`v'/agg_`v'[1]
	}

	foreach v in HHI HHI_top6 {   
	qui bys year: egen agg_`v'=total(`v'*wsales_N2)
	qui gen agg_`v'_norm = agg_`v'/agg_`v'[1]
	}
	* varinput-cost weighted HHI 
	foreach v in HHI_varinp HHI_varinp_top6 {   
	qui bys year: egen agg_`v'=total(`v'*wcost_vpN2)
	qui gen agg_`v'_norm = agg_`v'/agg_`v'[1]
	}	
	*employment-weighted HHI
	foreach v in HHI_emp HHI_emp_top6 {   
	qui bys year: egen agg_`v'=total(`v'*wemp_N2)
	qui gen agg_`v'_norm = agg_`v'/agg_`v'[1]
	}
	*wage bill-weighted HHI
	foreach v in HHI_wage HHI_wage_top6 {   
	qui bys year: egen agg_`v'=total(`v'*wage_N2)
	qui gen agg_`v'_norm = agg_`v'/agg_`v'[1]
	}
	
	
	foreach v in theil_md mld_md md_EMX_nace2_wsales {   
	qui bys year: egen agg_`v'=total(`v'*wage_N2)
	qui gen agg_`v'_norm = agg_`v'/agg_`v'[1]
	}
	
	save ${path}\figures\markups_markdowns_implications.dta, replace
	
	
********************************************************************	 
************Figures for Aggregate HHI and Theil Indicies************
********************************************************************
	cd ${path}\figures
	
	preserve 
	bys year: keep if _n==1
	* Figures of HHI
	#delimit;
	    qui graph tw
		    
					(connected agg_HHI_varinp_norm year)
					(connected agg_HHI_varinp_top6_norm year),
					xtitle("Year") /*ytitle("Aggregate Sales HHI")*/ xsize(4) ysize(3)
					legend(order(1 "Aggregate Variable Input Cost HHI" 2 "Aggregate Variable Input Cost HHI of Top 6 Firms from Each Industry") rows(2)  position(6)) 
					ylabel(0.4(0.15)1.15) xlabel(2004(2)2019) ;	
	#delimit cr	
	graph export ${path}\figures\agg_HHI_varinp.pdf, as(pdf) replace 
	
	#delimit;
	   qui graph tw
		    
					(connected agg_HHI_emp_norm year)
					(connected agg_HHI_emp_top6_norm year),
					xtitle("Year") /*ytitle("Aggregate Employment HHI")*/ xsize(4) ysize(3)
					legend(order(1 "Aggregate Employment HHI" 2 "Aggregate Employment HHI of Top 6 Employers from Each Industry") rows(2)  position(6)) 
					ylabel(0.6(0.08)1) xlabel(2004(2)2019) ;	
	#delimit cr	
	graph export ${path}\figures\agg_HHI_emp.pdf, as(pdf) replace 

	#delimit;
	   qui graph tw
		    
					(connected agg_HHI_wage_norm year)
					(connected agg_HHI_wage_top6_norm year),
					xtitle("Year") /*ytitle("Aggregate Wage HHI")*/ xsize(4) ysize(3)
					legend(order(1 "Aggregate Wage HHI" 2 "Aggregate Wage HHI of Top 6 Employers from Each Industry") rows(2)  position(6)) 
					ylabel(0.3(0.14)1) xlabel(2004(2)2018) ;	
	#delimit cr	
	graph export ${path}\figures\agg_HHI_wage.pdf, as(pdf) replace 
	
		#delimit;
	   qui graph tw
		            (connected agg_HHI_varinp_norm year)
					(connected agg_HHI_varinp_top6_norm year)
					(connected agg_HHI_wage_norm year)
					(connected agg_HHI_wage_top6_norm year),
					xtitle("Year") /*ytitle("Aggregate Wage HHI")*/ xsize(4) ysize(3)
					legend(order(1 "Aggregate Varibale Input Cost HHI" 2 "Aggregate Variable Input Cost HHI of Top 6 Firms from Each Industry" 3 "Aggregate Wage HHI" 4 "Aggregate Wage HHI of Top 6 Employers from Each Industry") rows(4)  position(6)) 
					ylabel(0.4(0.15)1.15) xlabel(2004(2)2018) ;	
	#delimit cr	
	graph export ${path}\figures\agg_HHIs.pdf, as(pdf) replace 
	
    * Figures of Theil
	#delimit;
    qui graph tw 
					(connected agg_mu_EMX_nace2_wsales_norm year, saving(1)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Normalized Aggregate Markup) 
					ylabel(0.95(0.02)1.05) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 
                    (connected agg_theil_mu_norm year, saving(2)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Aggregate Theil Index for Markup)
					ylabel(0.8(0.1)1.3) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 						
                    (connected agg_md_EMX_nace2_wsales_norm  year, saving(3)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Normalized Aggregate Markdown)
					ylabel(0.95(0.02)1.05) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 				
                    (connected agg_theil_md_norm  year, saving(4)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Aggregate Theil Index for Markdown)
					ylabel(0.8(0.06)1.1) xlabel(2004(2)2018);
    #delimit cr		
									 
    gr combine 1.gph 2.gph 3.gph 4.gph, col(2)				
    graph export ${path}\figures\agg_Theil.pdf, as(pdf) replace 
    erase ${path}\figures\1.gph
    erase ${path}\figures\2.gph
    erase ${path}\figures\3.gph
    erase ${path}\figures\4.gph
	restore
	
	
	preserve
	  bys year: keep if _n==1
	  corr agg_theil_mu agg_mu_EMX_nace2_wsales
	  corr agg_theil_md agg_md_EMX_nace2_wsales
	  corr agg_mld_mu   agg_mu_EMX_nace2_wsales
	  corr agg_mld_md   agg_md_EMX_nace2_wsales
	restore


	 
********************************************************************	 
****Static OP at NACE2 level for markup, markdown, HHI and Theil****
********************************************************************
	use ${path}\figures\markups_markdowns_EMX_NACE2_`filename'.dta, clear

	use ${path}\figures\markups_markdowns_EMX_NACE2_TL_LL_purch.dta, clear

	qui merge 1:1 year NACE2 using ${path}\Data_temp\wsales_N2.dta, nogen keep(3)
	qui merge 1:1 year NACE2 using ${path}\Data_temp\wcost_vpN2.dta, nogen keep(3)
    qui merge 1:1 year NACE2 using ${path}\Data_temp\wage_N2.dta, nogen keep(3)
    qui merge 1:1 year NACE2 using ${path}\Data_temp\wemp_N2.dta, nogen keep(3)

	qui merge 1:1 year NACE2 using ${path}\Data_temp\HHI.dta, nogen keep(3)
	qui merge 1:1 year NACE2 using ${path}\Data_temp\HHI_varinp.dta, nogen keep(3)
	qui merge 1:1 year NACE2 using ${path}\Data_temp\HHI_emp.dta, nogen keep(3)
	qui merge 1:1 year NACE2 using ${path}\Data_temp\HHI_wage.dta, nogen keep(3)
	qui merge 1:1 year NACE2 using ${path}\Data_temp\Theil.dta, nogen keep(3)
	
	foreach v in HHI {
		foreach w in wcost_vpN2{	 
		   qui bys year: egen `v'_`w'= sum(`v'*`w')
		   qui bys year: egen `v'_`w'_mean = mean(`v') 	
		   qui bys year: gen cov_`v'_`w' = `v'_`w'-`v'_`w'_mean
		}
	}
	
	foreach v in HHI {
		foreach w in wsales_N2 {	 
		   qui bys year: egen `v'_`w'= sum(`v'*`w')
		   qui bys year: egen `v'_`w'_mean = mean(`v') 	
		   qui bys year: gen cov_`v'_`w' = `v'_`w'-`v'_`w'_mean
		}
	}
	
	foreach v in HHI_emp {
		foreach w in wemp_N2 {	 
		   qui bys year: egen `v'_`w'= sum(`v'*`w')
		   qui bys year: egen `v'_`w'_mean = mean(`v') 	
		   qui bys year: gen cov_`v'_`w' = `v'_`w'-`v'_`w'_mean
		}
	}
	
	foreach v in HHI_wage {
		foreach w in wage_N2 {	 
		   qui bys year: egen `v'_`w'= sum(`v'*`w')
		   qui bys year: egen `v'_`w'_mean = mean(`v') 	
		   qui bys year: gen cov_`v'_`w' = `v'_`w'-`v'_`w'_mean
		}
	}
	foreach v in mu_EMX theil_mu mld_mu {
		foreach w in wcost_vpN2 {	 
		   qui bys year: egen `v'_`w'= sum(`v'*`w')
		   qui bys year: egen `v'_`w'_mean = mean(`v') 	
		   qui bys year: gen cov_`v'_`w' = `v'_`w'-`v'_`w'_mean
		}
	}
	
	foreach v in md_EMX theil_md mld_md {
		foreach w in wage_N2 {	 
		   qui bys year: egen `v'_`w'= sum(`v'*`w')
		   qui bys year: egen `v'_`w'_mean = mean(`v') 	
		   qui bys year: gen cov_`v'_`w' = `v'_`w'-`v'_`w'_mean
		}
	}
	
	*creating plots for some sectors to showcase sectoral reallocation  
	*top 5 markups in 2004 NACE2: 74 71 72 93 82
	foreach j in 74 71 72 93 82{
		
	preserve
	
	  keep if NACE2 ==`j'
	  sort year
	  
	  foreach v in wcost_vpN2 wsales_N2 wage_N2 mu_EMX md_EMX{   
	  qui gen `v'_norm = `v'/`v'[1]
	     }
		 
      qui	tw (connect mu_EMX_norm  year) (connect wcost_vpN2_norm year), xtitle("Year") ytitle("") legend(order(1 "Markup" 2 "Intermediate cost share") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) /*ylabel(1(0.3)2.5)*/
	  graph export ${path}\figures\2004_top5_mu_`j'.pdf, as(pdf) replace 
	  
	restore
	}
	
	*bottom 5 markdown in 2004 NACE2 with : 17 27 82 23 80
	foreach j in 17 27 82 23 80{
		
	preserve
	
	  keep if NACE2 ==`j'
	  sort year
	  
	  foreach v in wsales_N2 wage_N2 mu_EMX md_EMX{   
	  qui gen `v'_norm = `v'/`v'[1]
	     }
	  
	       qui	tw (connect md_EMX_norm  year) (connect wage_N2_norm year), xtitle("Year") ytitle("") legend(order(1 "Markdown" 2 "Wage share") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) /*ylabel(1(0.3)2.5)*/
	  graph export ${path}\figures\2004_bot5_md_`j'.pdf, as(pdf) replace 
	  
	restore
	}
	
	
	keep year cov* *_mean 
	bys year: keep if _n==1
	
	foreach v in HHI_wsales_N2_mean HHI_wcost_vpN2_mean HHI_emp_wemp_N2_mean HHI_wage_wage_N2_mean mu_EMX_wcost_vpN2_mean theil_mu_wcost_vpN2_mean mld_mu_wcost_vpN2_mean md_EMX_wage_N2_mean theil_md_wage_N2_mean mld_md_wage_N2_mean {   
	qui gen `v'_norm = `v'/`v'[1]
	}	
	 
	save ${path}\figures\markups_markdowns_EMX_NACE2_OP_cov_`filename'.dta, replace

	cd ${path}\figures
	
    * Figures of static OP
	#delimit;
    qui graph tw 
					(connected mu_EMX_wcost_vpN2_mean_norm year, saving(1)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title((A) Normalized Average Markup-NACE2)
					ylabel(0.95(0.02)1.05) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 
                    (connected cov_mu_EMX_wcost_vpN2  year, saving(2)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title((B) Cov (Sector size, Markup))
					ylabel(-0.06(0.02)0.04) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 						
                    (connected md_EMX_wage_N2_mean_norm  year, saving(3)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title((C) Normalized Average Markdown-NACE2)
					ylabel(0.95(0.02)1.05) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 				
                    (connected cov_md_EMX_wage_N2  year, saving(4)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title((D) Cov (Sector size, Markdown))
					ylabel(-0.06(0.02)0.04) xlabel(2004(2)2018);
    #delimit cr		
									 
    gr combine 1.gph 2.gph 3.gph 4.gph, col(2)				
    graph export ${path}\figures\static_op.pdf, as(pdf) replace 
    erase ${path}\figures\1.gph
    erase ${path}\figures\2.gph
    erase ${path}\figures\3.gph
    erase ${path}\figures\4.gph
	
	
	* Figures of static OP for HHI
	#delimit;
    qui graph tw 
					(connected HHI_wcost_vpN2_mean_norm year, saving(1)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Normalized Average Variable Input HHI-NACE2)
					ylabel(0.5(0.1)1) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 
                    (connected cov_HHI_wcost_vpN2  year, saving(2)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Cov (Sector size, Variable Input HHI))
					ylabel(-0.07(0.01)-0.02) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 						
                    (connected HHI_wage_wage_N2_mean_norm  year, saving(3)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Normalized Average Wage HHI-NACE2)
					ylabel(0.6(0.08)1) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 				
                    (connected cov_HHI_wage_wage_N2  year, saving(4)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Cov (Sector size, Wage HHI))
					ylabel(-0.06(0.005)-0.03) xlabel(2004(2)2018);
    #delimit cr		
									 
    gr combine 1.gph 2.gph 3.gph 4.gph, col(2)				
    graph export ${path}\figures\static_op_HHI.pdf, as(pdf) replace 
    erase ${path}\figures\1.gph
    erase ${path}\figures\2.gph
    erase ${path}\figures\3.gph
    erase ${path}\figures\4.gph
	
	* Figures of static OP for Theil
	#delimit;
    qui graph tw 
					(connected theil_mu_wcost_vpN2_mean_norm year, saving(1)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Normalized Average Markup Theil-NACE2)
					ylabel(0.7(0.06)1) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 
                    (connected cov_theil_mu_wcost_vpN2  year, saving(2)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Cov (Sector size, Markup Theil))
					ylabel(-0.002(0.001)-0.007) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 						
                    (connected theil_md_wage_N2_mean_norm  year, saving(3)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Normalized Average Markdown Theil-NACE2)
					ylabel(0.75(0.07)1.1) xlabel(2004(2)2018);
    #delimit cr		

    #delimit;
    qui graph tw 				
                    (connected cov_theil_md_wage_N2  year, saving(4)),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Cov (Sector size, Markdown Theil))
					ylabel(-0.012(0.001)-0.007) xlabel(2004(2)2018);
    #delimit cr		
									 
    gr combine 1.gph 2.gph 3.gph 4.gph, col(2)				
    graph export ${path}\figures\static_op_Theil.pdf, as(pdf) replace 
    erase ${path}\figures\1.gph
    erase ${path}\figures\2.gph
    erase ${path}\figures\3.gph
    erase ${path}\figures\4.gph
	
	use ${path}\figures\markups_markdowns_EMX_NACE2_OP_cov_`filename'.dta, clear
	
  
	* Erase temp files
	cd ${path}\Data_temp
	local list : dir . files "*.dta"
	foreach f of local list {
		erase "`f'"
    }
	

	
end