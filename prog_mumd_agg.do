



*************************************************************************
*************************************************************************
***************** Aggregate Markup and Markdown: TL  ********************
*************************************************************************
*************************************************************************


program define prog_mumd_agg 
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
	
// 	drop *agg2
		
	gen e_vp_y_agg = betavarinp + 2*betavarinp2*varinpagg + betakvarinp*kagg + betalvarinp*lagg	
	gen e_l_y_agg  = betal + 2*betal2*lagg + betalk*kagg + betalvarinp*varinpagg
	
	gen e_vp_y_binp = 2*betavarinp2*varinpagg 
	gen e_vp_y_bkinp = betakvarinp*kagg 
	gen e_vp_y_blinp = betalvarinp*lagg	

	gen e_l_y_bl = 2*betal2*lagg 
	gen e_l_y_bkl = betalk*kagg 
	gen e_l_y_binpl = betalvarinp*varinpagg	 
	
	
	* Drop if negative elasticities
	
	drop if e_l_y_agg<0
	drop if e_vp_y_agg<0
	
// 	drop kagg lagg varinpagg
	
	
	* Rename variables
	
	rename varinp vp
	rename markup_varinp markup_vp
	rename alpha_varinp alpha_vp
	rename e_varinp_y e_vp_y
	
	
	* Save for later 

	save ${path}\Data_temp\finaldata_`filename'_more.dta, replace
	
	
	* Gen dummies for manufactures and services 
	gen manuf     = NACE2<33
	gen service   = NACE2>=45
	gen trade     = NACE2>=45 & NACE2<=47
	gen otherserv = NACE2>=55 
	
	* Sales revenue weights
	
	qui bys year: egen tot_sales = sum(exp(y_c))
	qui bys NACE2 year: egen tot_salesN2 = sum(exp(y_c))

	qui gen wsales_N2_f = exp(y_c) / tot_salesN2
	qui gen wsales_N2 = tot_salesN2 / tot_sales
	
	

	* Identify the top6 sectors that have the largest sales share 
	
	preserve 
	  qui egen TOT_SALES = sum(exp(y_c))
	  qui bys NACE2: egen TOT_SALES_N2 = sum(exp(y_c))
	  qui gen WSALES_N2 = TOT_SALES_N2/TOT_SALES
	  qui bys WSALES_N2: keep if _n==1

 	  qui keep if _n > _N - 6
	  qui keep NACE2 
	  qui sort NACE2

	save ${path}\Data_temp\top6_NACE2.dta, replace
    restore 

	* Identify the bottom6 sectors that have the largest sales share 
	
	preserve 
	  qui egen TOT_SALES = sum(exp(y_c))
	  qui bys NACE2: egen TOT_SALES_N2 = sum(exp(y_c))
	  qui gen WSALES_N2 = TOT_SALES_N2/TOT_SALES
	  qui bys WSALES_N2: keep if _n==1

 	  qui keep if _n < 7
	  qui keep NACE2 
	  qui sort NACE2
	
	save ${path}\Data_temp\bottom6_NACE2.dta, replace
    restore
	

	* Set aside industry weights for industry-level static OP
	
	preserve 
	    keep year NACE2 wsales_N2
		bys year NACE2: keep if _n==1		
		save ${path}\Data_temp\wsales_N2.dta, replace
	restore


	
	* Varinp weights
	
	qui bys year: egen tot_cost_vp = sum(exp(vp))
	qui bys NACE2 year: egen tot_cost_vpN2 =  sum(exp(vp))
	
	qui gen wcost_vpN2_f = exp(vp) / tot_cost_vpN2
	qui gen wcost_vpN2 = tot_cost_vpN2 / tot_cost_vp 
	

	qui bys year: egen tot_cost_vp_manuf = sum(exp(vp)) if manuf == 1
	qui bys NACE2 year: egen tot_cost_vpN2_manuf =  sum(exp(vp)) if manuf == 1
	
	qui gen wcost_vpN2_manuf = tot_cost_vpN2_manuf / tot_cost_vp_manuf 	
	
	
	qui bys year: egen tot_cost_vp_service = sum(exp(vp)) if service == 1
	qui bys NACE2 year: egen tot_cost_vpN2_service =  sum(exp(vp)) if service == 1
	
	qui gen wcost_vpN2_service = tot_cost_vpN2_service / tot_cost_vp_service 	
	
	
	qui bys year: egen tot_cost_vp_trade = sum(exp(vp)) if trade == 1
	qui bys NACE2 year: egen tot_cost_vpN2_trade =  sum(exp(vp)) if trade == 1
	
	qui gen wcost_vpN2_trade = tot_cost_vpN2_trade / tot_cost_vp_trade
	
	
	qui bys year: egen tot_cost_vp_otherserv = sum(exp(vp)) if otherserv == 1
	qui bys NACE2 year: egen tot_cost_vpN2_otherserv = sum(exp(vp)) if otherserv == 1
	
	qui gen wcost_vpN2_otherserv = tot_cost_vpN2_otherserv / tot_cost_vp_otherserv 
	
	
	* Number of employees weights
	
	qui bys year: egen tot_emp = sum(empl)
	qui bys NACE2 year: egen tot_empN2 =  sum(empl)

	qui gen wemp_N2_f = empl / tot_empN2
	qui gen wemp_N2 = tot_empN2 / tot_emp  
	

	* Employment costs weights
	
	qui bys year: egen tot_wage = sum(exp(l))
	qui bys NACE2 year: egen tot_wageN2 =  sum(exp(l))

	qui gen wage_N2_f = exp(l) / tot_wageN2
	qui gen wage_N2 = tot_wageN2 / tot_wage 
	
	
	qui bys year: egen tot_wage_manuf = sum(exp(l)) if manuf == 1
	qui bys NACE2 year: egen tot_wageN2_manuf =  sum(exp(l)) if manuf == 1
	
	qui gen wage_N2_manuf = tot_wageN2_manuf / tot_wage_manuf 	

	
	qui bys year: egen tot_wage_service = sum(exp(l)) if service == 1
	qui bys NACE2 year: egen tot_wageN2_service = sum(exp(l)) if service == 1
	
	qui gen wage_N2_service = tot_wageN2_service / tot_wage_service 	
	
	
	qui bys year: egen tot_wage_trade = sum(exp(l)) if trade == 1
	qui bys NACE2 year: egen tot_wageN2_trade = sum(exp(l)) if trade == 1
	
	qui gen wage_N2_trade = tot_wageN2_trade / tot_wage_trade 	
	
	
	qui bys year: egen tot_wage_otherserv = sum(exp(l)) if otherserv == 1
	qui bys NACE2 year: egen tot_wageN2_otherserv = sum(exp(l)) if otherserv == 1
	
	qui gen wage_N2_otherserv = tot_wageN2_otherserv / tot_wage_otherserv 	
	
	
	
	* Create aggregate measure based on Edmond, Midrigan and Xu (2018)

	gen lwedge = (e_l_y/alpha_l)
	
	gen upp_sh = (e_l_y/e_l_y_agg)*wsales_N2_f*(1/lwedge)
	gen down_sh = (e_vp_y/e_vp_y_agg)*wsales_N2_f*(1/markup_vp)

	bys NACE2 year: egen down_avg = total(down_sh)
	bys NACE2 year: egen upp_avg = total(upp_sh)
	
	gen md_EMX = down_avg/upp_avg
	gen mu_EMX = 1/down_avg
	

	* Markup and markdown equivalence
	
	bys NACE2 year: egen totvp = total(exp(vp))
	bys NACE2 year: egen totl = total(exp(l))

// 	qui bys NACE2 year: egen tot_salesN2 = sum(exp(y_c))
		
	gen sharel = totl/tot_salesN2
	gen sharevp = totvp/tot_salesN2
	
	gen invsharevp = 1/sharevp 
	gen mu_eqv = e_vp_y_agg * invsharevp
	

	gen invsharel = 1/sharel 
	gen invmu = 1/mu_eqv 
	gen invelasvp = 1/e_vp_y_agg 
	gen invtotl = 1/totl 
	
	gen prodelas = e_l_y_agg * invelasvp
	gen prodcost = totvp * invtotl
	
	gen md_eqv =  e_l_y_agg * invmu * invsharel
	gen md_eqv2 = prodelas * prodcost
	
	gen mu_md = e_l_y_agg * invsharel
	

	* Keep one observation to avoid repetitions
	
 	bys NACE2 year: keep if _n == 1	
	

	* Counterfactual markup and markdown
	
	bys NACE2: egen invsharevp_mean = mean(invsharevp) 
	bys NACE2: egen e_vp_y_agg_mean = mean(e_vp_y_agg) 

	bys NACE2: egen invsharel_mean = mean(invsharel) 
	bys NACE2: egen e_l_y_agg_mean = mean(e_l_y_agg) 
	bys NACE2: egen invmu_mean = mean(invmu) 

	bys NACE2: egen prodelas_mean = mean(prodelas) 
	bys NACE2: egen prodcost_mean = mean(prodcost) 
	
	
	* Define markup at N2 level as a linearization of a product
	
	gen mu_1 = e_vp_y_agg*invsharevp_mean 
	gen mu_2 = invsharevp*e_vp_y_agg_mean 
	gen mu_3 = e_vp_y_agg_mean*invsharevp_mean

	gen mu_comp = mu_1 + mu_2 - mu_3
// 	bro mucomp mu_eqv

		
	* Define markdown at N2 level as a linearization of a product 
			
	gen md_1 = prodelas*prodcost_mean 
	gen md_2 = prodcost*prodelas_mean 
	gen md_3 = prodelas_mean*prodcost_mean
	
	gen md_comp = md_1 + md_2 - md_3
//  bro md_comp md_eqv


// 	gen md_1 = e_l_y_agg*invmu_mean*invsharel_mean 
// 	gen md_2 = invmu*e_l_y_agg_mean*invsharel_mean
// 	gen md_3 = invsharel*e_l_y_agg_mean*invmu_mean
// 	gen md_4 = e_l_y_agg_mean*invmu_mean*invsharel_mean
//	
// 	gen md_comp = md_1 + md_2 + md_3 - 2*md_4	
	
	
// CAREFUL (sum of wgt is 15.9999999407446)	
// 	reghdfe mu_comp mu_1 [pweight=wage_N2], absorb(NACE2) 
// 	reghdfe mu_comp mu_2 [pweight=wage_N2], absorb(NACE2) 
//	
// 	reghdfe md_comp md_1 [pweight=wage_N2], absorb(NACE2) 
// 	reghdfe md_comp md_2 [pweight=wage_N2], absorb(NACE2) 
	
// 	gen diff = md_eqv - md_EMX
// 	gen diff2 = md_eqv - md_comp
// 	sum diff2, d
//		
// 	summarize diff2, d
// 	qui gen p5 =`r(p5)'
//
// 	gen flagOUT = 1 if diff2 < p5
//	
// 	gen repmd = md_comp+diff2
//	
// 	replace md_comp = repmd if flagOUT == 1
//	
// 	gen diff3 = md_eqv - md_comp	
// 	sum diff3, d


// 	bro md_comp md_eqv flagOUT



	* Savings at NACE2 level
	
	preserve
	keep  year NACE2 mu_EMX* md_EMX* w* 
	order year NACE2 mu_EMX* md_EMX* w* 
	
	save ${path}\figures\markups_markdowns_EMX_NACE2_`filename'.dta, replace
	restore 

	
	* Aggregation per year
	
	foreach v in mu md {
	    foreach w in EMX eqv comp "1" "2" "3" {
			bys year: egen `v'_`w'_agg_now = mean(`v'_`w')    
			bys year: egen `v'_`w'_agg_ws = sum(`v'_`w'*wsales_N2)     
			bys year: egen `v'_`w'_agg_wc = sum(`v'_`w'*wcost_vpN2)    
			bys year: egen `v'_`w'_agg_we = sum(`v'_`w'*wemp_N2)    
			bys year: egen `v'_`w'_agg_wl = sum(`v'_`w'*wage_N2)  
		}
	} 
	
	
	* Aggregation per year mu, md components
	
	foreach v in e_vp_y_agg e_l_y_agg invsharevp invsharel prodelas prodcost e_vp_y_binp e_vp_y_bkinp e_vp_y_blinp e_l_y_bl e_l_y_bkl e_l_y_binpl varinpagg kagg lagg totvp totl tot_salesN2 {
			bys year: egen `v'_agg_now = mean(`v')    
			bys year: egen `v'_agg_ws = sum(`v'*wsales_N2)     
			bys year: egen `v'_agg_wc = sum(`v'*wcost_vpN2)    
			bys year: egen `v'_agg_we = sum(`v'*wemp_N2)    
			bys year: egen `v'_agg_wl = sum(`v'*wage_N2)  
	} 
	

	* Keep one obs per year
	
	bys year : keep if _n == 1	
	
	
	* Normalization 
	
	foreach x in now ws wc we wl {
	    foreach y in EMX eqv comp  "1" "2" "3" {
			sort year
			gen mu_norm_`y'_`x' = mu_`y'_agg_`x'/mu_`y'_agg_`x'[1]
			sort year
			gen md_norm_`y'_`x' = md_`y'_agg_`x'/md_`y'_agg_`x'[1]

		}
	}
	

	* Normalization 
	
	foreach x in now ws wc we wl {
	    foreach y in e_vp_y_agg e_l_y_agg invsharevp invsharel prodelas prodcost e_vp_y_binp e_vp_y_bkinp e_vp_y_blinp e_l_y_bl e_l_y_bkl e_l_y_binpl varinpagg kagg lagg totvp totl tot_salesN2 {
			sort year
			gen norm_`y'_`x' = `y'_agg_`x'/`y'_agg_`x'[1]
		}
	}
	

	* Some growth rates
	
	foreach x in now ws wc we wl {
	    foreach y in e_vp_y_agg e_l_y_agg invsharevp invsharel prodelas prodcost e_vp_y_binp e_vp_y_bkinp e_vp_y_blinp e_l_y_bl e_l_y_bkl e_l_y_binpl varinpagg kagg lagg totvp totl tot_salesN2 {
			sort year
			gen growth_`y'_`x' = (`y'_agg_`x'-`y'_agg_`x'[_n-1])/`y'_agg_`x'[_n-1]
		}
	}	
	

	* Savings at aggregate level
	
	keep year *agg* *norm* growth*
	order year mu* md*

    save ${path}\figures\markups_markdowns_EMX_`filename'.dta, replace

	
	
	* Aggregate by manufacturers and service types
	
foreach u in manuf service trade otherserv {
	
	use ${path}\figures\markups_markdowns_EMX_NACE2_`filename'.dta, clear
	
		
	* Aggregation per year
	
	foreach v in mu md {
	    foreach w in EMX {
			bys year: egen `v'_`w'_agg_wc_`u' = sum(`v'_`w'*wcost_vpN2_`u') 
			bys year: egen `v'_`w'_agg_wl_`u' = sum(`v'_`w'*wage_N2_`u') 
		}
	} 
	
	
	* Keep one obs per year
	
	bys year : keep if _n == 1	
	
	
	* Normalization 
	
	foreach x in wc wl {
	    foreach y in EMX {
			sort year
			gen mu_norm_`y'_`x'_`u' = mu_`y'_agg_`x'_`u'/mu_`y'_agg_`x'_`u'[1]
			sort year
			gen md_norm_`y'_`x'_`u' = md_`y'_agg_`x'_`u'/md_`y'_agg_`x'_`u'[1]

		}
	}


	* Savings at aggregate level
	
	keep year *agg* *norm*
	order year mu* md*

    save ${path}\figures\markups_markdowns_EMX_`filename'_`u'.dta, replace
	
}


   
/*
    * Top6 industry-level markups and markdown
	
   	use ${path}\figures\markups_markdowns_EMX_NACE2_`filename', clear
	qui merge m:1 NACE2 using ${path}\Data_temp\top6_NACE2.dta, nogen keep(3)
	qui sort NACE2 year
	
	* Call markup and markdown at NACE2 
	
	bys NACE2: gen md_EMX_norm_nace2 = md_EMX/md_EMX[1]
	bys NACE2: gen mu_EMX_norm_nace2 = mu_EMX/mu_EMX[1]

	order year NACE2 mu* md*  
	save ${path}\figures\markups_markdowns_EMX_NACE2_top6_`filename'.dta, replace
	
	
	
	* Static OP at NACE2 level for markup and markdown, CAREFUL HERE YOUR WEIGHTS DOES NOT SUM TO 1
	
	use ${path}\figures\markups_markdowns_EMX_NACE2_`filename'.dta, clear
	qui merge 1:1 year NACE2 using ${path}\Data_temp\wsales_N22.dta, nogen keep(3)

	foreach v in mu_EMX md_EMX {
		foreach w in wsales_N22 {	 
		   qui bys year: egen `v'_`w'= sum(`v'*`w')
		   qui bys year: egen `v'_`w'_mean = mean(`v'*`w') 	
		   qui bys year: gen cov_`v'_`w' = `v'_`w'-`v'_`w'_mean
		}
	}
	
	keep year cov* *_mean
	bys year: keep if _n==1
	 
	save ${path}\figures\markups_markdowns_EMX_NACE2_OP_cov_`filename'.dta, replace
*/
 

 
 
	* Figures 
	qui grstyle init
	qui grstyle set plain, compact grid dotted /*no grid*/
	qui grstyle color major_grid gs13
	qui grstyle set symbol
	qui grstyle set lpattern
	qui grstyle set color Dark2, n(4) 
	qui grstyle set color Dark2, n(4)  opacity(34): p#markfill



// qui	tw (connect mu_norm_eqv_ws  year) (connect mu_norm_EMX_ws year) (connect mu_norm_comp_ws year)
// qui	tw (connect md_norm_eqv_ws  year) (connect md_norm_EMX_ws year) (connect md_norm_comp_ws year)


	* Markdown and markup figures
	
 	use ${path}\figures\markups_markdowns_EMX_`filename'.dta, clear 
	
	/* *plot for voxeu writeup
	use ${path}\figures\markups_markdowns_EMX_TL_LL_purch.dta, clear 

	qui	tw  (connect mu_norm_EMX_wc year) (connect md_norm_EMX_wl year), xtitle("Year") ytitle("") legend(order(1 "Aggregate Markup" 2 "Aggregate Markdown") cols(3))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(0.94(0.02)1.04)
		qui graph export ${path}\figures\LT_mumd.png, as(png) replace
    */

	qui	tw (connect mu_norm_EMX_ws year) (connect mu_norm_EMX_wc year) (connect mu_norm_EMX_wl year), xtitle("Year") ytitle("") legend(order(1 "Sales revenue" 2 "Intermediate input cost" 3 "Labor cost") cols(3))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(0.96(0.01)1.04)
		qui graph export ${path}\figures\MarkupsLT.pdf, as(pdf) replace

	qui	tw (connect md_norm_EMX_ws year) (connect md_norm_EMX_wc year) (connect md_norm_EMX_wl year), xtitle("Year") ytitle("") legend(order(1 "Sales revenue" 2 "Intermediate input cost" 3 "Labor cost") cols(3))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(0.94(0.01)1.02)
		qui graph export ${path}\figures\MarkdownsLT.pdf, as(pdf) replace
		


	qui	tw (connect mu_EMX_agg_ws year) (connect mu_EMX_agg_wc year) (connect mu_EMX_agg_wl year), xtitle("Year") ytitle("") legend(order(1 "Sales revenue" 2 "Intermediate input cost" 3 "Labor cost") cols(3))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(1.06(0.01)1.16)
		qui graph export ${path}\figures\MarkupslevelsLT.pdf, as(pdf) replace

	qui	tw (connect md_EMX_agg_ws year) (connect md_EMX_agg_wc year) (connect md_EMX_agg_wl year), xtitle("Year") ytitle("") legend(order(1 "Sales revenue" 2 "Intermediate input cost" 3 "Labor cost") cols(3))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(1.9(0.05)2.35)
		qui graph export ${path}\figures\MarkdownslevelsLT.pdf, as(pdf) replace		
		
		
	qui	tw (connect mu_EMX_agg_now year) (connect mu_EMX_agg_wc year), xtitle("Year") ytitle("") legend(order(1 "Unweighted" 2 "Intermediate input cost") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(1.07(0.01)1.17)
		qui graph export ${path}\figures\MarkupslevelsLTcomparison.pdf, as(pdf) replace

	qui	tw (connect md_EMX_agg_now year) (connect md_EMX_agg_wl year), xtitle("Year") ytitle("") legend(order(1 "Unweighted" 2 "Labor cost") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(1.9(0.02)2.05)
		qui graph export ${path}\figures\MarkdownslevelsLTcomparison.pdf, as(pdf) replace		
		
		
		
/*	
	qui	tw connect mu_norm_EMX_wc year, xtitle("Year") ytitle("Aggregate Markup") legend(off)  xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(0.96(0.01)1.04)
		qui graph export ${path}\figures\MarkupsLT1.pdf, as(pdf) replace

	qui	tw connect md_norm_EMX_wl year, xtitle("Year") ytitle("Aggregate Markdown") legend(off)  xlabel(2004(2)2018) yline(1, lcolor(black%50))
		qui graph export ${path}\figures\MarkdownsLT1.pdf, as(pdf) replace
*/		

//  qui	tw (connect norm_e_vp_y_agg_ws  year) (connect norm_invsharevp_ws year) (connect mu_norm_EMX_ws year), xtitle("Year") ytitle("") legend(order(1 "elas" 2 "inv share" 3 "Mu") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50))
// 	qui	tw (connect norm_prodelas_ws  year) (connect norm_prodcost_ws year) (connect md_norm_EMX_ws year), xtitle("Year") ytitle("") legend(order(1 "elas ratio" 2 "cost ratio" 3 "Md") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50))
//
// 	qui tw (connect mu_norm_comp_ws year) (connect mu_norm_EMX_ws year), xtitle("Year") ytitle("") legend(order(1 "Mu comp" 2 "Mu") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50))
// 	qui tw (connect md_norm_comp_ws year) (connect md_norm_EMX_ws year), xtitle("Year") ytitle("") legend(order(1 "Md comp" 2 "Md") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50))
	
		
// 	* Markdown and markup counterfactual figures

	qui	tw (connect mu_norm_1_wc year) (connect mu_norm_2_wc year), xtitle("Year") ytitle("") legend(order( 1 "Markup output elasticity" 2 "Markup inverse share") cols(3))  xlabel(2004(2)2018) yline(1, lcolor(black%50))
	qui graph export ${path}\figures\MarkupsLTcount.pdf, as(pdf) replace
		

	qui	tw (connect md_norm_1_wl year) (connect md_norm_2_wl year), xtitle("Year") ytitle("") legend(order( 1 "Markdown output elasticity ratio" 2 "Markdown cost ratio") cols(3))  xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(0.8(0.1)1.2)
	qui graph export ${path}\figures\MarkdownsLTcount.pdf, as(pdf) replace
	

	 
	* Aggregate output elasticities 
// 	qui	tw (connect e_vp_y_agg_agg_ws year) (connect e_vp_y_binp_agg_ws year) (connect e_vp_y_bkinp_agg_ws year) (connect e_vp_y_blinp_agg_ws year), xtitle("Year") ytitle("") legend(order( 1 "elast varinp" 2 "elast varinp sqr2" 3 "elast varinp-capital" 4 "elast varinp-labor") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50))
// 	qui graph export ${path}\figures\Elastvarinpdec.pdf, as(pdf) replace
	
	
	qui	line growth_e_vp_y_agg_wc year, xtitle("Year") ytitle("elast varinp") name(elas1, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_e_vp_y_binp_wc year, xtitle("Year") ytitle("elast varinp sqr2") name(elas2, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_e_vp_y_bkinp_wc year, xtitle("Year") ytitle("elast varinp-capital") name(elas3, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_e_vp_y_blinp_wc year, xtitle("Year") ytitle("elast varinp-labor") name(elas4, replace) nodraw xlabel(2004(2)2018)
	
	graph combine elas1 elas2 elas3 elas4
	qui graph export ${path}\figures\Elastvarinpdec.pdf, as(pdf) replace	
	

// 	qui	tw (connect e_l_y_agg_agg_ws year) (connect e_l_y_bl_agg_ws year) (connect e_l_y_bkl_agg_ws year) (connect e_l_y_binpl_agg_ws year), xtitle("Year") ytitle("") legend(order( 1 "elast labor" 2 "elast labor sqr2" 3 "elast labor-capital" 4 "elast labor-varinp") cols(2))  xlabel(2004(2)2018) yline(1, lcolor(black%50))
// 	qui graph export ${path}\figures\Elastlabordec.pdf, as(pdf) replace
	
	
	qui	line growth_e_l_y_agg_wl year, xtitle("Year") ytitle("elast labor") name(elas11, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_e_l_y_bl_wl year, xtitle("Year") ytitle("elast labor sqr2") name(elas22, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_e_l_y_bkl_wl year, xtitle("Year") ytitle("elast labor-capital") name(elas33, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_e_l_y_binpl_wl year, xtitle("Year") ytitle("elast labor-varinp") name(elas44, replace) nodraw xlabel(2004(2)2018)
	
	graph combine elas11 elas22 elas33 elas44
	qui graph export ${path}\figures\Elastlabordec.pdf, as(pdf) replace		
	
	
	qui	line growth_varinpagg_wc year, xtitle("Year") ytitle("varinp agg") name(inputs1, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_lagg_wc year, xtitle("Year") ytitle("capital agg") name(inputs2, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_kagg_wc year, xtitle("Year") ytitle("labor agg") name(inputs3, replace) nodraw xlabel(2004(2)2018)

	graph combine inputs1 inputs2 inputs3
	qui graph export ${path}\figures\Inputsagg.pdf, as(pdf) replace	
	
	
	qui	line growth_invsharevp_wc year, xtitle("Year") ytitle("inv. share vp agg") name(share1, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_invsharel_wc year, xtitle("Year") ytitle("inv. share labor agg") name(share2, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_totvp_wc year, xtitle("Year") ytitle("vp agg") name(share3, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_totl_wc year, xtitle("Year") ytitle("labor agg") name(share4, replace) nodraw xlabel(2004(2)2018)
	qui	line growth_tot_salesN2_wc year, xtitle("Year") ytitle("sales agg") name(share5, replace) nodraw xlabel(2004(2)2018)
	qui	tw (connect growth_totvp_wc year) (connect growth_totl_wc year) (connect growth_tot_salesN2_wc year), xtitle("Year") ytitle("") legend(order( 1 "vp agg" 2 "labor agg" 3 "sales agg")) name(share6, replace) nodraw xlabel(2004(2)2018)

	graph combine share1 share2 share3 share4 share5 share6
	qui graph export ${path}\figures\Invshareagg.pdf, as(pdf) replace		

	
	
	
	* Markdown and markup figures for manufacturers
	
 	use ${path}\figures\markups_markdowns_EMX_`filename'_manuf.dta, clear 	
	qui merge m:1 year using ${path}\figures\markups_markdowns_EMX_`filename'_trade.dta
	drop _m
	qui merge m:1 year using ${path}\figures\markups_markdowns_EMX_`filename'_otherserv.dta
	drop _m
	
	qui	tw (connect mu_norm_EMX_wc_manuf year) (connect mu_norm_EMX_wc_trade year) (connect mu_norm_EMX_wc_otherserv year), xtitle("Year") ytitle("") legend(order(1 "Manufacturing" 2 "Wholesale and retail" 3 "Other services") cols(3)) xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(0.94(0.02)1.06) name(markupmanserv1, replace) 
	qui graph export ${path}\figures\MarkupsManufServiceLT.pdf, as(pdf) replace

	
	qui	tw (connect md_norm_EMX_wl_manuf year) (connect md_norm_EMX_wl_trade year) (connect md_norm_EMX_wl_otherserv year), xtitle("Year") ytitle("") legend(order(1 "Manufacturing" 2 "Wholesale and retail" 3 "Other services") cols(3)) xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(0.88(0.02)1.02) name(markdownmanserv1, replace) 
	qui graph export ${path}\figures\MarkdownsManufServiceLT.pdf, as(pdf) replace
	
	
	
// 	graph combine markupmanserv1 markdownmanserv1
// 	qui graph export ${path}\figures\MarkupsMarkdownsManufServiceLT.pdf, as(pdf) replace	
	
	
 	use ${path}\figures\markups_markdowns_EMX_`filename'_trade.dta, clear 	
	qui merge m:1 year using ${path}\figures\markups_markdowns_EMX_`filename'_otherserv.dta
	drop _m
	
	qui	tw (connect mu_norm_EMX_wc_trade year) (connect mu_norm_EMX_wc_otherserv year), xtitle("Year") ytitle("") legend(order(1 "Trade" 2 "Other services") cols(2)) xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(0.94(0.02)1.06)
	qui graph export ${path}\figures\MarkupsBDServiceLT.pdf, as(pdf) replace

	
	qui	tw (connect md_norm_EMX_wl_trade year) (connect md_norm_EMX_wl_otherserv year), xtitle("Year") ytitle("") legend(order(1 "Trade" 2 "Other services") cols(2)) xlabel(2004(2)2018) yline(1, lcolor(black%50)) ylabel(0.88(0.02)1.02) 
	qui graph export ${path}\figures\MarkdownsBDServiceLT.pdf, as(pdf) replace	
	
	

	
	* Erase temp files
	
	cd ${path}\Data_temp
	local list : dir . files "*.dta"
	foreach f of local list {
		erase "`f'"
    }

	
end 


