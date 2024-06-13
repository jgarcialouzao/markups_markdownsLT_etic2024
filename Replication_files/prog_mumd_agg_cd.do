



**********************************************************************************
********************         Aggregate Markup and Markdown                  ********************
program define prog_mumd_agg_cd

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
		
	gen e_vp_y_agg = betavarinp 
	gen e_l_y_agg  = betal 
	 
	 
	* Drop if negative elasticities 
	
	drop if e_l_y_agg<0
	drop if e_vp_y_agg<0
	
// 	drop kagg lagg varinpagg
	
	
	* Rename variables
	
	rename varinp vp
	rename markup_varinp markup_vp
	rename alpha_varinp alpha_vp
	rename e_varinp_y e_vp_y
	
	
	* Sales revenue weights
	
	qui bys year: egen tot_sales = sum(exp(y_c))
	qui bys NACE2 year: egen tot_salesN2 = sum(exp(y_c))

	qui gen wsales_N2_f = exp(y_c) / tot_salesN2
	qui gen wsales_N2 = tot_salesN2 / tot_sales
	
	
	* Varinp weights
	
	qui bys year: egen tot_cost_vp = sum(exp(vp))
	qui bys NACE2 year: egen tot_cost_vpN2 =  sum(exp(vp))

	qui gen wcost_vpN2_f = exp(vp) / tot_cost_vpN2
	qui gen wcost_vpN2 = tot_cost_vpN2 / tot_cost_vp 
	

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
	
	gen md_eqv =  prodelas*prodcost
	

	* Keep one observation to avoid repetitions
	
	bys NACE2 year: keep if _n == 1		
	
	
	* Savings at NACE2 level
	
	preserve
	keep  year NACE2 mu_EMX* md_EMX*
	order year NACE2 mu_EMX* md_EMX* 
	
	save ${path}\figures\markups_markdowns_EMX_NACE2_`filename'.dta, replace
	restore 
	
	
	* Aggregation per year
	
	foreach v in mu md {
	    foreach w in EMX eqv {
			bys year: egen `v'_`w'_agg_now = mean(`v'_`w')    
			bys year: egen `v'_`w'_agg_ws = sum(`v'_`w'*wsales_N2)     
			bys year: egen `v'_`w'_agg_wc = sum(`v'_`w'*wcost_vpN2)    
			bys year: egen `v'_`w'_agg_we = sum(`v'_`w'*wemp_N2)    
			bys year: egen `v'_`w'_agg_wl = sum(`v'_`w'*wage_N2)  
		}
	} 

	
	bys year : keep if _n == 1	
	
	
	* Normalization 
	
	foreach x in now ws wc we wl {
	    foreach y in EMX eqv {
			sort year
			gen mu_norm_`y'_`x' = mu_`y'_agg_`x'/mu_`y'_agg_`x'[1]
			sort year
			gen md_norm_`y'_`x' = md_`y'_agg_`x'/md_`y'_agg_`x'[1]

		}
	}
	
	
	* Savings at aggregate level
		
	keep year *agg* *norm*
	order year mu* md*

    save ${path}\figures\markups_markdowns_EMX_`filename'.dta, replace
	
	
	* Figures 

	qui grstyle init
	qui grstyle set plain, compact grid dotted /*no grid*/
	qui grstyle color major_grid gs13
	qui grstyle set symbol
	qui grstyle set lpattern
	qui grstyle set color Dark2, n(4) 
	qui grstyle set color Dark2, n(4)  opacity(34): p#markfill



	qui	tw (connect mu_norm_EMX_ws year) (connect mu_norm_EMX_wc year) (connect mu_norm_EMX_wl year), xtitle("Year") ytitle("") legend(order(1 "Sales revenue" 2 "Variable input cost" 3 "Labor cost") cols(3))  xlabel(2004(2)2018) yline(1, lcolor(black%50))
		qui graph export ${path}\figures\MarkupsLTCD.pdf, as(pdf) replace


	qui	tw (connect md_norm_EMX_ws year) (connect md_norm_EMX_wc year) (connect md_norm_EMX_wl year), xtitle("Year") ytitle("") legend(order(1 "Sales revenue" 2 "Variable input cost" 3 "Labor cost") cols(3))  xlabel(2004(2)2018) yline(1, lcolor(black%50))
		qui graph export ${path}\figures\MarkdownsLTCD.pdf, as(pdf) replace
		

	
	
end 