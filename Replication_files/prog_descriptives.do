**********************************************************************************
********************           Descriptives                   ********************
program define prog_descriptives

	syntax [, filename(string)]


******************** General summary statistics
foreach s in mean sd p10 p50 p90 {
preserve 
use ${path}\Data_stata\finaldata_`filename'.dta, clear
keep y y_c tfp l varinp k alpha_l alpha_varinp alpha_k alpha_o pirate_gross pirate_ebitda 
gen statistic = "`s'"
gen nobs = 1
label var nobs "No. Observations"
*gcollapse (`s') y y_c tfp l varinp k alpha_l alpha_varinp alpha_k alpha_o pirate_gross pirate_ebitda (sum) nobs , by( statistic)
foreach x in y l varinp k {
    replace `x' = exp(`x')

}
gcollapse (`s') y l varinp k (sum) nobs , by( statistic)
format y l varinp k %12.2f
save ${path}\tables\stat_`s'.dta, replace
restore
}

clear 
foreach s in mean sd p10 p50 p90 {
append using ${path}\tables\stat_`s'.dta
}    
save ${path}\tables\data_stats_1.dta, replace
 foreach s in mean sd p10 p50 p90 {
   erase   ${path}\tables\stat_`s'.dta
}


******************** Translog elasticities by sector
foreach s in mean sd  {
preserve 
use ${path}\Data_stata\finaldata_`filename'.dta, clear
keep e_varinp_y e_l_y e_k_y NACE2
gen statistic = "`s'"
gen nobs = 1
label var nobs "No. Observations"
gcollapse (`s') e_varinp_y e_l_y e_k_y (sum) nobs , by(statistic NACE2)
save ${path}\tables\stat_`s'.dta, replace
restore
}

clear 
foreach s in mean sd  {
append using ${path}\tables\stat_`s'.dta
}    
sort NACE2 statistic
save ${path}\tables\data_elasticities_sector.dta, replace
 foreach s in mean sd {
   erase   ${path}\tables\stat_`s'.dta
}

******************** Cost shares
use ${path}\Data_stata\finaldata_`filename'.dta, clear
** Weights: sales and costs (total and PF induced)
use ${path}\Data_stata\finaldata_TL_LL_purch.dta, clear

drop if year>2018 

* No weight 
bys year: gen nobs = _N
bys NACE2 year: gen nobs_n2 = _N
gen now      = 1 / nobs 
gen now_N2_f = 1 / nobs_n2
gen now_N2   = nobs_n2 / nobs


* Sales revenue
qui bys year: egen tot_sales = sum(exp(y_c))
qui bys NACE2 year: egen tot_salesN2 = sum(exp(y_c))
qui gen wsales_f    = exp(y_c)  / tot_sales
qui gen wsales_N2_f = exp(y_c)  / tot_salesN2
qui gen wsales_N2   = tot_salesN2 / tot_sales


* Variabel input costs
qui bys year: egen tot_cost_mat = sum(exp(varinp))
qui bys NACE2 year: egen tot_cost_matN2 =  sum(exp(varinp))
qui gen wcost_matf    = exp(varinp) / tot_cost_mat 
qui gen wcost_matN2_f = exp(varinp) / tot_cost_matN2
qui gen wcost_matN2   = tot_cost_matN2 / tot_cost_mat 


* Number of employees
qui bys year: egen tot_emp = sum(empl)
qui bys NACE2 year: egen tot_empN2 =  sum(empl)
qui gen wemp_f    = empl / tot_emp 
qui gen wemp_N2_f = empl / tot_empN2
qui gen wemp_N2   = tot_empN2 / tot_emp 

* Employment costs
qui bys year: egen tot_wage = sum(exp(l))
qui bys NACE2 year: egen tot_wageN2 =  sum(exp(l))
qui gen wage_f    = exp(l) / tot_wage 
qui gen wage_N2_f = exp(l) / tot_wageN2
qui gen wage_N2   = tot_wageN2 / tot_wage 


* Aggregate-level: for the cost and profit shares in the appendix A.2
foreach v in markup_varinp markdown_l pirate_gross pirate_ebitda alpha_l alpha_varinp alpha_k alpha_o {
foreach w in now_N2 wsales_N2 /*wcost_sN2*/ wcost_matN2 wemp_N2 wage_N2{
qui bys year: egen `v'_2d_`w' = sum(`w'*`w'_f*`v')	
}
}
preserve 
keep year markup_varinp_2d* pirate_gross_2d* pirate_ebitda_2d* alpha_l_2d* alpha_varinp_2d* alpha_k_2d* alpha_o_2d*
bys year: keep if _n == 1
foreach v in alpha_varinp_2d_now_N2 alpha_varinp_2d_wsales_N2 alpha_l_2d_now_N2 alpha_l_2d_wsales_N2 alpha_k_2d_now_N2 alpha_k_2d_wsales_N2 pirate_ebitda_2d_now_N2 pirate_ebitda_2d_wsales_N2{
	gen norm_`v' = `v'/`v'[1]
}
save ${path}\figures\markups_cost_profits_AGG2D_TL_LL_cogs.dta, replace
restore 

* Plot and combine figures to produce Figure A.4
cd ${path}\figures

use ${path}\figures\markups_cost_profits_AGG2D_TL_LL_cogs.dta, clear



#delimit;
   qui graph tw 
					(connected alpha_varinp_2d_now_N2 year, saving(1))
					(connected alpha_varinp_2d_wsales_N2 year),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Variable Input Cost/Sales)
					legend(order(1 "Unweighted" 2 "Sales Weighted" ) cols(2)  position(6)) 
					ylabel(0.5(0.05)0.75) xlabel(2004(2)2018);
#delimit cr		

#delimit;
   qui graph tw 
                    (connected alpha_l_2d_now_N2  year, saving(2))
					(connected alpha_l_2d_wsales_N2 year),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Labour Cost/Sales)
					legend(order(1 "Unweighted" 2 "Sales Weighted" ) cols(2)  position(6)) 
					ylabel(0.1(0.04)0.3) xlabel(2004(2)2018);
#delimit cr		

#delimit;
   qui graph tw 						
                    (connected alpha_k_2d_now_N2  year, saving(3))
					(connected alpha_k_2d_wsales_N2 year),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Capital Cost/Sales)
					legend(order(1 "Unweighted" 2 "Sales Weighted" ) cols(2)  position(6)) 
					ylabel(0(0.03)0.15) xlabel(2004(2)2018);
#delimit cr		

#delimit;
   qui graph tw 				
                    (connected pirate_ebitda_2d_now_N2  year, saving(4))
					(connected pirate_ebitda_2d_wsales_N2 year),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(EBITDA/Sales)
					legend(order(1 "Unweighted" 2 "Sales Weighted" ) cols(2)  position(6)) 
					ylabel(0(0.04)0.2) xlabel(2004(2)2018);
#delimit cr		
									
gr combine 1.gph 2.gph 3.gph 4.gph, col(2)				
graph export ${path}\figures\cost_profit.pdf, as(pdf) replace 
erase ${path}\figures\1.gph
erase ${path}\figures\2.gph
erase ${path}\figures\3.gph
erase ${path}\figures\4.gph

*Normalized graph
#delimit;
   qui graph tw 
					(connected norm_alpha_varinp_2d_now_N2 year, saving(1))
					(connected norm_alpha_varinp_2d_wsales_N2 year),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Variable Input Cost/Sales)
					legend(order(1 "Unweighted" 2 "Sales Weighted" ) cols(2)  position(6)) 
					ylabel(0.85(0.04)1.05) xlabel(2004(2)2018);
#delimit cr		

#delimit;
   qui graph tw 
                    (connected norm_alpha_l_2d_now_N2  year, saving(2))
					(connected norm_alpha_l_2d_wsales_N2 year),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Labour Cost/Sales)
					legend(order(1 "Unweighted" 2 "Sales Weighted" ) cols(2)  position(6)) 
					ylabel(0.95(0.06)1.25) xlabel(2004(2)2018);
#delimit cr		

#delimit;
   qui graph tw 						
                    (connected norm_alpha_k_2d_now_N2  year, saving(3))
					(connected norm_alpha_k_2d_wsales_N2 year),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(Capital Cost/Sales)
					legend(order(1 "Unweighted" 2 "Sales Weighted" ) cols(2)  position(6)) 
					ylabel(0.65(0.24)1.85) xlabel(2004(2)2018);
#delimit cr		

#delimit;
   qui graph tw 				
                    (connected norm_pirate_ebitda_2d_now_N2  year, saving(4))
					(connected norm_pirate_ebitda_2d_wsales_N2 year),
					xtitle("Year") ytitle("") xsize(4) ysize(3)
					title(EBITDA/Sales)
					legend(order(1 "Unweighted" 2 "Sales Weighted" ) cols(2)  position(6)) 
					ylabel(0.1(0.4)2.1) xlabel(2004(2)2018);
#delimit cr		
									
gr combine 1.gph 2.gph 3.gph 4.gph, col(2)				
graph export ${path}\figures\norm_cost_profit.pdf, as(pdf) replace 
erase ${path}\figures\1.gph
erase ${path}\figures\2.gph
erase ${path}\figures\3.gph
erase ${path}\figures\4.gph


 
end 


