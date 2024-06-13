program define  xlsfirms

if `1'<2000 {

qui import delimited "${path}\Data_raw\n`1'" , case(preserve) clear

*monetary variables before 2000 in Lytas, use fixed exchange rate to convert to Euros (3.4528)
qui g year =`1'
qui rename sal_kodas id 
qui label var id "Firm's ID"
qui drop if id==.
qui rename likviduota legal_death_y
qui label var legal_death_y "Firm's liquidation year"
qui destring legal_death_y, replace
qui rename registracijos_data legal_birth
qui label var legal_birth "Firm's creation date"
qui rename isregistravimo_data legal_death
qui label var legal_death "Firm's liquidation date"
qui rename veikla2 NACE1_2
qui label var NACE1_2 "Sector of activity (NACE 1.2)"

if `1' < 1999 {
qui rename rusis type
qui label var type "Type of a firm (code)"
}

if `1' == 1999 {
qui rename RUSIS type
qui label var type "Type of a firm (code)"
}

qui rename NF own
qui label var own "Form of ownership"
qui rename kapitalas_valst_sav cap_loc_gov
qui label var cap_loc_gov "Local state ownership (the value of firm's equity by state or municipalities)"
qui replace cap_loc_gov = cap_loc_gov/3.4528
qui rename kapitalas_fiz cap_loc_priv
qui label var cap_loc_priv "Local private ownership (the value of firm's equity by Lithuanian natural or legal persons)"
qui replace cap_loc_priv = cap_loc_priv/3.4528
qui rename kapitalas_uzsienio cap_int
qui label var cap_int  "Foreign ownership (the value of firm's equity by foreign investors)"
qui replace cap_int = cap_int/3.4528
qui rename R_100 empl
qui label var emp "Average number of employees"
qui rename R_101 empl_p
qui label var empl_p "Average number of part-time employees"
qui rename R_103 hours
qui label var  hours "Number of hours worked"
qui rename R_106 hours_p
qui label var hours_p "Number of hours by part-time workers"
qui rename R_327 a_tot
qui label var a_tot "Total assets"
qui replace a_tot=a_tot/3.4528
qui rename R_300 fa_intang
qui label var fa_intang "Intangible fixed assets"
qui replace fa_intang=fa_intang/3.4528
qui rename R_301 fa_tang
qui label var fa_tang "Tangible fixed assets"
qui replace fa_tang=fa_tang/3.4528
qui rename R_302 fa_fin
qui label var fa_fin "Financial fixed assets"
qui replace fa_fin=fa_fin/3.4528
qui rename R_329 fa_oth
qui label var fa_oth "Other fixed assets"
qui replace fa_oth=fa_oth/3.4528
qui rename R_325 ca_cash
qui label var ca_cash "Cash and cash equivalents"
qui replace ca_cash = ca_cash/3.4528
qui rename R_309 ca_inv
qui label var ca_inv "Total inventories"
qui replace ca_inv=ca_inv/3.4528
qui rename R_1115 depr
qui label var depr "Depreciation"
qui replace depr=depr/3.4528
qui rename R_1206 amort
qui label var amort "Amortization"
qui replace amort=amort/3.4528
qui rename R_407 subs
qui label var subs "Grants and subsidies"
qui replace subs=subs/3.4528
qui rename R_419 cl_tot
qui label var cl_tot "Short-term  debt"
qui replace cl_tot = cl_tot/3.4528
qui rename R_410 ncl_tot
qui label var ncl_tot "Long-term debt"
qui replace ncl_tot=ncl_tot/3.4528
qui rename R_421 cl_fin
qui label var cl_fin "Short-term financial debt"
qui replace cl_fin = cl_fin/3.4528
qui rename R_411 ncl_fin
qui label var ncl_fin "Long-term financial debt"
qui replace ncl_fin=ncl_fin/3.4528
qui rename  R_430 eq_liab_tot
qui label var eq_liab_tot "Total equity and liabilities"
qui replace eq_liab_tot=eq_liab_tot/3.4528
qui rename  R_600 rev_sales
qui label var rev_sales "Sales revenue"
qui replace rev_sales = rev_sales/3.4528
qui rename  R_622 debt_service
qui label var debt_service "Interest rate paid"
qui rename  R_550 purch_tot
qui label var purch_tot "Purchases of goods and services"
qui replace purch_tot=purch_tot/3.4528
qui rename  R_501 purch_mat
qui label var purch_mat "Purchases of production materials"
qui replace purch_mat=purch_mat/3.4528
qui rename  R_104 cost_empl
qui label var cost_empl "Total labor costs"
qui replace cost_empl=cost_empl/3.4528
qui rename  R_1041 cost_empl_t
qui label var cost_empl_t "Taxable labor costs"
qui replace cost_empl_t=cost_empl_t/3.4528
qui rename  R_1042 cost_empl_nt
qui label var cost_empl_nt "Other employee compensation"
qui replace cost_empl_nt=cost_empl_nt/3.4528
qui rename  R_105 cost_ssec
qui label var cost_ssec "Employer SS contribution"
qui replace cost_ssec=cost_ssec/3.4528
qui rename  R_606 pi_gross
qui label var pi_gross "Gross profits/loss"
qui replace pi_gross=pi_gross/3.4528
qui rename  R_610 pi_oper
qui label var pi_oper "Operational profits"
qui replace pi_oper=pi_oper/3.4528
qui rename  R_625 pi_btax
qui label var pi_btax "Profits before taxes"
qui replace pi_btax=pi_btax/3.4528
qui rename  R_627 pi_net
qui label var pi_net "Net profit/losses"
qui replace pi_net=pi_net/3.4528
qui rename PVGK VA
qui label var VA "Valued added (producer prices)"
qui replace VA=VA/3.4528
qui label var EXP "Exports of goods"
qui replace EXP=EXP/3.4528
qui label var IMP "Imports of goods"
qui replace IMP=IMP/3.4528

qui keep year id legal_birth legal_death legal_death_y NACE1_2 type own cap_* empl* hours* a_tot fa_* ca_* subs cl_* ncl_* depr amort eq_liab_tot rev_sales debt_service purch_* cost_* pi_* VA EXP IMP
qui order year id
qui sort year id
qui compress

}

if `1'>=2000 & `1'<2009 {

qui import delimited "${path}\Data_raw\n`1'" , case(preserve) clear

qui g year =`1'
qui rename sal_kodas id 
qui label var id "Firm's ID"
qui drop if id==.
qui rename likviduota legal_death_y
qui label var legal_death_y "Firm's liquidation year"
qui destring legal_death_y, replace
qui rename registracijos_data legal_birth
qui label var legal_birth "Firm's creation date"
qui rename isregistravimo_data legal_death
qui label var legal_death "Firm's liquidation date"
qui rename veikla2 NACE2 
qui label var NACE2 "Sector of activity (NACE 2)"
qui rename RUSIS type
qui label var type "Type of a firm (code)"
qui rename NF own
qui label var own "Form of ownership"
qui rename kapitalas_valst_sav cap_loc_gov
qui label var cap_loc_gov "Local state ownership (the value of firm's equity by state or municipalities)"
qui rename kapitalas_fiz cap_loc_priv
qui label var cap_loc_priv "Local private ownership (the value of firm's equity by Lithuanian natural or legal persons)"
qui rename kapitalas_uzsienio cap_int
qui label var cap_int  "Foreign ownership (the value of firm's equity by foreign investors)"
qui rename R_100 empl
qui label var emp "Average number of employees"
qui rename R_101 empl_p
qui label var empl_p "Average number of part-time employees"
qui rename R_103 hours
qui label var  hours "Number of hours worked"
qui rename R_106 hours_p
qui label var hours_p "Number of hours by part-time workers"
qui rename R_327 a_tot
qui label var a_tot "Total assets"
qui rename R_300 fa_intang
qui label var fa_intang "Intangible fixed assets"
qui rename R_301 fa_tang
qui label var fa_tang "Tangible fixed assets"
qui rename R_302 fa_fin
qui label var fa_fin "Financial fixed assets"
qui rename R_329 fa_oth
qui label var fa_oth "Other fixed assets"
qui rename R_325 ca_cash
qui label var ca_cash "Cash and cash equivalents"
qui rename R_309 ca_inv
qui label var ca_inv "Total inventories" 
qui rename R_1115 depr
qui label var depr "Depreciation"
qui rename R_1206 amort
qui label var amort "Amortization"
qui rename R_407 subs
qui label var subs "Grants and subsidies"
qui rename R_419 cl_tot
qui label var cl_tot "Short-term  debt"
qui rename R_410 ncl_tot
qui label var ncl_tot "Long-term debt"
qui rename R_421 cl_fin
qui label var cl_fin "Short-term financial debt"
qui rename R_411 ncl_fin
qui label var ncl_fin "Long-term financial debt"
qui rename  R_430 eq_liab_tot
qui label var eq_liab_tot "Total equity and liabilities"
qui rename  R_600 rev_sales
qui label var rev_sales "Sales revenue"
qui rename  R_622 debt_service
qui label var debt_service "Interest rate paid"
qui rename  R_550 purch_tot
qui label var purch_tot "Purchases of goods and services"
qui rename  R_501 purch_mat
qui label var purch_mat "Purchases of production materials"
qui rename R_506 purch_resell
qui label var purch_resell "Purchases of goods for reselling"
qui rename  R_507 purch_fuel
qui label var purch_fuel "Purchases of fuel"
qui rename  R_511 purch_elect
qui label var purch_elect "Purchases of electricity"
qui rename  R_512 purch_energy
qui label var purch_energy "Purchases of energy"
qui rename  R_526 purch_rent
qui label var purch_rent "Rent"
qui rename  R_536 purch_gs
qui label var purch_gs "Purchases of other goods and services"
qui rename R_522 cost_out_empl
qui label var cost_out_empl "Cost of non-company employees"
qui rename  R_104 cost_empl
qui label var cost_empl "Total labor costs"
qui rename  R_1041 cost_empl_t
qui label var cost_empl_t "Taxable labor costs"
qui rename  R_1042 cost_empl_nt
qui label var cost_empl_nt "Other employee compensation"
qui rename  R_105 cost_ssec
qui label var cost_ssec "Employer SS contribution"
qui rename  R_606 pi_gross
qui label var pi_gross "Gross profits/loss"
qui rename  R_610 pi_oper
qui label var pi_oper "Operational profits"
qui rename  R_625 pi_btax
qui label var pi_btax "Profits before taxes"
qui rename  R_627 pi_net
qui label var pi_net "Net profit/losses"
qui rename PVGK VA
qui label var VA "Valued added (producer prices)"
qui label var EXP "Exports of goods"
qui label var IMP "Imports of goods"

qui keep year id legal_birth legal_death legal_death_y NACE2 type own cap_* empl* hours* a_tot fa_* ca_* subs cl_* ncl_* depr amort eq_liab_tot rev_sales debt_service purch_* cost_* pi_* VA EXP IMP
qui order year id
qui sort year id
qui compress

}


if `1'>=2009 & `1'<2013 {

qui import delimited "${path}\Data_raw\n`1'" , case(preserve) clear

qui g year =`1'
qui rename sal_kodas id 
qui label var id "Firm's ID"
qui drop if id==.
qui rename likviduota legal_death_y
qui label var legal_death_y "Firm's liquidation year"
qui destring legal_death_y, replace
qui rename registracijos_data legal_birth
qui label var legal_birth "Firm's creation date"
qui rename isregistravimo_data legal_death
qui label var legal_death "Firm's liquidation date"
qui rename veikla2_2 NACE2 
qui label var NACE2 "Sector of activity (NACE 2)"
qui rename RUSIS type
qui label var type "Type of a firm (code)"
qui rename NF own
qui label var own "Form of ownership"
qui rename kapitalas_valst_sav cap_loc_gov
qui label var cap_loc_gov "Local state ownership (the value of firm's equity by state or municipalities)"
qui rename kapitalas_fiz cap_loc_priv
qui label var cap_loc_priv "Local private ownership (the value of firm's equity by Lithuanian natural or legal persons)"
qui rename kapitalas_uzsienio cap_int
qui label var cap_int  "Foreign ownership (the value of firm's equity by foreign investors)"
qui rename R_100 empl
qui label var emp "Average number of employees"
qui rename R_101 empl_p
qui label var empl_p "Average number of part-time employees"
qui rename R_103 hours
qui label var  hours "Number of hours worked"
qui rename R_106 hours_p
qui label var hours_p "Number of hours by part-time workers"
qui rename R_327 a_tot
qui label var a_tot "Total assets"
qui rename R_300 fa_intang
qui label var fa_intang "Intangible fixed assets" 
qui rename R_301 fa_tang
qui label var fa_tang "Tangible fixed assets"
qui rename R_302 fa_fin
qui label var fa_fin "Financial fixed assets"
qui rename R_329 fa_oth
qui label var fa_oth "Other fixed assets"
qui rename R_325 ca_cash
qui label var ca_cash "Cash and cash equivalents"
qui rename R_309 ca_inv
qui label var ca_inv "Total inventories"
qui rename R_1115 depr
qui label var depr "Depreciation"
qui rename R_1206 amort
qui label var amort "Amortization"
qui rename R_407 subs
qui label var subs "Grants and subsidies"
qui rename R_419 cl_tot
qui label var cl_tot "Short-term  debt"
qui rename R_410 ncl_tot
qui label var ncl_tot "Long-term debt"
qui rename R_421 cl_fin
qui label var cl_fin "Short-term financial debt"
qui rename R_411 ncl_fin
qui label var ncl_fin "Long-term financial debt"
qui rename  R_430 eq_liab_tot
qui label var eq_liab_tot "Total equity and liabilities"
qui rename  R_600 rev_sales
qui label var rev_sales "Sales revenue"
qui rename  R_622 debt_service
qui label var debt_service "Interest rate paid"
qui rename  R_550 purch_tot
qui label var purch_tot "Purchases of goods and services"
qui rename  R_501 purch_mat
qui label var purch_mat "Purchases of production materials"
qui rename R_506 purch_resell
qui label var purch_resell "Purchases of goods for reselling"
qui rename  R_507 purch_fuel
qui label var purch_fuel "Purchases of fuel"
qui rename  R_511 purch_elect
qui label var purch_elect "Purchases of electricity"
qui rename  R_512 purch_energy
qui label var purch_energy "Purchases of energy"
qui rename  R_526 purch_rent
qui label var purch_rent "Rent"
qui rename  R_536 purch_gs
qui label var purch_gs "Purchases of other goods and services"
qui rename R_522 cost_out_empl
qui label var cost_out_empl "Cost of non-company employees"
qui rename  R_104 cost_empl
qui label var cost_empl "Total labor costs"
qui rename  R_1041 cost_empl_t
qui label var cost_empl_t "Taxable labor costs"
qui rename  R_1042 cost_empl_nt
qui label var cost_empl_nt "Other employee compensation"
qui rename  R_105 cost_ssec
qui label var cost_ssec "Employer SS contribution"
qui rename  R_606 pi_gross
qui label var pi_gross "Gross profits/loss"
qui rename  R_610 pi_oper
qui label var pi_oper "Operational profits"
qui rename  R_625 pi_btax
qui label var pi_btax "Profits before taxes"
qui rename  R_627 pi_net
qui label var pi_net "Net profit/losses"
qui rename PVGK VA
qui label var VA "Valued added (producer prices)"
qui label var EXP "Exports of goods"
qui label var IMP "Imports of goods"
qui rename  klausimas_1  group
qui label var group "Does your company belong to a group of companies?"
qui rename  klausimas_1_1 parent
qui label var parent "Is your company a parent company in your group of companies?"
qui rename  klausimas_1_2 subs_loc
qui label var subs_loc "Is your company a subsidiary, the parent of which is another company registered in Lithuania? "
qui rename  klausimas_1_3 subs_int
qui label var subs_int "Is your company a subsidiary, the parent of which is another company registered abroad?"

qui keep year id legal_birth legal_death legal_death_y NACE2 type own cap_* empl* hours* a_tot fa_* ca_* subs cl_* ncl_* depr amort eq_liab_tot rev_sales debt_service purch_* cost_* pi_* VA EXP IMP group parent subs_* 
qui order year id
qui sort year id
qui compress

}


if `1'>= 2013 & `1'<2016 {

qui import delimited "${path}\Data_raw\n`1'" , case(preserve) clear

qui g year =`1'
qui rename sal_kodas id 
qui label var id "Firm's ID"
qui drop if id==.
qui rename G_likvidavimo_metai legal_death_y
qui label var legal_death_y "Firm's liquidation year"
qui destring legal_death_y, replace
qui rename REG_DATA legal_birth
qui label var legal_birth "Firm's creation date"
qui replace veikla = substr(veikla,2,3)
qui destring veikla, replace
qui rename veikla NACE2 
qui label var NACE2 "Sector of activity (NACE 2)"
qui rename V_RUSIS type
qui label var type "Type of a firm (code)"
qui rename V_NF own
qui label var own "Form of ownership"
qui rename V_201_204 cap_loc_gov
qui label var cap_loc_gov "Local state ownership (the value of firm's equity by state or municipalities)"
qui rename V_207_210 cap_loc_priv
qui label var cap_loc_priv "Local private ownership (the value of firm's equity by Lithuanian natural or legal persons)"
qui rename V_211 cap_int
qui label var cap_int  "Foreign ownership (the value of firm's equity by foreign investors)"
qui rename V_100 empl
qui label var emp "Average number of employees"
qui rename V_101 empl_p
qui label var empl_p "Average number of part-time employees"
qui rename V_103 hours
qui label var  hours "Number of hours worked"
qui rename V_106 hours_p
qui label var hours_p "Number of hours by part-time workers"
qui rename V_327 a_tot
qui label var a_tot "Total assets"
qui rename V_300_1 fa_intang
qui label var fa_intang "Intangible fixed assets"
qui rename V_301_1 fa_tang
qui label var fa_tang "Tangible fixed assets"
qui rename V_302 fa_fin
qui label var fa_fin "Financial fixed assets"
qui rename V_329 fa_oth
qui label var fa_oth "Other fixed assets"
qui rename V_325 ca_cash
qui label var ca_cash "Cash and cash equivalents"
qui rename V_309_1 ca_inv
qui label var ca_inv "Total inventories"
qui rename V_1115 depr
qui label var depr "Depreciation"
qui rename V_1206 amort
qui label var amort "Amortization"
qui rename V_407 subs
qui label var subs "Grants and subsidies"
qui rename V_419 cl_tot
qui label var cl_tot "Short-term  debt"
qui rename V_410 ncl_tot
qui label var ncl_tot "Long-term debt"
qui rename V_421 cl_fin
qui label var cl_fin "Short-term financial debt" 
qui rename V_411 ncl_fin
qui label var ncl_fin "Long-term financial debt"
qui rename  V_430 eq_liab_tot
qui label var eq_liab_tot "Total equity and liabilities"
qui rename  V_600 rev_sales
qui label var rev_sales "Sales revenue"
qui rename V_604 cost_sales
qui label var cost_sales "Cost of sales"
qui rename V_607 oper_exp
label var oper_exp "Operating expenses"
qui rename  V_622 debt_service
qui label var debt_service "Interest rate paid"
qui rename  V_550 purch_tot
qui label var purch_tot "Purchases of goods and services"
qui rename  V_501 purch_mat
qui label var purch_mat "Purchases of production materials"
qui rename V_506 purch_resell
qui label var purch_resell "Purchases of goods for reselling"
qui rename  V_507 purch_fuel
qui label var purch_fuel "Purchases of fuel"
qui rename  V_511 purch_elect
qui label var purch_elect "Purchases of electricity"
qui rename  V_512 purch_energy
qui label var purch_energy "Purchases of energy"
qui rename  V_526 purch_rent
qui label var purch_rent "Rent"
qui rename  V_536 purch_gs
qui label var purch_gs "Purchases of other goods and services"
qui rename V_522 cost_out_empl
qui label var cost_out_empl "Cost of non-company employees"
qui rename  V_104 cost_empl
qui label var cost_empl "Total labor costs"
qui rename  V_1041 cost_empl_t
qui label var cost_empl_t "Taxable labor costs"
qui rename  V_1042 cost_empl_nt
qui label var cost_empl_nt "Other employee compensation"
qui rename  V_105 cost_ssec
qui label var cost_ssec "Employer SS contribution"
qui rename  V_606 pi_gross
qui label var pi_gross "Gross profits/loss"
qui replace pi_gross=pi_gross 
qui rename  V_610 pi_oper
qui label var pi_oper "Operational profits"
qui rename  V_625 pi_btax
qui label var pi_btax "Profits before taxes"
qui rename  V_627 pi_net
qui label var pi_net "Net profit/losses"
qui rename PVGK VA
qui label var VA "Valued added (producer prices)"
qui label var EXP "Exports of goods"
qui label var IMP "Imports of goods"
qui rename V_671 subs_prod
qui label var subs_prod "Subsidies on products"
qui rename V_672 subs_prodn
qui label var subs_prodn "Subsidies on production"
qui rename V_660 tax_prodn
qui label var tax_prodn "Taxes on production"

qui rename V1 group
qui label var group "Does your company belong to a group of companies?"
qui rename V1_1 parent
qui label var parent "Is your company a parent company in your group of companies?"
qui rename V1_2 subs_loc
qui label var subs_loc "Is your company a subsidiary, the parent of which is another company registered in Lithuania? "
qui rename V1_3 subs_int
qui label var subs_int "Is your company a subsidiary, the parent of which is another company registered abroad?"

qui keep year id legal_birth legal_death legal_death_y NACE2 type own cap_* empl* hours* a_tot fa_* ca_* subs cl_* ncl_* depr amort eq_liab_tot rev_sales debt_service purch_* cost_* pi_* VA EXP IMP group parent subs_* tax_* oper_*
qui order year id
qui sort year id
qui compress

}

if `1'==2016 {
	
qui import delimited "${path}\Data_raw\n`1'" , case(preserve) clear

gen year = `1'	

qui rename sal_kodas id 
qui label var id "Firm's ID"
qui drop if id==.
qui replace veikla = substr(veikla,2,3)
qui destring veikla, replace
qui rename veikla NACE2 
qui label var NACE2 "Sector of activity (NACE 2)"
qui rename rusis type
qui label var type "Type of a firm (code)"
qui rename NF own
qui label var own "Form of ownership"
qui rename V_201_204 cap_loc_gov
qui label var cap_loc_gov "Local state ownership (the value of firm's equity by state or municipalities)"
qui rename V_207_210 cap_loc_priv
qui label var cap_loc_priv "Local private ownership (the value of firm's equity by Lithuanian natural or legal persons)"
qui rename V_211 cap_int
qui label var cap_int  "Foreign ownership (the value of firm's equity by foreign investors)"
qui rename V_100 empl
qui label var emp "Average number of employees"
qui rename V_101 empl_p
qui label var empl_p "Average number of part-time employees"
qui label var fte "Full-time equivalent employees"
qui rename V_103 hours
qui label var  hours "Number of hours worked"
qui rename V_106 hours_p
qui label var hours_p "Number of hours by part-time workers"
qui rename V_327 a_tot
qui label var a_tot "Total assets"
qui rename V_300_1 fa_intang
qui label var fa_intang "Intangible fixed assets"
qui rename V_301_1 fa_tang
qui label var fa_tang "Tangible fixed assets"
qui rename V_302 fa_fin
qui label var fa_fin "Financial fixed assets"
qui rename V_329 fa_oth
qui label var fa_oth "Other fixed assets"
qui rename V_325 ca_cash
qui label var ca_cash "Cash and cash equivalents"
qui rename V_309_1 ca_inv
qui label var ca_inv "Total inventories"
qui rename V_1115 depr
qui label var depr "Depreciation"
qui rename V_1206 amort
qui label var amort "Amortization"
qui rename V_407 subs
qui label var subs "Grants and subsidies"
qui rename V_419 cl_tot
qui label var cl_tot "Short-term  debt"
qui rename V_410 ncl_tot
qui label var ncl_tot "Long-term debt"
qui rename V_421 cl_fin
qui label var cl_fin "Short-term financial debt"
qui rename V_411 ncl_fin
qui label var ncl_fin "Long-term financial debt"
qui rename  V_430 eq_liab_tot
qui label var eq_liab_tot "Total equity and liabilities"
qui rename  V_600 rev_sales
qui label var rev_sales "Sales revenue"
qui rename V_604 cost_sales
qui label var cost_sales "Cost of sales"
qui rename V_607 oper_exp
label var oper_exp "Operating expenses"
qui rename  V_622 debt_service
qui label var debt_service "Interest rate paid"
qui rename  V_550 purch_tot
qui label var purch_tot "Purchases of goods and services"
qui rename  V_501 purch_mat
qui label var purch_mat "Purchases of production materials"
qui rename V_506 purch_resell
qui label var purch_resell "Purchases of goods for reselling"
qui rename  V_507 purch_fuel
qui label var purch_fuel "Purchases of fuel"
qui rename  V_511 purch_elect
qui label var purch_elect "Purchases of electricity"
qui rename  V_512 purch_energy
qui label var purch_energy "Purchases of energy"
qui rename  V_526 purch_rent
qui label var purch_rent "Rent"
qui rename  V_536 purch_gs
qui label var purch_gs "Purchases of other goods and services"
qui rename V_522 cost_out_empl
qui label var cost_out_empl "Cost of non-company employees"
qui rename  V_104 cost_empl
qui label var cost_empl "Total labor costs"
qui rename  V_1041 cost_empl_t
qui label var cost_empl_t "Taxable labor costs"
qui rename  V_1042 cost_empl_nt
qui label var cost_empl_nt "Other employee compensation"
qui rename  V_105 cost_ssec
qui label var cost_ssec "Employer SS contribution"
qui rename  V_606 pi_gross
qui label var pi_gross "Gross profits/loss"
qui rename  V_610 pi_oper
qui label var pi_oper "Operational profits"
qui rename  V_625 pi_btax
qui label var pi_btax "Profits before taxes"
qui rename  V_627 pi_net
qui label var pi_net "Net profit/losses"
qui rename PVGK VA
qui label var VA "Valued added (producer prices)"
qui label var EXP "Exports of goods"
qui label var IMP "Imports of goods"
qui rename V_671 subs_prod
qui label var subs_prod "Subsidies on products"
qui rename V_672 subs_prodn
qui label var subs_prodn "Subsidies on production"
qui rename V_660 tax_prodn
qui label var tax_prodn "Taxes on production"


qui keep imput year id  NACE2 type own cap_* empl* hours* a_tot fa_* ca_* subs cl_* ncl_* depr amort eq_liab_tot rev_sales debt_service purch_* cost_* pi_* VA EXP IMP subs_* tax_* // group  parent subs_*
qui order year id
qui sort year id
qui compress
}

if `1'>=2017 {
	
qui import delimited "${path}\Data_raw\n`1'" , case(preserve) clear

gen year = `1'	

qui rename sal_kodas id 
qui label var id "Firm's ID"
qui drop if id==.
qui replace veikla = substr(veikla,2,3)
qui destring veikla, replace
qui rename veikla NACE2 
qui label var NACE2 "Sector of activity (NACE 2)"
qui rename rusis type
qui label var type "Type of a firm (code)"
qui rename NF own
qui label var own "Form of ownership"
qui rename V_201_204 cap_loc_gov
qui label var cap_loc_gov "Local state ownership (the value of firm's equity by state or municipalities)"
qui rename V_207_210 cap_loc_priv
qui label var cap_loc_priv "Local private ownership (the value of firm's equity by Lithuanian natural or legal persons)"
qui rename V_211 cap_int
qui label var cap_int  "Foreign ownership (the value of firm's equity by foreign investors)"
qui rename V_100 empl
qui label var emp "Average number of employees"
qui rename V_101 empl_p
qui label var empl_p "Average number of part-time employees"
qui label var fte "Full-time equivalent employees"
qui rename V_103 hours
qui label var  hours "Number of hours worked"
qui rename V_106 hours_p
qui label var hours_p "Number of hours by part-time workers"
qui rename V_327 a_tot
qui label var a_tot "Total assets"
qui rename V_300_1 fa_intang
qui label var fa_intang "Intangible fixed assets"
qui rename V_301_1 fa_tang
qui label var fa_tang "Tangible fixed assets"
qui rename V_302 fa_fin
qui label var fa_fin "Financial fixed assets"
qui rename V_329 fa_oth
qui label var fa_oth "Other fixed assets"
qui rename V_325 ca_cash
qui label var ca_cash "Cash and cash equivalents"
qui rename V_309_1 ca_inv
qui label var ca_inv "Total inventories"
qui rename V_1115 depr
qui label var depr "Depreciation"
qui rename V_1206 amort
qui label var amort "Amortization"
qui rename V_407 subs
qui label var subs "Grants and subsidies"
qui rename V_419 cl_tot
qui label var cl_tot "Short-term  debt"
qui rename V_410 ncl_tot
qui label var ncl_tot "Long-term debt"
qui rename V_421 cl_fin
qui label var cl_fin "Short-term financial debt"
qui rename V_411 ncl_fin
qui label var ncl_fin "Long-term financial debt"
qui rename  V_430 eq_liab_tot
qui label var eq_liab_tot "Total equity and liabilities"
qui rename  V_600 rev_sales
qui label var rev_sales "Sales revenue"
qui rename V_604 cost_sales
qui label var cost_sales "Cost of sales"
qui rename  V_622 debt_service
qui label var debt_service "Interest rate paid"
qui rename  V_550 purch_tot
qui label var purch_tot "Purchases of goods and services"
qui rename  V_501 purch_mat
qui label var purch_mat "Purchases of production materials"
qui rename V_506 purch_resell
qui label var purch_resell "Purchases of goods for reselling"
qui rename  V_507 purch_fuel
qui label var purch_fuel "Purchases of fuel"
qui rename  V_511 purch_elect
qui label var purch_elect "Purchases of electricity"
qui rename  V_512 purch_energy
qui label var purch_energy "Purchases of energy"
qui rename  V_526 purch_rent
qui label var purch_rent "Rent"
qui rename  V_536 purch_gs
qui label var purch_gs "Purchases of other goods and services"
qui rename V_522 cost_out_empl
qui label var cost_out_empl "Cost of non-company employees"
qui rename  V_104 cost_empl
qui label var cost_empl "Total labor costs"
qui rename  V_1041 cost_empl_t
qui label var cost_empl_t "Taxable labor costs"
qui rename  V_1042 cost_empl_nt
qui label var cost_empl_nt "Other employee compensation"
qui rename  V_105 cost_ssec
qui label var cost_ssec "Employer SS contribution"
qui rename  V_606 pi_gross
qui label var pi_gross "Gross profits/loss"
qui rename  V_610 pi_oper
qui label var pi_oper "Operational profits"
qui rename  V_625 pi_btax
qui label var pi_btax "Profits before taxes"
qui rename  V_627 pi_net
qui label var pi_net "Net profit/losses"
qui rename PVGK VA
qui label var VA "Valued added (producer prices)"
*qui label var EXP "Exports of goods"
*qui label var IMP "Imports of goods"
qui rename V_671 subs_prod
qui label var subs_prod "Subsidies on products"
qui rename V_672 subs_prodn
qui label var subs_prodn "Subsidies on production"
qui rename V_660 tax_prodn
qui label var tax_prodn "Taxes on production"


qui keep imput year id  NACE2 type own cap_* empl* hours* a_tot fa_* ca_* subs cl_* ncl_* depr amort eq_liab_tot rev_sales debt_service purch_* cost_* pi_* VA /*EXP IMP*/ subs_* tax_* // group  parent subs_*
qui order year id
qui sort year id
qui compress
}

end