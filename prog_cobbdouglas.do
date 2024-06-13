
program define prog_cobbdouglas

	syntax [, variableinput(string) gmmtype(string) filename(string)]

	

**           Production function estimation
*            ACF procedure following De Loecker and Warzynski (2012) Markups and Firm-Level Exports, American Economic Review

mata: mata clear


use ${path}\Data_stata\firmdata.dta, clear


** Intermediate input expenditures // consider as variable input all costs that vary with production (total purchases, except rent costs)
qui gen variableinput=  purch_tot - purch_rent 
qui gen gmmtype="`gmmtype'"
qui gen filename="`filename'"

qui egen k_cost_2 = rsum(amort depr debt_service)
label var k_cost_2 "rsum(amort depr debt_service)"



** Merge industry deflators
gen geo_code = "LT"
qui merge m:1 year nace_r2_code geo_code using ${path}\Data_aux\national_accounts_ALL.dta, keep(3) keepusing(VA_PI GO_PI II_PI)
drop geo_code _m
foreach v in VA_PI GO_PI II_PI {
qui bys nace_r2_code: egen mean = mean(`v')
qui replace `v' = mean if `v'==.
qui drop mean	
}
qui drop if VA_PI==.

replace EXP=0 if EXP==.
foreach v in rev_sales EXP pi_gross pi_oper pi_btax pi_net {
qui replace `v' = `v'/(GO_PI/100)	
}
replace IMP = 0 if IMP==.
foreach v in cost_empl fa_tang /*k_cost_1*/ k_cost_2 IMP {
qui replace `v' = `v'/(VA_PI/100) 
}

qui replace variableinput = variableinput/(II_PI/100)


* gen variables after deflating
qui gen y     = rev_sales
qui gen l     = cost_empl
qui gen k     = fa_tang
qui rename variableinput varinp
qui gen cost_oper  = pi_gross  - pi_oper
qui gen cost_sales = rev_sales - pi_gross
qui drop *_PI

** Data contraints
*  Select only valid years
keep if year>=$miny_analysis & year<=$maxy_analysis

** Remove primary sector as well as health and education they are not representative
drop if  NACE2>=1 & NACE2<=9 |  NACE2>=64 & NACE2<=68 | NACE2>=85 & NACE2<=88 

** Remove transportation due to several regulatory changes 
drop if NACE2==49  
 
** Utilities supplier, heavily regulated
drop if NACE2==35  | NACE2==36

** Remove firms with gaps in the time dimension - spurious entry and exit
qui bys id (year):  gen gap = 1 if year!=year[_n-1]+1 & _n!=1
qui bys id (gap ):  replace gap  = gap[1] if gap==.
drop if gap == 1
qui drop gap

** Remove single-worker firms-year
qui bys id: egen min=mean(empl)
drop if min<2
qui drop min

** Remove cost shares of materials and labor with abnormal values
gen ly = l/y
gen varinpy = varinp/y
gen flag = 1 if ly<=0 | ly>1 | varinpy<=0 | varinpy>1  
bys id (flag): replace flag = flag[1] if flag==. 
drop if flag==1
drop flag 
drop ly varinpy  

** Remove observations with zero or missing in the production function variables 
foreach v in y l k varinp {
drop if `v'<=1 | `v'==.
}

** Remove industry with less than 10 firms per year 
qui bys NACE2 year: gen nobs = _N
qui bys NACE2: egen min=min(nobs)
drop nobs
qui bys NACE2: gen nobs = _N
qui bys NACE2: egen min1=min(nobs)
keep if min>=10 
qui drop nobs min*

** Winsorize distribution of PF variables
foreach v in y l k varinp  {
qui bys NACE2: egen `v'_p1   = pctile(`v'), p(2)
replace `v' = `v'_p1 if `v'<`v'_p1
qui bys NACE2: egen `v'_p99  = pctile(`v'), p(98)
replace `v' = `v'_p99 if `v'>`v'_p99
}
drop *_p1 *_p99 


* Variables in logs
qui replace y = ln(y)
qui replace l = ln(l)
qui replace varinp = ln(varinp)
qui replace k = ln(k)


** Market shares
qui bys NACE2 year: egen tot_sales = sum(rev_sales)
qui gen sales_share = rev_sales/tot_sales
qui drop tot_sales

* Employment based
qui bys NACE2 year: egen total_empl = total(empl)
gen emp_share = empl/total_empl
drop total_empl


do ${path}\Do\dlw_`gmmtype'.ado 	

keep id nace* NACE2 nace_r2_code year y l varinp k emp_share sales_share k_cost* pi_gross pi_oper pi_btax pi_net amort depr EXP IMP empl year_birth year_death type own cost_oper purch_fuel purch_resell purch_elect purch_energy cost_out_empl
qui compress

egen ind = group(NACE2)
qui sum ind
local N = `r(max)'

qui gen phi = . 
qui gen e   = . 

** FIRST STAGE: compute measurement error in sales and recover phi = f(l,m,k) + omega = PF + PRODUCTIVITY for each industry 
quietly {
forvalues n = 1/`N' {
reg y l k varinp i.year sales_share if ind==`n'
predict xb, xb
replace phi = xb if ind==`n' 
predict resid, residuals
replace e   = resid if ind==`n' 
drop xb resid
}

* Lagged values, some of them are instruments in the GMM procedure: capital, first lag of materials and labor, and their interactions
xtset id year
foreach v in y phi e l varinp k {
gen `v'_lag = L.`v'
}


preserve
keep if phi_lag==. 
tempfile lag
save `lag', replace
restore 

drop if phi_lag ==. | phi == . | y == . | y_lag==. | l==. | l_lag==. | varinp==. | varinp_lag==. | k==. | k_lag==. 
gen c = 1
}

** SECOND STAGE: GMM use instruments to obtain PF coefficients and separate f(l,varinp,k) from omega
drop ind
egen ind = group(NACE2)
qui sum ind
local N = `r(max)'


forvalues n = 1/`N' {
preserve
disp in red "industry `n'"
qui keep if ind==`n'

* OLS starting values 
qui reg phi l varinp k 
mat init = (_b[l], _b[varinp], _b[k], _b[_cons])
mat colnames init = l varinp k c
mat list init

* Produce industry-specific elasticities 
qui dlw_`gmmtype' init  // the function needs to be fed with the initial values (init)
mat betas = beta_dlwcd
mat colnames betas = l varinp k c
mat list betas

* Betas
qui gen betal = beta_dlwcd[1,1] 
qui gen betavarinp = beta_dlwcd[1,2]
qui gen betak = beta_dlwcd[1,3]


qui keep NACE2 beta*
qui bys NACE2: keep if _n == 1

qui tempfile betas_industry`n'
qui save `betas_industry`n''
    restore 
}
	
quietly {
preserve 
clear 

forvalues n=1/`N' {
qui append using `betas_industry`n''
qui tempfile betas
qui save `betas'
}
restore 
}

drop *_lag* 

forvalues n=1/`N' {
qui	merge m:1 NACE2 using `betas', nogen
}
append using `lag'

foreach v in betal betavarinp betak  {
bys NACE2 (`v'): replace `v' = `v'[1] if `v'==. 	
}


** Generate results and save them 
*  Productivity: phi - f_hat
gen tfp = phi - (betal*l + betavarinp*varinp + betak*k)

*  Output-input elastcities
gen e_varinp_y = betavarinp 
gen e_l_y = betal 
gen e_k_y = betak 
gen rts = e_varinp_y + e_l_y + e_k_y
qui ttest (rts)==1

*  Corrected input-cost share of revenues
gen y_c = y - e
gen alpha_k = k_cost_2/exp(y_c)
gen alpha_o = cost_oper/exp(y_c)
gen alpha_varinp = exp(varinp)/exp(y_c)
gen alpha_l = exp(l)/exp(y_c)


* Profit rate
gen pirate_gross  = pi_gross/exp(y)
gen pirate_ebitda =(pi_btax+amort+depr)/exp(y)

* Remove negative elasticities
drop if e_l_y<=0
drop if e_varinp_y<=0
	

*Winsorize markup and markdown distribution within industries by winsorizing components, keep consistency for decomposition
foreach v in alpha_varinp alpha_l e_varinp_y e_l_y {
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


save ${path}\Data_stata\finaldata_`filename'.dta, replace
	
	

end