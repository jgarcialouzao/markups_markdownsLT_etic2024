

 
*                 Extract original data from excel to dta and basic cleaning

** Time dimension - use the whole panel  to correct for tv-variables
local miny = 2000
local maxy = 2020

** Read csv files and transform to dta files
forvalues y=`miny'(1)`maxy' {
qui xlsfirms_new `y' // notice it is a different program to extract a different group of data!
save ${path}\Data_stata\firmdata`y'.dta, replace
}


forvalues y=2016(1)`maxy' {
qui xlsfirms `y'
save ${path}\Data_stata\firmdata`y'_old.dta, replace
}

** Append years into a single firm-level dataset -- for the new data, MERGE permanent information from old files
forvalues y=2016(1)2018 {
	use ${path}\Data_stata\firmdata`y'.dta, clear 
	merge 1:1 id year using "${path}\Data_stata\imp_exp_`y'.dta", keep(1 3)
	drop _m
	replace EXP = EXP*1000
	replace IMP = IMP*1000
	save ${path}\Data_stata\firmdata`y'.dta, replace
}

clear
cd ${path}
forvalues y=`miny'(1)`maxy' {
qui append using ${path}\Data_stata\firmdata`y'.dta, force
}

forvalues y=2016(1)`maxy' {
merge 1:1 id year using  ${path}\Data_stata\firmdata`y'_old.dta, keep(1 3) keepusing(NACE2 own type)
drop _m
}

forvalues y=`miny'(1)`maxy' {
qui erase ${path}\Data_stata\firmdata`y'.dta 
}
forvalues y=2016(1)`maxy' {
qui erase ${path}\Data_stata\firmdata`y'_old.dta 
}


** Remove EXP and IMP variables, we will bring it later at a more granular level from customs data
drop IMP EXP

bys id: egen minyear=min(year)
bys id: egen maxyear=max(year)

** Birth and death years
gen year_birth = substr(legal_birth,1,4)
destring year_birth, replace 
bys id (year): replace year_birth = year[1]  if year_birth==. & _n == 1 & year>2015
replace year_birth = minyear if year_birth == .

gen year_death = substr(legal_death,1,4)
destring year_death, replace 
replace year_death = maxyear if year_death==. & maxyear<=2019
replace year_death = maxyear if year_death>maxyear & year_death!=.
drop minyear maxyear legal_birth legal_death*

***Homogeneize variables over time and 
** Recover time-variant information using longitudinal information
foreach v in NACE2 own type year_birth year_death {
qui gen negy = -year
qui bys id (negy): replace `v' = `v'[_n-1] if `v'==. &  `v'[_n-1] != .
qui bys id (year): replace `v' = `v'[_n-1] if `v'==. &  `v'[_n-1] != .
qui drop negy
}
** Set mode if there are changes - this should not changed over time
foreach v in type own type year_birth year_death {
qui bys id: egen mode=mode(`v'), maxmode
qui replace `v' = mode
drop mode 
}
** Fixed sector to the most common
qui bys id: egen mode=mode(NACE2), maxmode
qui replace NACE2 = mode
drop mode

** Potential error 
gen flag = 1 if year>year_death
bys id (flag): replace flag = flag[1] if flag==. 
drop if flag==1
drop flag

** Data after 2019 changes in wage concept, adjust
*replace cost_empl = 1.289*cost_empl if year<2019

** From 2019 onwards employment does not include owner
*replace empl = empl+1 if year>=2019 

qui {
qui gen nace_r2_code = ""
qui replace nace_r2_code= "A" if NACE2>=1 & NACE2<=3
qui replace nace_r2_code = "B" if NACE2>=4 & NACE2<=9
qui replace nace_r2_code = "C10-C12" if NACE2>=10 & NACE2<=12
qui replace nace_r2_code = "C13-C15" if NACE2>=13 & NACE2<=15
qui replace nace_r2_code = "C16-C18" if NACE2>=16 & NACE2<=18
qui replace nace_r2_code = "C16-C18" if NACE2==19
qui replace nace_r2_code = "C20-C21" if NACE2==20
qui replace nace_r2_code = "C20-C21" if NACE2==21
qui replace nace_r2_code = "C22-C23" if NACE2>=22 & NACE2<=23
qui replace nace_r2_code = "C24-C25" if NACE2>=24 & NACE2<=25
qui replace nace_r2_code = "C26" if NACE2==26
qui replace nace_r2_code = "C27" if NACE2==27
qui replace nace_r2_code = "C28" if NACE2==28
qui replace nace_r2_code = "C29-C30" if NACE2>=29 & NACE2<=30
qui replace nace_r2_code = "C31-C33" if NACE2>=31 & NACE2<=33
qui replace nace_r2_code = "D" if NACE2==35
qui replace nace_r2_code = "E" if NACE2>=36 & NACE2<=39
qui replace nace_r2_code = "F" if NACE2>=41 & NACE2<=43
qui replace nace_r2_code = "G45" if NACE2==45
qui replace nace_r2_code = "G46" if NACE2==46
qui replace nace_r2_code = "G47" if NACE2==47
qui replace nace_r2_code = "H49" if NACE2==49
qui replace nace_r2_code = "H50" if  NACE2==50
qui replace nace_r2_code = "H51" if NACE2==51
qui replace nace_r2_code = "H52" if NACE2==52
qui replace nace_r2_code = "H53" if NACE2==53
qui replace nace_r2_code = "I" if NACE2>=55 & NACE2<=56
qui replace nace_r2_code = "J58-J60" if NACE2>=58 & NACE2<=60
qui replace nace_r2_code = "J61" if NACE2==61
qui replace nace_r2_code = "J62-J63" if NACE2>=62 & NACE2<=63
qui replace nace_r2_code = "K" if NACE2>=64 & NACE2<=66
qui replace nace_r2_code = "L" if NACE2==68
qui replace nace_r2_code = "M" if NACE2>=69 & NACE2<=75
qui replace nace_r2_code = "N" if NACE2>=77 & NACE2<=82
qui replace nace_r2_code = "O" if NACE2==84
qui replace nace_r2_code = "P" if NACE2==85
qui replace nace_r2_code = "Q86" if NACE2==86
qui replace nace_r2_code = "Q87-Q88" if NACE2>=87 & NACE2<=88
qui replace nace_r2_code = "R" if NACE2>=90 & NACE2<=93
qui replace nace_r2_code = "S" if NACE2>=94 & NACE2<=96	
	
}

** Recover investment
* bys id (year): gen inv_fa_tang = fa_tang[_n+1]-fa_tang[_n]+depr[_n] // I_{it}=K_{i,t+1}-K_{i,t} + delta*K_{i,t}, the last term is our `depr' variable in the data


keep id year year_birth year_death type own empl fa_intang fa_tang fa_fin depr amort cl_tot rev_sales purch_tot purch_mat purch_fuel purch_resell purch_elect purch_energy purch_rent purch_gs debt_service cost_empl cost_ssec  cost_out_empl pi_gross pi_oper pi_btax pi_net VA NACE2 nace_r2_code 
order id year 

save ${path}\Data_stata\firmdata.dta, replace
 






