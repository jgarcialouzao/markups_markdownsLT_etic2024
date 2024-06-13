********************************************************************************
*** Stata 17
** Master file to replicate paper on Mark-Ups and Mark-Down in the Cross-Section and Over-time: Evidence from Lithuania
* Josh Ding, Jose Garcia-Louzao and Valentin Jouvanceau


clear all
capture log close
capture program drop _all
clear mata
set matastrict off 
macro drop _all
set more 1
set seed 13
set cformat %5.4f

** Set main directory


//global path /*"{Replication_files}"*/ "[directory here]" // main directory here but recall one needs to have the sub-folders within the diretory, i.e., do_files, dta_files

cd ${path}


** Installation of external programs required for estimation or saving results
/*
* ftools (remove program if it existed previously)
ssc install ftools, replace
ssc install gtools, all replace
ssc install grstyle, replace
ssc install palettes, replace
ssc install colrspace, replace

* reghdfe 
ssc install reghdfe, replace

*
ssc install hhi5, replace

*graph setting

	   qui grstyle init
	   qui grstyle set plain, compact grid dotted /*no grid*/
	   qui grstyle color major_grid gs13
	   qui grstyle set symbol
	   qui grstyle set lpattern
	   qui grstyle set color Dark2, n(4) 
	   qui grstyle set color Dark2, n(4)  opacity(34): p#markfill

*/

** Sequence of routines to produce the results

** Time dimension for analysis 
global miny_analysis 2004
global maxy_analysis 2018


///// 1. EXTRACT DATA AND SET MAIN VARIABLES
do ${path}\Do\data_extraction.do

///// 2. FIRM-LEVEL PF ESTIMATION 
do ${path}\Do\prog_translog
prog_translog, variableinput(purch_tot - purch_rent) gmmtype(TL_LL) filename(TL_LL_purch)

do ${path}\Do\prog_cobbdouglas
prog_cobbdouglas, variableinput(purch_tot - purch_rent) gmmtype(CD_LL) filename(CD_LL_purch)

///// 3. BASIC SUMMARY STATS
do ${path}\Do\prog_descriptives
prog_descriptives, filename(TL_LL_purch)

///// 4. MICRO EVIDENCE ON MARKUPS 
do ${path}\Do\prog_mu_micro

///// 5. MICRO EVIDENCE ON MARKDOWNS 
do ${path}\Do\prog_md_micro

///// 6. CROSS-SECTION REGRESSIONS
do ${path}\Do\prog_het_regression
prog_het_regression, filename(TL_LL_purch)

///// 7. AGGREGATE MARKUPS AND MARKDOWNS AND THEIR IMPLICATIONS
do ${path}\Do\prog_mumd_agg
prog_mumd_agg, filename(TL_LL_purch)

do ${path}\Do\prog_mumd_agg_exp_imp_gvc
prog_mumd_agg_exp_imp_gvc, filename(TL_LL_purch)

do ${path}\Do\prog_mumd_agg_cd
prog_mumd_agg_cd, filename(CD_LL_purch)

do ${path}\Do\prog_mumd_implications
prog_mumd_implications, filename(TL_LL_purch)

///// 8. FHK DECOMPOSITION OF MARKUPS AND MARKDOWNS
do ${path}\Do\prog_FHK
prog_FHK, filename(TL_LL_purch)
