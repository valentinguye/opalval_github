
*********************************************** DATA EXPLORATORY ANALYSIS ******************************************************

/*
Quick note about the total function for egen. 
It calculates a column sum.
"if" restricts BOTH the number of cells that enter the sum and the cells that get the value in the new variable. 
Because we need that only one cell per year gets the value for our plots, the latter is not an issue. 
*/
***** COUNTRY OFFICIAL STATS *****
* There are several possible sources: FAOSTAT, UN comtrade, indexmundi, Indonesian MoA... 

* When the commodity is not specified, it is CPO. 
global commodities cpo pko 

***** IndexMundi (source is USDA) ***************** 
import excel "$base_path_wd\analysis\input\official_aggregates\indexmundi_production_export.xlsx", sheet("Sheet1") firstrow clear
*convert it to million tonnes

foreach commo of global commodities{
	replace PRODUCTION_`commo'_im = PRODUCTION_`commo'_im/1000
	replace EXPORT_`commo'_im = EXPORT_`commo'_im/1000

	gen EXPORT_SHARE_`commo'_im = EXPORT_`commo'_im/PRODUCTION_`commo'_im
}
* remove labels for further graphs
ds
foreach var of varlist `r(varlist)'{
label variable `var' ""
}
save "$base_path_wd\analysis\input\official_aggregates\indexmundi.dta", replace


***** FAOSTAT *****************
import excel "$base_path_wd\analysis\input\official_aggregates\FAOSTAT_production.xlsx", sheet("Sheet1") firstrow clear
*convert it to million tonnes
foreach commo of global commodities{
replace PRODUCTION_`commo'_fs = PRODUCTION_`commo'_fs/(10^6)
}

ds
foreach var of varlist `r(varlist)'{
label variable `var' ""
}
save "$base_path_wd\analysis\input\official_aggregates\FAOSTAT_production.dta", replace



***** UN TRADE STATISTICS ***************** 
import excel "$base_path_wd\analysis\input\official_aggregates\Comtrade_1998_2015.xlsx", sheet("Sheet1") firstrow clear
* it is in kg, and we want to convert it in million tonnes. 
replace EXPORT_cpo_un = EXPORT_cpo_un/(10^9)
rename EXPORT_cpo_un EXPORT_un 
ds
foreach var of varlist `r(varlist)'{
label variable `var' ""
}
save "$base_path_wd\analysis\input\official_aggregates\Comtrade_1998_2015.dta", replace 



***** Ministry of Agriculture ***************** 
import excel "$base_path_wd\analysis\input\official_aggregates\MoA_prod_exp.xlsx", sheet("Sheet1") firstrow clear 

replace PRODUCTION_ton_MoA = PRODUCTION_ton_MoA/(10^6)
rename PRODUCTION_ton_MoA PRODUCTION_MoA


replace EXPORT_kg_MoA = EXPORT_kg_MoA/(10^9)
rename EXPORT_kg_MoA EXPORT_MoA 


replace EXPORT_usd_MoA = EXPORT_usd_MoA/(10^6)
rename EXPORT_usd_MoA EXPORT_val_MoA

ds
foreach var of varlist `r(varlist)'{
label variable `var' ""
}
save "$base_path_wd\analysis\input\official_aggregates\MoA_prod_exp.dta", replace 


****************************

use "$base_path_wd\build\output\IBS_mills_final.dta", clear
sort firm_id year 

merge m:1 year using "$base_path_wd\analysis\input\official_aggregates\indexmundi.dta", nogenerate

merge m:1 year using "$base_path_wd\analysis\input\official_aggregates\FAOSTAT_production.dta", nogenerate

merge m:1 year using "$base_path_wd\analysis\input\official_aggregates\Comtrade_1998_2015.dta", nogenerate

merge m:1 year using "$base_path_wd\analysis\input\official_aggregates\MoA_prod_exp.dta", nogenerate

* From FAOSTAT and MoA we have only production, and from UN COM TRADE only exports. 
gen EXPORT_SHARE_fs_un = EXPORT_un/PRODUCTION_cpo_fs
gen EXPORT_SHARE_MoA_un = EXPORT_un/PRODUCTION_MoA
gen EXPORT_SHARE_im_un = EXPORT_un/PRODUCTION_cpo_im

/*
Statistical description of the data is to be made over 4 different samples. 
Indeed, there is two kinds of distinction between two samples: 
Two samples have different degrees of imputation (imp1 and imp2)
Two samples have different purposes: one is the largest one we possibly have (indiced f for full), and the other one is the data actually available for analysis (e for effective). 
The latter gathers all observations with geo coordinates and a cpo price. 
Currently, it's 3819 obs for cpo_price_imp1 and 4445 for cpo_price_imp2. 
*/
gen geo = (!mi(lat) )
*| unref == 1

*Pour le moment on utilise unref, mais à terme ce sera seulement ceux qui ont vraiment des coordonnées. 


***** PRODUCTION *****
* Generate aggregated production of palm commodities, in million tons and in million 2010 USD. 
cd "$base_path_wd\analysis\output\production"

set graphics off
global commodities cpo pko
global unit ton val
foreach commo of global commodities{
	foreach uni of global unit{
		forvalues a = 1/2{

		** TOTAL PRODUCTION_
		* generate for full and effective sample
		bys year: egen OUT_`uni'_`commo'_imp`a'_f = total(out_`uni'_`commo'_imp`a') 
		bys year: egen OUT_`uni'_`commo'_imp`a'_e = total(out_`uni'_`commo'_imp`a') if geo == 1 & !mi(`commo'_price_imp`a')
		
		* convert in million 
		replace OUT_`uni'_`commo'_imp`a'_f = OUT_`uni'_`commo'_imp`a'_f/(10^6) 
		replace OUT_`uni'_`commo'_imp`a'_e = OUT_`uni'_`commo'_imp`a'_e/(10^6) 
/*		
		*graph it
		twoway (line OUT_`uni'_`commo'_imp`a'_f year, sort) (line OUT_`uni'_`commo'_imp`a'_e year, sort) ///
		(line PRODUCTION_`commo'_im year, sort)  ///
		(line PRODUCTION_`commo'_fs year, sort) ///
		(line PRODUCTION_MoA year, sort)
		local name OUT_`uni'_`commo'_imp`a'
		graph save Graph "`name'.gph", replace 
*/		
		}
	}
}
/*
set graphics on

	forvalues a = 1/2{
		graph use "OUT_ton_cpo_imp`a'.gph" 
	}
*/

** COMMENTS
* pko is much higher in sample than in official stats. May be because the latter don't include meals etc. but strictly kernel OIL. 

* for both CPO and PKO and for both imp1 and imp2, the effective sample follows roughly the same evolution as the full sample. 
* Evolutions are also comparable between imp1 and imp2 for CPO and PKO quantities and values, in both full and effective sample.



***** PREX VARIABLES ***** 

* rajouter le PREX non pondéré pour montrer que les exporteurs sont en moyennes des plus gros producteurs. 

* TROUVER D'AUTRES SOURCES MACRO 


/*
The propper way to compute the samples' export shares is to compute all individal exported amounts (derived from individual commodity export shares and outputs)
then sum them. The result is the total export from subpopulation of those who reported their export shares (within either the full or the effective sample). 
divide by the total production *of those subpopulations*. 
*/

* generate individual exported quantity and value. 
global commodities cpo pko
global unit ton val 
foreach commo of global commodities{
	foreach uni of global unit{
		forvalues a = 1/2{
		gen double exp_`uni'_`commo'_imp`a' = (prex_`commo'_imp`a'/100)*out_`uni'_`commo'_imp`a'
		order exp_`uni'_`commo'_imp`a', before(prex_`commo')
		
		* and with not imputed prex (imp are only for output)
		gen double exp_`uni'_`commo'_outimp`a' = (prex_`commo'/100)*out_`uni'_`commo'_imp`a'
		}
	}
}	

*browse exp_ton_cpo_imp2 prex_cpo_imp2 out_ton_cpo_imp2 if mi(prex_cpo_imp2) & !mi(out_ton_cpo_imp2)

* generate aggregated quantities and values exported (total and mean). So its 4 aggregates of interest, each on 4 different samples:
* imp 1 or 2, either on largest possible sample or on actually available for analysis sample (geolocalized and with a non missing price)

*** AGGREGATE 

** Non weighted mean
foreach commo of global commodities{
	forvalues a = 1/2{
		bys year: egen PREX_`commo'_imp`a'_f = mean(prex_`commo'_imp`a'/100) 
		bys year: egen PREX_`commo'_imp`a'_e = mean(prex_`commo'_imp`a'/100) if geo == 1 & !mi(`commo'_price_imp`a')
	}
}	

/* Simple unweigted average export shares are lower than those weighted with production.
This shows that firms that produce more also have higher export shares.   
*/

**Weighted means of prex_cpo (calculated for both quantities and values) 
cd "$base_path_wd\analysis\output\export"
set graphics off
global commodities cpo pko
global unit ton val 
foreach commo of global commodities{
	foreach uni of global unit{
		forvalues a = 1/2{

		** TOTAL PRODUCTION OF THOSE WHO INFORM THE EXPORT SHARE
			bys year: egen OUT_ofexp_`uni'_`commo'_imp`a'_f = total(out_`uni'_`commo'_imp`a') if !mi(exp_`uni'_`commo'_imp`a')
			bys year: egen OUT_ofexp_`uni'_`commo'_imp`a'_e = total(out_`uni'_`commo'_imp`a') if !mi(exp_`uni'_`commo'_imp`a') & geo == 1 & !mi(`commo'_price_imp`a')
			* convert in million
			replace OUT_ofexp_`uni'_`commo'_imp`a'_f = OUT_ofexp_`uni'_`commo'_imp`a'_f/(10^6) 
			replace OUT_ofexp_`uni'_`commo'_imp`a'_e = OUT_ofexp_`uni'_`commo'_imp`a'_e/(10^6) 
/*
			* graph it
			twoway (line OUT_ofexp_`uni'_`commo'_imp`a'_f year, sort) (line OUT_ofexp_`uni'_`commo'_imp`a'_e year, sort) /// 
				   (line OUT_`uni'_`commo'_imp`a'_f year, sort) (line OUT_`uni'_`commo'_imp`a'_e year, sort) ///
				   (line PRODUCTION_`commo'_im year, sort) ///
				   (line PRODUCTION_`commo'_fs year, sort) ///
				   (line PRODUCTION_MoA year, sort)
			local name OUT_ofexp_`uni'_`commo'_imp`a'
			graph save Graph "`name'.gph", replace 
*/

		** TOTAL EXPORT
			* generate for full and effective sample
			bys year: egen EXP_`uni'_`commo'_imp`a'_f = total(exp_`uni'_`commo'_imp`a') 
			bys year: egen EXP_`uni'_`commo'_imp`a'_e = total(exp_`uni'_`commo'_imp`a') if geo == 1 & !mi(`commo'_price_imp`a')
			* convert in million		
			replace EXP_`uni'_`commo'_imp`a'_f = EXP_`uni'_`commo'_imp`a'_f/(10^6) 
			replace EXP_`uni'_`commo'_imp`a'_e = EXP_`uni'_`commo'_imp`a'_e/(10^6) 
/*
			* graph it 			
			twoway (line EXP_`uni'_`commo'_imp`a'_f year, sort) (line EXP_`uni'_`commo'_imp`a'_e year, sort) /// 
				   (line EXPORT_`commo'_im year, sort) ///
				   (line EXPORT_un year, sort) ///
				   (line EXPORT_MoA year, sort) 
			local name EXP_`uni'_`commo'_imp`a'
			graph save Graph "`name'.gph", replace 
*/

		** EXPORT SHARE
			bys year: gen PREX_`uni'_`commo'_imp`a'_f = EXP_`uni'_`commo'_imp`a'_f/OUT_ofexp_`uni'_`commo'_imp`a'_f
			bys year: gen PREX_`uni'_`commo'_imp`a'_e = EXP_`uni'_`commo'_imp`a'_e/OUT_ofexp_`uni'_`commo'_imp`a'_e 
/*
			* graph it 
			twoway (line PREX_`uni'_`commo'_imp`a'_f year, sort) (line PREX_`uni'_`commo'_imp`a'_e year, sort) /// 
				   (line EXPORT_SHARE_fs_un year, sort) /// let's not include EXPORT_SHARE_`commo'_im because the export amount is not coherent with other sources. 
				   (line EXPORT_SHARE_MoA_un year, sort) ///
				   (line EXPORT_SHARE_im_un year, sort)
			local name PREX_`uni'_`commo'_imp`a'
			graph save Graph "`name'.gph", replace 
*/			
		
		* and perform everything for not imputed prex
		bys year: egen OUT_ofexp_`uni'_`commo'_outimp`a'_f = total(out_`uni'_`commo'_imp`a') if !mi(exp_`uni'_`commo'_outimp`a')
		bys year: egen OUT_ofexp_`uni'_`commo'_outimp`a'_e = total(out_`uni'_`commo'_imp`a') if !mi(exp_`uni'_`commo'_outimp`a') & geo == 1 & !mi(`commo'_price_imp`a')

		replace OUT_ofexp_`uni'_`commo'_outimp`a'_f = OUT_ofexp_`uni'_`commo'_outimp`a'_f/(10^6) 
		replace OUT_ofexp_`uni'_`commo'_outimp`a'_e = OUT_ofexp_`uni'_`commo'_outimp`a'_e/(10^6) 	

		bys year: egen EXP_`uni'_`commo'_outimp`a'_f = total(exp_`uni'_`commo'_outimp`a') 
		bys year: egen EXP_`uni'_`commo'_outimp`a'_e = total(exp_`uni'_`commo'_outimp`a') if geo == 1 & !mi(`commo'_price_imp`a')
			* convert in million		
		replace EXP_`uni'_`commo'_outimp`a'_f = EXP_`uni'_`commo'_outimp`a'_f/(10^6) 
		replace EXP_`uni'_`commo'_outimp`a'_e = EXP_`uni'_`commo'_outimp`a'_e/(10^6)

		bys year: gen PREX_`uni'_`commo'_outimp`a'_f = EXP_`uni'_`commo'_outimp`a'_f/OUT_ofexp_`uni'_`commo'_outimp`a'_f
		bys year: gen PREX_`uni'_`commo'_outimp`a'_e = EXP_`uni'_`commo'_outimp`a'_e/OUT_ofexp_`uni'_`commo'_outimp`a'_e 
		
		}
	}	
}

set graphics on


		forvalues a = 1/2{
		graph use "$base_path_wd\analysis\output\export\PREX_ton_cpo_imp`a'.gph" 
		}

graph use "$base_path_wd\analysis\output\export_WITH_imputed_zeros\PREX_ton_cpo_imp`a'.gph" 
		graph use "$base_path_wd\analysis\output\export_WO_imputed_zeros\PREX_ton_cpo_imp`a'.gph" 


* PRODUCTION OF EXPORT-SHARE RESPONDENTS
 (line OUT_ofexp_ton_cpo_imp1_f year, sort) (line OUT_ofexp_ton_cpo_imp1_e year, sort) (line OUT_ofexp_ton_cpo_imp2_f year, sort) (line OUT_ofexp_ton_cpo_imp2_e year, sort) /// 
twoway (line OUT_ofexp_ton_cpo_outimp1_f year, sort) (line OUT_ofexp_ton_cpo_outimp1_e year, sort) (line OUT_ofexp_ton_cpo_outimp2_f year, sort) (line OUT_ofexp_ton_cpo_outimp2_e year, sort) ///
			(line PRODUCTION_cpo_im year, sort) ///
			(line PRODUCTION_cpo_fs year, sort) ///
			(line PRODUCTION_MoA year, sort) 	


twoway (line OUT_ton_cpo_imp1_f year, sort) (line OUT_ton_cpo_imp1_e year, sort) (line OUT_ton_cpo_imp2_f year, sort) (line OUT_ton_cpo_imp2_e year, sort) ///
 (line PRODUCTION_cpo_im year, sort) ///
			(line PRODUCTION_cpo_fs year, sort) ///
			(line PRODUCTION_MoA year, sort) 	

*EXPORT
local a 2

twoway (line EXP_ton_cpo_imp1_f year, sort) (line EXP_ton_cpo_imp1_e year, sort) (line EXP_ton_cpo_imp2_f year, sort) (line EXP_ton_cpo_imp2_e year, sort) /// 
			(line EXPORT_un year, sort) 					   		   

* EXPORT SHARE
local a 1

twoway (line PREX_ton_cpo_imp1_f year, sort) (line PREX_ton_cpo_imp1_e year, sort) (line PREX_cpo_imp1_f year, sort) (line PREX_cpo_imp1_e year, sort)


/// 
				   (line EXPORT_SHARE_im_un year, sort) ///
				   (line EXPORT_SHARE_fs_un year, sort) ///
				   (line EXPORT_SHARE_MoA_un year, sort) 


 browse firm_id year out_ton_cpo_imp1 out_ton_cpo_imp2 exp_ton_cpo_imp1 exp_ton_cpo_imp2 OUT_ofexp_ton_cpo_imp1_f OUT_ofexp_ton_cpo_imp1_e OUT_ton_cpo_imp1_f OUT_ton_cpo_imp1_e if year == 2006


set graphics on 

** COMMENTS 
/*
PKO exports are still higher than national aggregate, still for the likely reason that indexmundi does not include all kernel products in. 

I.e. with or without these 3353 obs. that are turned or not from missing to zero, the overall pattern of aggregated cpo export share
does not change much. This must be because those 3353 obs. weigh little in the aggregation (they produce null or little)
Because in following imputations (cf. cleaning_IBS.do) missings can be replaced with zeros in some restricted cases. 

For imp2, the difference is even smaller. 
*/
		
/*
***Regarding ZEROS***
			
There is a risk that firms answer 0 for either the total export share or commo export share while their outputs are actually exported but they don't know it. 
This would mostly be in cases where their outputs are transformed by another domestic stakeholder before being exported. 
To a smaller extent it may be in cases where their products are not transformed domestically but they simply ignore whether their clients export them or not. 
It could be that smaller firms have less information, and it could also be that its bigger mills that have less visibility if they tend to be selling to bigger refineries. 

Let's see what some tests can tell us on this risk (only for CPO). 

	To analyse these PREX variables, one can look at the mean prex_cpo by year and compare it to the aggregate. 
	It is possible that firms which produce more also export more and that these firms are more or less represented in our sample than in the pop. for instance.  
	Therefore we should rather calculate the weighted (i.e. the effective) sample export share. 
	Either sum of fractions or fraction of sums. Not necessarily equal. 
	Using the former as a mean statistic assumes each obs. has weight=1 
	The latter weighs each individual prex by qty/sample_qty. 

	**Different samples are to be distinguished**
		- There are different samples coming from different data imputations (imp1 and imp2)
		- There are different samples based on different variables being fully non-missing: 
			prex_cpo_ non missing (only for unweighted mean)
			prex_cpo and out_ton_cpo non missing 
			prex_cpo and out_val_cpo non missing
			prex_cpo and price_cpo non missing. 
			The three first ones are informative, the latter is the actual sample we will use for analysis. 

		Hence, the macros produced below are indexed following this rule: samplexy is sample with 
		- impa (a=1;2),
		- b = 0 if only out_ton_cpo OR out_val_cpo (depending which is used to compute the export share) is non-missing 
			and b = 1 is the subsample with price_cpo non-missing. 

	*/


**For non-weighted mean of prex_cpo. 	
/*
	forvalues a = 1/2 {
		forvalues y = 1998/2015{	
				// Among non-missing out_ton  
				quietly tabstat prex_cpo_imp`a' if year == `y' & out_ton_cpo_imp`a'<., statistics(mean) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'o_prex_cpo_`y' = stats[1,1]
				// Among non-missing out_val 
				quietly tabstat prex_cpo_imp`a' if year == `y' & out_val_cpo_imp`a'<., statistics(mean) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'v_prex_cpo_`y' = stats[1,1]
				// Among non-missing cpo_price 
				quietly tabstat prex_cpo_imp`a' if year == `y' & cpo_price_imp`a'<., statistics(mean) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'p_prex_cpo_`y' = stats[1,1]
		}
	}
*/
