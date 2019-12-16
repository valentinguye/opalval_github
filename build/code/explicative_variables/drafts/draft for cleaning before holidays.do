*use /Users/valentinguye/Desktop/IBS_1998old.dta, clear  
use "C:\Users\guyv\ownCloud\opalval\build\input\IBS_1998.dta", clear
sort firm_id year 



order  ag_out_qty_ton_cpo ag_out_value_cpo avg_out_prex_cpo ag_out_qty_cpo tag_multioutput_cpo out_measurement_unit_cpo   ag_out_qty_ton_pko ag_out_value_pko avg_out_prex_pko ag_out_qty_pko tag_multioutput_pko out_measurement_unit_pko  ag_out_qty_ton_rpo ag_out_value_rpo avg_out_prex_rpo ag_out_qty_rpo tag_multioutput_rpo out_measurement_unit_rpo  ag_out_qty_ton_rpko ag_out_value_rpko avg_out_prex_rpko ag_out_qty_rpko tag_multioutput_rpko out_measurement_unit_rpko, after( industry_code)
order in_ton_ffb in_val_ffb flag_multiinput in_measurement_unit_ffb, after(industry_code)
rename ag_out_qty_* out_* 
rename ag_out_value* out_val* 

global output_vars out_ton_cpo out_val_cpo out_ton_pko out_val_pko out_ton_rpo out_val_rpo out_ton_rpko out_val_rpko

/*
gen ghost= (out_ton_cpo>=.& out_ton_pko>=.& out_ton_rpo>=.& out_ton_rpko>=.)
547
gen fullghost= (out_ton_cpo>=.& out_ton_pko>=.& out_ton_rpo>=.& out_ton_rpko>=. & out_val_cpo>=.& out_val_pko>=.& out_val_rpo>=.& out_val_rpko>=.)
count if ghost!=fullghost
--> 0. So there is no obs. that has info on values but on no qty: all obs that lack info on qty also feature missings for values. 
These guys are obs that were on either IBS_inputs or IBS_full but not in IBS_outputs. 
we can keep them  

duplicates report firm_id out_ton_cpo out_val_cpo out_ton_pko out_val_pko out_ton_rpo out_val_rpo out_ton_rpko out_val_rpko if ghost==0
--> 0. meaning duplicates in terms of both qty and value for all outputs at the same time are only ghosts. 
*/

*Creating indicators of validity for output variables: 1 for obs. that are neither 0 nor missing. 
foreach var of varlist $output_vars {
	gen valid_`var' = (`var' <. & `var'>0)
}

gen valid_cpo = (valid_out_ton_cpo==1 & valid_out_val_cpo==1)
gen valid_pko = (valid_out_ton_pko==1 & valid_out_val_pko==1)
gen valid_rpo = (valid_out_ton_rpo==1 & valid_out_val_rpo==1)
gen valid_rpko = (valid_out_ton_rpko==1 & valid_out_val_rpko==1)
/*g valid_ffb = (in_ton_ffb<. & in_ton_ffb>0 & in_val_ffb<. & in_val_ffb>0)
order valid_ffb, after(in_measurement_unit_ffb)
*/

*Creating lags for all output variables. 
sort firm_id year
foreach var of varlist $output_vars {
	bys firm_id : gen double lag_`var' = `var'[_n-1] 
}
global lag_output_vars lag_out_ton_cpo lag_out_val_cpo lag_out_ton_pko lag_out_val_pko lag_out_ton_rpo lag_out_val_rpo lag_out_ton_rpko lag_out_val_rpko



/*Plan: 

***DUPLICATES***
***DEFLATE***
***otlS***
	0. statistical otls
	1. output/input ratio
	2. cpo/pko ratio
	3. variation rate

***PRICE***
***AGGREGATED OBS.***
***PERCENTAGE EXPORTED***
***REFINING***

Over all the process, imp1 variables are associated with stronger imputations than imp2. It is to say that imp1 sample will be more modified than imp2, in an attempt to reduce noise.
  
*/




*********************************************************************************************************************************************************************
***DUPLICATES***
*********************************************************************************************************************************************************************







**WITHIN FIRM_ID duplicates in terms of out_ton_ and out_val_ . 

	/* Why are there such duplicates?
	Either BPS, the Indonesian stat agency, has filled some missing cells with the information from the same firm one other year before. 
	Or the mill itself did that because it did not know the exact amount at the time of the survey. 
	Or it is a data manipulation error. 
	In the two first cases, which are most likely, the copies still contain some information, if they are attempts of estimation of the real amount. 
	So we will have to extreme logic: 
	_imp1 will consider that these estimations are too noisy and should be rather missing; while 
	_imp2 will keep some of them, considering they might still contain some actual info. 
	*/

	*_imp1: any copy is too noisy* 
		// So for out_ton_cpo_imp1 it is 832 real changes to missing, and for out_val_cpo_imp1 it is only 29. 
		foreach var of varlist $output_vars {
			gen double `var'_imp1 = `var'
			order `var'_imp1, after(`var')
			gen flag_du_firm_`var' = (`var'== lag_`var' & valid_`var'==1)
			replace `var'_imp1 = . if flag_du_firm_`var'==1 
			drop flag_du_firm_`var'
		}

	*_imp2: only duplicates in terms of both qty and value are too noisy, but when either one varies, there will be some information in the price that will be worth the noise. 
		// So for out_ton_cpo_imp2 it is only 16 real changes to missing. 	
		foreach var of varlist $output_vars {
			gen double `var'_imp2 = `var'
			order `var'_imp2, after(`var'_imp1)
		}

		gen flag_du_firm_qty_val_cpo = (lag_out_ton_cpo == out_ton_cpo & lag_out_val_cpo==out_val_cpo & valid_cpo==1)
		gen flag_du_firm_qty_val_pko = (lag_out_ton_pko == out_ton_pko & lag_out_val_pko==out_val_pko & valid_pko==1)
		gen flag_du_firm_qty_val_rpo = (lag_out_ton_rpo == out_ton_rpo & lag_out_val_rpo==out_val_rpo & valid_rpo==1)
		gen flag_du_firm_qty_val_rpko = (lag_out_ton_rpko == out_ton_rpko & lag_out_val_rpko==out_val_rpko & valid_rpko==1)

		global out_cpo_vars out_ton_cpo out_val_cpo
		foreach var of varlist $out_cpo_vars {
			replace `var'_imp2 = . if flag_du_firm_qty_val_cpo
		}
		
		global out_pko_vars out_ton_pko out_val_pko
		foreach var of varlist $out_pko_vars {
			replace `var'_imp2 = . if flag_du_firm_qty_val_pko
		}		

		global out_rpo_vars out_ton_rpo out_val_rpo
		foreach var of varlist $out_rpo_vars {
			replace `var'_imp2 = . if flag_du_firm_qty_val_rpo
		}		

		global out_rpko_vars out_ton_rpko out_val_rpko
		foreach var of varlist $out_rpko_vars {
			replace `var'_imp2 = . if flag_du_firm_qty_val_rpko
		}

		drop flag_du_firm_qty_val_cpo flag_du_firm_qty_val_pko flag_du_firm_qty_val_rpo flag_du_firm_qty_val_rpko
		
		foreach var of varlist $lag_output_vars{
			drop `var'
		}

**WITHIN YEAR duplicates in terms of out_ton_ and out_val_  

	/* Why are there such duplicates? Probably because BPS wanted to fill some missings when it was their first year (so they could not imput from previous years within same firm).
	Then, they may have used the information from an already existing mill, or a mill appearing this year too but without missing. 
	Here, we would like to keep the originals. If they were startup firms too, then we cannot identify them, but if among a group of duplicates one and only one obs. was existing before 
	then it is likely to be the original. 
	In the more conservative _imp1 variables, we remove also such original obs. 

	Also, we keep duplicates in terms of either only qty or value, considering that within a whole cross-section this is possible. 
	*/

	*For CPO
		local a year out_ton_cpo out_val_cpo
		egen du_id_year_cpo = group(`a')
		duplicates tag `a' if valid_cpo==1, gen(tag_du_year_cpo)
		bysort firm_id : gen lag_year = year[_n-1]
		gen nostartup = (lag_year<.)
		bysort du_id_year_cpo : egen n_nostartup_cpo = total(nostartup)
		gen original_cpo = (n_nostartup_cpo ==1 & nostartup==1)
		
		global imp1_cpo_vars out_ton_cpo_imp1 out_val_cpo_imp1 
		foreach var of varlist $imp1_cpo_vars {
			replace `var' = . if tag_du_year_cpo>0 & tag_du_year_cpo<. 
		}
		global imp2_cpo_vars out_ton_cpo_imp2 out_val_cpo_imp2 
		foreach var of varlist $imp2_cpo_vars {
			replace `var' = . if tag_du_year_cpo>0 & tag_du_year_cpo<. & original_cpo==0 
		}

	*And for PKO
		local b year out_ton_pko out_val_pko
		egen du_id_year_pko = group(`b')
		duplicates tag `b' if valid_pko==1, gen(tag_du_year_pko)

		bysort du_id_year_pko : egen n_nostartup_pko = total(nostartup)
		gen original_pko = (n_nostartup_pko ==1 & nostartup==1)
		
		global imp1_pko_vars out_ton_pko_imp1 out_val_pko_imp1 
		foreach var of varlist $imp1_pko_vars {
			replace `var' = . if tag_du_year_pko>0 & tag_du_year_pko<. 
		}
		global imp2_pko_vars out_ton_pko_imp2 out_val_pko_imp2 
		foreach var of varlist $imp2_pko_vars {
			replace `var' = . if tag_du_year_pko>0 & tag_du_year_pko<. & original_pko==0 
		}

	*And for RPO 
		local c year out_ton_rpo out_val_rpo
		egen du_id_year_rpo = group(`c')
		duplicates tag `c' if valid_rpo==1, gen(tag_du_year_rpo)

		bysort du_id_year_rpo : egen n_nostartup_rpo = total(nostartup)
		gen original_rpo = (n_nostartup_rpo ==1 & nostartup==1)
		
		global imp1_rpo_vars out_ton_rpo_imp1 out_val_rpo_imp1 
		foreach var of varlist $imp1_rpo_vars {
			replace `var' = . if tag_du_year_rpo>0 & tag_du_year_rpo<. 
		}
		global imp2_rpo_vars out_ton_rpo_imp2 out_val_rpo_imp2 
		foreach var of varlist $imp2_rpo_vars {
			replace `var' = . if tag_du_year_rpo>0 & tag_du_year_rpo<. & original_rpo==0 
		}

	*And for RPKO
		local d year out_ton_rpko out_val_rpko
		egen du_id_year_rpko = group(`d')
		duplicates tag `d' if valid_rpko==1, gen(tag_du_year_rpko)

		bysort du_id_year_rpko : egen n_nostartup_rpko = total(nostartup)
		gen original_rpko = (n_nostartup_rpko ==1 & nostartup==1)
		
		global imp1_rpko_vars out_ton_rpko_imp1 out_val_rpko_imp1 
		foreach var of varlist $imp1_rpko_vars {
			replace `var' = . if tag_du_year_rpko>0 & tag_du_year_rpko<. 
		}
		global imp2_rpko_vars out_ton_rpko_imp2 out_val_rpko_imp2 
		foreach var of varlist $imp2_rpko_vars {
			replace `var' = . if tag_du_year_rpko>0 & tag_du_year_rpko<. & original_rpko==0 
		}
drop du_id_year_cpo du_id_year_pko tag_du_year_cpo tag_du_year_pko lag_year nostartup n_nostartup_cpo n_nostartup_pko original_cpo original_pko  ///
du_id_year_rpo tag_du_year_rpo n_nostartup_rpo original_rpo /// 
du_id_year_rpko tag_du_year_rpko n_nostartup_rpko original_rpko

/* 
browse firm_id year du_id_year_cpo tag_du_year_cpo if tag_du_year_cpo>0 & tag_du_year_cpo<.
One can see that in most cases these duplicates are duplicates within firm_id also. This means that our explanation "they filled missing startupers with existing firms" is not so true. 
So there may be another story behind these duplicates. Anyway, they are already removed from the duplicates treatment above then. Except for those that encompass startupers, then 
the program here saved 
*/









**********************************************************************************************************************************************************
***DEFLATE***
**********************************************************************************************************************************************************



merge m:1 year using C:\Users\guyv\ownCloud\opalval\build\input\wpi_palmoil_1998_2015.dta, nogenerate 
*Change reference year from 2000 to 2010: 
replace wpi = wpi/290.133117675781
rename wpi wpi_2010
*codebook wpi_2010 if year == 2010

global monetary_vars in_val_ffb ///
						out_val_cpo ///
						out_val_pko ///
						out_val_rpo ///
						out_val_rpko ///
						out_val_cpo_imp1 ///
						out_val_pko_imp1 ///
						out_val_rpo_imp1 ///
						out_val_rpko_imp1 ///
						out_val_cpo_imp2 ///
						out_val_pko_imp2 ///
						out_val_rpo_imp2 ///
						out_val_rpko_imp2

/*
Convert to full IDR (monetary values are measured in 1000 IDR in survey)
And deflate to 2010 price level. 
*/
foreach mon_var of varlist $monetary_vars {
	quietly replace `mon_var' = `mon_var'*1000
	quietly replace `mon_var' = `mon_var'/wpi_2010
}

// Convert to 2010 Dollars

foreach mon_var of varlist $monetary_vars {
	quietly replace `mon_var' = `mon_var'/9090.433333
}










**********************************************************************************************************************************************************
***OUTLIERS***
**********************************************************************************************************************************************************



/* 

The main idea behind removing qty and val extreme values and not merely extreme prices is that, for some reason, there are impossibly high qty and val, and we don't want to risk 
their ratio is deemed a normal price and these obs. used in the analyses. 

First, let us define otls. 
Statistically, they are those obs for which var > (p75+1.5iqr). And to be more precise we want to apply this definition within yearly cross-sections. 
Let us restrict this definition: otls are the obs that are statistical otls AND fail at least one test of likelihood. 
These tests use output/input ratio and CPO/PKO ratio and the variation_rate over time.
In other words, we will keep statistical otls that are however coherent with ALL the information we have. 
We don't extand the definition of otls (i.e. deem as otls obs. that are not statistical otls but are however not coherent with some of the information we have)
because this would require us to have high quality information on these other variables used (inputs and pko), which is not the case. This means 
that we don't see cases of extremely high say output/input ratio with normal output quantity as a hint of flawed measurement of output. We assume this is purely due to
input mismeasurement, and never because of a combination of mismeasurement of both. 

Regarding low otls, i.e. <(p25 - 1.5iqr). In theory, they should also be a concern. 
	...but there is none (for out_ton_cpo and out_val_cpo). For the ratios in the tests, we are not interested in spotting low otls because, since there are 
	only high otls for the variables of interest, they would only spot abnormally high secondary information (input, pko or previous). 

Regarding the test on variation_rate.  
 	Why is it ok to add it for cpo and pko qty and not for value nor for rpo and rpko qty? 
 	Because as such, this test is not very precise (and designing more sophisticated one would be time consuming and not necessarily worth it).
 	For out_ton_, including it makes an additional condition to the loosing of the otl definition, i.e. adding this test makes always less obs. "saved" from being statistical otls. 
 	On the other hand, adding it when it is the only test, in the out_val_ cases, means that adding this test makes always more obs. "saved", since otherwise you only use statistical 
 	otl definition. 



INFINE, 4 cases: 
	- Neither out_ton_cpo nor out_val_cpo are otls --> the obs. is kept. 
	- out_ton_cpo is an otl (it is a statistical otl and it failed one of tests 1,2,3) and out_val_cpo is not an otl --> obs is droped
	- out_ton_cpo is not an otl but out_val_cpo is --> obs is droped
	- out_ton_cpo and out_val_cpo are otls --> obs is droped. 


Some notes on the codes below. 
	- tabstat is not conditioned because we remove zeros (see just below) and missings don't impact the distribution calculation. 

	- Taking the `var'<. in the if and not in the () enables to interpret otl_`var'=0 as "the obs. is not an otl", and not as
	"the obs. is not an otl OR it is missing on out_ton_cpo". Recall, this "if" ensures that missings are not counted as otls.

Beside, we will only modify _imp1 and _imp2, and the procedures need to be done separately for each, because they don't have the same distributions
and hence not the same otl thresholds etc. 


In order for the 0 cells not to be accounted as such in the tabstats. This is justified by the fact that we won't be able to interpret and hence to use a price coming from a firm 
with 0 in one of these cells. 
*/

global imp_out_vars out_ton_cpo_imp1 out_val_cpo_imp1 /// 
					out_ton_pko_imp1 out_val_pko_imp1 /// 
					out_ton_rpo_imp1 out_val_rpo_imp1 /// 
					out_ton_rpko_imp1 out_val_rpko_imp1 /// 
					out_ton_cpo_imp2 out_val_cpo_imp2 /// 
					out_ton_pko_imp2 out_val_pko_imp2 /// 
					out_ton_rpo_imp2 out_val_rpo_imp2 /// 
					out_ton_rpko_imp2 out_val_rpko_imp2
foreach var of varlist $imp_out_vars {
	replace `var'= . if `var' == 0 
}

/*

First let's compute the flags for outliers regarding additional information. 
This does not need to be done in a yearly  fashion. 

1. OUTPUT/INPUT RATIO
	
*/

	capture program drop output_input_ratio
	program define output_input_ratio
	gen oir_`1' = `1'/in_ton_ffb if `1'<. & in_ton_ffb <. & in_ton_ffb >0
	quietly tabstat oir_`1', statistics(iqr, p75) columns(variables) save
	matrix stats = r(StatTotal)
	local iqr_oir_`1' = stats[1,1]
	local p75_oir_`1' = stats[2,1]
	local otl_th_oir_`1' = `p75_oir_`1'' + 1.5*`iqr_oir_`1''
	gen otl_oir_`1' = (oir_`1'> `otl_th_oir_`1'') if oir_`1'<.
	drop oir_`1'
	*display `otl_th_oir_`1''
	end
	output_input_ratio out_ton_cpo_imp1
	output_input_ratio out_ton_cpo_imp2
	output_input_ratio out_ton_pko_imp1
	output_input_ratio out_ton_pko_imp2

	*browse out_ton_cpo oir if otl_oir==1 & otl_outtoncpo==0

	/*
	So among the 423 outtoncpo otls, 140 are also otls in terms of oir. Meaning that for these, both output is abnormal alone and it is abnormal with respect to input. 
	the remaining 283 outtoncpo otls are still to be evaluated with creterion below. 
	Regarding the 722 outtoncpo-non-otl that however are OIR otls, the only thing we can do is assuming this is because of an error on the sole input cell. 

	Note also that the median is .22 which is in line with the oil/bunch ratio of 21-23% in the literature. The mean is highly falsed by otls. 


2. CPO/PKO RATIO

 	*/
 	forvalues a = 1/2 {
		gen cpopko_ratio_imp`a' = out_ton_cpo_imp`a'/out_ton_pko_imp`a' if out_ton_cpo_imp`a'<. & out_ton_pko_imp`a'<.
		quietly tabstat cpopko_ratio_imp`a' if cpopko_ratio_imp`a'<., statistics(iqr, p75) columns(variables) save
		matrix stats = r(StatTotal)
		local iqr_cpopko_ratio = stats[1,1]
		local p75_cpopko_ratio = stats[2,1]
		local otl_th_cpopko_ratio = `p75_cpopko_ratio' + 1.5*`iqr_cpopko_ratio'
		gen otl_cpopko_ratio_imp`a' = (cpopko_ratio_imp`a' > `otl_th_cpopko_ratio') if cpopko_ratio_imp`a'<.
		drop cpopko_ratio_imp`a'
		*display `otl_th_cpopko_ratio'
	}

	/*	
	Here again, the median of cpopko_ratio of 4.5 is pretty broadly consistent with the literature giving an oil/bunch ratio between 3 and 4 times higher than the kernel/bunch ratio. 
	In Byerlee et al. (book) Table B2.1 the oil and kernel extraction rates are respectively 22.8 and 4.70 in Sime Darby plantations in 2012/2013 in Indonesia. 

	This is while still accounting for otls, the actual descriptive statistics will come later of course. 


3. VARIATION RATE
*/	
	sort firm_id year
	global imp_cpo_pko_ton_vars out_ton_cpo_imp1 out_ton_pko_imp1 out_ton_cpo_imp2 out_ton_pko_imp2
	foreach var of varlist $imp_cpo_pko_ton_vars {	
		bys firm_id : gen double lag_`var' = `var'[_n-1]	
		gen dt_`var' = (`var'-lag_`var')/(lag_`var') if `var'<. & lag_`var'<.
		quietly tabstat dt_`var', statistics(iqr p75) columns(variables) save
		matrix stats = r(StatTotal)
		local iqr_dt_`var' = stats[1,1]
		local p75_dt_`var' = stats[2,1]
		local otl_th_dt_`var'  = `p75_dt_`var'' + 1.5*`iqr_dt_`var''
		quietly gen otl_dt_`var' = (dt_`var' > `otl_th_dt_`var'') if dt_`var'<.
		drop lag_`var' dt_`var' 
		*display `otl_th_dt_`var''
	} 
	
/*


4. "STATISTICAL" OUTLIERS **

	This is to define statistical outliers ACROSS years	
		foreach var of varlist $imp_out_vars {
			quietly tabstat `var', statistics(iqr p75) columns(variables) save
			matrix stats = r(StatTotal)
			local iqr_`var' = stats[1,1]
			local p75_`var' = stats[2,1]
			local otl_th_`var' = `p75_`var'' + 1.5*`iqr_`var''
			quietly gen otl_`var' = (`var' > `otl_th_`var'') if `var'<.
			*display `otl_th_`var''
		}

	This is to define statistical outliers WITHIN years 	
	*/
	foreach var of varlist $imp_out_vars {
		quietly gen otl_`var' = 0 if `var'<.
		forvalues y = 1998/2015 {
			quietly tabstat `var' if year == `y', statistics(iqr p75) columns(variables) save
			matrix stats = r(StatTotal)
			local iqr_`var' = stats[1,1]
			local p75_`var' = stats[2,1]
			local otl_th_`var' = `p75_`var'' + 1.5*`iqr_`var''
			quietly replace otl_`var' = 1 if year == `y' & `var' > `otl_th_`var'' & `var'<.
			*display `otl_th_`var''
		}
	}
/*
browse if otl_out_ton_cpo_imp1==1 & out_measurement_unit_cpo!="Ton"
it does not seem that the mesurement unit conversions made in IBS_output_preparation.do have triggered unproportionnaly more otls. 
browse firm_id if firm_id == 2117 | firm_id == 10451 | firm_id == 56313 | firm_id == 68245 | firm_id == 70379



**DEFINE WHAT IS NOT AN OUTLIER IN THE END**

We should be be careful that when additional information is missing (there is no input data, or pko data, or it is the first observed year for this mill) 
the obs. is neither automatically discarded or kept: the statistical definition of otl should decide alone. 
generate otl_cpo = (otl_out_ton_cpo==1 & (otl_oir==1 | otl_cpopko_ratio==1 | otl_dt_out_ton_cpo==1))
for instance, this synthax is not good because statistical otls that happen to have no information on input, pko and t-1 out_ton_cpo fill non of the conditions in the second parentheses
thus they would be kept while we want the statistical otl definition to rule them out in this case. 
*/

* For CPO and PKO quantities
	forvalues a = 1/2 {
		global imp`a'_cpo_pko_ton_vars out_ton_cpo_imp`a' out_ton_pko_imp`a'
		foreach var of global imp`a'_cpo_pko_ton_vars {
			generate not_otl_`var' = (otl_`var'==0 | ///
				(otl_`var'==1 & otl_oir_`var'==0 & otl_cpopko_ratio_imp`a'==0 & otl_dt_`var'==0)) ///
				if otl_`var' <. 
			replace `var' = . if not_otl_`var'==0 
			drop not_otl_`var'
		}
	}

	/*
	With this code:
		Suppose you are missing on qty_cpo, then your otl_out_ton_cpo is neither 1 nor 0, and get a 0 here. 
		Suppose you are missing on val_cpo, then you should be discarded, but later, not on the behalf of being an out_ton_cpo otl.   
		Suppose you are missing on additional information (input, pko or lag), then you cannot have related flags equal to zero and what you get depends only on the first line. 
	So this synthax enables to interpret not_otl_outtoncpo==0 as otls among out_ton_cpo-valid information.

* For CPO and PKO values 
	Outliers are merely statistical outliers. 
	*/

	global imp_cpo_pko_val_vars out_val_cpo_imp1 out_val_pko_imp1 out_val_cpo_imp2 out_val_pko_imp2
	foreach var of varlist $imp_cpo_pko_val_vars{
		replace `var' = . if otl_`var'==1
	}
	
/*

To summarize: 
# removed obs. respectively for: 	out_ton_cpo_imp1 ; out_ton_pko_imp1 ; out_ton_cpo_imp2 ; out_ton_pko_imp2 ; out_val_cpo_imp1 ; out_val_pko_imp1 ; out_val_cpo_imp2 ; out_val_pko_imp2
Within year otl & with deflation	337					369					388					452				406					534					406					534
Within year otl & w/o deflation 	all the same
Across year otl & with deflation	329					392					384					478				426					565					426					565
Across year otl & w/o deflation 	329					392					384					478				357					517					357					517

What's to take away: 
Many qty outliers are duplicates on qty (likely within firm_id, since this is where most duplicates where) that kept only in imp2. 
Second line being the same as first line is logic: deflation does not change qty, nor does it change the identification of outliers for values if this is made within year anyway. 
If one defines outliers across the whole panel, more values are identified as being outliers after being deflated. 
Defining outliers across the whole panel rather than within each year does not lead to a very different number of obs. being removed. 


**Refined outliers removal**
We imput on a case by case basis among the remaining extreme values. 
	*For CPO
	There is still one extreme value for out_ton_cpo_imp1: It would not be used for final analyses because its value is extreme and removed. 
	But let us still remove it so it does not flaw aggregation checks implying qty for instance. 
	*/
	replace out_ton_cpo_imp2 = . if out_ton_cpo_imp2> 1000000 & out_ton_cpo_imp2<.
	*the other extreme values "saved" with out_ton_cpo_imp2 > 150,000 let's just keep them in imp2, they are not in imp1. 

	*For PKO, there remain important outliers (5 in imp2 out of which 2 in imp1). They are all outliers on val_imp1 and 2 anyway. Let's just remove those from imp1. 
	replace out_ton_pko_imp1 = . if out_ton_pko_imp1 > 200000 & out_ton_pko_imp1<. 

/*

For RPO AND PKO ... Let's see below how we treat them 
	global imp2_rpo_rpko_ton_vars out_ton_rpo_imp2 out_ton_rpko_imp2
	foreach var of varlist $imp2_rpo_rpko_ton_vars {
		replace `var' = . if otl_`var'==1 
	}

*graph hbox out_ton_cpo_imp1 if otl_out_ton_cpo_imp1 ==0
*codebook out_ton_cpo_imp1 out_ton_cpo_imp2

*sum out_ton_cpo out_ton_cpo_imp1 out_ton_cpo_imp2, detail


graph hbox out_val_cpo 
sum out_val_cpo, detail
*/

drop otl_oir_out_ton_cpo_imp1 ///
otl_oir_out_ton_cpo_imp2 ///
otl_oir_out_ton_pko_imp1 ///
otl_oir_out_ton_pko_imp2 ///
otl_cpopko_ratio_imp1 ///
otl_cpopko_ratio_imp2 ///
otl_dt_out_ton_cpo_imp1 ///
otl_dt_out_ton_pko_imp1 ///
otl_dt_out_ton_cpo_imp2 ///
otl_dt_out_ton_pko_imp2 ///
otl_out_ton_cpo_imp1 ///
otl_out_val_cpo_imp1 ///
otl_out_ton_pko_imp1 ///
otl_out_val_pko_imp1 ///
otl_out_ton_rpo_imp1 ///
otl_out_val_rpo_imp1 ///
otl_out_ton_rpko_imp1 ///
otl_out_val_rpko_imp1 ///
otl_out_ton_cpo_imp2 ///
otl_out_val_cpo_imp2 ///
otl_out_ton_pko_imp2 ///
otl_out_val_pko_imp2 ///
otl_out_ton_rpo_imp2 ///
otl_out_val_rpo_imp2 ///
otl_out_ton_rpko_imp2 ///
otl_out_val_rpko_imp2





*******************************************************************************************************************************************************************************************************
***PRICE***
*******************************************************************************************************************************************************************************************************




*CPO price
forvalues a = 1/2 {
	gen cpo_price_imp`a' = out_val_cpo_imp`a'/out_ton_cpo_imp`a'
	order cpo_price_imp`a', before(out_ton_cpo)
}

*PKO price 
forvalues a = 1/2 {
	gen pko_price_imp`a' = out_val_pko_imp`a'/out_ton_pko_imp`a'
	order pko_price_imp`a', before(out_ton_pko)
}
/* 
There are many otls in the price. We attribute them to extremely low quantities which we have not treated as outliers supra. 
Indeed it is difficult to say until which minimum production of a commodity a mill can go, it can credibly produce very little sometimes. 
For imputation, we couldn't use input or byproducts because the mismeasurement can as well be rather there. 
So by removing price upper outliers we remove obs. that have either mismeasurement of quantity (too low) relative to value, or mismeasurement of value (too high though not outlier) relative to 
a true small quantity. 
And by removing price lower outlier we remove obs. that have either mismeasurement of value (too low) relative to quantity, or mismeasurement of quantity (too high though not outlier) relative to 
a true small value. 

For these two processes, is there a reason to spot outliers within years? To me no, because extreme values are only deemed as such with respect to a contemporaneous information.

*/

global imp_prices cpo_price_imp1 pko_price_imp1 cpo_price_imp2 pko_price_imp2 
foreach var of varlist $imp_prices{
	quietly tabstat `var', statistics(iqr p75 p25) columns(variables) save
	matrix stats = r(StatTotal)
	local iqr_`var' = stats[1,1]
	local p75_`var' = stats[2,1]
	local p25_`var' = stats[3,1]
	local otl_uth_`var' = `p75_`var'' + 1.5*`iqr_`var''
	local otl_lth_`var' = `p25_`var'' - 1.5*`iqr_`var''
	quietly gen u_otl_`var' = (`var'>=`otl_uth_`var'') if `var'<.
	quietly gen l_otl_`var' = (`var'<=`otl_lth_`var'') if `var'<.
	*display `otl_lth_`var''
	*count if u_otl_`var'==1 
	*count if l_otl_`var'==1 
	replace `var' = . if u_otl_`var' ==1 
	replace `var' = . if l_otl_`var' ==1 
	drop u_otl_`var'
	drop l_otl_`var'
}

*graph hbox cpo_price_imp1








*************************************************************************************************************************************************************************************
***AGGREGATED OBS.***
*************************************************************************************************************************************************************************************







/*
/*At the end of IBS_output preparation, FOR CPO, there are 317 obs. that come from an aggregation. 
The remaining are either zero (not coming from an aggregation) or missing (the obs. is only producing other commodities, 1210 instances). 
Now (once merged), the number of missing has raised of course (no matches) and the number of obs. coming from an aggregation is still 317. 
Among which 65 are otls. Hence an important proportion, raising awareness that other aggregated obs. that are not deemed as otls are problematic as they are likely to be inflated. 
If one takes the statistical definition of otls, there are still 65 instances coming from aggregation. 
*/
inspect tag_multioutput_cpo
count if tag_multioutput_cpo >0 & tag_multioutput_cpo<. & otl_cpo == 1 
count if tag_multioutput_cpo >0 & tag_multioutput_cpo<. & (otl_out_ton_cpo == 1 | otl_out_val_cpo==1)
count if tag_multioutput_cpo >0 & tag_multioutput_cpo<. & (otl_out_ton_cpo == 1)
count if tag_multioutput_cpo >0 & tag_multioutput_cpo<. & (otl_out_val_cpo == 1)
count if tag_multioutput_cpo >0 & tag_multioutput_cpo<. & (otl_cpo_price == 1)
browse if tag_multioutput_cpo >0 & tag_multioutput_cpo<. & otl_cpo == 1 
browse if tag_multioutput_cpo >0 & tag_multioutput_cpo<.
/*
For remaining price otls, 7 obs come from aggregation and are price otls, of the 190 price otls at this stage. And these are not otls in terms of qty or value. 
In fact, they are small qtys with normal values.   
*/
browse if tag_multioutput_cpo >0 & tag_multioutput_cpo<. & (otl_cpo_price == 1)

/*
Voilà, sachant cela et avec la réponse de BPS on décidera de garder ces 247 (317-70) obs. ou non.  
On peut aussi checker comment ca se passe pour pko. */
inspect tag_multioutput_pko
*There are 326 obs. coming from aggregation of pko. 
count if tag_multioutput_pko >0 & tag_multioutput_pko<. & otl_pko == 1 
*Parmi lesquelles 179 ont été considérées comme otls ! 
count if tag_multioutput_pko >0 & tag_multioutput_pko<. & (otl_out_ton_pko == 1 | otl_out_val_pko==1)
*184 si on considère les définitions statistiques d'otl (qui produisent davantage d'otls)
count if tag_multioutput_pko >0 & tag_multioutput_pko<. & (otl_out_ton_pko == 1)
*160 obs. venant de l'aggrégation de pko sont des otls en terme de quantité 
count if tag_multioutput_pko >0 & tag_multioutput_pko<. & (otl_out_val_pko == 1)
*et 155 en termes de value. 
count if tag_multioutput_pko >0 & tag_multioutput_pko<. & (otl_pko_price == 1)
*seulement 6 en termes de prix: lorsque l'aggrégation a produit des valeurs très élevées pour quantité et value, le ratio lui n'est pas affecté. 
browse if tag_multioutput_pko >0 & tag_multioutput_pko<. & otl_pko == 1 
browse if tag_multioutput_pko >0 & tag_multioutput_pko<.
*Pour être vraiment rigoureux il faudrait faire un test de comparaison des distributions de ces 247 obs. et de l'ensemble des obs nettes d'otls. 
gen flag_multioutput = (tag_multioutput_cpo>0) if tag_multioutput_cpo<. 
ksmirnov out_ton_cpo, by(flag_multioutput) exact
ksmirnov out_val_cpo, by(flag_multioutput) exact
*/









**************************************************************************************************************************************************************
***PERCENTAGE EXPORTED***
**************************************************************************************************************************************************************

/*
In what follows export_pct, export_pct_imp or total/average/mean export share refer to the firm-level averaged percentage of all its products exported. 
prex refers to commodity specific export shares. 
export_pct_imp differs from export_pct in that full duplicates in the main IBS dataset are removed from the former. 
imp1 and imp2 are not related to qty and value imp1 and imp2 above. But the same logic applies: imp1 is subject to stronger imputations than imp2, i.e. imp1_prex_vars take value on 
a more modified sample. Here, modifying the sample means adding obs, while for qty and value vars it meant removing obs. 


The main problems are that there are many missings some years (2006, 2013, 2014, 2015) and apparently too many zeros all the times in all variables. 

***Regarding MISSINGS***

The reason for this is probably that respondents don't know that their outputs are exported, so they respond 0. 
To deal with the missings problem, we combine prex_ vars and export_pct_imp. This is not always equally convincing as an estimate. 
There may be many cases.
For CPO, 
Among obs. that have prex_cpo missing and export_pct non missing (we can deal with zeros later), 
	1. Suppose you produce only CPO. Then we replace prex_cpo_imp1 and prex_cpo_imp2 with export_pct if prex_cpo <. 

	2. If you produce CPO and other commodities - FOR WHICH PREX_ IS KNOWN - we can impute prex_cpo rather precisely (not exactly because other products than those 4 may be exported). 
		There will be a different formula depending on whether the firm produces only on other commo, 2, or 3 other commodities.  
	
	3. If you produce CPO and other commodities - FOR WHICH PREX_ OR NECESSARY INFO IS MISSING (so that we cannot compute prex_cpo) - there are two extreme options: 
		a) we are conservative and don't impute more than that, this is _imp2. 
		b) we use the firm level export share as an estimate for the commodity level export share, which is the same as assuming that all commodities produced 
			were exported in the same proportions., this is _imp1. 
	These options don't depend on whether the firm produces 1, 2, or 3 other commodities. 

Then repeat this for PKO. 
Regarding RPO and RPKO, these are firms that are more likely to export other products that are not under our radar, because they are bigger factories and because there are many different
processed palm oil products. 
So the imputation is likely to be even less robust, so we don't do it at all. 

*/
	**Some variable preparation first **
		rename avg_out_prex* prex*
		*For some reason some export_pct_imp are higher than 100. It is not clear whether they should be replaced with 100 or divided by 10 (or 100), so remove them. 
		replace export_pct_imp = . if export_pct_imp >100 | export_pct_imp <0 

		*Also, because of the avg over multioutput, some avg_out_prex are slightly higher than 100.
		global prex_vars prex_cpo prex_pko prex_rpo prex_rpko

		foreach var of varlist $prex_vars{
			replace `var' = 100 if `var'> 100 & `var' <. 
		}

		*also; this is not so likely that firms reported export shares of less than 1%, it is thus likely that these answers were expressed in fractional form. 
		foreach var of varlist $prex_vars{
			replace `var' = `var'*100 if `var' <1 & `var' >0 
		}

		replace export_pct_imp = export_pct_imp*100 if export_pct_imp<1& export_pct_imp>0

		*for export shares exactly equal to one, this is a bit less straightforward, so let us not change anything.
		foreach var of varlist $prex_vars{
			gen double `var'_imp1 = `var'
			gen double `var'_imp2 = `var'
		}
			
		/*
		browse $prex_vars export_pct export_pct_imp if prex_cpo_imp1 >=. & export_pct_imp>0 & export_pct_imp <. 


	**For CPO**
		1.*/ 
		global imp_prex_cpo prex_cpo_imp1 prex_cpo_imp2

		foreach var of varlist $imp_prex_cpo {
			replace `var' = export_pct_imp if /// 
			(`var'>=. & export_pct_imp <. ///
			& (valid_out_ton_cpo==1 | valid_out_val_cpo==1) ///
			& valid_out_ton_pko == 0 & valid_out_val_pko == 0 ///
			& valid_out_ton_rpo == 0 & valid_out_val_rpo == 0 ///
			& valid_out_ton_rpko == 0 & valid_out_val_rpko == 0 ///
				)
		}
		/* 
		This yields 655 real changes mad, among which 111 are not zeros. 
		For cpo vars we don't use imp variables just for the sake of code writing simplicity. But in the end the duplicates and outliers lines of cpo vars will not be taken into account anyway. 
		We condition on validity of qty OR value so that the aggregates on export shares can be calculated on as many information as possible. 
		We condition on invalidity of both qty and value at non-imp state because we want to be sure that there is no sign of the production of something else than CPO. 
		For instance, suppose you have a positive cpo qty (and you want your prex_cpo to be replaced by export_pct only if you produced only cpo) and an outlying pko qty. 
		Then it means you probably also produced pko. But if one looks at you out_ton_pko_imp vars, you look like you don't produce pko. 
		Or, your raw pko info is duplicated from your previous for some reason. It is likely that you are a producer of pko, but this might not appear in your imp vars. 
		
		2. 
		The choice of val and imp2 is because this is where there are the least missings, and also not the outliers. Repeating the command with ton and imp2 makes no new change. 
		*/	

		foreach var of varlist $imp_prex_cpo {
			global second_commo pko rpo rpko 
			foreach commo of global second_commo {
				local all_unproduced_commo : list global(second_commo) - local(commo)
				local unproduced_commo1 : word 1 of `all_unproduced_commo'
				local unproduced_commo2 : word 2 of `all_unproduced_commo'
				replace `var' = (export_pct_imp - (out_val_`commo'_imp2/(out_val_cpo_imp2 + out_val_`commo'_imp2))*prex_`commo')*(out_val_cpo_imp2+out_val_`commo'_imp2)/out_val_cpo_imp2 if /// 
				`var'>=. & export_pct_imp <. /// prex_cpo is missing and it is available in export_pct_imp 
				& prex_`commo' <. 								/// and the information on the prex_ of the other commo is available. 
				& out_val_cpo_imp2 <. 							 /// and there is a sign of production in cpo 
				& out_val_`commo'_imp2<.						 /// and there is a sign of production in the second commo 
				& out_val_`unproduced_commo1'_imp2 >=. & out_val_`unproduced_commo2'_imp2>=. 
				* and there is no sign of production for the two other commos. 	
				*di "`unproduced_commo2'"
			}
		}

		 * 11 real changes made for CPO (among which 5 are not zeros)
				*browse prex_cpo_imp1 if prex_cpo>=. & export_pct_imp >0 & export_pct_imp <. & prex_pko <. & out_val_cpo_imp2<. & out_val_pko_imp2<. 
				* As a sign that this is not so precise (may be there were other products exported?) this gives two prex_cpo_imp > 100. 
				replace prex_cpo_imp1 = 100 if prex_cpo_imp1> 100 & prex_cpo_imp1 <. 
				replace prex_cpo_imp2 = 100 if prex_cpo_imp2> 100 & prex_cpo_imp2 <. 	
				*Just for generality: 
				replace prex_cpo_imp1 = 0 if prex_cpo_imp1<0  
				replace prex_cpo_imp2 = 0 if prex_cpo_imp2<0  	
		/*

		Among these obs. that have a sign of cpo and pko and non missing prex_pko, there is no obs that has also sign of a production for other commo, so there is no need to go for thoses cases. 

		3. 
		*/
			replace prex_cpo_imp1 = export_pct_imp if prex_cpo >=. & export_pct_imp <. & ///
				(industry_code==15141 | industry_code== 10431 | industry_code== 31151) & ///
				(valid_out_ton_cpo==1 | valid_out_val_cpo==1) 	 & ///
				(valid_out_ton_pko == 1 | valid_out_val_pko == 1 | ///
				valid_out_ton_rpo == 1 | valid_out_val_rpo == 1  | ///
				valid_out_ton_rpko == 1 | valid_out_val_rpko == 1)	& /// 
				(prex_pko>=. | out_val_pko_imp2 >=.) & (prex_rpo>=. | out_val_rpo_imp2>=.) & (prex_rpko>=. | out_val_rpko_imp2>=.)	 

		/*
		1208 real changes among which 210 non zeros. 
		So here with this, we replace prex_cpo_imp1 with the export_pct in the cases that are not treated above, it is to say: 
		this is not case 1. because there is some sign of another commodity being produced 
		this is not case 2. because the prex_vars are missing. (last line)
		Also, we restrict this to firms that have a crude vegetable oil industry_code in order to limit the risk of multiple commodities and hence commo export share different than 
		total export share. 


	**For PKO**
		1.
		*/

		global imp_prex_pko prex_pko_imp1 prex_pko_imp2

		foreach var of varlist $imp_prex_pko {
			replace `var' = export_pct_imp if /// 
			(`var'>=. & export_pct_imp <. ///
			& (valid_out_ton_pko==1 | valid_out_val_pko==1) ///
			& valid_out_ton_cpo == 0 & valid_out_val_cpo == 0 ///
			& valid_out_ton_rpo == 0 & valid_out_val_rpo == 0 ///
			& valid_out_ton_rpko == 0 & valid_out_val_rpko == 0 ///
				)
		}
		/* 
		
		2. 
		*/	

		foreach var of varlist $imp_prex_pko {
			global second_commo cpo rpo rpko 
			foreach commo of global second_commo {
				local all_unproduced_commo : list global(second_commo) - local(commo)
				local unproduced_commo1 : word 1 of `all_unproduced_commo'
				local unproduced_commo2 : word 2 of `all_unproduced_commo'
				replace `var' = (export_pct_imp - (out_val_`commo'_imp2/(out_val_pko_imp2 + out_val_`commo'_imp2))*prex_`commo')*(out_val_pko_imp2+out_val_`commo'_imp2)/out_val_pko_imp2 if /// 
				`var'>=. & export_pct_imp <. /// prex_pko is missing and it is available in export_pct_imp 
				& prex_`commo' <. 								/// and the information on the prex_ of the other commo is available. 
				& out_val_pko_imp2 <. 							 /// and there is a sign of production in pko 
				& out_val_`commo'_imp2<.						 /// and there is a sign of production in the second commo 
				& out_val_`unproduced_commo1'_imp2 >=. & out_val_`unproduced_commo2'_imp2>=. 
				* and there is no sign of production for the two other commos. 	
				*di "`commo'"
			}
		}

		/*  38 real changes made when CPO is the other commo, among which 31 are non zeros. 
			Several of these obs. are negative on prex_pko_imp, showing that these cases are not so exact imputation. There were other products weighing in the firm level export_pct
			or this is the sign of some other measurement error. 
		*/
				replace prex_pko_imp1 = 0 if prex_pko_imp1<0 
				replace prex_pko_imp2 = 0 if prex_pko_imp2<0 
				replace prex_pko_imp1 = 100 if prex_pko_imp1> 100 & prex_pko_imp1 <. 
				replace prex_pko_imp2 = 100 if prex_pko_imp2> 100 & prex_pko_imp2 <. 		
					*browse prex_pko_imp1 prex_pko_imp2 if prex_pko>=. & export_pct_imp >0 & export_pct_imp <. & out_val_pko_imp2 <. & prex_rpo <. & out_val_rpo_imp2<. 


		/*Among these obs. that have a sign of pko and cpo and non missing prex_cpo or prex_rpo, there is no obs that has also sign of a production for other commo, 
		so there is no need to go for thoses cases. 

		3. 
		*/

			replace prex_pko_imp1 = export_pct_imp if prex_pko >=. & export_pct_imp <. & ///
				(industry_code==15141 | industry_code== 10431 | industry_code== 31151) & ///
				prex_cpo>=. & prex_rpo>=. & prex_rpko>=. 		 & /// 
				(valid_out_ton_pko==1 | valid_out_val_pko==1) 	 & ///
				(valid_out_ton_cpo == 1 | valid_out_val_cpo == 1 | ///
				valid_out_ton_rpo == 1 | valid_out_val_rpo == 1  | ///
				valid_out_ton_rpko == 1 | valid_out_val_rpko == 1)	& /// 
				(prex_cpo>=. | out_val_cpo_imp2 >=.) & (prex_rpo>=. | out_val_rpo_imp2>=.) & (prex_rpko>=. | out_val_rpko_imp2>=.)	


		/*

Let's just summarize what this section did regarding null prex and null export_pct. 
	- It replaced any missing prex with export_pct when the latter was null, following the argument that a firm may have not informed the prex for each commodity when it is 
	exporting nothing anyway, and just responded 0 for the total export share. 
	- It never replaced prex with export_pct when the former was positive and the latter null, assuming there is no reason for a firm to respond a positive value on a commo line if the export is null, 
	so the error has to come from export_pct... 
	- It never replaced prex with export_pct when the former was null and the latter positive bc it could be positive for other commo but null for one in particular and hence positive on average. 


***Regarding ZEROS***
			
There is a risk that firms answer 0 for either the total export share or commo export share while their outputs are actually exported but they don't know it. 
This would mostly be in cases where their outputs are transformed by another domestic stakeholder before being exported. 
To a smaller extent it may be in cases where their products are not transformed domestically but they simply ignore whether their clients export them or not. 
It could be that smaller firms have less information, and it could also be that its bigger mills that have less visibility if they tend to be selling to bigger refineries. 

Let's see what some tests can tell us on this risk. 

	To analyse this PREX variables, one can look at the mean prex_cpo by year and compare it to the aggregate. 
	But it is possible that firms which produce more also export more and that these firms are more or less represented in our sample than in the pop. for instance.  
	Therefore we should rather calculate the weighted (i.e. the effective) sample export share. 
	*/

	
	forvalues a = 1/2 {
		gen double exp_ton_cpo_imp`a' = prex_cpo_imp`a'*out_ton_cpo_imp`a'
		bys year: tabstat exp_ton_cpo_imp`a', statistics(sum mean p50) columns(variables) save
		matrix stats = r(StatTotal)
		global sample_imp`a'_exp_cpo = stats[1,1]
			tabstat out_ton_cpo_imp`a' if year == `y', statistics(sum mean p50) columns(variables) save
			matrix stats = r(StatTotal)
			global sample_imp`a'_ton_cpo_`y' = stats[1,1]

			tabstat 
			global sample_imp`a'_prex_cpo_`y' = 

		}
	}



	forvalues a = 1/2 {
		gen double exp_ton_cpo_imp`a' = prex_cpo_imp`a'*out_ton_cpo_imp`a'
		forvalues y = 1998/2015{
			tabstat exp_ton_cpo_imp`a' if year == `y', statistics(sum mean p50) columns(variables) save
			matrix stats = r(StatTotal)
			global sample_imp`a'_exp_cpo_`y' = stats[1,1]
			tabstat out_ton_cpo_imp`a' if year == `y', statistics(sum mean p50) columns(variables) save
			matrix stats = r(StatTotal)
			global sample_imp`a'_ton_cpo_`y' = stats[1,1]

			tabstat 
			global sample_imp`a'_prex_cpo_`y' = 

		}
	}


bys year: display $sample_imp1_prex_cpo
	gen onee = 1 
			tabstat onee, statistics(sum) columns(variables) save
			matrix stats = r(StatTotal)
			global sume = stats[1,1]

di $sample_imp1_exp_cpo_2015 

	bys year: egen double annual_sample

/*
Anyway, we

	** The export dummy can be used to identify true zeros. 
	But it is available only for years 2001, 2002, 2003 and 2005 
	And also we can impute 100% export share or something when answer is yes? 
	No, it is too uncertain, it could be any share, and it could still be 0 for cpo and positive for other commos. 

			Notes on this variable: 
			mention of "(by own or others)" from 2002 on, i.e. before 2002 the question is less clear. 
			2006 is weird: question is under another code in pdf: YTPREX instead of EKSPOR. but correctly named (EKSPOR06) in the excel file.
			It is always coded as: 1 = there is some export, 2 = there is no export from this establishment. 
	
	global imp_prex_vars prex_cpo_imp1 prex_cpo_imp2 prex_pko_imp1 prex_pko_imp2 prex_rpo_imp1 prex_rpo_imp2 prex_rpko_imp1 prex_rpko_imp2
	foreach var of varlist $imp_prex_vars{
		replace `var' = 0 if export_dummy_imp==2 
	}
	*/ 

	*And those who were zeros in 

	count if export_pct_imp==0 & export_dummy_imp==2
	browse export_pct_imp if export_dummy_imp<.

	


***********************************************************************************************************************************
/*
ANALYSE OF PREXXXXXXXXXXXXXXX

About the zeros. 
Let us create temporarily imp10 and imp20 prex vars which are imp1 and imp2 with zeros rrmoved (except those confirmed by export_dummy=2 in years 2001-03 and 2005)
*/
gen double prex_cpo_imp10 = prex_cpo_imp1
gen double prex_cpo_imp20 = prex_cpo_imp2
gen double export_pct_imp0 = export_pct_imp

replace prex_cpo_imp10 = . if prex_cpo_imp1 ==0 
replace prex_cpo_imp20 = . if prex_cpo_imp2 ==0 
replace export_pct_imp0 = . if export_pct_imp ==0 

replace prex_cpo_imp10 = 0 if export_dummy_imp==2
replace prex_cpo_imp20 = 0 if export_dummy_imp==2
replace export_pct_imp0 = 0 if export_dummy_imp==2

bys year: sum export_pct_imp export_pct_imp0 prex_cpo prex_cpo_imp1 prex_cpo_imp2 prex_cpo_imp10 prex_cpo_imp20
bys year: sum export_pct_imp export_pct_imp0 prex_cpo prex_cpo_imp1 prex_cpo_imp2 prex_cpo_imp10 prex_cpo_imp20 if cpo_price_imp1<.

count if (prex_cpo >0 & prex_cpo<.)|(prex_pko>0 & prex_pko<.) & export_pct_imp==0 

browse firm_id year industry_code cpo_price_imp1 prex_cpo export_pct export_pct_imp export_dummy_imp prex_cpo_imp1 ///
prex_cpo_imp2 prex_pko_imp1 prex_pko_imp2 prex_rpo_imp1 prex_rpo_imp2 prex_rpko_imp1 prex_rpko_imp2 if prex_cpo_imp2 >0 & prex_cpo_imp2<. & export_pct_imp==0 
/*
If we run this command after the whole code above, what do we see: 

	- In years 2015-13, prex_cpo had a lot of missings and not an abnormal share of zeros, so removing zeros does not changes the mean so much. 
		In export_pct however, removing zeros removes lots of obs. and changes the mean a lot. Because the national export share
		is around 75% in these years, there is a suggestion for an abnormally high number of zeros. 
	- From 2012-07 and 2004 nthe same phenomenon appears for prex_cpo vars. 
	- In 2006, there were no prex per commo in the questionnaire, so no prex_cpo. Hence the big difference between imp1 and imp2, 
		and hence the absence of difference with imp10 and imp20: obs. in imp1 and imp2 already have no zeros by construction. 
	- In 2005-03-02-01, there were no info on export_pct, and only on export_dummy. Hence the two first lines. 
		Interestingly, for the first time, even imp10 and 20 are low (15%), meaning that accounting for zeros confirmed by the dummy 
		can decrease the prex_cpo to the level it has when accounting for all zeros, hence suggesting that zeros are, at least this year, 
		not abnormally numerous. But the level it reaches is much lower than aggregated export share from indexmundi. 

Questions are: should we look at this prex_cpo as a CPO export share: the producers of CPO who responded to this question in the questionaire export it directly. 
It is mentioned "by own or others" but they can't really know what proportion of the CPO they sell to a refinery is exported in the end. 
So the positive prex_cpo we observe are only the ones corresponding to direct export. So this should be compared with national export of CPO. 

Or we take the variable as it is described in the census, and it is the percentage directly or indirectly exported. And then we should compare it to total palm oil exports. 
And then it is very much biased. 

The meaning we chose to give to this variable defines what we use as an instrument: export taxes are not the same on palm oil as a whole or on crude. 
--> See Riffin. 
For the first acceptation: those with prex_cpo = 0 either truely don't export, or sell to a refinery that export refined palm oil. In either case, the product is not subjected to CPO export tariff.  


*/



gen exp_cpo = (prex_cpo/100)*out_ton_cpo
by year, sort: egen double annual_total_exp_cpo = total(exp_cpo) 
*by year, sort: sum annual_total_exp_cpo
by year, sort: egen double annual_total_ton_cpo = total(out_ton_cpo) 
by year, sort: sum annual_total_ton_cpo
gen double annual_export_share = annual_total_exp_cpo/annual_total_ton_cpo
by year, sort: sum annual_export_share
codebook firm_id if year ==2015 & avg_out_prex_cpo_imp>0 & avg_out_prex_cpo_imp<.




*/
inspect export_dummy_imp
sum export_dummy_imp
sort firm_id year
count if (prex_cpo_imp1 ==0 | prex_cpo_imp1 >=.) & export_dummy_imp==1 
browse firm_id year prex_cpo prex_pko export_pct_imp prex_cpo_imp1 prex_cpo_imp2 if (prex_cpo_imp1 ==0 | prex_cpo_imp1 >=.) & export_dummy_imp==1 


bys year: sum prex_cpo prex_cpo_imp1 prex_cpo_imp2


gen fl_prex_cpo_nonmiss15 = (prex_cpo_imp1 <. & year == 2015)
count if fl_prex_cpo_nonmiss15 >=.
bys firm_id: egen export_in_15 = max(fl_prex_cpo_nonmiss15)
browse firm_id year cpo_price_imp1 cpo_price_imp2 prex_cpo prex_pko export_pct_imp prex_cpo_imp1 prex_cpo_imp2 if export_in_15==1

sort firm_id year

bys year: sum prex_cpo_imp1 prex_cpo_imp2 if (cpo_price_imp1 <. | cpo_price_imp2<.) & prex_cpo_imp2>0 

count if prex_cpo_imp1!= prex_cpo_imp2 & industry_code!=15141 & industry_code!= 10431 & industry_code!= 31151









/*


*/
count if out_ton_pko_imp1 <. & out_val_pko_imp1>=. 

count if out_val_pko_imp1 <. & out_ton_pko_imp1>=. 

inspect out_val_pko_imp1 out_ton_pko_imp1



by year, sort: inspect avg_out_prex_cpo
by year, sort: inspect avg_out_prex_rpo
by year, sort: inspect export_pct export_pct_imp 
if district <.
browse firm_id year avg_out_prex_cpo export_pct export_pct_imp if export_pct <. 
sum export_pct_imp
sum avg_out_prex_cpo

***OK; THIS TAKING EXPORT_PCT INSTEAD OF OUT_PREX WHEN IT IS MISSING: WE SHOULD NOT DO IT WHEN REFINING ACTIVITY IS TOO MUCH MAY BE; AND RATHER USE IT TO REPLACE AVG_OUT_PREX_RPO

gen double avg_out_prex_cpo_imp = avg_out_prex_cpo 
replace avg_out_prex_cpo_imp = export_pct_imp if avg_out_prex_cpo_imp>=. 
by year, sort: inspect avg_out_prex_cpo avg_out_prex_cpo_imp 
*by year, sort: inspect avg_out_prex_cpo avg_out_prex_cpo_imp
/* This makes available many more obs. Is it ok? 
The only pb I see is that when both information are available they are not necessarily equal, probably meaning that export_pct is just an average of all point values avg_out_prex_cpo. 
In the questionnaires, PRPREX is the cell at the bottom of the column on percentage of goods exported, meaning it is the average pct of all goods exported. 
This would still be an estimation, may be imprecise likely worth it as it brings so many obs. ? 

How realistic is it? 
*/ 
gen exp_cpo = (prex_cpo/100)*out_ton_cpo
by year, sort: egen double annual_total_exp_cpo = total(exp_cpo) 
by year, sort: sum annual_total_exp_cpo
by year, sort: egen double annual_total_ton_cpo = total(out_ton_cpo) 
by year, sort: sum annual_total_ton_cpo
gen double annual_export_share = annual_total_exp_cpo/annual_total_ton_cpo
by year, sort: sum annual_export_share
codebook firm_id if year ==2015 & avg_out_prex_cpo_imp>0 & avg_out_prex_cpo_imp<.
// seulement 60 mill exportent en 2015... 

count if out_ton_cpo<. & 
/*
So, the export share is abnormally to low, so is the annual_total_exp_cpo. The quantity is pretty normal (i.e. proportional to the total population represenation.)

Is it a problem? 
--> YES, because it means our exp_cpo variables is downward mismeasured. In particular, it is likely to often zero. One explanation behind 
that is the mills selling to a refinery that then exports, but they don't know that, or what proportion will be exported of what they sell to the refinery, so they answer 0. 
The consequence will not necessarily be a bias in the first stage, because these firms - responding zero export pct while their outputs were actually exported 
and the prices they got for it were hence actually (more) exposed to international dynamics - don't have systematically higher or lower prices. 
However, the coefficient of the first-stage will be very imprecisly measured. 

Moreover, what does it mean exposure (export_pct) is zero? Well if we are in the DID specification this is clear: firms that don't export are not exposed to a change in export tax for instance
In the panel case, ´zero exposure implies that national price is not impacted by international price... 
*/






********************************************************************************************************************************************************************************************************
*** Are you a mill or a refinery doing also a bit of CPO? *** 
********************************************************************************************************************************************************************************************************




/* 
	So there are firms that produce only crude (CPO/PKO), only refined (RPO/RPKO) or some of both. As soon as they produce SOME crude, we should consider them as a mill, in that sense 
	that they must be near some plantations. 
	So we separate those who produce only refined, call them refineries, and those who produce a positive amount of crude. 
*/
gen only_refine = (((valid_out_ton_rpo ==1 | valid_out_val_rpo==1 | valid_out_ton_rpko==1 | valid_out_val_rpko ==1) | ///
				(industry_code ==10432 & revenue_total<.) | /// 
				(industry_code==15144 & revenue_total<. ) | ///
				(industry_code==31154 & revenue_total<. )) & ///
				valid_out_ton_cpo==0 & valid_out_val_cpo==0& ///
				valid_out_ton_pko==0 & valid_out_val_pko==0)
gen some_refine = ((valid_out_ton_rpo ==1 | valid_out_val_rpo==1 | valid_out_ton_rpko==1 | valid_out_val_rpko ==1) | ///
				(industry_code ==10432 & revenue_total<.) | /// 
				(industry_code==15144 & revenue_total<. ) | ///
				(industry_code==31154 & revenue_total<. )) 

bys year: inspect some_refine
*So if it is those wo do only refine, they are around 35 per year, and when it is some refine, it is around 80 per year. This is before cleaning.  

browse firm_id year in_ton_ffb in_val_ffb avg_out_prex_cpo avg_out_prex_pko avg_out_prex_rpo avg_out_prex_rpko export_pct revenue_total revenue_total_imp3 if only_refine==1
/*Among these refineries, some have ffb inputs: well, they may be big factories that do all the processing from ffb to rpo. But let's consider them as refineries: they have no crude output. 
so we could not compare them in terms of price. 


*/
inspect valid_out_ton_cpo

*Explore this all.
count if (valid_out_ton_rpo ==1 | valid_out_val_rpo==1 | valid_out_ton_rpko==1 | valid_out_val_rpko ==1) & ((valid_out_ton_cpo ==1 & valid_out_val_cpo==1) | (valid_out_ton_pko==1 & valid_out_val_pko==1))
*So among the valid CPO obs. there are 211 obs. that have some signal of refined output
*and among the obs. with either valid CPO or valid PKO, 434 have a signal of refining activity. 
count if some_refine==1
*1439

/*Now, we would like to create a variable that summarizes the importance of refining activity for each firm. Indeed, when avg_out_prex_ is missing, 
we would like to replace it, as an estimate, with export_pct. We can either replace it as it is everywhere. But if the avg_out_prex of other commo is very different and these commo weigh 
much in the total revenue, then this is a bad estimate. Or we can only replace it where we are more confident in the importance of the commo in the total. 
Ok, so there may be many cases.
Among obs. that have avg_out_prex_cpo missing and export_pct non missing. 
	1. Suppose you produce only CPO. Then we replace automatically avg_out_prex by export_pct. 
	2. If you produce CPO and one other commodity. Then either you have missing avg_out_exp_other or not. 
		a) If it is not missing, then we can calculate exactly the avg_out_prex_missing. 
		b) if it is missing, then we can either consider that we shouldn't impute anything, or consider it was not different than avg_out_prex_cpo and then replace both by export_pct. 
			Another way to proceed would be to replace only when the share of cpo is bigger than a certain amount, but this is intermediate rationale. 
	3. If you produce CPO and two or three other commo. Same rationale: 
		a) We know all the other avg_out_prex_ and only then replace avg_out_prex_cpo by its true value, 
		b) we don't know all of them and we never replace by export_pct (in _imp1) and always replace by export_pct (in _imp2)



*/
** Before doing this code we should clean refine_vars. 
out_stat_otls out_ton_rpo 
out_stat_otls out_val_rpo 
out_stat_otls out_ton_rpko 
out_stat_otls out_val_rpko 

global refine_vars out_ton_rpo out_val_rpo out_ton_rpko out_val_rpko
foreach var of varlist $refine_vars{
	tabstat `var', statistics(median) columns(variables) save
	matrix stats = r(StatTotal)
	local med_`var' = stats[1,1]
	replace `var'= `med_`var'' if otl_`var'==1
}
/*
*when a refine_vars is statistical otl, it is replaced by the median of the var, in order to not minimize too much the share of refining activity for these obs. 
But this is not fair because crude_vars are treated as 0 when they are missing because were otls. Yes but this is not a problem because then we won't consider these obs. anymore anyway
while those with a valid crude_var, we don't want to underestimate (too much) their implication in refining activities. Well it is still a bit of a problem because for obs. 
that are missing on pko for instance because were otls, and have valid cpo, then the refine_share is biased upwards: the denominator should be higher. 
We can still check whether one of the crude vars was flaged an otl. 
*/

gen refine_val_share = (cond(mi(out_val_rpo),0,out_val_rpo) + cond(mi(out_val_rpko),0,out_val_rpko))/ /// 
	(cond(mi(out_val_cpo),0,out_val_cpo)+cond(mi(out_val_pko),0,out_val_pko) + /// 
	cond(mi(out_val_rpo),0,out_val_rpo)+cond(mi(out_val_rpko),0,out_val_rpko)) ///
	if valid_out_val_cpo==1 | valid_out_val_pko==1 | valid_out_val_rpo==1 | valid_out_val_rpko==1 

 browse firm_id year out_val_cpo out_val_pko out_val_rpo out_val_rpko refine_val_share if refine_val_share <. & refine_val_share >.5

gen refine_ton_share = (cond(mi(out_ton_rpo),0,out_ton_rpo) + cond(mi(out_ton_rpko),0,out_ton_rpko))/(cond(mi(out_ton_cpo),0,out_ton_cpo)+cond(mi(out_ton_pko),0,out_ton_pko) /// 
	+cond(mi(out_ton_rpo),0,out_ton_rpo)+cond(mi(out_ton_rpko),0,out_ton_rpko)) ///
if valid_out_ton_cpo==1 | valid_out_ton_pko==1 | valid_out_ton_rpo==1 | valid_out_ton_rpko==1 

 browse if refine_val_share == 1 & year == 2014
 *In 2014 we would have at least 66 pure refineries (producing no crude). 
 count if refine_val_share==1 & year == 2014 & in_val_ffb >0 & in_val_ffb<.

*But many still have some ffb as input, suggesting they still have an impact on plantations...




















	/*	
	L'idee c'est que ces obs. nous niquent un peu tout statistiquement, donc on les garde si vraiment elles sont coherentes avec toute l'info qu'on a. 	

	output ET value sont otls. LË†, la question se pose de savori si on valide l'obs. si le prix est du coup normal ? 
		a priori non, on ne la valide pas, c'est bien pour ca qu'on a fait des tests sur les composants du prix (quantite et valeur) et pas simplement retire les 
		valeurs extrenes du ratio calcule. 

* 1. Est-ce que la production de PKO est plus grande que celle de CPO. (reste Ã  dÃ©finir le ratio...), le calculer pour chaque obs et voir sa distribution. 
* 2. Est-ce que pour celles qui ont un bon ratio, la production des DEUX est encore anormalement faible ? 
* 3. Est-ce que le prix est anormal ? 
* 4. Est-ce que lorsque le prix est normal, la valeur dans l'absolue est anormale ? 
* 5. Est-ce que les valeurs du mÃªme individu sont trÃ¨s diffÃ©rentes les autres annÃ©es ? 
* 6. Est-ce que le ratio avec l'input est anormal ? 
* 7. Est-ce que lorsque le ratio output/input est normal, l'input ET l'output dans l'absolu sont anormaux ? 

***CLEANING IN_TOT_QTY_TON***

*Bunch weight
*23-27 kg
*Fruit/bunch
*60-65 %
*Oil/bunch
*21-23 %
*Kernel/bunch
*5-7 %
*Mesocarp/bunch
*44-46 %
*Mesocarp/fruit
*71-76 %
*Kernel/fruit
*21-22
*Shell/fruit
*10-11
*However, such high yields are rarely achieved in practice because climatic conditions are usually less than ideal. Rainfall is erratic in Central and West Africa and hence the tree suffer water-related stresses. 


*Malaysia: 33.32% the oil in bunch content in ripe bunch

*from http://jopr.mpob.gov.my/wp-content/uploads/2013/07/joprv11n1-p21.pdf 
*--> Oil/bunch (of mean weight of 13.8kg) btw 13% and 43% with mean 25%
*for Kernel it is rather 7%

*https://www.researchgate.net/publication/319298126_Characteristics_of_Fresh_Fruit_Bunch_Yield_and_the_Physicochemical_Qualities_of_Palm_Oil_during_Storage_in_North_Sumatra_Indonesia
 *the  ratio  of  fruit  to  bunch(67.7%), bref voir le screenshot. 
 



	**Missing and zeros. 
	*browse if in_tot_qty_ton_ffb ==0|in_tot_qty_ton_ffb>=.

	*Comme on ne se sert du input en l'occurrence que pour avoir un ordre de grandeur pour Ã©valuer les ouputs, on va prendre une conversion des kelapa sawit segar: 
	*According to Wikipedia Tenera is the main species used. So use the fruit/bunch ratio of 66.5% 
	

*count if flag_multiinput_ffb==1 & (in_tot_qty_ton_ffb==0|in_tot_qty_ton_ffb>=.)
*And there remains 331 fantom obs. of which 280 are zeros. May be they will explain the zero/missing values of output. 
*But in any case either both input and output are zero and it makes sense but we can't do anything with it, or output are missing and we can't do anything.  
*let's just drop them for the time of cleaning the input, but then keep them when merging with O. 
*drop if in_tot_qty_ton_ffb==0|in_tot_qty_ton_ffb>=.
*We start seeing something under 1Mt

***Are the agregated inputs producing otls?***
gen flag_multiinput = (flag_multiinput_ffb==1|flag_multiinput_kss==1)
label variable flag_multiinput "input was agregated"


