*¨Prepare most recent version of UML. This version has establishment year variable, est_year, taking values earlier than 2004. 
* Such values come from different sources: "We have found some establishment dates going back to the 80s and up to 2019 based on satellite imagery, 
* journal articles, company reports, mill installation contractor websites, and government websites"

import excel  "$base_path_wd\build\input\mills_estyear_clean.xlsx", firstrow clear 
rename latitude lat
rename longitude lon

rename mill_id trase_code

* correct the one duplicate case on coordinates with information from mills_20200129.xlsx
replace lat = 1.078	 if uml_id == "PO1000004435"
replace lon = 100.261 if uml_id == "PO1000004435"
duplicates list lat lon 

* add the guys that are not here but were in traseMills_capEstyear.xlsx though.  
merge 1:1 trase_code using "$base_path_wd\build\input\mill_geolocalization\traseMills_capEstyear_selected.dta", generate(_merge_ibs) 


* round coordinates
replace lat = round(lat, 0.001) 
replace lon = round(lon, 0.001) 
duplicates list lat lon 


save "$base_path_wd\build\input\mills_estyear_clean.dta", replace 


/*
use "$base_path_wd\build\input\mill_geolocalization\traseMills_capEstyear_selected.dta", clear
browse if trase_code == "M-01200"

parent_co	mill_name	lat	lon
PT Pelita Agung Agrindustri	PT. Pelita Agung Agro Industri	1.427	101.189
*/


* read most recent version of UML, that does not have est_year variable.
use "$base_path_wd\build\input\mill_geolocalization\mills_20200129.dta", clear 

* add the est_year variable 
merge 1:1 trase_code using "$base_path_wd\build\input\mills_estyear_clean.dta", nogenerate keepusing(est_year) 

* add the min_year variable from IBS 
merge 1:1 trase_code using "$base_path_wd\build\input\IBS_UML_cs.dta", nogenerate keepusing(firm_id year min_year max_year)

gen est_year_imp = est_year

*** MILLS WITH DOUPTFUL ESTABLISHMENT YEAR

** mills that appeared in IBS prior to UML establishment year 
count if !mi(est_year) & min_year < est_year
* 183 

** est_year = 2004 
count if est_year == 2004
* 234
count if est_year == 2004 & !mi(min_year)
* 199
count if est_year == 2004 & min_year == 2004
* 13
count if est_year == 2004 & min_year < 2004
* 100

/*  
234 UML mills have an establishment year in 2004; Out of which, 199 are matched with IBS; out of which 13 appear "indeed" in IBS in 2004 for the first time, 
100 appear before 2004, and 86 after 2004. 

183 mills have an establishment year that is questioned by their first year of appearance in IBS (min_year). 100 of these are mills with 
establishment year in 2004. 
*/

* we deem that the earliest year of est_year and min_year is the best approximation of the true establishment year.  
replace est_year_imp = min_year if !mi(est_year_imp) & min_year < est_year_imp

* this replaced the 100 est_year = 2004 cases, plus another 83 cases of mills appearing in IBS prior to UML est_year. 


*** MILLS WITHOUT ESTABLISHMENT YEAR 
count if mi(est_year) 
* 337  
count if mi(est_year) & !mi(min_year)
* 84
count if mi(est_year) & !mi(min_year) & min_year ==2004
* 4
count if mi(est_year) & !mi(min_year) & min_year > 2004
* 49

/* 
So 337 UML mills don't have an est_year. We approximate their establishment years with the first years of appearance in IBS. 
Out of 337 mills with missing establishment years, 84 can be approximated this way, of which 49 with a first year in IBS later than 2004, 4 in 2004
and 31 before 2004. 
*/

replace est_year_imp = min_year if mi(est_year_imp) & !mi(min_year)

save "$base_path_wd\build\output\UML_valentin_imputed_est_year.dta", replace 




*** UPDATE EST_YEAR variable in IBS_UML DATA SETS. 
use "$base_path_wd\build\input\IBS_UML_panel.dta", clear

merge m:1 trase_code using "$base_path_wd\build\output\UML_valentin_imputed_est_year.dta", nogenerate keepusing(est_year_imp) 

order est_year_imp, after(est_year)
sort firm_id year 
save "$base_path_wd\build\input\IBS_UML_panel_est_year.dta", replace


/*
codebook firm_id if !mi(lat)
codebook firm_id if !mi(trase_code)
codebook firm_id if !mi(mill_name)
*/

/*
export excel firm_id year trase_code uml_id mill_name parent_co lat lon district_name kec_name village_name /// 
min_year est_year est_year_imp startYear max_year active industry_code ///
ffb_price_imp1 ffb_price_imp2 in_ton_ffb in_ton_ffb_imp1 in_ton_ffb_imp2 in_val_ffb in_val_ffb_imp1 in_val_ffb_imp2 flag_multiinput_ffb ///
in_dom_cpo_price_imp1 in_dom_cpo_price_imp2 in_dom_ton_cpo in_dom_ton_cpo_imp1 in_dom_ton_cpo_imp2 in_dom_val_cpo in_dom_val_cpo_imp1 in_dom_val_cpo_imp2 ///
in_imp_cpo_price_imp1 in_imp_cpo_price_imp2 in_imp_ton_cpo in_imp_ton_cpo_imp1 in_imp_ton_cpo_imp2 in_imp_val_cpo in_imp_val_cpo_imp1 in_imp_val_cpo_imp2 ///
in_tot_cpo_price_imp1 in_tot_cpo_price_imp2 in_tot_ton_cpo in_tot_ton_cpo_imp1 in_tot_ton_cpo_imp2 in_tot_val_cpo in_tot_val_cpo_imp1 in_tot_val_cpo_imp2 ///
cpo_price_imp1 cpo_price_imp2 out_ton_cpo out_ton_cpo_imp1 out_ton_cpo_imp2 out_val_cpo out_val_cpo_imp1 out_val_cpo_imp2 prex_cpo prex_cpo_imp1 prex_cpo_imp2 out_cpo ///
pko_price_imp1 pko_price_imp2 out_ton_pko out_ton_pko_imp1 out_ton_pko_imp2 out_val_pko out_val_pko_imp1 out_val_pko_imp2 prex_pko prex_pko_imp1 prex_pko_imp2 out_pko ///
out_ton_rpo out_ton_rpo_imp1 out_ton_rpo_imp2 out_val_rpo out_val_rpo_imp1 out_val_rpo_imp2 prex_rpo prex_rpo_imp1 prex_rpo_imp2 out_rpo tag_multioutput_rpo ///
out_ton_rpko out_ton_rpko_imp1 out_ton_rpko_imp2 out_val_rpko out_val_rpko_imp1 out_val_rpko_imp2 prex_rpko prex_rpko_imp1 prex_rpko_imp2 out_rpko ///
EKSPOR export_pct export_pct_imp revenue_total pct_own_cent_gov_imp pct_own_loc_gov_imp pct_own_nat_priv_imp pct_own_for_imp workers_total_imp3 ///
using "$base_path_wd\build\output\IBS_UML_panel_est_year.xlsx", firstrow(variables) replace 
*/

* save a cross-sectional selected version 
keep if !mi(lat)
keep firm_id year trase_code uml_id mill_name parent_co lat lon island_factor island_name district_name kec_name village_name /// 
min_year est_year est_year_imp startYear max_year active industry_code ///
avg_in_tot_ton_cpo_imp2 last_in_tot_ton_cpo_imp2 avg_in_ton_ffb_imp2 last_in_ton_ffb_imp2 avg_ffb_price_imp2 ///
avg_out_ton_cpo_imp2 last_out_ton_cpo_imp2 avg_cpo_price_imp1 avg_cpo_price_imp2

sort firm_id year 
duplicates drop firm_id, force
sort firm_id
save "$base_path_wd\build\input\IBS_UML_cs_est_year.dta", replace
