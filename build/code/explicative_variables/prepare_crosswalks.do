***** PREPARE RAW CROSSWALKS TO USABLE FORM IN cleaning_IBS.do TO ADD DISTRICT AND VILLAGE NAMES. ************

**** prepare district crosswalk 
import delimited "$base_path_wd\build\input\mill_geolocalization\District-Proliferation-Crosswalk_complete.csv", clear 
reshape long bps_ name_ , i(v1) j(year)
egen year_prov_distr = concat(year bps_)
duplicates drop year_prov_distr, force
save "$base_path_wd\build\input\mill_geolocalization\province_district_code_names_93_2016.dta", replace 
********************************************************************************************************


**** prepare desa crosswalk 
import delimited "$base_path_wd\build\input\mill_geolocalization\desa_crosswalk_1998_2014.csv", clear 
*not all id variables are of the same type 
forvalues y = 1998/2014{
	destring id`y', replace force 
}
reshape long id nm, i(v1) j(year)
tostring id, generate(desa_id)
egen year_desa_id = concat(year desa_id)
duplicates drop year_desa_id, force
save "$base_path_wd\build\input\mill_geolocalization\desa_code_names_98_2014.dta", replace 
