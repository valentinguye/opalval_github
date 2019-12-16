/* Eventhough IBS_1998_cleaned is updated with desa_code_2000 in cleaning_IBS.do, mode_fl occurs more when 
computed in IBS_desa2000 and then merged to IBS_1998_cleaned than when computed in the latter directly (491 vs 458).
Thus it is more conservative to compile the flag in IBS_desa2000 and to then export it to IBS_1998_cleaned rather than compiling it directly there. 
It results in 902 mills being deemed valid for polygon matching with Heilmayr data (vs 911)
*/
use "C:\Users\guyv\ownCloud\opalval\build\input\mill_geolocalization\IBS_desa2000.dta", clear

bys firm_id (year): egen desa_code_2000_mode = mode(desa_code_2000)

g mode_fl = 1 if desa_code_2000 != desa_code_2000_mode & !mi(desa_code_2000) & !mi(desa_code_2000_mode)
g nmode_fl = 1 if mode_fl != 1 & !mi(desa_code_2000) & !mi(desa_code_2000_mode)

bys firm_id (year): egen mode_fl_fi_total = total(mode_fl)
bys firm_id (year): egen nmode_fl_fi_total = total(nmode_fl)

g mode_nreplace = 1 if mode_fl_fi_total > 1

g village_code_2000_md = desa_code_2000_mode

g firm_village_fl_md = 1 if mode_nreplace == 1

drop desa_code_2000_mode

drop nmode_fl mode_fl_fi_total nmode_fl_fi_total mode_nreplace

drop desa_code desa_name kab_code ///
kab_name kec_code kec_name prov_code prov_name kel_name desa_id desa_code_2000 village_code_2000_md

save "C:\Users\guyv\ownCloud\opalval\build\temp\mill_geolocalization\IBS_desa2000_modefl.dta", replace

*************************************************************************************************

use "C:\Users\guyv\ownCloud\opalval\build\output\IBS_1998_cleaned.dta", clear
*we work on a sample of IBS that is pre 2011 bc we don't have the desa_id variable for the years after.
drop if year > 2010 

g valid_desa_id = (!mi(desa_id) & string_fl != 1 & mode_fl != 1)
codebook firm_id if valid_desa_id == 1

by firm_id: egen most_recent_vld = max(year) if valid_desa_id == 1 

g polygon_4match = (valid_desa_id == 1 & year == most_recent_vld)

keep if polygon_4match == 1 

destring desa_id, replace

keep firm_id year desa_id 


merge 1:1 firm_id year using "C:\Users\guyv\ownCloud\opalval\build\temp\mill_geolocalization\IBS_desa2000_modefl.dta", generate(merge_modefl) keepusing(mode_fl)
* toutes les not matched from master sont les obs. 1998 et 1998. 
drop if merge_modefl == 2

merge 1:1 firm_id year using "C:\Users\guyv\ownCloud\opalval\build\temp\mill_geolocalization\desa_fl.dta", generate(merge_stringfl) keepusing(desa_fl)
*idem, desa_fl n'a des obs qu'entre 2000 et 2010. 
drop if merge_stringfl == 2 
rename desa_fl string_fl

/*flag the year to use for a valid polygon for each mill. 
It should be non missing, the smallest possible (hence the most recent), and valid (i.e. not being flaged)*/
sort firm_id year 



* 902 mills to find with their desa_id 

save "C:\Users\guyv\ownCloud\opalval\build\temp\mill_geolocalization\IBSmills_valid_desa.dta", replace

