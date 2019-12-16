***MATCHING MILL BTW IBS AND ROBERT***
*We first match between IBS_1998_cleaned and dat.firms_reduced, i.e. we add the desa information to the mill dataset. 

*Destring desa_code variables in dat.firms_reduced
use "C:\Users\guyv\ownCloud\opalval\build\input\dat.firms_reduced.dta", clear
destring desa_id, replace 
destring desa_code_2000, replace
*But desa_id contains non numeric caracters so destring does not work... 

*generate in the using dataset the key for matching
egen firm_id_year_str = concat(firm_id year)
drop firm_id year
save "C:\Users\guyv\ownCloud\opalval\build\input\dat.firms_reduced2.dta", replace
*firm_id_year_str identifies uniquely each observation of dat.firms_reduced

*generate in the master dataset the key for matching
use "C:\Users\guyv\ownCloud\opalval\build\input\IBS_1998_cleaned.dta", clear
egen firm_id_year_str = concat(firm_id year)
*firm_id_year_str identifies uniquely each observation of IBS_1998_cleaned


*merge
merge 1:1 firm_id_year_str using C:\Users\guyv\ownCloud\opalval\build\input\dat.firms_reduced2.dta, generate(merge_desacode)

codebook merge_desacode

drop if merge_desacode == 2

order desa_id desa_code_2000, after(industry_code)


*now merge this IBS mills-and-their-desacodes dataset with Robert's mill dataset with desa_code corresponding to the polygons encompassing the mills' point coordinates. 

merge m:m desa_code_2000 using C:\Users\guyv\ownCloud\opalval\build\input\heilmayr_desa.dta, generate(merge_heilmayr)

*so now the merge is not so good, let's wait to have desa_codes for all years. 

save "C:\Users\guyv\ownCloud\opalval\build\input\IBS_1998_geo.dta", replace



use "C:\Users\guyv\ownCloud\opalval\build\input\heilmayr_desa.dta", clear

use "C:\Users\guyv\ownCloud\opalval\build\input\IBS_1998_geo.dta", clear
