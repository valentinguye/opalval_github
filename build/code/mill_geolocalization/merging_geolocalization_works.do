/*
This script merges together the spreadsheets used for manual geolocalization. 
Then, it makes manual modifications to resolve conflicts (mill name or coordinates duplicates).

It needs: 

"noto_done.xls", "Sheet1"
"mills_to_georeference_pre2011_done.xlsx", "Mills to georef"
"mills_to_georeference_post2010_done.xlsx", "mills to georef"
"traseMills_capEstyear.xlsx", "traseMills_capEstyear"
"IBS_PO_98_15_cleaned.dta"

*/


*Put everything back together with the coordinates.

*** oto is already in .dta from R file, it was not processed in an excel format. 

*** for noto, pre2011 and post2010

/* We want to check for each subset that there is only one mill_name and one coordinate value within firm_id 

Duplicated mill_name and coordinate value across firm_id will be checked later, once they are all together.
*/

** noto
import excel "$base_path_wd\build\input\mill_geolocalization\noto_done.xls", sheet("Sheet1") firstrow clear

* also, in case the excel sheet produces new empty lines: 
drop if mi(firm_id)
sort firm_id year 

*drop those that could not be geolocalized (remember that some obs. - in green in excel basically - have coordinates but no names
* and others have names from directories but no coordinates could be found. We want to keep both kinds.)
drop if mi(lat) & mi(mill_name)

*check that there is only one mill_name and one coordinate value within firm_id 
by firm_id mill_name, sort: gen nvals = (_n ==1)
by firm_id: replace nvals = sum(nvals)
count if nvals != 1
drop nvals
by firm_id lat, sort: gen nvals = (_n ==1)
by firm_id: replace nvals = sum(nvals)
count if nvals != 1
drop nvals

sort firm_id year 
duplicates drop firm_id, force

/* 
duplicates list mill_name
duplicates list lat lon
check duplicated mill_name and coordinate value across firm_id (4 instances here, which are the 4 cases 
of firm_id that need to be changed because they are found to be the mill as another firm_id)
*/

save "$base_path_wd\build\input\mill_geolocalization\noto_done.dta", replace 
*that's 102 mills with coordinates + 1 (AGROINDO INDAH PERKASA) with name but no coordinates. 

** pre2011
import excel "$base_path_wd\build\input\mill_geolocalization\mills_to_georeference_pre2011_done.xlsx", sheet("Mills to georef") firstrow clear
rename latitude lat
rename longitude lon
drop if mi(firm_id)

*drop those that could not be geolocalized (remember that some obs. - in green in excel basically - have coordinates but no names
* and others have names from directories but no coordinates could be found. We want to keep both kinds.)
drop if mi(lat) & mi(mill_name)
* it is 123 mills (485 obs.) that could not be geolocalized. 

*check that there is only one mill_name and one coordinate value within firm_id 
by firm_id mill_name, sort: gen nvals = (_n ==1)
by firm_id: replace nvals = sum(nvals)
by firm_id: replace nvals = nvals[_N]
count if nvals != 1
drop nvals
by firm_id lat, sort: gen nvals = (_n ==1)
by firm_id: replace nvals = sum(nvals)
count if nvals != 1
drop nvals

sort firm_id year 
duplicates drop firm_id, force

/* 
duplicates list mill_name
duplicates list lat lon
across firm_id there is one duplicated mill_name (BONANZA MEGAH LTD, PT) and the only duplicated coordinates are missings. 
*/

save "$base_path_wd\build\input\mill_geolocalization\mills_to_georeference_pre2011_done.dta", replace 
/*that's 38 mills with coordinates + 14 with names only (
SEGAR KEMBANG SEJATI, PT 
SEI SEKALA 
LEMBAH KRYA, PT 
PMK SUMBER WARAS 
PRISCOLIN, PT 
INDOSCO UTAMA PT 
BINA KARYA PRIMA, PT 
BONANZA MEGAH LTD, PT 
SUKUR, PT 
SAMPURNA SPUTNIK PT 
KETUPAT MAS,PT 
DUTA SUMBER NABATI, PT 
KURNIA TUNGGAL NUGRAHA, PT 
BONANZA MEGAH LTD, PT 
)
*/

**post2010    
import excel "$base_path_wd\build\input\mill_geolocalization\mills_to_georeference_post2010_done.xlsx", sheet("mills to georef") firstrow clear
sort firm_id year
rename latitude lat
rename longitude lon
drop if mi(firm_id)

*drop those that could not be geolocalized or obs. of geolocalized mills for which the info was not written
* (remember that some obs. - in green in excel basically - have coordinates but no names
* and others have names from directories but no coordinates could be found. We want to keep both kinds.)
drop if mi(lat) & mi(mill_name)

*check that there is only one mill_name and one coordinate value within firm_id 
by firm_id mill_name, sort: gen nvals = (_n ==1)
by firm_id: replace nvals = sum(nvals)
by firm_id: replace nvals = nvals[_N]
sort firm_id year 
count if nvals != 1
drop nvals
by firm_id lat, sort: gen nvals = (_n ==1)
by firm_id: replace nvals = sum(nvals)
count if nvals != 1
drop nvals

sort firm_id year 
duplicates drop firm_id, force

save "$base_path_wd\build\input\mill_geolocalization\mills_to_georeference_post2010_done.dta", replace 
/*That is 98 mills with name and coordinates, and 4 mills with only names
TUNAS LESTARI SEJATI, 
PTSUMATERA JAYA AGRO INDUSTRI, 
PTANAM KOTO, 
PTANJ AGRI SIASIS
*/



** Heilmayr mill list 
import excel "$base_path_wd\build\input\traseMills_capEstyear.xlsx", sheet("traseMills_capEstyear") firstrow clear
keep trase_code parent_co mill_name latitude longitude est_year
rename latitude lat
rename longitude lon
destring lat, replace
destring lon, replace
codebook trase_code
save "$base_path_wd\build\input\traseMills_capEstyear_selected.dta", replace 





*merge them 
use "$base_path_wd\build\output\IBS_PO_98_15_cleaned.dta", clear
sort firm_id year

merge m:1 firm_id using "$base_path_wd\build\input\mill_geolocalization\oto.dta", generate(merge_oto) keepusing(parent_co mill_name est_year lat lon) update

merge m:1 firm_id using "$base_path_wd\build\input\mill_geolocalization\noto_done.dta", generate(merge_noto) keepusing(parent_co mill_name est_year lat lon) update

merge m:1 firm_id using "$base_path_wd\build\input\mill_geolocalization\mills_to_georeference_pre2011_done.dta", generate(merge_pre2011) keepusing(mill_name lat lon) update

merge m:1 firm_id using "$base_path_wd\build\input\mill_geolocalization\mills_to_georeference_post2010_done.dta", generate(merge_post2010) keepusing(mill_name lat lon) update

merge m:1 firm_id using "$base_path_wd\build\input\mill_geolocalization\ibs_unref.dta", generate(merge_unref)
* (this one is just firm_id, the point is only to flage those mills that don't have coordinates yet but have a desa polygon)

gen unref = (merge_unref == 3)
drop merge_unref

* MANUAL WORK FOR CONFLICTING CASES WHERE DIFFERENT firm_id HAVE THE SAME MILL_NAME OR SAME COORDINATES

/*firm_id that were found to be the same mill as another firm_id while performing the manual geolocalization.
The oldest firm_id is kept
*/
replace firm_id = 10464 if firm_id == 68838 
replace firm_id = 36434 if firm_id == 69486
replace firm_id = 52333 if firm_id == 55834
replace firm_id = 66746 if firm_id == 70267  

/*
For BONANZA MEGAH LTD, there are two different refineries in the directories indeed, with different number of workers which each match with one of 18244 and 60758
but they have exactly the same adress. Just had a prime in front of the string. 
*/
replace mill_name = "'BONANZA MEGAH LTD, PT" if firm_id == 60758

/* 
For Baras case, there are two mills in directories, and according to this document and to google image, these mills have different capacities. 
According to number of workers and all cpo and pko variables available, we assume that 34164 is the biggest and 67078 is the smallest one. This also 
makes sense in terms of apparent age difference. 
http://ispo-org.or.id/images/notifikasi/notif_152_PT%20Unggul%20Widya%20Teknologi%20Lestari.pdf
*/
replace mill_name = "Agribaras" if firm_id == 67078
replace parent_co = "PT Unggul Widya Teknologi Lestari" if firm_id == 67078
replace lat = -1.521139 if firm_id == 67078
replace lon = 119.433556 if firm_id == 67078

/*
From directories, PT. Agri Andalas is both 36728 and 69026. 
*/
replace firm_id = 36728 if firm_id == 69026

/*
The case of 55627, 66728 and PT. Bumi Pratama Khatulistiwa: it is seemingly all the same mill but id are following but there is a duplicate in 2008
(because number of workers are the same in 2008 and many info are missing in 2008 for 55627.)
*/
drop if firm_id == 55627 & year == 2008
replace firm_id = 55627 if firm_id == 66728

/*
5283 and 69025 are also likely the same mill, namely PT. Dario Dharma Pratama - Ipuh because number of workers match for both in contemporaneous directories. 
Moreover, 70449 and 70450 are less likely Ipuh because they match only on one number of workers while 69025 matches on 133 and 136. 
*/
replace firm_id = 5283 if firm_id == 69025 

replace mill_name = "" if firm_id == 70449 
replace parent_co = "" if firm_id == 70449 
replace lat = . if firm_id == 70449 
replace lon = . if firm_id == 70449 

replace mill_name = "" if firm_id == 70450 
replace parent_co = "" if firm_id == 70450 
replace lat = . if firm_id == 70450 
replace lon = . if firm_id == 70450 
/*
2125 and 2127 in Torgamba. 
2125 matches in directories with TASIK RAJA, PT, which is in heilmayr mills but outside of BUKIT TUJUH village. 2127 remains PT. Torganda - PKS Sibisa Mangantur, the only
mill from Torganda in this area (2127 matches with a TORGANDA, PT in directories, the only one in this region.)
*/
replace mill_name = "PT. Tasik Raja" if firm_id == 2125
replace parent_co = "PT Tasik Raja" if firm_id == 2125
replace lat = 1.669279923 if firm_id == 2125
replace lon = 100.1615075 if firm_id == 2125


/*
in some years there are two PASANGKAYU, PT in the same desa in the directories (and at the same time 48970 and 67074 overlap)
There is only one big mill in this area. Hence, it is likely all the same mill. 
In the four years (2006-2009) when the two firm_id overlap in IBS, quantities are all duplicated. As if they had just replicated the quantities from 2005. 
This is one more hint that it is the same mill, with valid obs. from 1998 to 2005 for 48970 and then from 2006 for 67074. 
*/
drop if firm_id == 48970 & (year == 2006 | year == 2007 | year == 2008 | year == 2009)
replace firm_id = 48970 if firm_id == 67074 


/*
PT Sawita Leidong Jaya matches with 52434 on several numbers of workers, while 76315 only on 105 (because only one observation in 2015)
*/

replace mill_name = "" if firm_id == 76315 
replace parent_co = "" if firm_id == 76315 
replace lat = . if firm_id == 76315 
replace lon = . if firm_id == 76315 

/*
70396 has 2 matches with directories while 511439 has much more. 
*/
replace mill_name = "" if firm_id == 70396 
replace parent_co = "" if firm_id == 70396 
replace lat = . if firm_id == 70396 
replace lon = . if firm_id == 70396 

/*
68248 (from oto) clearly matches Citra Riau Sarana in Muara Langsat, which is CRS - Teso II in ref_mills. 
72997 has observations only since 2013 and matches CRS - Pks Jake in 2015. But Pks Jake has directories since 2006 so it is necessarily 
CRS - Teso I (because Teso I is estimated to appear in 2004 and Teso III in 2013 - and google earth 2007 images confirm Teso III was not there but Teso I was). 
As we don't take appearance year in IBS as an trustworthy indicator of actual birthdate, we can suppose that 72997 is CRS - Teso 1. 
*/
replace mill_name = "PT. Citra Riau Sarana - Teso I" if firm_id == 72997 
replace parent_co = "PT Citra Riau Sarana" if firm_id == 72997 
replace lat = -0.212423211 if firm_id == 72997 
replace lon = 101.4734808 if firm_id == 72997 



/*
68244 matches several times and more precisely on geography with Hasil than 76312
*/
replace mill_name = "" if firm_id == 76312 
replace parent_co = "" if firm_id == 76312 
replace lat = . if firm_id == 76312 
replace lon = . if firm_id == 76312 

/*
55727 matched spatially with PT. Kalimantan Sawit Abadi - Natai Baru, but this mill (found in directory) never matches with in terms of workers with 55727. 
Rather, 55727 is found to match sawit sumber mas sarana in directories, in Sulung desa. . 
*/
replace mill_name = "PT. Sawit Subermas Sarana - Sulung Mill" if firm_id == 55727 
replace parent_co = "PT Sawit Sumbermas Sarana" if firm_id == 55727 
replace lat = -2.304451535 if firm_id == 55727 
replace lon = 111.6132597 if firm_id == 55727 

/*
77946 seems to be the same mill as 75041 because their years are continuous, and they have the same number of workers in the same district. 
*/
replace firm_id = 75041 if firm_id == 77946

/*
54271 cannot be assumed to be the same mill as 72995 because there are at least two mills in Pelintung according to imagery.
72995 matching on number of workers is more trustworthy than the mere spatial match of 54271. 
*/
replace mill_name = "" if firm_id == 54271 
replace parent_co = "" if firm_id == 54271 
replace lat = . if firm_id == 54271 
replace lon = . if firm_id == 54271 

/*
Clearly, the automatic spatial match between 52344 and Begerpang mill is right. 
The match in 2015 on 99 for 72956 likely links to a head quarter office rather because we cannot find two Lonsum mills in deli serdang.  
*/
replace mill_name = "" if firm_id == 72956 
replace parent_co = "" if firm_id == 72956 
replace lat = . if firm_id == 72956 
replace lon = . if firm_id == 72956

/*
51434 matches with masterindo on several numbers of workers while 72994 only on 132.
72994 matches with Swastisiddhi Amagra matches 2013 ibs with 2014 directory. 
*/ 		
replace mill_name = "PT. Swastisidi Amagra" if firm_id == 72994 
replace parent_co = "PT. Swastisiddhi Amagra" if firm_id == 72994 
replace lat = 0.210324795 if firm_id == 72994 
replace lon = 101.2753159 if firm_id == 72994

/*
55635 and 72781 are likely both PT. Poliplant Sejahtera, because they both match it in their contemporaneous directories 
and don't have overlapping years. 
*/
replace firm_id = 55635 if firm_id == 72781

/*
68247 is a better match than 76319
*/
replace mill_name = "" if firm_id == 76319 
replace parent_co = "" if firm_id == 76319 
replace lat = . if firm_id == 76319 
replace lon = . if firm_id == 76319



sort firm_id year 
*we check for duplicates firm_id coordinates (or very close ones, relating to the same mill) on R. These cases are: 

/*
4780 (found with spatial matching) is clearly PTP Nusantara Sungai Lengi based on number of workers. 
The other one, 56417, is matching with another PTP Nusantara VII in directories, but it cannot be found in ref, nor on 
google maps/earth, nor on internet. Although it produces CPO in IBS, i tmight be the plantations associated to Sungai Lengi,
as is stated here: http://www.ptpn7.com/displaycontent.aspx?topic=Distrik%20Muara%20Enim 
*/
replace mill_name = "" if firm_id == 56417 
replace parent_co = "" if firm_id == 56417 
replace lat = . if firm_id == 56417 
replace lon = . if firm_id == 56417

/*
The case of 36342 and 67075 with Letawa, PT. 
36342 has observations from 1998 to 2009, and all duplicates from 2006 on. It matches directories in 2003 and 2006. 
In these years, there are only one Letawa in directories. 
Then, in 2009 and 2010, there are 2 Letawa in directories, in the same desa, with different numbers of workers that match 
36342 and 67075 respectively. 
Then, there is only one Letawa again, that matches 67075 now. 
Therefore, it seems likely that it has been the same mill from the beginning, but with some change in firm_id. 
The overlapping years of firm_id are 2006-2009. 
For 36342, these are duplicates of 2006. 
For 67075, the quantity is misreported (>100k tons of CPO.)
Therefore, we believe the most credible observation of Letawa is 36342 until 2006 included, and 67075 afterwards
*/
drop if firm_id == 36342 & year > 2006 
drop if firm_id == 67075 & year == 2006 
replace firm_id = 36342 if firm_id == 67075 

/*
see https://www.industryabout.com/country-territories-3/1937-indonesia/vegetable-oil-industry/30072-bsp-agro-mitra-madani-palm-oil-mill
https://www.tuv.com/media/indonesia/brochure_2/system_clients_2016/per_2017/update_public_summary_rspo_part_1/RSPO_ASA4_Public_Summary_PT_Agrowiyana_BSP_Jambi.pdf
67238 matches Agro MITRA MADANI, which cannot be located where we thought it was according to these two sources, because this 
is alreadz where PT Trimitra Lestari is. But these sources are trustworthy (TÜV especially). Other documents online listing
palm oil mills give the same coordinates to both mills, pointing at the same factory.
What happens is that there is another mill very closeby, a bit more west. According to google names, this is Mitra Madani 
(they also give the name of Agro Wiyana, which relates to Mitra Madani according to above tüv source.) and the one at which 
all documents point at is Trimitra Lestari. 
*/
replace lat = -1.078053 if firm_id == 67238 
replace lon = 103.110272 if firm_id == 67238


/*
69531 is PT. Agro Sarimas Indonesia.
54274 might also be PT. Agro Sarimas Indonesia because it is observed in same desa years continuously before,
but that is likely rather a coco mill than a palm mill
https://www.gmdu.net/corp-414564.html and we have no palm data in IBS. 
May be it was a conversion of activity. Let us remove mill name and coordinates to 54274 and assume it indeed was not a 
palm oil mill at this time.  
*/
replace mill_name = "" if firm_id == 54274 
replace parent_co = "" if firm_id == 54274 
replace lat = . if firm_id == 54274 
replace lon = . if firm_id == 54274

/*
56234 is a more precise match with Andalas than 76289. 
*/
replace mill_name = "" if firm_id == 76289 
replace parent_co = "" if firm_id == 76289 
replace lat = . if firm_id == 76289 
replace lon = . if firm_id == 76289

/*
70390 and 76329 have both only one match with directories when looking only to n-1, n and n+1 year directories, 
which RA did in the first place. But actually, 70390 has two matches when looking at n-2 year directories. 
Therefore we assume 70390 is PT. Bina Sawit Nusantara 
*/
replace mill_name = "PT. Bina Sawit Nusantara" if firm_id == 70390 
replace parent_co = "PT. Bina Sawit Nusantara" if firm_id == 70390 
replace lat = 0.165799445 if firm_id == 70390 
replace lon = 101.3260084 if firm_id == 70390

/*
55775 has better matches with PT. Socfin - Lae Butar (Desa rimo in directories).
71593 might rather be PT. Delima Makmur.   
*/
replace mill_name = "PT. Delima Makmur" if firm_id == 71593 
replace parent_co = "PT. Delima Makmur" if firm_id == 71593 
replace lat = 2.245276912 if firm_id == 71593 
replace lon = 98.03032975 if firm_id == 71593

/*
2062 is clearly PTP Nusantara Sei Silau. 
In 2015 directory there is Sei Silau and SEI S´, each matching with one of these 2 IBS mills. 
2062 has no observation in 2015, and many missings in 2014. 
72957 has obs. in 2013-2015 but mainly missing in 2013, 2014. 
They have the same number of workers in IBS (185 in 2014 for 2062 and in 2015 for 72957)
That looks like a firm_id change that resulted in some information lost. 
*/
drop if firm_id == 72957 & (year == 2013 | year == 2014)
replace firm_id = 2062 if firm_id == 72957

/*
49003 better matches Sei Mangkei 
*/
replace mill_name = "" if firm_id == 72959 
replace parent_co = "" if firm_id == 72959 
replace lat = . if firm_id == 72959 
replace lon = . if firm_id == 72959

/*
For 2119, obs. on years 2004 and 2005 are duplicates of year 2003 (for quantities but not for values, as is often the case)
72959 goes from 2004, with same number of workers at that time. 
*/
drop if firm_id == 2119 & (year == 2004 | year == 2005)
replace firm_id = 2119 if firm_id == 52344

/*
browse firm_id year workers_total_imp3 district_name kec_name village_name  mill_name parent_co lat lon ///
merge_oto merge_noto merge_pre2011 merge_post2010 in_ton_ffb ffb_price_imp1 out_ton_cpo out_ton_cpo_imp2 out_val_cpo out_val_cpo_imp2 ///
cpo_price_imp2 out_ton_pko out_ton_pko_imp2 out_val_pko out_val_pko_imp2 pko_price_imp2     ///
if firm_id == 2119 | firm_id == 52344
*/

/*
*check these conflicting cases after the manual modifications
duplicates tag mill_name if !mi(mill_name), generate(du_mill_name) 
duplicates tag firm_id mill_name if !mi(mill_name), generate(du_firm_id_mill_name) 
sort mill_name firm_id year 

browse firm_id year workers_total_imp3 district_name kec_name village_name  mill_name parent_co lat lon ///
merge_oto merge_noto merge_pre2011 merge_post2010 du_mill_name du_firm_id_mill_name if du_mill_name != du_firm_id_mill_name
*/



merge m:1 lat lon using "$base_path_wd\build\input\traseMills_capEstyear_selected.dta", generate(merge_heilmayr) keepusing(trase_code parent_co est_year) update 
drop if merge_heilmayr == 2
sort firm_id year 

* send to exploratory data analysis. 
save "$base_path_wd\build\output\IBS_mills_final.dta", replace

* send to outcome variables building in R: 
keep if !mi(lat)
keep firm_id year trase_code mill_name lat lon 
duplicates drop firm_id, force
save "$base_path_wd\build\input\mill_geolocalization\IBS_mills_geolocalized.dta", replace






/*
keep firm_id year startYear industry_code ffb_price_imp1 ffb_price_imp2 in_ton_ffb in_ton_ffb_imp1 in_ton_ffb_imp2 in_val_ffb in_val_ffb_imp1 in_val_ffb_imp2 in_dom_cpo_price_imp1 in_dom_cpo_price_imp2 in_dom_ton_cpo in_dom_ton_cpo_imp1 in_dom_ton_cpo_imp2 in_dom_val_cpo in_dom_val_cpo_imp1 in_dom_val_cpo_imp2 in_imp_cpo_price_imp1 in_imp_cpo_price_imp2 in_imp_ton_cpo in_imp_ton_cpo_imp1 in_imp_ton_cpo_imp2 in_imp_val_cpo in_imp_val_cpo_imp1 in_imp_val_cpo_imp2 in_tot_cpo_price_imp1 in_tot_cpo_price_imp2 in_tot_ton_cpo in_tot_ton_cpo_imp1 in_tot_ton_cpo_imp2 in_tot_val_cpo in_tot_val_cpo_imp1 in_tot_val_cpo_imp2 cpo_price_imp1 cpo_price_imp2 out_ton_cpo out_ton_cpo_imp1 out_ton_cpo_imp2 out_val_cpo out_val_cpo_imp1 out_val_cpo_imp2 prex_cpo prex_cpo_imp1 prex_cpo_imp2 out_cpo pko_price_imp1 pko_price_imp2 out_ton_pko out_ton_pko_imp1 out_ton_pko_imp2 out_val_pko out_val_pko_imp1 out_val_pko_imp2 prex_pko prex_pko_imp1 prex_pko_imp2 out_pko out_ton_rpo out_ton_rpo_imp1 out_ton_rpo_imp2 out_val_rpo out_val_rpo_imp1 out_val_rpo_imp2 prex_rpo prex_rpo_imp1 prex_rpo_imp2 out_rpo out_ton_rpko out_ton_rpko_imp1 out_ton_rpko_imp2 out_val_rpko out_val_rpko_imp1 out_val_rpko_imp2 prex_rpko prex_rpko_imp1 prex_rpko_imp2 out_rpko EKSPOR export_pct export_pct_imp ///
revenue_total pct_own_cent_gov_imp pct_own_loc_gov_imp pct_own_nat_priv_imp pct_own_for_imp desa_code desa_name kab_code kab_name kec_code kec_name prov_code prov_name district_name village_name parent_co mill_name est_year lat lon trase_code
*/
