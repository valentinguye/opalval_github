import excel "$base_path_wd\build\input\prices_exp.xlsx", sheet("Sheet1") firstrow clear
drop Domesticprice FOBPrice
save "$base_path_wd\build\input\prices_exp.dta", replace

*use "C:\Users\guyv\ownCloud\opalval\build\input\IBS_1998_cleaned.dta", clear
use "$base_path_wd\build\input\IBS_UML_panel_est_year.dta", clear
merge m:1 year using "$base_path_wd\build\input\prices_exp.dta", nogenerate 
sort firm_id year 
*need to change the global for all new price variables each time (and don't forget to NOT DELFATE taxeffectiverate). 
global prices ref_int_cpo_price cif_rtdm_cpo dom_blwn_cpo fob_blwn_cpo spread_int_dom rho dom_blwn_pko cif_rtdm_pko

label variable spread_int_dom "Paspi calculation of fob_blwn_cpo minus dom_blwn_cpo"			

*DEFLATE 
foreach var of varlist $prices {
	quietly replace `var'=`var'/wpi_2010
}

/*
graph hbox cpo_price_imp2 in_tot_cpo_price_imp2 if in_tot_cpo_price_imp2<.& cpo_price_imp2<.
graph hbox cpo_price_imp1 in_tot_cpo_price_imp1 if in_tot_cpo_price_imp1<.& cpo_price_imp1<.
*celles qui ont à la fois un prix d'achat et un prix de vente de cpo ont le premier globalement plus bas. 
*/

replace taxeffectiverate = taxeffectiverate/100

gen double spread1 = cif_rtdm_cpo - dom_blwn_cpo - taxeffectiverate*cif_rtdm_cpo
label variable spread1 "spread_cpo_rtdm_blwn"
gen double spread2 = fob_blwn_cpo - dom_blwn_cpo - taxeffectiverate*fob_blwn_cpo
label variable spread2 "spread_cpo_fobblwn_blwn"
gen double spread3 = spread_int_dom - rho
label variable spread3 "spread"
gen double spread4 = ref_int_cpo_price - dom_blwn_cpo - taxeffectiverate*ref_int_cpo_price
label variable spread4 "spread_cpo_refpr_blwn"
tabstat spread1 spread2 spread3 spread4, statistics( mean ) by(year)

forvalues s = 1/4{
gen double iv`s'_imp1 = prex_cpo_imp1*spread`s'
gen double iv`s'_imp2 = prex_cpo_imp2*spread`s'
}



*** Log variables
global to_log ffb_price_imp1 ffb_price_imp2 cpo_price_imp1 cpo_price_imp2 pko_price_imp1 pko_price_imp2 ///
out_val_cpo_imp1 out_val_cpo_imp2 out_val_pko_imp1 out_val_pko_imp2 revenue_total ///
ref_int_cpo_price cif_rtdm_cpo dom_blwn_cpo fob_blwn_cpo spread_int_dom rho dom_blwn_pko cif_rtdm_pko ///
iv1_imp1 iv1_imp2 iv2_imp1 iv2_imp2 iv3_imp1 iv3_imp2 iv4_imp1 iv4_imp2

foreach var of varlist $to_log {
	gen `var'_ln = ln(`var')
}

sort firm_id year 

save "$base_path_wd\build\output\IBS_UML_panel_final.dta", replace 









