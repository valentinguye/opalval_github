use "$base_path_wd\build\input\IBS_UML_panel.dta", replace

keep if !mi(lat)

xtset firm_id year

xtsum ffb_price_imp1 ffb_price_imp2 cpo_price_imp1 cpo_price_imp2 ///
prex_cpo_imp1 prex_cpo_imp2 pct_own_cent_gov_imp pct_own_loc_gov_imp pct_own_nat_priv_imp pct_own_for_imp
