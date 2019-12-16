*** Some code for alternative treatment of outliers.  

	/* This is for only CPO
	global out_cpo_vars out_ton_cpo out_val_cpo
	 foreach var of varlist $out_cpo_vars {

		tabstat `var' if valid_`var'==1, statistics(iqr p25 p75 p50) columns(variables) save
		matrix stats = r(StatTotal)
		local iqr_`var' = stats[1,1]
		local p75_`var' = stats[3,1]
		local outlier_th_`var' = `p75_`var'' + 1.5*`iqr_`var''
		gen flag_outlier_`var' = (`var'>=`outlier_th_`var'') if valid_`var'==1
		display `outlier_th_`var''
	}
	count if flag_outlier_out_ton_cpo==1
	count if flag_outlier_out_val_cpo==1
	count if flag_outlier_out_ton_cpo==1 | flag_outlier_out_val_cpo==1
	*/


	/* If we want to show low outliers: 
	global out_cpo_vars out_ton_cpo out_val_cpo
	 foreach var of varlist $out_cpo_vars {
		tabstat `var' if valid_`var'==1, statistics(iqr p25 p75 p50) columns(variables) save
		matrix stats = r(StatTotal)
		local iqr_`var' = stats[1,1]
		local p25_`var' = stats[2,1]
		local p75_`var' = stats[3,1]
		local outlier_lth_`var' = `p25_`var'' - 1.5*`iqr_`var''
		local outlier_uth_`var' = `p75_`var'' + 1.5*`iqr_`var''
		gen flag_l_outlier_`var' = (`var'<=`outlier_lth_`var'') if valid_`var'==1
		gen flag_u_outlier_`var' = (`var'>=`outlier_uth_`var'') if valid_`var'==1
		display `outlier_lth_`var''
		display `outlier_uth_`var''
	}
	
	count if flag_l_outlier_out_ton_cpo==1
	count if flag_l_outlier_out_val_cpo==1
	count if flag_u_outlier_out_ton_cpo==1
	count if flag_u_outlier_out_val_cpo==1
	count if flag_u_outlier_out_ton_cpo==1 | flag_u_outlier_out_val_cpo==1
	count if flag_l_outlier_out_ton_cpo==1 | flag_l_outlier_out_val_cpo==1 | flag_u_outlier_out_ton_cpo==1 | flag_u_outlier_out_val_cpo==1
