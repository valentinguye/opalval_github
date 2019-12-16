*** Some code for some alternative treatment of duplicates 


*duplicates tag firm_id out_ton_cpo if valid_out_ton_cpo==1&valid_out_val_cpo==1, generate(tag_du_firm_outtoncpo) --> sert a rien ca en fait. 
										sort firm_id year
										by firm_id: gen double lag2_out_ton_cpo = out_ton_cpo[_n-2]
										gen lasting_du = (out_ton_cpo==lag_out_ton_cpo & lag_out_ton_cpo==lag2_out_ton_cpo)

										gen flag_du_firm_outtoncpo = (valid_out_ton_cpo==1&valid_out_val_cpo==1 & (lasting_du==1 | (lag_out_ton_cpo == out_ton_cpo & lag_out_val_cpo==out_val_cpo)))
										inspect flag_du_firm_outtoncpo
										browse firm_id year out_ton_cpo lag_out_ton_cpo lag2_out_ton_cpo lasting_du flag_du_firm_outtoncpo
										replace out_ton_cpo = . if flag_du_firm_outtoncpo==1 
										drop lasting_du lag2_out_ton_cpo
										/*
										So what this does is removing an observation when it has the same out_ton_cpo for the third consecutive year or 
										for the second year if it has also the same out_val_cpo over these 2 years. 

									In a Program for all commo, and for values too (i.e. replaces out_val_ by missing if it is repeating for the third consecutive year, even if qty is varying, OR if it is a duplicate 
									in terms of both qty and value... which we know does not occur here.)
									*/ 
									sort firm_id year 
									count if lag_out_ton_cpo == out_ton_cpo & lag_out_val_cpo==out_val_cpo & valid_out_ton_cpo==1&valid_out_val_cpo==1
									duplic

									global out_ton_vars out_ton_cpo out_ton_pko out_ton_rpo out_ton_rpko
									foreach var of varlist $out_ton_vars{
										by firm_id: gen double lag2_`var' = `var'[_n-2]
										gen lasting_du_`var' = (`var'==lag_`var' & lag_`var'==lag2_`var')

										gen flag_du_firm_`var' = (valid_out_ton_cpo==1&valid_out_val_cpo==1 & (lasting_du==1 | (lag_out_ton_cpo == out_ton_cpo & lag_out_val_cpo==out_val_cpo)))
										inspect flag_du_firm_outtoncpo
										browse firm_id year out_ton_cpo lag_out_ton_cpo lag2_out_ton_cpo lasting_du flag_du_firm_outtoncpo
										replace out_ton_cpo = . if flag_du_firm_outtoncpo==1 
										drop lasting_du lag2_out_ton_cpo
									}


*** For duplicates within year : 

	duplicates list year out_ton_cpo out_val_cpo if valid_out_ton_cpo==1&valid_out_val_cpo==1
	duplicates report year out_ton_cpo if valid_out_ton_cpo==1&valid_out_val_cpo==1
	/* These are the peeps that are duplicated within the same year, because may be the indonesian stat office wanted to fill the blank gaps for some obs.  
	so they put the values similar mill? 
	In other words: these duplicates are really different firms, but this information has been duplicated on purpose. 
	--> then it should be different for some other variables, and in this case may be we want to keep them? This is a lot of noise said Sebi... 
	--> if they are not different on anything (only firm_id), then they are not-intended duplicates and should be removed. 
	duplicates 

	unab a: _all
	local omit firm_id
	local all_but_firmid : list a - omit
	duplicates tag `all_but_firmid' if valid_out_ton_cpo==1&valid_out_val_cpo==1, gen(tag_du_allbutfirmid)
	gen flag_du_allbutfirmid =(tag_du_allbutfirmid >0 & tag_du_allbutfirmid<.)

	Ok duplicates in terms of only year and out_ton_cpo can be realistic, just by pure chance, so let's not remove them. 
	duplicates in terms of both plus out_val_cpo becomes less likely... 
	FIND A WAY TO THINK IDENTIFY WHEN IT CANNOT BE BY PURE CHANCE
	OR DELETE DUPLICATES IN TERMS OF BOTH + VALUE*/

	*this is to compare duplicates in there original datasets. 
	/*
	local ibsfull_du materials_tot revenue_total 
	local inputs_du in_ton_ffb in_val_ffb
	local outputs_du out_ton_cpo out_val_cpo
	duplicates tag year `ibsfull_du' if materials_tot >0 & materials_tot<. & revenue_total>0 & revenue_total<., gen(tag_ibsfull_du)
	duplicates tag year `inputs_du' if in_ton_ffb >0 & in_ton_ffb<. & in_val_ffb>0 & in_val_ffb <. , gen(tag_inputs_du)
	duplicates tag year `outputs_du' if valid_out_ton_cpo==1&valid_out_val_cpo==1, gen(tag_outputs_du)
	gen flag_du_ibsfull =(tag_ibsfull_du>0 & tag_ibsfull_du<.)
	gen flag_du_inputs =(tag_inputs_du>0 & tag_inputs_du<.)
	gen flag_du_outputs =(tag_outputs_du>0 & tag_outputs_du<.)
	count if (materials_tot ==0 | revenue_total ==0) & valid_out_ton_cpo==1&valid_out_val_cpo==1

	Ok so: if they are duplicates in Sebi's terms: drop 
	Among those who remain, they are duplicates in terms of revenues_emp1-2 but not necessarily in terms of workers or other variables like this. 
	I think we will remove them (they are not numerous), but let's flag them differently. 

	The general idea is that we accept duplicates only in terms of year and out_ton_cpo, and then we add layers that flag duplicates in terms of increasing number of variables. 
	the number of duplicates will be decreasing along these groups of terms (cummulative): 
	year out_ton_cpo > out_val_cpo > revenue_total > materials_tot > in_ton_ffb > in_val_ffb ... > all_but_firmid 
	In other terms, the more terms, the less obs. we drop. 
	The questions are: when do we choose to stop keeping, and is this the right way to order terms? 

	So in the program below, if `1' is revenue_total, I will eventually remove observations that are duplicates in terms of out_ info and revenue_total. 
	So this would not remove obs. that are ghosts in terms of out_ and different in terms of revenue_total. 
	If `1' is materials_tot, it will eventually remove observations that are duplicates in terms of out_ info, revenue_total, and materials_tot. 
	So this would not remove obs. that are ghosts in terms of out_ and in terms of revenue_total but are different in terms of materials_tot.
	But these would be removed because of being ghosts. 
	More importantly, duplicates in terms of _out info that are ghosts in terms of revenue and materials (because of matching especially) would be removed 
	while others would not just because different in terms of either revenue and materials. This is not really treating all duplicates equally. 
*/
	capture program drop du_inyear_cpo
	program define du_inyear_cpo
	local du_out_val_cpo year out_ton_cpo out_val_cpo
	local du_revenue_total year out_ton_cpo out_val_cpo revenue_total
	local du_materials_tot year out_ton_cpo out_val_cpo revenue_total materials_tot
	local du_in_ton_ffb year out_ton_cpo out_val_cpo revenue_total materials_tot in_ton_ffb
	local du_in_val_ffb year out_ton_cpo out_val_cpo revenue_total materials_tot in_ton_ffb in_val_ffb
	duplicates tag `du_`1'', gen(tag_du_year_`1') 
	count if tag_du_year_`1'>0 & valid_out_ton_cpo==1
	replace out_ton_cpo = . if tag_du_year_`1'>0 
	replace out_val_cpo = . if tag_du_year_`1'>0 		 
	drop tag_du_year_`1'
	end
	du_inyear_cpo out_val_cpo

	capture program drop du_inyear_pko
	program define du_inyear_pko
	local du_out_val_pko year out_ton_pko out_val_pko
	local du_revenue_total year out_ton_pko out_val_pko revenue_total
	local du_materials_tot year out_ton_pko out_val_pko revenue_total materials_tot
	local du_in_ton_ffb year out_ton_pko out_val_pko revenue_total materials_tot in_ton_ffb
	local du_in_val_ffb year out_ton_pko out_val_pko revenue_total materials_tot in_ton_ffb in_val_ffb
	duplicates tag `du_`1'', gen(tag_du_year_`1') 
	count if tag_du_year_`1'>0 & valid_out_ton_pko==1
	replace out_ton_pko = . if tag_du_year_`1'>0 
	replace out_val_pko = . if tag_du_year_`1'>0 		 
	drop tag_du_year_`1'
	end
	du_inyear_pko out_val_pko





