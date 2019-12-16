***FIRST STAGE***
import excel "C:\Users\guyv\ownCloud\opalval\analysis\input\prices_exp.xlsx", sheet("Sheet1") firstrow clear
drop Domesticprice FOBPrice
save "C:\Users\guyv\ownCloud\opalval\analysis\input\prices_exp.dta", replace

use "C:\Users\guyv\ownCloud\opalval\build\input\IBS_1998_cleaned.dta", clear

merge m:1 year using C:\Users\guyv\ownCloud\opalval\analysis\input\prices_exp.dta, nogenerate 
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

forvalues a = 1/2 {
	gen m_cpo_price_imp`a' = .
	forvalues y = 1998/2015{	
			quietly tabstat cpo_price_imp`a' if year == `y', statistics(mean) columns(variables) save
			matrix stats = r(StatTotal)
			quietly replace m_cpo_price_imp`a' = stats[1,1] if year == `y'
	}
}

forvalues a = 1/2 {
	gen m_in_dom_cpo_price_imp`a' = .
	forvalues y = 1998/2015{	
			quietly tabstat in_dom_cpo_price_imp`a' if year == `y', statistics(mean) columns(variables) save
			matrix stats = r(StatTotal)
			quietly replace m_in_dom_cpo_price_imp`a' = stats[1,1] if year == `y'
	}
}

*distinguish btw exporters and non exporters. 
gen export_cpo_dum_imp1 = (prex_cpo_imp1<. & prex_cpo_imp1>0)
gen export_cpo_dum_imp2 = (prex_cpo_imp2<. & prex_cpo_imp2>0)
forvalues a = 1/2 {
	gen m_cpo_price_exp_imp`a' = .
	forvalues y = 1998/2015{	
			quietly tabstat cpo_price_imp`a' if year == `y' & export_cpo_dum_imp1==1, statistics(mean) columns(variables) save
			matrix stats = r(StatTotal)
			quietly replace m_cpo_price_exp_imp`a' = stats[1,1] if year == `y'
	}
}
forvalues a = 1/2 {
	gen m_cpo_price_0exp_imp`a' = .
	forvalues y = 1998/2015{	
			quietly tabstat cpo_price_imp`a' if year == `y' & export_cpo_dum_imp1==0, statistics(mean) columns(variables) save
			matrix stats = r(StatTotal)
			quietly replace m_cpo_price_0exp_imp`a' = stats[1,1] if year == `y'
	}
}


twoway (line m_cpo_price_0exp_imp1 year) (line m_in_dom_cpo_price_imp1 year) 

(line cif_rtdm_cpo year) (line dom_blwn_cpo year) 





*****REGRESSIONS*****

*create year dummy 
forvalues y=1998/2015{
	gen d_`y' = (year==`y')
} 

**Generate treatments
	*With current and lag export shares	
		forvalues a=1/2{
		sort firm_id year
		by firm_id : gen double lag0_prex_cpo_imp`a'= prex_cpo_imp`a' 
		by firm_id : gen double lag1_prex_cpo_imp`a'= prex_cpo_imp`a'[_n-1] 
		by firm_id : gen double lag2_prex_cpo_imp`a'= prex_cpo_imp`a'[_n-2] 
		forvalues s=1/4{
			forvalues l=0/2 {
				gen double treatment_lag`l'_spread`s'_imp`a' = (lag`l'_prex_cpo_imp`a'/100)*spread`s'
			}
		}
		*gen double treatment_ter_imp`a' = (lag_prex_cpo_imp`a'/100)*taxeffectiverate
		*gen double treatment_rho_imp`a' = (lag_prex_cpo_imp`a'/100)*rho
	}

	* With fixed export shares
		 forvalues a=1/2{
			sort firm_id year
			by firm_id : egen double fixed_prex_cpo_imp`a'= mean(prex_cpo_imp`a')
			forvalues s=1/4{
					gen double treatment_fixed_spread`s'_imp`a' = (fixed_prex_cpo_imp`a'/100)*spread`s'
				}
		}			 





**Regressions
** ADD CLUSTER AT THE UNIT LEVEL BY DEFAULT ALWAYS 

forvalues a=1/2{
	forvalues s=1/4{
		*forvalues l=0/2{
			*xtreg cpo_price_imp`a' treatment_lag`l'_spread`s'_imp`a' d_2008 d_2009 d_2010 d_2011 d_2012 d_2013 d_2014 d_2015, fe 
			xtreg out_ton_cpo_imp`a' treatment_lag0_spread`s'_imp`a', fe
		*}
	}
}

dom_blwn_cpo
pct_own_loc_gov_imp pct_own_nat_priv_imp pct_own_for_imp
/*run like this, the coefficients are to be interpreted as: an increase in the net (of tariff) spread of 1 $US/MT leads on average to a difference _b $US/MT between
fully exporting mills and non-exporting mills.  
Here en l'occurrence, l'effet du spread est globalement négatif, ce qui n'est pas logique, donc on a du faire quelque chose de tordu.
Par ailleurs, le spread lui même est négatif. 

This is not because of too many zeros in the export shares because when removing all zeros, there is still many times a negative coefficient on the treatment_spread and 
a positive one on the dom_blwn price variable. 

*/


xtreg cpo_price_imp1 treatment_lag1_spread1_imp1 d_2008 d_2009 d_2010 d_2011 d_2012 d_2013 d_2014 d_2015, fe

/*

A faire:  
https://www.princeton.edu/~otorres/Panel101.pdf
- rechecker ce rho, pourquoi il est si grand. rechecker les formules pour calculer la taxe etc.ca a l'air correct et ca colle avec Riffin.  

- revérifier tout ce qui a été fait aujourd'hui mardi 23 avril# et ajouter les times series complètes de manière générale.
	notamment, qui touche quoi, quel prix est le plus pertinent à utiliser, qu'est-ce qui colle le plus à la réalité.
	Ce qui est le plus realiste c'est que les mills qui exportent touchent  
- créer des variables standardisées:	
										y' = (y-m)/s					
										where the estimate of b becomes the effect measured in standard deviations of y instead of in points on the test. 
										So finding that the estimate of b in equation (3) is 0.15, you would conclude that taking the test-prep class would lead to an 
										increase in one’s quantitative GRE score of 0.15 standard deviation.
										You can standardize your outcome variable, a right-hand side (RHS) variable, or both. 
										If you standardize an RHS variable, the interpretation becomes in terms of what happens to y in its own units if the standardized x increases by one standard deviation. 
										If you standardize on both sides, the interpretation is in terms of standard deviation on both sides, or what happens to y (in standard deviations) for 
										a one standard deviation increase in x. That really is all there is to standardization.

- créer des variables en log. 
http://marcfbellemare.com/wordpress/11763

ca ne sert à rien de faire plein de régressions, l'exposure est encore trop fausse tant qu'on a pas la dummy (et encore...)
et attendons aussi d'avoir toutes les infos sur les times series. 
Mais a priori les time series sont corrects, donc le signe négatif du coeff reflète soit : 
une erreur systématique dans l'export share
une erreur dans le calcul du prix mais je vois pas pk
une mauvaise modélisation, i.e. les prix internationaux et domestiques utilisés ne déterminent pas les prix des mills de cette manière là 
(si par exemple celles qui exportent vendent d'abord à des intermédiaires qui achètent en fonction du prix national)
