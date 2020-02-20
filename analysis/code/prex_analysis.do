
/*
***Regarding ZEROS***
			
There is a risk that firms answer 0 for either the total export share or commo export share while their outputs are actually exported but they don't know it. 
This would mostly be in cases where their outputs are transformed by another domestic stakeholder before being exported. 
To a smaller extent it may be in cases where their products are not transformed domestically but they simply ignore whether their clients export them or not. 
It could be that smaller firms have less information, and it could also be that its bigger mills that have less visibility if they tend to be selling to bigger refineries. 

Let's see what some tests can tell us on this risk (only for CPO). 

	To analyse these PREX variables, one can look at the mean prex_cpo by year and compare it to the aggregate. 
	It is possible that firms which produce more also export more and that these firms are more or less represented in our sample than in the pop. for instance.  
	Therefore we should rather calculate the weighted (i.e. the effective) sample export share. 
	Either sum of fractions or fraction of sums. Not necessarily equal. 
	Using the former as a mean statistic assumes each obs. has weight=1 
	The latter weighs each individual prex by qty/sample_qty. 

	**Different samples are to be distinguished**
		- There are different samples coming from different data imputations (imp1 and imp2)
		- There are different samples based on different variables being fully non-missing: 
			prex_cpo_ non missing (only for unweighted mean)
			prex_cpo and out_ton_cpo non missing 
			prex_cpo and out_val_cpo non missing
			prex_cpo and price_cpo non missing. 
			The three first ones are informative, the latter is the actual sample we will use for analysis. 
		- There are different samples depending on whether we look at all null prex or only those valided by export_dummy. 

		Hence, the macros produced below are indexed following this rule: samplexyz is sample with 
		- impx (x=1;2),
		- y = 0 if only out_ton_cpo OR out_val_cpo (depending which is used to compute the export share) is non-missing 
			and y = 1 is the subsample with price_cpo non-missing. 
		- z = 0 if we count all zeros and z = 1 if we count only dummy-validated zeros. 

	*/

*use "C:\Users\guyv\ownCloud\opalval\build\temp\mill_geolocalization\IBS_mills_geolocalized.dta", clear
use "$base_path_wd\build\output\IBS_UML_panel.dta", clear

	forvalues a = 1/2 {
		gen double exp_ton_cpo_imp`a' = (prex_cpo_imp`a'/100)*out_ton_cpo_imp`a'
		gen double exp_val_cpo_imp`a' = (prex_cpo_imp`a'/100)*out_val_cpo_imp`a'
		gen sample_`a'z1 = ((prex_cpo_imp`a'>0 & prex_cpo_imp`a'<.) | (prex_cpo_imp`a'==0 & export_dummy_imp == 2)) 
	}
	// So to what does sample`a'z1 refer to? It refers to obs. that have a positive prex 


**Weighted means of prex_cpo (calculated for both quantities and values) AMONG NON-MISSING OUT_TON OR OUT_VAL 

	forvalues a = 1/2 {
		forvalues y = 1998/2015{
			// total export 
				// total exports for all zeros
				quietly tabstat exp_ton_cpo_imp`a' if year == `y', format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'00_exp_ton_cpo_`y' = stats[1,1]

				quietly tabstat exp_val_cpo_imp`a' if year == `y', format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'00_exp_val_cpo_`y' = stats[1,1]

				// total exports for dummy-validated zeros
				quietly tabstat exp_ton_cpo_imp`a' if year == `y' & sample_`a'z1==1, format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'01_exp_ton_cpo_`y' = stats[1,1]

				quietly tabstat exp_val_cpo_imp`a' if year == `y' & sample_`a'z1==1, format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'01_exp_val_cpo_`y' = stats[1,1]

			// total production
				// total production for all zeros
				quietly tabstat out_ton_cpo_imp`a' if year == `y' & prex_cpo_imp`a' <. , format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'00_out_ton_cpo_`y' = stats[1,1]

				quietly tabstat out_val_cpo_imp`a' if year == `y' & prex_cpo_imp`a' <. , format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'00_out_val_cpo_`y' = stats[1,1]

				// total production for dummy-validated zeros
				quietly tabstat out_ton_cpo_imp`a' if year == `y' & sample_`a'z1==1, format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'01_out_ton_cpo_`y' = stats[1,1]

				quietly tabstat out_val_cpo_imp`a' if year == `y' & sample_`a'z1==1, format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'01_out_val_cpo_`y' = stats[1,1]

			// export shares
				//export shares for all zeros
				global sample`a'00_w_prex_ton_cpo_`y' = 100*${sample`a'00_exp_ton_cpo_`y'}/${sample`a'00_out_ton_cpo_`y'}
				global sample`a'00_w_prex_val_cpo_`y' = 100*${sample`a'00_exp_val_cpo_`y'}/${sample`a'00_out_val_cpo_`y'}

				// export shares for dummy-validated zeros
				global sample`a'01_w_prex_ton_cpo_`y' = 100*${sample`a'01_exp_ton_cpo_`y'}/${sample`a'01_out_ton_cpo_`y'}
				global sample`a'01_w_prex_val_cpo_`y' = 100*${sample`a'01_exp_val_cpo_`y'}/${sample`a'01_out_val_cpo_`y'}

		}
	}

**Weighted means of prex_cpo (calculated for both quantities and values) AMONG NON-MISSING PRICE_CPO
	forvalues a = 1/2 {
		forvalues y = 1998/2015{
		//Weighted means
			// total export 
				// total exports for all zeros
				quietly tabstat exp_ton_cpo_imp`a' if year == `y' & cpo_price_imp`a'<., format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'10_exp_ton_cpo_`y' = stats[1,1]

				quietly tabstat exp_val_cpo_imp`a' if year == `y' & cpo_price_imp`a'<., format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'10_exp_val_cpo_`y' = stats[1,1]

				// total exports for dummy-validated zeros
				quietly tabstat exp_ton_cpo_imp`a' if year == `y' & cpo_price_imp`a'<. & sample_`a'z1==1, format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'11_exp_ton_cpo_`y' = stats[1,1]

				quietly tabstat exp_val_cpo_imp`a' if year == `y' & cpo_price_imp`a'<. & sample_`a'z1==1, format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'11_exp_val_cpo_`y' = stats[1,1]

			// total production
				// total production for all zeros
				quietly tabstat out_ton_cpo_imp`a' if year == `y' & cpo_price_imp`a'<. & prex_cpo_imp`a' <. , format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'10_out_ton_cpo_`y' = stats[1,1]

				quietly tabstat out_val_cpo_imp`a' if year == `y' & cpo_price_imp`a'<. & prex_cpo_imp`a' <. , format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'10_out_val_cpo_`y' = stats[1,1]

				// total production for dummy-validated zeros
				quietly tabstat out_ton_cpo_imp`a' if year == `y' & cpo_price_imp`a'<. & sample_`a'z1==1, format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'11_out_ton_cpo_`y' = stats[1,1]

				quietly tabstat out_val_cpo_imp`a' if year == `y' & cpo_price_imp`a'<. & sample_`a'z1==1, format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'11_out_val_cpo_`y' = stats[1,1]

			// export shares
				//export shares for all zeros
				global sample`a'10_w_prex_ton_cpo_`y' = 100*${sample`a'10_exp_ton_cpo_`y'}/${sample`a'10_out_ton_cpo_`y'}
				global sample`a'10_w_prex_val_cpo_`y' = 100*${sample`a'10_exp_val_cpo_`y'}/${sample`a'10_out_val_cpo_`y'}

				// export shares for dummy-validated zeros
				global sample`a'11_w_prex_ton_cpo_`y' = 100*${sample`a'11_exp_ton_cpo_`y'}/${sample`a'11_out_ton_cpo_`y'}
				global sample`a'11_w_prex_val_cpo_`y' = 100*${sample`a'11_exp_val_cpo_`y'}/${sample`a'11_out_val_cpo_`y'}

		}
	}

*For non-weighted mean of prex_cpo. 	
	forvalues a = 1/2 {
		forvalues y = 1998/2015{	
			// With all zero prex_cpo 
				// Among non-missing out_ton  
				quietly tabstat prex_cpo_imp`a' if year == `y' & out_ton_cpo_imp`a'<., statistics(mean) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'o0_1w_prex_cpo_`y' = stats[1,1]
				// Among non-missing out_val 
				quietly tabstat prex_cpo_imp`a' if year == `y' & out_val_cpo_imp`a'<., statistics(mean) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'v0_1w_prex_cpo_`y' = stats[1,1]
				// Among non-missing cpo_price 
				quietly tabstat prex_cpo_imp`a' if year == `y' & cpo_price_imp`a'<., statistics(mean) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'p0_1w_prex_cpo_`y' = stats[1,1]

			// With only dummy-validated zeros. 
				// Among non-missing out_ton  
				quietly tabstat prex_cpo_imp`a' if year == `y' & out_ton_cpo_imp`a'<. & sample_`a'z1==1, statistics(mean) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'o1_1w_prex_cpo_`y' = stats[1,1]
				// Among non-missing out_val 
				quietly tabstat prex_cpo_imp`a' if year == `y' & out_val_cpo_imp`a'<. & sample_`a'z1==1, statistics(mean) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'v1_1w_prex_cpo_`y' = stats[1,1]
				// Among non-missing cpo_price 
				quietly tabstat prex_cpo_imp`a' if year == `y' & cpo_price_imp`a'<. & sample_`a'z1==1, statistics(mean) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'p1_1w_prex_cpo_`y' = stats[1,1]
		}
	}


/*
		quietly tabstat exp_ton_cpo_imp`a' if year == `y', format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'0_exp_ton_cpo_`y' = stats[1,1]

				quietly tabstat exp_val_cpo_imp`a' if year == `y', format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'0_exp_val_cpo_`y' = stats[1,1]

			// total production
				quietly tabstat out_ton_cpo_imp`a' if year == `y' & prex_cpo_imp`a' <. , format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'0_out_ton_cpo_`y' = stats[1,1]

				quietly tabstat out_val_cpo_imp`a' if year == `y' & prex_cpo_imp`a' <. , format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'0_out_val_cpo_`y' = stats[1,1]

				replace OUT_ton_cpo_imp`a'0 = ${sample`a'0_out_ton_cpo_`y'} if year == `y'
				replace OUT_val_cpo_imp`a'0 = ${sample`a'0_out_val_cpo_`y'} if year == `y'

			// export shares
				global sample`a'0_w_prex_ton_cpo_`y' = 100*${sample`a'0_exp_ton_cpo_`y'}/${sample`a'0_out_ton_cpo_`y'}
				global sample`a'0_w_prex_val_cpo_`y' = 100*${sample`a'0_exp_val_cpo_`y'}/${sample`a'0_out_val_cpo_`y'}

				replace EXP_ton_cpo_imp`a'0 = ${sample`a'0_w_prex_ton_cpo_`y'} if year == `y'
				replace EXP_val_cpo_imp`a'0 = ${sample`a'0_w_prex_val_cpo_`y'} if year == `y'

		}
	}

**Weighted means of prex_cpo (calculated for both quantities and values) AMONG NON-MISSING PRICE_CPO
	forvalues a = 1/2 {		
		gen OUT_ton_cpo_imp`a'1 = .
		gen OUT_val_cpo_imp`a'1 = .
		gen EXP_ton_cpo_imp`a'1 = .
		gen EXP_val_cpo_imp`a'1 = .

		forvalues y = 1998/2015{
		//Weighted means
			// total export 

				quietly tabstat exp_ton_cpo_imp`a' if year == `y' & cpo_price_imp`a'<., format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'1_exp_ton_cpo_`y' = stats[1,1]

				quietly tabstat exp_val_cpo_imp`a' if year == `y' & cpo_price_imp`a'<., format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'1_exp_val_cpo_`y' = stats[1,1]


			// total production
				quietly tabstat out_ton_cpo_imp`a' if year == `y' & cpo_price_imp`a'<. & prex_cpo_imp`a' <. , format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'1_out_ton_cpo_`y' = stats[1,1]

				quietly tabstat out_val_cpo_imp`a' if year == `y' & cpo_price_imp`a'<. & prex_cpo_imp`a' <. , format statistics(sum) columns(variables) save
				matrix stats = r(StatTotal)
				global sample`a'1_out_val_cpo_`y' = stats[1,1]

				replace OUT_ton_cpo_imp`a'1 = ${sample`a'1_out_ton_cpo_`y'} if year == `y'
				replace OUT_val_cpo_imp`a'1 = ${sample`a'1_out_val_cpo_`y'} if year == `y'

			// export shares
				global sample`a'1_w_prex_ton_cpo_`y' = 100*${sample`a'1_exp_ton_cpo_`y'}/${sample`a'1_out_ton_cpo_`y'}
				global sample`a'1_w_prex_val_cpo_`y' = 100*${sample`a'1_exp_val_cpo_`y'}/${sample`a'1_out_val_cpo_`y'}

				replace EXP_ton_cpo_imp`a'1 = ${sample`a'1_w_prex_ton_cpo_`y'} if year == `y'
				replace EXP_val_cpo_imp`a'1 = ${sample`a'1_w_prex_val_cpo_`y'} if year == `y'
		}
	}
*/


forvalues y = 0/1 {
	forvalues year = 1998/2015{ 	
		display ${sample1`y'0_w_prex_ton_cpo_`year'}, ///
				${sample2`y'0_w_prex_ton_cpo_`year'}, ///
				${sample1`y'0_w_prex_val_cpo_`year'}, ///
				${sample2`y'0_w_prex_val_cpo_`year'}	
	}
}


forvalues year = 1998/2015{ 
	display ${sample1o0_1w_prex_cpo_`year'}, ///
			${sample2o0_1w_prex_cpo_`year'}, ///
			${sample1v0_1w_prex_cpo_`year'}, ///
			${sample2v0_1w_prex_cpo_`year'}, ///
			${sample1p0_1w_prex_cpo_`year'}, ///
			${sample2p0_1w_prex_cpo_`year'}
}


forvalues y = 0/1 {
	forvalues year = 1998/2015{ 
		display ${sample1`y'0_exp_ton_cpo_`year'}, ///
				${sample2`y'0_exp_ton_cpo_`year'}	
	}
}

forvalues y = 0/1 {
	forvalues year = 1998/2015{ 
		display ${sample1`y'0_out_ton_cpo_`year'}, ///
				${sample2`y'0_out_ton_cpo_`year'}	
	}
}

forvalues y = 0/1 {
	forvalues year = 2001/2005{ 	
		display ${sample1`y'1_w_prex_ton_cpo_`year'}, ///
				${sample2`y'1_w_prex_ton_cpo_`year'}, ///
				${sample1`y'1_w_prex_val_cpo_`year'}, ///
				${sample2`y'1_w_prex_val_cpo_`year'}	
	}
} 

/*
Comparing with Indoneasian Ministry of Agriculture time series of production and export of CPO indicates that: 
- our samples' (imp1 or imp2) mean export shares (weighted or not) are smaller than the population's. 
	This may come either from mismeasurement of prex variable (1) or from sampling error (2)
- weighted means are globally higher, which indicates that firms that have higher export shares 
	produce proportionally more than those who have smaller ones. 
	This is not obvious from a scatter plot however. 
- For several years (2013, 2009, 2008, 2007, 2006, 2004, 2003) imp2 export shares are much smaller than imp1's. 

	The export dummy can be used to identify true zeros. 
	But it is available only for years 2001, 2002, 2003 and 2005 
	And also we can impute 100% export share or something when answer is yes? 
	No, it is too uncertain, it could be any share, and it could still be 0 for cpo and positive for other commos. 

			Notes on this variable: 
			mention of "(by own or others)" from 2002 on, i.e. before 2002 the question is less clear. 
			2006 is weird: question is under another code in pdf: YTPREX instead of EKSPOR. but correctly named (EKSPOR06) in the excel file.
			It is always coded as: 1 = there is some export, 2 = there is no export from this establishment. 
	


So among those who have a missing export_dummy, most prex are null and 2 or 3 are positive, with "credible" prex. 

***What are the problems with sample export shares being smaller than population's?***
	This can come from different reasons and the problems induced depend on them. 
	1- A significant number of mills that actually did export have reported a null prex (measurement error)
	2- Our sample is exporting less than the population for some reason due to sampling method (sampling error)

Depending on what causes these small sample export shares, problems are different. 

1- In case of measurement error, either misreporting (i.e. under-estimating) one's prex (and in particular reporting zero while some share of output was exported) is 
	a) a random phenomenon 
	b) something that occurs systematically for some kind of mills. 

2- In case of sampling error, either 
	a) only chance explains that we have sampled less-exporting mills 
	b) or there is a reason explaining why only less-exporting mills were surveyed

As there is no hint at case 2, nor way to test it, let's focus on case 1.
*Case 1 is also difficult to test once we have kept only validated zeros and export shares are still too small and the mismeasurement is then only (mainly) on positive reported answers. 

If we keep all the zeros: 
One way to test which of 1-a or 1-b is most likely is to look at the difference in the distributions of some other features of mills between 
the group deemed as likely misreporting and the group of positive prex it is supposed to belong to. 
The latter are those who reported positive prex. 
The former can be mills reporting null prex without confirming with the export_dummy (export_dummy_imp !=2)
or mills reporting null prex and missing export_dummy. 

The p values of the Kolmogorov-smirnov test are the answers to this question:  
	"If the two samples were randomly sampled from identical populations, 
	what is the probability that the two cumulative frequency distributions would be as far apart as observed?"
Hence a significantly low p-value would mean that such a difference in the distributions of the two samples is very unlikely under the hypothesis 
that they belong to a common supra population. 

So for null prex this is 
  2001 140 out of which 59 are not confirmed by the dummy (i.e. it's !=2), and 39 are missing on the dummy. 
  2002 137 out of which 70 are not confirmed by the dummy, and 59 are missing on the dummy. 
  2003 162 out of which 46 are not confirmed by the dummy, and 44 are missing on the dummy. 
  2004 212 out of which 212 are not confirmed by the dummy, and 212 are missing on the dummy. 
  2005 195 out of which 42 are not confirmed by the dummy, and 36 are missing on the dummy. 

Let's compare the distributions within these different groups: 
*/
*just add or remove the condition export_dummy_imp!=1 to go to the comparison with missing dummy zero prex. 
forvalues a=1/2{
	drop misrep_imp`a'
	gen misrep_imp`a' = (prex_cpo_imp`a'==0 & export_dummy_imp!=2) if prex_cpo_imp`a'<. & (year == 2001 | year == 2002 | year == 2003 | year == 2005)   
	replace misrep_imp`a' = . if prex_cpo_imp`a' ==0 & export_dummy_imp==2
}
*Donc misrep est nul si prex_cpo_imp`a' est positif, ou si export_dummy_imp==2, or on ne veut pas du groupe de ceux qui ont une prex nulle et dummy==2 
*donc on les retire

*and to define the sample of misreporters based on "export_dummy is missing":
forvalues a=1/2{
	*drop misrep_imp`a'
	gen misrep_imp`a' = (prex_cpo_imp`a'==0 & export_dummy_imp>=.) if prex_cpo_imp`a'<. & (year == 2001 | year == 2002 | year == 2003 | year == 2005)   
	// here again the group these misreporters should come from does not include those who don't export cpo and answered the dummy. 
	replace misrep_imp`a' = . if prex_cpo_imp`a' ==0 & (export_dummy_imp==2 | export_dummy_imp==1)
}

forvalues y=2001/2005{
	inspect misrep_imp2 if year == `y'
}

**with sum**
forvalues a=1/2 {
	sum cpo_price_imp`a' if misrep_imp`a' == 0, detail
	sum cpo_price_imp`a' if misrep_imp`a' == 1, detail
}

**With Ksmirnov**
forvalues a=1/2 {
	ksmirnov cpo_price_imp`a', by(misrep_imp`a') exact
}

forvalues a=1/2 {
	ksmirnov out_ton_cpo_imp`a', by(misrep_imp`a') exact
}

**With qqplot**
forvalues c=0/1{
	forvalues a=1/2{
		*drop  cpo_price_imp`a'_`c'
		gen double cpo_price_imp`a'_`c' = cpo_price_imp`a' if misrep_imp`a'==`c'
	}
}
qqplot cpo_price_imp1_0 cpo_price_imp1_1 

forvalues c=0/1{
	forvalues a=1/2{
		*drop  cpo_price_imp`a'_`c'
		gen double out_ton_cpo_imp`a'_`c' = out_ton_cpo_imp`a' if misrep_imp`a'==`c'
	}
}
qqplot out_ton_cpo_imp2_0 out_ton_cpo_imp2_1 

/*For prices there is a pattern: quantiles of exposed group are higher than quantiles of misreporters. Meaning the former have higher cpo_price. 
And meaning they apparently don't come from the same population, they are not alike, 
misreporting prex is not something that hits any exporting firm randomly, so we shall not make the assumption that the TEs in 
the two samples will be the same. (even though quantity wise they are alike)

We had also done this previously between misreporters and true reporters of zero prex, and also found a diff in the distributions. 

BUT, this whole thing is not a problem anymore when one uses only zeros confirmed by the dummy. 
YET, the export shares are still too low compared with the population's, and even with the imputation that
replaced prex_cpo_imp1 with export_pct_imp when this was positive also (though for other years than those with the dummy
the sample export shares increase significantly with this compared to imp2 and imp1 more conservative.) 

Checks of gaps of 0s or missing (eventhough it is pretty clear that it is rather the positive prex that are misreported now):
I don't think it changed everything but at least it is a bit cleaner with it. So let's not recompute all the export shares just because of this little change. 

And then wait for the dummy to come for all years and see what it gives. 
If we use the dummy to confirm all the zeros, we need to know better why there are so many missings in 2013-2015 because otherwise 
all other years will be pretty cleaned zero-wise while these years will have too many imputations from (confirmed) export_pct==0 RELATIVE to the few non-missing and 
hence positive prex_cpo they have. A possibilty would be to make the extreme imputation "prex_cpo = export_pct if prex missing and export_pct >0" only in these years. 

Besides: 
start doing the outcome variable for the panel estimation
estimate the first stage as such when come the prices from Hanif
start thinkg about the macro stuff. 
*/
******* 
*     *
*     *
*     *
*     *
*******
/*
What would be the consequences of 1-a and 1-b? 
The situation is that many obs. are likely misreporters who reported a null prex while some of their cpo was actually exported.
Hence when using these obs. as controls, we will be using obs that actually have a bigger treatment effect than what what is calculated in our estimation. 


*/ 








*****************************************************************************************************************************************************************************
***Descriptive statistics***
*****************************************************************************************************************************************************************************


