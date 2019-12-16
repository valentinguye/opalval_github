********************************************************************************
* Master do file for the opal repo - works more like readme currently but should
* be turned into an executable do file with shell commands calling R scripts
********************************************************************************

/// THIS GLOBAL IS USED IN EVERY PATH IN ALL STATA SCRIPTS \\\ still needs to be addressed for R scripts. 
*INRA 
global base_path_wd "C:\Users\GUYE\Desktop\opalval_migrant"
*MCC
global base_path_wd "C:\Users\guyv\ownCloud\opalval"

cd $base_path_wd

///// download
	//// extract palm oil mills from IBS main input/output datasets (IBS_IO) - and clean measurement unit and reshape. 
	IBS_inputs_preparation.do
	* input: \download\input\IBS_IO\IBS_inputs    downloaded here from C:\Users\guyv\ownCloud\opal (2)\download\output\IBS_IO
	* output: \download\input\prepared_IO\IBS_inputs_prep.dta
	IBS_outputs_preparation.do
	* input: \download\input\IBS_IO\IBS_outputs   downloaded here from C:\Users\guyv\ownCloud\opal (2)\download\output\IBS_IO
	* output: \download\input\prepared_IO\IBS_outputs_prep.dta
	
	//// merge them together and with main IBS. 
	merge_IBSmain_IBSIO.do
	* input: \download\input\prepared_IO\IBS_outputs_prep.dta
	*		 \download\input\prepared_IO\IBS_inputs_prep.dta
	*		 \download\input\IBS_full\si_panel.dta  (Rothenberg data) uploaded here from C:\Users\guyv\ownCloud\opal (2)\download\input\IBS_full\si_panel.dta
	* 		 \download\input\IBS_final_panel.dta  (Sebastian cleaned data) uploaded here from C:\Users\guyv\ownCloud\opal (2)\build\output\IBS_final_panel.dta
	
	* output: \download\output\IBS_1998.dta

///// build
	//// clean IBS mill dataset. 
		// some preparatory work with district and village crosswalks
		prepare_crosswalks.do
		* input 	\build\input\mill_geolocalization\District-Proliferation-Crosswalk_complete.csv
		*           \build\input\mill_geolocalization\desa_crosswalk_1998_2014.csv 

		* output:   \build\input\mill_geolocalization\province_district_code_names_93_2016.dta
		*		    \build\input\mill_geolocalization\desa_code_names_98_2014.dta
		
		// clean IBS_1998 (including geographic variables)
		cleaning_IBS.do
		* input: \download\output\IBS_PO_98_15.dta
		*		 \download\input\IBS_final_panel.dta
		*        \build\input\mill_geolocalization\IBS_base_prelu.dta
		*        \build\input\mill_geolocalization\province_district_code_names_93_2016.dta
		*        \build\input\mill_geolocalization\desa_code_names_98_2014.dta

		* output: \build\output\IBS_PO_98_15_cleaned.dta

	//// geo-localize IBS
		/// automatic matching with Heilmayr mill list. 
			// give the true min_year ´(potentially < 1998) and keep mill obs. of most recent year with a valid desa_id
			keep_valid_recent_desa.do
			* input: \build\output\IBS_PO_98_15_cleaned.dta
			* output: \build\input\mill_geolocalization\IBSmills_valid_desa.dta

			// give IBS mills a desa geometry
			make_IBSmills_desageom.R 
			* input: \build\input\mill_geolocalization\IBSmills_valid_desa.dta
			* 		 \build\input\mill_geolocalization\village_shapefiles\desa_1998_crosswalked.shp 
			*		 ... 
			*        \build\input\mill_geolocalization\village_shapefiles\desa_2009_crosswalked.shp
			* 		 \build\input\mill_geolocalization\village_shapefiles\desa_map_2010\indo_by_desa_2010.shp
			
			* output: \build\input\mill_geolocalization\IBSmills_desageom.Rdata

			// perform spatial matching and output distinct subsets that will need different processing
			IBS_heilmayr_matching.R
			* input: \build\input\mill_geolocalization\IBSmills_desageom.Rdata
			*  		 \build\input\mill_geolocalization\traseMills_capEstyear.xlsx
			
			* output: \build\input\mill_geolocalization\pre2011_bad_desa_id.dta
			* 		  \build\input\mill_geolocalization\unreferenced_mill_desa.shp (driver geojson)
			*		  \build\input\mill_geolocalization\ibs_unref.dta
			* 		  \build\input\mill_geolocalization\oto.dta
			* 		  \build\input\mill_geolocalization\noto.dta


		/// manual matching / geo-localization
			// Take sub-dataset of mills with observations only since 2011 included and thus surely no desa_id.
			georeferencing_IBSmills_post2010.do
			* input: \build\input\mill_geolocalization\province_district_code_names_2011_15.xlsx
			*  		 \build\output\IBS_PO_98_15_cleaned.dta
			
			* output: \build\input\mill_geolocalization\mills_to_georeference.xls

			// Take sub-datasets of mills with at least one observation before 2011 but that cannot match automatically (no valid desa_id, or conflicting matches - "noto"). 
			georeferencing_IBSmills_pre2011.do
			* input: \download\input\IBS_final_panel.dta
			*		 \build\output\IBS_PO_98_15_cleaned.dta
			* 		 \build\input\mill_geolocalization\pre2011_bad_desa_id.dta
			* 		 \build\input\mill_geolocalization\noto.dta

			* output: \build\input\mill_geolocalization\mills_to_georeference_pre2011.xls
			* 		  \build\input\mill_geolocalization\noto.xls  

			// MANUAL WORK; **NO CODE**
			* input: \build\input\mill_geolocalization\mills_to_georeference.xls
			* 		 \build\input\mill_geolocalization\mills_to_georeference_pre2011.xls
			* 		 \build\input\mill_geolocalization\noto.xls 

			* output:  \build\input\mill_geolocalization\mills_to_georeference_post2010_done.xls
			* 		   \build\input\mill_geolocalization\mills_to_georeference_pre2011_done.xlsx
			* 		   \build\input\mill_geolocalization\noto_done.xls

		/// merge automatic and manual works - with many manual cleaning of duplicates.  
		merging_geolocalization_works.do
		* input: \build\input\mill_geolocalization\mills_to_georeference_post2010_done.xls 	 sheet("mills to georef")
		*  	     \build\input\mill_geolocalization\mills_to_georeference_pre2011_done.xlsx	 sheet("Mills to georef")
		*  	     \build\input\mill_geolocalization\noto_done.xls 	sheet("Sheet1")
		*  		 \build\input\traseMills_capEstyear.xlsx 	sheet("traseMills_capEstyear")
		*  	     \build\output\IBS_PO_98_15_cleaned.dta

		* output: \build\input\mill_geolocalization\mills_to_georeference_post2010_done.dta 	 
		*  	      \build\input\mill_geolocalization\mills_to_georeference_pre2011_done.dta	 
		*  	      \build\input\mill_geolocalization\noto_done.dta 	
		* 		  \build\input\traseMills_capEstyear_selected.dta

		*  	      \build\output\IBS_mills_geolocalized.dta
		*  		  \build\input\mill_geolocalization\IBS_mills_geolocalized.dta

	//// build outcome variables 
		/// prepare annual maps of deforestation according to different definitions
		prepare_deforestation.R 
		* input: \build\input\mill_geolocalization\IBS_mills_geolocalized.dta

		/// Extract deforestation maps imputable to oil palms around georeferenced mills (those from Heilmayr AND those added from manual works)
		extract_deforestation.R


///// analysis
