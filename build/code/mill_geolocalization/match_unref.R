# SO, what we wanna do here contributes to the effort of georeferencing IBS mills. 
# More specifically; we want to georeference those mills that have an a priori valid desa_id
# but no referenced (UML) mill is within the associated village polygons.
# In many cases, the IBS observation is actually not a mill but rather a refinery or an other factory using some CPO in its inpputs. 
# There are still cases where these observations are mills. They are then either 
# - UML mills actually laying slightly outside the polygon they reported in IBS. 
# - Not UML mills. 

# In the first case, we want to geolocalize them using the coordinates known in UML. 
# To do so, we need to associate a name to IBS unref mills. 
# This can be done thanks to the manufacturing directories. 


# First, screen (flag) only ibs_unref mills that produce at least once some CPO. 
# For each of them, there are several obs. of number of workers. 
# report all the lines in MD that match with one of these observations; 
# AND whose adresses encompasse the IBS mill's district. 

# How to 

rm(list = ls())

# PACKAGES
#install.packages("sf", source = TRUE)
library(sf)

neededPackages = c("data.table","plyr", "dplyr", "tidyr", "readxl", "writexl", "foreign", "data.table", "readstata13", "here",
                   "rgdal", "sjmisc", "stringr","Hmisc", "doBy", 
                   "rlist", "parallel", "foreach", "iterators", "doParallel" )

allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}
# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)

# no CRAN packages
if (!require(devtools)) install.packages("devtools")
# package tictoc
devtools::install_github("jabiru/tictoc")
library(tictoc)


### LOCAL WORKING DIRECTORY, just to shorten calls.
setwd(here("./build/input/mill_geolocalization/matching_unref"))


# read in the digitized manufacturing directories.
md <- read_excel("direktori_industri_merged_cleaned.xlsx")
# read in the full IBS panel 
ibs <- read.dta13(here("./build/output/IBS_PO_98_15_cleaned.dta"))

# get only firm ids of unref "mills". 
unref_cross <- readRDS(here("./build/input/mill_geolocalization/ibs_unref.Rdata"))
unref_cross <- unref_cross[,"firm_id"]
unref_cross <- st_set_geometry(unref_cross, NULL)



# get every annual records of each unref mill.
unref <- merge(ibs, unref_cross, by = "firm_id")
setorder(unref, firm_id, year)

length(unique(unref[,"firm_id"]))  == nrow(unref_cross)

# flag those mills who never sell CPO
unref_dt <- data.table(unref)
cpo <- unref_dt[, mean(out_ton_cpo, na.rm = TRUE), by = firm_id]
names(cpo)[names(cpo) != "firm_id"] <- "any_cpo_output"
cpo[,"any_cpo_output"] <- !is.na(cpo[,"any_cpo_output"])

unref <- merge(unref, cpo, by = "firm_id", all = TRUE)
length(unique(unref[unref$any_cpo_output == TRUE,"firm_id"]))
# 268 different mills have produced CPO at least once. 



## Match to each unref record, all the records of MD that have the same number of workers (i.e. be it the same year or not, same district or not)

#this adds a list column. There is one list element for each unref row. 
match <- nest_join(unref, md, by = c("workers_total_imp3" = "no_workers"), keep = TRUE)
# each list element is a dataframe
class(match$y[[1]])

## Restrict to matches that are in the same district
#some cleaning of the district name variable (no need to bother about case, bc its handled in the function)
match$district_name <- str_replace(string = match$district_name, pattern = "Kab. ", replacement = "")
# not elegant but enables that empty district_names are not matched with adresses in MD
match$district_name[match$district_name == ""] <- "123456789xyz"

tic()
match$n_match <- NA
for(i in 1:nrow(match)){
  #specified as such (with switch = T), the function checks whether x is in any of the patterns (wierd phrasing but that's the way to go with this function)
  kab_filter <- str_contains(x = match$district_name[i], pattern = match$y[[i]]$address, ignore.case = TRUE, switch = TRUE) 
  # keep only the matches that are in the same district
  match$y[[i]] <- filter(match$y[[i]], kab_filter)
  # report the number of different mills that matched
  match$n_match[i] <- length(unique(match$y[[i]]$company_name)) 
}
toc()

# tic()
# match$n_match2 <- NA
# f <- function(i){
#   kab_filter <- str_contains(x = match$district_name[i], pattern = match$y[[i]]$address, ignore.case = TRUE, switch = TRUE) 
#   # keep only the matches that are in the same district
#   match$y[[i]] <- filter(match$y[[i]], kab_filter)
#   # report the number of different mills (based on COMPANY NAME) that matched
#   match$n_match2[i] <- length(unique(match$y[[i]]$company_name)) 
#   return(match$n_match2[i])
# }
# match$n_match2 <- sapply(1:nrow(match), f)
# toc()
# 
# table(match$n_match2, match$any_cpo_output)


## Make different categories of establishments, depending on how many different matches they have over their records with MD mills. 

# make groups of establishments, based on how many matches they have repeatedly
grp_n_match <- ddply(match, "firm_id", summarise, 
                     # those establishments that never match
                      no_match = length(n_match[n_match == 0])==length(year), 
                     # one_match category allows for some records, but not for all, to have zero match. 
                     # single matches can be different from one year to another. 
                      one_match = length(n_match[(n_match == 1 | n_match == 0) & no_match == FALSE])==length(year), 
                     # svl_match is true as soon as their is as least one year with 2 different matches. 
                      svl_match = no_match == FALSE & one_match == FALSE)

# ddply note: summarise is the .fun argument and then in ... we give the ... argument of summarise, 
# i.e. we give the "name = value pairs" (see ?summarise). 
match <- merge(match, grp_n_match, by = "firm_id") 


ddply(cfl_resolved, "firm_id", summarise, e <- all_na(company_name))

# substract from the one_match category those that have different company name matches across years.
for(i in unique(match[match$one_match == TRUE, "firm_id"])){
  # extract the names of all the MD matches of establishment i within the one_match category 
  names <- lapply(match[match$firm_id == i, "y"], function(i.elmt) i.elmt$company_name)
  names <- unlist(names)
  # for those who have matched different company names across years, switch one_match from TRUE to FALSE 
  new_logicals <- rep(FALSE, nrow(match[match$firm_id == i,]))
  match[match$firm_id == i, "one_match"][length(unique(names)) > 1] <- new_logicals
  # for those who have matched different company names across years, switch svl_match from FALSE to TRUE
  new_logicals <- rep(TRUE, nrow(match[match$firm_id == i,]))
  match[match$firm_id == i, "svl_match"][length(unique(names)) > 1] <- new_logicals
}
# checks: 
# when the company name is the same across annual matches, the one_match status remains unchanged
match[match$firm_id == 2028, "y"]
grp_n_match[grp_n_match$firm_id == 2028,]
match[match$firm_id == 2028, c("no_match","one_match", "svl_match")]

# when the company name is not the same across annual matches, the one_match status changes from TRUE to FALSE and the svl_match from FALSE to TRUE
match[match$firm_id == 2076, "y"]
grp_n_match[grp_n_match$firm_id == 2076,]
match[match$firm_id == 2076, c("no_match","one_match", "svl_match")]

# descriptive part
describe(match[match$no_match == TRUE, "firm_id"]) # 371 establishments (1697 records)
describe(match[match$one_match == TRUE, "firm_id"]) # 111 establishments (924 records)
describe(match[match$svl_match == TRUE, "firm_id"]) # 110 establishments (1399 records)

match$i_n_match <- "never matches with anything"
match$i_n_match[match$one_match == TRUE] <- "matches always with the same company name or with nothing"
match$i_n_match[match$svl_match == TRUE] <- "matches with several company names, either the same year or across years"

ddply(match, c("i_n_match","any_cpo_output"), summarise, 
      n_mills = length(unique(firm_id)))

# la question est est-ce qu'on décide de valider systématiquement les cas où il n'y a zéro ou qu'un seul match toujours identique entre les années d'une mill ibs. 
# on pourrait dire : oui à condition qu'il y ait au moins deux occurrences de ce match. 
# ou même pas, manuellement on avait validé même quand il n'y avait qu'une obs. qui matchait.
# une partie de ces cas sont écartés ensuite pendant la phase de résolution des conflits. 

cfl <- match[match$svl_match == TRUE & match$any_cpo_output == TRUE, ]
length(unique(cfl$firm_id))


### Prepare the data to resolve "within" conflicts, i.e. conflicts in matched company names within each firm_id. 

# add variables on the total different matches for one ibs establishment.  
cfl$diff_names <- rep(NA, nrow(cfl))
cfl$n_diff_names <- rep(NA, nrow(cfl))
for(i in unique(cfl$firm_id)){
  names <- lapply(cfl[cfl$firm_id == i, "y"], function(i.elmt) i.elmt$company_name)
  cfl[cfl$firm_id == i, "diff_names"] <- paste(unique(unlist(names)), collapse = "; ")
  cfl[cfl$firm_id == i, "n_diff_names"] <- length(unique(unlist(names)))
}

#cfl[cfl$firm_id == 1763, "y"]


# extract all the matches from the list column 
l <- list()
for(i in 1:nrow(cfl)){
    s <- cfl[i, "y"][[1]]
    s$matched_firm_id <- rep(cfl[i, "firm_id"], nrow(cfl[i, "y"][[1]]))
    s$matched_year <- rep(cfl[i, "year"], nrow(cfl[i, "y"][[1]]))
    l[[i]]<- s
}
md_matches <- bind_rows(l)
rm(l)


# and merge them with the panel cfl
# rename first the year variable in md_matches
names(md_matches)[names(md_matches) == "year"] <- "md_year"
cfl <- merge(cfl, md_matches, by.x = c("firm_id","year"), by.y = c("matched_firm_id", "matched_year"), all = TRUE)

# export relevant variables to manual work
cfl <- dplyr::select(cfl,	firm_id, min_year, year,	workers_total_imp3,	n_diff_names, diff_names,  
                     md_year, company_name, no_workers, 
                     district_name, kec_name,	village_name, address,
                     main_product, 
                     in_ton_ffb_imp1,	in_ton_ffb_imp2, out_ton_cpo_imp1,	out_ton_cpo_imp2,	out_ton_pko_imp1, out_ton_pko_imp2,	
                     out_ton_rpo_imp1, out_ton_rpo_imp2, out_ton_rpko_imp1, out_ton_rpko_imp2,
                     pct_own_cent_gov_imp,	pct_own_loc_gov_imp,	pct_own_nat_priv_imp,	pct_own_for_imp)

write_xlsx(cfl, "unref_cfl.xlsx")



### now we want to resolve "between" conflicts, i.e. conflicts in matched company names between IBS firm_ids. 
# For this purpose, it is necessary to have all years for each ibs firm_id in the two categories 
# (resolved conflict cases and one_match cases (only those who produced CPO at least once))

## For resolved within conflict cases, one can keep only the lines of cfl that have the company_name equal to the resolved one. 
# import mannually done work 
cfl_resolved <- read_excel("unref_cfl_done.xlsx")

# for each firm_id, keep only the row with the mannually deemed correct company name. 
cfl_resolved <- cfl_resolved[is.na(cfl_resolved$company_name)== FALSE,] 

length(unique(cfl_resolved$company_name)) 
# So there was 104 different firm_id that had within conflicting md company names. 
# For 102 of them, we could resolve the within conflict. 
# Among them, there are only 100 unique company names, meaning that there are some between conflicts. 

# rename the company name variable 
names(cfl_resolved)[names(cfl_resolved) == "company_name"] <- "within_resolved_c_name"

# merge it with the cfl data frame, 
cfl2 <- merge(cfl, cfl_resolved[, c("firm_id", "within_resolved_c_name")], by = c("firm_id"), all = TRUE)

# we don't keep only the records where the company_name is the one that was chosen mannually, because in some cases we might  
# need records for the same mill but with a name differently spelled. 


## For one_match cases, one should just reproduce the procedure applied to prepare data for resolution of within conflicts. 

no_cfl <- match[match$one_match == TRUE & match$any_cpo_output == TRUE,]

# add variables on the total different matches for one ibs establishment.  
no_cfl$diff_names <- rep(NA, nrow(no_cfl))
no_cfl$n_diff_names <- rep(NA, nrow(no_cfl))
for(i in unique(no_cfl$firm_id)){
  names <- lapply(no_cfl[no_cfl$firm_id == i, "y"], function(i.elmt) i.elmt$company_name)
  no_cfl[no_cfl$firm_id == i, "diff_names"] <- paste(unique(unlist(names)), collapse = "; ")
  no_cfl[no_cfl$firm_id == i, "n_diff_names"] <- length(unique(unlist(names)))
}

#no_cfl[no_cfl$firm_id == 1763, "y"]


# extract all the matches from the list column 
l <- list()
for(i in 1:nrow(no_cfl)){
  s <- no_cfl[i, "y"][[1]]
  s$matched_firm_id <- rep(no_cfl[i, "firm_id"], nrow(no_cfl[i, "y"][[1]]))
  s$matched_year <- rep(no_cfl[i, "year"], nrow(no_cfl[i, "y"][[1]]))
  l[[i]]<- s
}
md_matches <- bind_rows(l)
rm(l)


# and merge them with the panel no_cfl
# rename first the year variable in md_matches
names(md_matches)[names(md_matches) == "year"] <- "md_year"
no_cfl <- merge(no_cfl, md_matches, by.x = c("firm_id","year"), by.y = c("matched_firm_id", "matched_year"), all = TRUE)

# export relevant variables to manual work
no_cfl <- dplyr::select(no_cfl,	firm_id, min_year, year,	workers_total_imp3,	n_diff_names, diff_names,  
                     md_year, company_name, no_workers, 
                     district_name, kec_name,	village_name, address,
                     main_product, 
                     in_ton_ffb_imp1,	in_ton_ffb_imp2, out_ton_cpo_imp1,	out_ton_cpo_imp2,	out_ton_pko_imp1, out_ton_pko_imp2,	
                     out_ton_rpo_imp1, out_ton_rpo_imp2, out_ton_rpko_imp1, out_ton_rpko_imp2,
                     pct_own_cent_gov_imp,	pct_own_loc_gov_imp,	pct_own_nat_priv_imp,	pct_own_for_imp)


## Spot the between conflicts
# have the same column in both data frames
no_cfl$within_resolved_c_name <- no_cfl$company_name
# merge them 
btw_cfl <- merge(cfl2, no_cfl, all = TRUE)

# now select only duplicates across firm_id
btw_duplicates <- ddply(btw_cfl, c("within_resolved_c_name"), summarise, 
      btw_duplicates = length(unique(firm_id)))
btw_cfl <- merge(btw_cfl, btw_duplicates, by = "within_resolved_c_name", all = TRUE)
btw_cfl <- btw_cfl[btw_cfl$btw_duplicates]

describe(btw_cfl$btw_duplicates)

# ON EN EST LA? SEE WHAT IS THIS 79, AND FILTER ON THE VALUES OF btw_duplicates

btw_cfl 
btw_cfl2<- btw_cfl[duplicated((btw_cfl$within_resolved_c_name)|duplicated(btw_cfl$within_resolved_c_name, fromLast = TRUE)),]
#################################################################################################################



### merge this company name variable to unref, both for 
## For one_match cases
no_cfl <- match[match$one_match == TRUE & match$any_cpo_output == TRUE,]

# extract all the matches from the list column 
l <- list()
for(i in 1:nrow(no_cfl)){
  s <- no_cfl[i, "y"][[1]]
  s$matched_firm_id <- rep(no_cfl[i, "firm_id"], nrow(no_cfl[i, "y"][[1]]))
  s$matched_year <- rep(no_cfl[i, "year"], nrow(no_cfl[i, "y"][[1]]))
  l[[i]]<- s
}
md_matches.no_cfl <- bind_rows(l)
rm(l)

names(md_matches.no_cfl)[names(md_matches.no_cfl) == "year"] <- "md_year"

length(unique(no_cfl$firm_id)) # it's 87 different firm_id
length(unique(md_matches.no_cfl$company_name)) # that have matched with only 82 company names. 
# we want to keep the 87 different IBS records, with their duplicates in company names. 
anyNA(md_matches.no_cfl$company_name) # company names are never missing
# so we just extract the first instance of duplicated firm_id. 
md_matches.no_cfl <- md_matches.no_cfl[!duplicated(md_matches.no_cfl$matched_firm_id),]

md_matches.no_cfl$no_cfl <- 1 

cfl <- merge(cfl, md_matches.no_cfl[, c("firm_id", "year","no_cfl")], by.x = c("firm_id","year"), by.y = c("matched_firm_id", "matched_year"), all = TRUE)

unref <- merge(unref, no_cfl[, c("firm_id", "year", "company_name")], by = c("firm_id", "year"), all = TRUE)


## For resolved conflicting cases
# import done manual work 
cfl_resolved <- read_excel("unref_cfl_done.xlsx")

# keep only the row with the mannually deemed correct company name. 
cfl_resolved <- cfl_resolved[is.na(cfl_resolved$company_name)== FALSE,] # (In 2 IBS mills cases it was not possible to resolve the conflict.) 

#merge
unref <- merge(unref, cfl_resolved[, c("firm_id", "year", "n_diff_names","diff_names", "company_name")], by = c("firm_id", "year"), all = TRUE)


length(unique(md_matches.no_cfl$matched_firm_id))





