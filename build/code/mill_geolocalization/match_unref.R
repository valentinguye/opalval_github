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
install.packages("sf", source = TRUE)
library(sf)

neededPackages = c("data.table","plyr", "dplyr", "tidyr", "readxl","foreign", "data.table", "readstata13", "here",
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

tic()
match$n_match2 <- NA
f <- function(i){
  kab_filter <- str_contains(x = match$district_name[i], pattern = match$y[[i]]$address, ignore.case = TRUE, switch = TRUE) 
  # keep only the matches that are in the same district
  match$y[[i]] <- filter(match$y[[i]], kab_filter)
  # report the number of different mills (based on COMPANY NAME) that matched
  match$n_match2[i] <- length(unique(match$y[[i]]$company_name)) 
  return(match$n_match2[i])
}
match$n_match2 <- sapply(1:nrow(match), f)
toc()

table(match$n_match2, match$any_cpo_output)


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

ddply(match, "i_n_match", summarise, 
      n_mills = length(unique(firm_id)))

# la question est est-ce qu'on décide de valider systématiquement les cas où il n'y a zéro ou qu'un seul match toujours identique entre les années d'une mill ibs. 
# on pourrait dire : oui à condition qu'il y ait au moins deux occurrences de ce match. 
# ou même pas, manuellement on avait validé même quand il n'y avait qu'une obs. qui matchait.
# une partie de ces cas sont écartés ensuite pendant la phase de résolution des conflits. 



# BON ON EN EST LA, EN GROS IL FAUT TROUVER UN MOYEN DE FAIRE CES 80 OBSERVATIONS RAPIDEMENT, C'EST A DIRE 
# mettre toutes les infos pertinentes dans un format ou sera facile de faire le tri manuellement. 

# ON POURRAIT RAJOUTER une condition de combien de fois chaque mill de MD match, pour une même mill de IBS











