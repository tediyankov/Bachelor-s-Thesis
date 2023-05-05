
## DATA TRANSFORMATION

## Preliminaries ===============================================================

## empty environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, haven, vdemdata, caTools, ggpubr, ISLR2, boot, mice)

## file_path
base_path <- file.path ("~/Desktop/dissertation_data")



## Assembling data with NAs   ==================================================

## Ethnicity data --------------------------------------------------------------

## loading in raw data 
er_raw <- read.csv (file.path (base_path, "ER-2021.csv"))
tek_raw <- read.csv (file.path (base_path, "TEK-2021.csv"))

## cleaning ER data
er <- er_raw %>%
  
  # recoding countries with their ISO3 codes
  mutate (coa = countrycode (coa, "country.name", "iso3c"),
          coo = countrycode (coo, "country.name", "iso3c")) %>%
  
  # removing NAs in countries
  filter (!(is.na (coa))) %>%
  filter (!(is.na (coo))) %>%
  
  # keep only relevant vars 
  dplyr::select (year, coa, coo, totalrefugees, groupname1, groupname2, groupname3)

## cleaning TEK data
tek <- tek_raw %>%
  
  # keep only relevant vars 
  dplyr::select (c("statename", "groupname")) %>%
  
  # recoding countries with their ISO3 codes
  mutate (statename = ifelse (statename == "Czechoslovakia", "CZE", 
                              ifelse (statename == "German Democratic Republic", "DEU",
                                      ifelse (statename == "Korea, People's Republic of", "KOR", 
                                              ifelse (statename == "Kosovo", "Serbia", 
                                                      ifelse (statename %in% c("Yemen (Arab Republic of Yemen)", "Yemen, People's Republic of"), "YEM", 
                                                              countrycode (statename, "country.name", "iso3c")
                                                      )))))) %>%
  filter (!(is.na (statename))) %>%
  
  # namually adding Roma after breaking up Czechoslovakua into CZE
  rbind (c("SVK", "Roma")) %>%
  
  # renaming pre-join
  rename_with (
    ~ case_when (
      . == "statename" ~ "coa",
      . == "groupname" ~ "ethnic_grp",
      TRUE ~ .
    )
  ) %>%
  
  # reformatting to have one row = onw country
  group_by (coa) %>% 
  dplyr::summarize(ethnic_grp = paste(ethnic_grp, collapse = ", "))

## Computing ethnic linkage var
dat1 <- er %>%
  
  # creating year-coo-coa dyad variable
  unite ("orig_dest_year", c(coo, coa, year), sep = "_", remove = F, na.rm = F) %>%
  
  # joining ER and TEK 
  left_join (tek, by = "coa") %>%
  
  # filtering out problematic values
  filter (!grepl('/', groupname1)) %>%
  filter (!is.na (ethnic_grp)) %>%
  
  # fixing issues
  mutate (groupname2 = ifelse (groupname2 == "Mandingue (and other eastern groups", "Mandingue", groupname2)) %>%
  mutate (groupname1 = ifelse (groupname1 == "Konkomba (Northern groups", "Konkomba", groupname1))

## filling in empty spaces with NAs
grourvars <- c("groupname1", "groupname2", "groupname3")
for (i in grourvars) {
  
  dat1[,i] <- ifelse (dat1[,i] == "", NA, dat1[,i])
  
}

## computing ethnic linkgage variables 
for (i in 1:nrow (dat1)) {
  
  dat1$eth_link_1 [i] <- ifelse (grepl (dat1$groupname1 [i], dat1$ethnic_grp [i]), 1, 0)
  dat1$eth_link_2 [i] <- ifelse (grepl (dat1$groupname2 [i], dat1$ethnic_grp [i]), 1, 0)
  dat1$eth_link_3 [i] <- ifelse (grepl (dat1$groupname3 [i], dat1$ethnic_grp [i]), 1, 0)
  
}

# final cleaning 
dat2 = dat1 %>%
  
  # renaming vars 
  rename_with (
    ~ case_when (
      . == "coa" ~ "dest",
      . == "coo" ~ "orig",
      TRUE ~ .
    )
  ) %>%
  
  #  creating single ethnic linkage variable
  
  # turn NAs into 0s
  mutate (eth_link_1 = ifelse (is.na (eth_link_1), 0, eth_link_1), 
          eth_link_2 = ifelse (is.na (eth_link_2), 0, eth_link_2), 
          eth_link_3 = ifelse (is.na (eth_link_2), 0, eth_link_2)) %>%
  
  # computing single ethnic linkage var per year
  group_by (orig, dest, year) %>%
  summarise (ethnic_link = ifelse (eth_link_1 == 1 | eth_link_2 == 1 | eth_link_3 == 1, 1, 0)) %>%
  
  # computing summary linkage var 
  group_by (orig, dest) %>%
  summarise (ethnic_link = mean (ethnic_link, na.rm = T)) %>%
  mutate (ethnic_link = ifelse (ethnic_link == 0.00000000, 0, 1)) %>%
  
  # adding orig-dest pair variable
  unite ("orig_dest", c(orig, dest), sep = "_", remove = F, na.rm = F)

# creating vector with dyads available in ER data 
er_dyads <- unique (dat2$orig_dest)




## UNHCR data ------------------------------------------------------------------

## loading data in
refugees_raw = read.csv (file.path (base_path, "wrd.csv"))

## cleaning data in
ref <- refugees_raw %>%
  
  # isolating vars 
  dplyr::select (origin, asylum, year, tot_compl) %>%
  
  # rename 
  rename_with (
    ~ case_when (
      . == "origin" ~ "orig",
      . == "asylum" ~ "dest",
      . == "tot_compl" ~ "totalrefugees",
      TRUE ~ .
    )
  ) %>%
  
  # converting to ISO3 codes 
  mutate (orig = countrycode (orig, "country.name", "iso3c"),
          dest = countrycode (dest, "country.name", "iso3c")) %>%
  
  # creating dyad var
  unite ("orig_dest", c(orig, dest), sep = "_", remove = F, na.rm = F)

## joining data
join_dat = ref %>% 
  left_join (dat2, by = c("orig_dest", "orig", "dest")) %>%
  filter (!is.na (ethnic_link))

## creating empty df with full time series
orig_dest_factor <- as.factor (unique (join_dat$orig_dest))
yearfactor <- as.factor (unique (join_dat$year))
input_data <- expand.grid (orig_dest = orig_dest_factor,
                           year = yearfactor) %>%
  
  # changing formats to enable matching 
  mutate (orig_dest = as.character (orig_dest), 
          year = as.numeric (as.character (year))) %>%
  
  # creating orig_dest_year var 
  unite ("orig_dest_year", c(orig_dest, year), sep = "_", remove = F, na.rm = F)
  
## adding into it refugees and ethnic links
input_data2 <- input_data %>% 
  
  # executing join
  left_join (join_dat, by = c("orig_dest", "year")) %>%
  
  # cleaning up duplicates
  filter (!(duplicated (orig_dest_year, fromLast = T)))



## CEPII data ------------------------------------------------------------------
  
geo = read.csv (file.path (base_path, "geo_clean.csv"))

## loading in raw data 
geo_raw = read.csv (file.path (base_path, "Gravity_V202211.csv"))

## cleaning data 
geo = geo_raw %>%
  
  # isolating vars 
  dplyr::select (year, iso3_o, iso3_d, contig, distw_harmonic, gdp_o, gdp_d, pop_o, pop_d, gdpcap_o, gdpcap_d, comlang_off, comlang_ethno, comcol, comrelig) %>%
  
  # renaming vars
  rename_with (
    ~ case_when (
      . == "iso3_d" ~ "dest",
      . == "iso3_o" ~ "orig",
      . == "distw_harmonic" ~ "dist",
      TRUE ~ .
    )
  ) %>%
  
  # adding orig-dest pair variable
  unite ("orig_dest", c(orig, dest), sep = "_", remove = F, na.rm = F) %>%
  
  # filtering to include only dyads for which we have data 
  filter (orig_dest %in% unique (dat$orig_dest))
  
input_data3 = input_data2 %>% 
  
  # joining with previous input
  left_join (geo, by = c("orig_dest", "orig", "dest", "year")) %>%
  
  # dealing with duplicates
  filter (!(duplicated (orig_dest_year, fromLast = T))) 



## QoG data --------------------------------------------------------------------

## load data in
qog_ts = read.csv (file.path (base_path, "qog_std_ts_jan23.csv"))

## clean data
qog <- qog_ts %>%
  
  # isolating vars
  dplyr::select (cname, year, cspf_sfi, bti_ci, chisols_dem, gd_ptsa, gpi_conf) %>%
  
  # renaming vars
  rename_with (
    ~ case_when (
      . == "cspf_sfi" ~ "state_fragility",
      . == "bti_ci" ~ "conflict_intensity",
      . == "chisols_dem" ~ "democ",
      . == "gd_ptsa" ~ "political_terror",
      . == "gpi_conf" ~ "ongoing_conflict",
      TRUE ~ .
    )
  ) %>%
  
  # keeping only relevant countries
  mutate (country = countrycode (cname, "country.name", "iso3c")) %>%
  filter (country %in% unique (c(as.character (input_data3$orig), as.character (input_data3$dest)))) 

## adding to main data 
qog_o <- qog %>% rename (orig = country) %>% dplyr::select (-c(cname))
qog_d <- qog %>% rename (dest = country) %>% dplyr::select (-c(cname))

input_data4 = input_data3 %>%
  
  # merging with orig countries 
  left_join (qog_o, by = c("orig", "year")) %>%
  left_join (qog_d, by = c("dest", "year"), suffix = c("_o", "_d")) %>%
  
  # dealing with duplicate dyads
  filter (!(duplicated (orig_dest_year, fromLast = T)))



## DEMIG Policy data -----------------------------------------------------------

library (haven)

## load raw files in
demig_raw = read_dta (file.path (base_path, "demig-policy-database_version-1-3.dta"))

## cleaning data 
demig = demig_raw %>%
  
  # keeping only relevant vars 
  dplyr::select (country, year, change_restrict) %>%
  
  # unwrapping country var 
  mutate (dest = as_factor (country)) %>%
  
  # reformatting country 
  mutate (dest = countrycode (dest, "country.name", "iso3c")) %>%
  relocate (dest, .before = year) %>%
  
  # keeping only relevant vars 
  dplyr::select (dest, year, change_restrict) %>%
  
  # filter away 9 and 999
  filter (!(change_restrict %in% c(9,999))) %>%
  
  # average change in restrictiveness 
  group_by (dest, year) %>%
  summarise (change_restrict = mean (change_restrict, na.rm = T))

## adding data to main file
input_data5 = input_data4 %>% left_join (demig, by = c("dest", "year")) %>% group_by ()


## Final data frame (with NAs) -------------------------------------------------

input_data_final_unimputed = input_data5 %>%
  
  # reducing conflict vars
  dplyr::select (c("orig_dest","orig_dest_year","orig","dest","year","totalrefugees",
                   "ethnic_link","contig","dist","gdp_o","gdp_d","pop_o","pop_d","gdpcap_o",
                   "gdpcap_d","comlang_off","comlang_ethno","comcol","comrelig","political_terror_o", 
                   "state_fragility_o", "conflict_intensity_o", "change_restrict"))


## exporting
write.csv (input_data_final_unimputed, file.path (base_path, "input_data_final_unimputed.csv"), row.names = F)


## Miscellaneous ---------------------------------------------------------------

## number of countries in dataset, per continent
ref_countries = data.frame (countries = unique (refugees_raw$origin))
ref_countries$continent = countrycode (ref_countries$countries, "country.name", "continent")
ref_cont = ref_countries %>%
  dplyr::group_by (continent) %>% 
  dplyr::summarise (total_count=n(),
            .groups = 'drop')

## checking percentage of NAs. 
sum (is.na (input_data_final_unimputed)) / prod (dim (input_data_final_unimputed)) ## 0.1586261
  

















