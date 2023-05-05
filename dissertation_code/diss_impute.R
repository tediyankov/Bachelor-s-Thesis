
## DATA IMPUTATION

## Preliminaries ===============================================================

## taking care of the NAs in orig, dest and year because those don't need to be imputed
input_data_final_unimputed2 = input_data_final_unimputed %>%
  
  # renaming old vars
  rename_with (
    ~ case_when (
      . == "orig" ~ "orig_old",
      . == "dest" ~ "dest_old",
      . == "year" ~ "year_old",
      TRUE ~ .
    )
  ) %>%
  
  # creating new var out of orig_dest_year 
  separate (orig_dest_year, into = c("orig", "dest", "year"), sep="_") %>%
  
  # removing old columns 
  dplyr::select (-c(orig_old, dest_old, year_old)) %>%
  
  # recomputing orig_dest_year
  unite ("orig_dest_year", c(orig_dest, year), sep = "_", remove = F, na.rm = F)

## Imputation with regular mean  ===============================================

library (tidyverse)

options (scipen = 999)
input_data_imp_mean = input_data_final_unimputed2 %>% 
  
  dplyr::group_by (orig_dest) %>% 
  
  # imputing
  dplyr::mutate (totalrefugees = ifelse (is.na (totalrefugees), mean (totalrefugees, na.rm = T), totalrefugees),
          ethnic_link = ifelse (is.na (ethnic_link), mean (ethnic_link, na.rm = T), ethnic_link),
          contig = ifelse (is.na (contig), mean (contig, na.rm = T), contig),
          dist = ifelse (is.na (dist), mean (dist, na.rm = T), dist),
          gdp_o = ifelse (is.na (gdp_o), mean (gdp_o, na.rm = T), gdp_o),
          gdp_d = ifelse (is.na (gdp_d), mean (gdp_d, na.rm = T), gdp_d),
          pop_o = ifelse (is.na (pop_o), mean (pop_o, na.rm = T), pop_o),
          pop_d = ifelse (is.na (pop_d), mean (pop_d, na.rm = T), pop_d),
          gdpcap_o = ifelse (is.na (gdpcap_o), mean (gdpcap_o, na.rm = T), gdpcap_o),
          gdpcap_d = ifelse (is.na (gdpcap_d), mean (gdpcap_d, na.rm = T), gdpcap_d),
          comlang_off = ifelse (is.na (comlang_off), mean (comlang_off, na.rm = T), comlang_off),
          comlang_ethno = ifelse (is.na (comlang_ethno), mean (comlang_ethno, na.rm = T), comlang_ethno),
          comcol = ifelse (is.na (comcol), mean (comcol, na.rm = T), comcol),
          comrelig = ifelse (is.na (comrelig), mean (comrelig, na.rm = T), comrelig),
          political_terror_o = ifelse (is.na (political_terror_o), mean (political_terror_o, na.rm = T), political_terror_o),
          state_fragility_o = ifelse (is.na (state_fragility_o), mean (state_fragility_o, na.rm = T), state_fragility_o),
          conflict_intensity_o = ifelse (is.na (conflict_intensity_o), mean (conflict_intensity_o, na.rm = T), conflict_intensity_o)) %>%
  
  # removing var with too many NaNs
  dplyr::select (-change_restrict)
  
  # removing dyads with NaNs
  #filter (!orig_dest %in% nans_od)
  #filter (complete.cases ())

input_data_imp_mean = input_data_imp_mean [complete.cases (input_data_imp_mean),]


## Imputation with Classification and Regression Trees (CART) (Breman et al. 1984) =========

input_data_imp_cart = complete (mice (input_data_final_unimputed2, m = 5, method = "cart"))

## touchups ====================================================================

## touch-ups 
final_input_data_imp_mean = input_data_imp_mean %>%
  
  # lagging vars
  dplyr::group_by (orig_dest) %>%
  dplyr:: mutate (ref_lag = Hmisc::Lag (totalrefugees, 1), 
          gdp_o_lag = Hmisc::Lag (gdp_o, 1), 
          gdp_d_lag = Hmisc::Lag (gdp_d, 1), 
          gdpcap_o_lag = Hmisc::Lag (gdpcap_o, 1), 
          gdpcap_d_lag = Hmisc::Lag (gdpcap_d, 1), 
          state_fragility_o_lag = Hmisc::Lag (state_fragility_o, 1), 
          conflict_intensity_o_lag = Hmisc::Lag (conflict_intensity_o, 1),
          political_terror_o_lag = Hmisc::Lag (political_terror_o, 1)) %>%
  relocate (ref_lag, .after = totalrefugees) %>%
  
  # impute gap years 
  dplyr::group_by (orig_dest) %>%
  dplyr::mutate (ref_lag = ifelse (is.na (ref_lag), mean (ref_lag, na.rm = T), ref_lag),
          gdp_o_lag = ifelse (is.na (gdp_o_lag), mean (gdp_o_lag, na.rm = T), gdp_o_lag),
          gdp_d_lag = ifelse (is.na (gdp_d_lag), mean (gdp_d_lag, na.rm = T), gdp_d_lag),
          gdpcap_o_lag = ifelse (is.na (gdpcap_o_lag), mean (gdpcap_o_lag, na.rm = T), gdpcap_o_lag),
          gdpcap_d_lag = ifelse (is.na (gdpcap_d_lag), mean (gdpcap_d_lag, na.rm = T), gdpcap_d_lag),
          state_fragility_o_lag = ifelse (is.na (state_fragility_o_lag), mean (state_fragility_o_lag, na.rm = T), state_fragility_o_lag),
          conflict_intensity_o_lag = ifelse (is.na (conflict_intensity_o_lag), mean (conflict_intensity_o_lag, na.rm = T), conflict_intensity_o_lag),
          political_terror_o_lag = ifelse (is.na (political_terror_o_lag), mean (political_terror_o_lag, na.rm = T), political_terror_o_lag))

## logging (x+1) for all vars that need to be logged
vars = c("totalrefugees", "ref_lag", "dist", "gdp_o", "gdp_d", "pop_o", "pop_d", "gdpcap_o", "gdpcap_d",
         "political_terror_o", "state_fragility_o", "conflict_intensity_o", "gdp_o_lag", "gdp_d_lag",
         "gdpcap_o_lag", "gdpcap_d_lag", "state_fragility_o_lag", "conflict_intensity_o_lag", "political_terror_o_lag")

for (i in vars) {
  
  final_input_data_imp_mean [,i] <- log1p (final_input_data_imp_mean [,i])
  
}

final_input_data_imp_cart = input_data_imp_cart %>%
  
  # lagging vars
  dplyr::group_by (orig_dest) %>%
  dplyr::mutate (ref_lag = Hmisc::Lag (totalrefugees, 1), 
          gdp_o_lag = Hmisc::Lag (gdp_o, 1), 
          gdp_d_lag = Hmisc::Lag (gdp_d, 1), 
          gdpcap_o_lag = Hmisc::Lag (gdpcap_o, 1), 
          gdpcap_d_lag = Hmisc::Lag (gdpcap_d, 1), 
          state_fragility_o_lag = Hmisc::Lag (state_fragility_o, 1), 
          conflict_intensity_o_lag = Hmisc::Lag (conflict_intensity_o, 1),
          political_terror_o_lag = Hmisc::Lag (political_terror_o, 1)) %>%
  relocate (ref_lag, .after = totalrefugees) %>%
  
  # impute gap years 
  dplyr::group_by (orig_dest) %>%
  dplyr::mutate (ref_lag = ifelse (is.na (ref_lag), mean (ref_lag, na.rm = T), ref_lag),
          gdp_o_lag = ifelse (is.na (gdp_o_lag), mean (gdp_o_lag, na.rm = T), gdp_o_lag),
          gdp_d_lag = ifelse (is.na (gdp_d_lag), mean (gdp_d_lag, na.rm = T), gdp_d_lag),
          gdpcap_o_lag = ifelse (is.na (gdpcap_o_lag), mean (gdpcap_o_lag, na.rm = T), gdpcap_o_lag),
          gdpcap_d_lag = ifelse (is.na (gdpcap_d_lag), mean (gdpcap_d_lag, na.rm = T), gdpcap_d_lag),
          state_fragility_o_lag = ifelse (is.na (state_fragility_o_lag), mean (state_fragility_o_lag, na.rm = T), state_fragility_o_lag),
          conflict_intensity_o_lag = ifelse (is.na (conflict_intensity_o_lag), mean (conflict_intensity_o_lag, na.rm = T), conflict_intensity_o_lag),
          political_terror_o_lag = ifelse (is.na (political_terror_o_lag), mean (political_terror_o_lag, na.rm = T), political_terror_o_lag))

## logging (x+1) for all vars that need to be logged
vars = c("totalrefugees", "ref_lag", "dist", "gdp_o", "gdp_d", "pop_o", "pop_d", "gdpcap_o", "gdpcap_d",
         "political_terror_o", "state_fragility_o", "conflict_intensity_o", "gdp_o_lag", "gdp_d_lag",
         "gdpcap_o_lag", "gdpcap_d_lag", "state_fragility_o_lag", "conflict_intensity_o_lag", "political_terror_o_lag")

for (i in vars) {
  
  final_input_data_imp_cart [,i] <- log1p (final_input_data_imp_cart [,i])
  
}


## integrity check =============================================================

dat_nas = input_data_final_unimputed [is.na (input_data_final_unimputed$totalrefugees),]

df1 <- final_input_data_imp_mean %>% filter (orig_dest_year %in% unique (dat_nas$orig_dest_year))
df2 <- final_input_data_imp_cart %>% filter (orig_dest_year %in% unique (dat_nas$orig_dest_year))

df2 <- df2 %>% filter (orig_dest_year %in% unique (df1$orig_dest_year))

final_input_data_imp_cart2 = final_input_data_imp_cart %>% filter (orig_dest_year %in% unique (final_input_data_imp_mean$orig_dest_year))

cor.test (final_input_data_imp_mean$totalrefugees, final_input_data_imp_cart2$totalrefugees, method = "pearson")



## correlation plot (Figure 2) =================================================

library (corrplot)

## df with proper variable names
input_data_final_unimputed3 <- input_data_final_unimputed %>%
  dplyr::rename_with (
    ~ case_when (
      . == "ethnic_link" ~ "Ethnic \nLink",
      . == "contig" ~ "Contiguous",
      . == "dist" ~ "Distance",
      . == "gdp_o" ~ "GDP (orig)",
      . == "gdp_d" ~ "GDP (dest)",
      . == "pop_o" ~ "Population \n(orig)",
      . == "pop_d" ~ "Population \n(dest)",
      . == "gdpcap_o" ~ "GDP per \ncapita (orig)",
      . == "gdpcap_d" ~ "GDP per \ncapita (dest)",
      . == "comlang_off" ~ "Common \nOffical \nLanguage",
      . == "comlang_ethno" ~ "Common \nSpoken \nLanguage",
      . == "comcol" ~ "Common \nColonial \nPast",
      . == "comrelig" ~ "Common \nReligion",
      . == "political_terror_o" ~ "Political \nTerror",
      . == "state_fragility_o" ~ "State \nFragility",
      . == "conflict_intensity_o" ~ "Conflict \nIntensity",
      . == "change_restrict" ~ "Policy \nRestrictiveness",
      TRUE ~ .
    ) )

corrplot::corrplot.mixed (
  cor (input_data_final_unimputed3 [,7:23],
       use = "complete.obs"),
  upper = "ellipse",
  tl.cex = 0.7,
  title = "Figure 3: Pairwise correlations of input data independent variables",
  mar = c(0,0,1,0),
  text.diag = "values",
  diag.direction = "diagonal.45",
  diag.cex = 0.5
)


















