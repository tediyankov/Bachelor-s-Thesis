
## preliminaries ===============================================================

## creating df of refugee means
df_refmeans = final_input_data_imp_cart %>%
  
  # reversing log transform
  dplyr::mutate (totalrefugees = exp (totalrefugees) - 1) %>%
  
  # filter into only UKR orig 
  dplyr::filter (orig == "UKR") %>%
  
  # grouping by orig-dest 
  dplyr::group_by (orig_dest) %>%
  dplyr::summarise (totalrefugees = mean (totalrefugees, na.rm = T)) %>%
  dplyr::mutate (totalrefugees = round (totalrefugees)) %>%
  
  # adding year and reordering
  dplyr::mutate (year = 2021) %>%
  dplyr::select (orig_dest, year, totalrefugees) %>%
  
  # removing Russia because no 2022 data
  dplyr::filter (!orig_dest == "UKR_RUS") %>%
  
  # creating orig and dest vars
  separate (orig_dest, into = c("orig", "dest"), sep="_") %>%
  
  # creating orig_dest_year and orig_dest again
  unite ("orig_dest_year", c(orig, dest, year), sep = "_", remove = F, na.rm = F) %>%
  unite ("orig_dest", c(orig, dest), sep = "_", remove = F, na.rm = F)
  
## Ukraine data ================================================================

## loading in refugee data
ukr = read.csv (file.path (base_path, "ukr_data.csv")) %>% 
  
  # removing empty columns
  dplyr::select (1:3) %>%
  
  # adding UKR as an orig country 
  dplyr::mutate (orig = "UKR") %>%
  
  # renaming country var to dest
  dplyr::rename (dest = country) %>%
  
  # changing names into country codes
  dplyr::mutate (dest = countrycode (dest, "country.name", "iso3c")) %>%
  
  # reorder
  dplyr::relocate (orig, dest, year, totalrefugees) %>%
  
  # add identifier vars
  unite ("orig_dest_year", c(orig, dest, year), sep = "_", remove = F, na.rm = F) %>%
  unite ("orig_dest", c(orig, dest), sep = "_", remove = F, na.rm = F)  %>%
  
  # adding in ref_lag var
  dplyr::arrange (orig_dest, year) %>%
  dplyr::group_by (orig_dest) %>%
  dplyr::mutate (ref_lag = Hmisc::Lag (totalrefugees, 1)) %>%
  dplyr::ungroup () %>%
  
  # filling in the NAs in ref_lag where possible
  dplyr::mutate (ref_lag = ifelse (orig_dest_year == "UKR_BLR_2022", df_refmeans$totalrefugees [df_refmeans$orig_dest == "UKR_BLR"],
                                   ifelse (orig_dest_year == "UKR_DEU_2022", df_refmeans$totalrefugees [df_refmeans$orig_dest == "UKR_DEU"],
                                           ifelse (orig_dest_year == "UKR_ITA_2022", df_refmeans$totalrefugees [df_refmeans$orig_dest == "UKR_ITA"],
                                                   ifelse (orig_dest_year == "UKR_AUT_2022", 91,
                                                           ifelse (orig_dest_year == "UKR_BEL_2022", 81,
                                                                   ifelse (orig_dest_year == "UKR_CHE_2022", 42, 
                                                                           ifelse (orig_dest_year == "UKR_CZE_2022", 264,
                                                                                   ifelse (orig_dest_year == "UKR_DNK_2022", 12,
                                                                                           ifelse (orig_dest_year == "UKR_ESP_2022", 1037,
                                                                                                   ifelse (orig_dest_year == "UKR_EST_2022", 5,
                                                                                                           ifelse (orig_dest_year == "UKR_FIN_2022", 9,
                                                                                                                   ifelse (orig_dest_year == "UKR_FRA_2022", 2039,
                                                                                                                           ifelse (orig_dest_year == "UKR_GBR_2022", 96,
                                                                                                                                   ifelse (orig_dest_year == "UKR_IRL_2022", 5,
                                                                                                                                           ifelse (orig_dest_year == "UKR_LTU_2022", 0,
                                                                                                                                                   ifelse (orig_dest_year == "UKR_LVA_2022", 7,
                                                                                                                                                           ifelse (orig_dest_year == "UKR_MDA_2022", 5,
                                                                                                                                                                   ifelse (orig_dest_year == "UKR_NLD_2022", 71,
                                                                                                                                                                           ifelse (orig_dest_year == "UKR_NOR_2022", 5,
                                                                                                                                                                                   ifelse (orig_dest_year == "UKR_POL_2022", 307,
                                                                                                                                                                                           ifelse (orig_dest_year == "UKR_PRT_2022", 11,
                                                                                                                                                                                                   ifelse (orig_dest_year == "UKR_SWE_2022", 441,
                                                                                                                                                                                                           ifelse (orig_dest_year == "UKR_HUN_2022", 0,
                                                                                                                                                                                                                   ifelse (orig_dest_year == "UKR_ROU_2022", 0,
                                                                                                                                                                                                                           ifelse (orig_dest_year == "UKR_SVN_2022", 0,
                                                                                                                                                                                                                                   ifelse (orig_dest_year == "UKR_SVK_2022", 0,
                                                                                                                                                                                                                                           ifelse (orig_dest_year == "UKR_ISL_2022", 0,
                                                                                                                                                                                                                                                   ifelse (orig_dest_year == "UKR_TUR_2022", 0,
                                                                                                                                                                                                                                                   ref_lag))))))))))))))))))))))))))))) %>%
  
  # adding ethnic linkage var
  dplyr::mutate (ethnic_link = c(0,0,
                                 0,0,
                                 0,0,
                                 1,1,
                                 0,0,
                                 0,0,
                                 0,0,
                                 1,1,
                                 0,0,
                                 0,0,
                                 0,0,
                                 1,1,
                                 0,0,
                                 0,0,
                                 0,0,
                                 1,1,
                                 1,1,
                                 1,1,
                                 0,0,
                                 0,0,
                                 1,1,
                                 0,0,
                                 1,1,
                                 1,1,
                                 1,1,
                                 0,0,
                                 0,0)) %>%
  
  # computing contig
  dplyr::mutate (contig = ifelse (dest %in% c("BLR", 'POL', 'SVK', 'HUN', 'MDA', 'ROU', 'RUS'), 1, 0)) %>%

  # creating distance 
  dplyr::left_join (geo %>% 
                      dplyr::select (orig_dest, dist) %>% 
                      dplyr::group_by (orig_dest) %>%
                      dplyr::summarise (dist = mean (dist, na.rm = T)), by = c("orig_dest")) %>%
  dplyr::mutate (dist = ifelse (dest == "AUT", 1242, 
                                ifelse (dest == "BEL", 1940, 
                                        ifelse (dest == "CHE", 1727, 
                                                ifelse (dest == "CZE", 1155, 
                                                        ifelse (dest == "DNK", 1707, 
                                                                ifelse (dest == "ESP", 2992.44, 
                                                                        ifelse (dest == "EST", 1177.93, 
                                                                                ifelse (dest == "FIN", 1546, 
                                                                                        ifelse (dest == "FRA", 2189, 
                                                                                                ifelse (dest == "GBR", 2282.40, 
                                                                                                        ifelse (dest == "HUN", 0, 
                                                                                                                ifelse (dest == "IRL", 2791, 
                                                                                                                        ifelse (dest == "ISL", 3455, 
                                                                                                                                ifelse (dest == "LTU", 906, 
                                                                                                                                        ifelse (dest == "LVA", 1044, 
                                                                                                                                                ifelse (dest == "MDA", 0, 
                                                                                                                                                        ifelse (dest == "NLD", 1881, 
                                                                                                                                                                ifelse (dest == "NOR", 1977, 
                                                                                                                                                                        ifelse (dest == "POL", 0, 
                                                                                                                                                                                ifelse (dest == "PRT", 3278, 
                                                                                                                                                                                        ifelse (dest == "ROU", 0, 
                                                                                                                                                                                                ifelse (dest == "SVK", 0, 
                                                                                                                                                                                                        ifelse (dest == "SVN", 1246, 
                                                                                                                                                                                                                ifelse (dest == "SWE", 1536, 
                                                                                                                                                                                                                        ifelse (dest == "TUR", 1137.30, 
                                                                                                                                                                                                                                dist))))))))))))))))))))))))))
## loading WEC for pop and GDP data
load_wec_dat = function (){
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  if (is.na (base_path)) {base_path = file.path("/Users",
                                                "teddyyankov",
                                                "Library",
                                                "CloudStorage",
                                                "GoogleDrive-teodor.yankov@worlddata.io",
                                                "Shared drives", 
                                                "DATA_WDL")}
  
  # load binary (Rda) file
  load (file.path (base_path,
                   "world_emissions_clock",
                   "01_data",
                   "WEC_data_binary_20230329.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  
}
load_wec_dat()

## creating a subset of WEC data 
wec_ukr = wec_dat %>%
  
  ungroup () %>%
  dplyr::select (iso3c, year, pop, gdp) %>%
  unique ()

## creating an orig and dest version for joining
wec_ukr_orig = wec_ukr %>% dplyr::rename (orig = iso3c)
wec_ukr_dest = wec_ukr %>% dplyr::rename (dest = iso3c)

ukr2 = ukr %>%
  
  # joining with gdp and pop data 
  left_join (wec_ukr_orig, by = c("orig", "year")) %>%
  left_join (wec_ukr_dest, by = c("dest", "year"), suffix = c("_o", "_d")) %>%
  
  # computing per capita
  dplyr::mutate (gdpcap_o = gdp_o / pop_o) %>%
  dplyr::mutate (gdpcap_d = gdp_d / pop_d) %>%
  
  # compute comlang_off
  dplyr::mutate (comlang_off = 0) %>%
  
  # compute comlang_off
  dplyr::mutate (comlang_ethno = c(0,0,
                                   0,0,
                                   0,0,
                                   1,1,
                                   1,1,
                                   0,0,
                                   0,0,
                                   1,1,
                                   0,0,
                                   0,0,
                                   0,0,
                                   1,1,
                                   0,0,
                                   0,0,
                                   0,0,
                                   1,1,
                                   1,1,
                                   1,1,
                                   0,0,
                                   0,0,
                                   1,1,
                                   0,0,
                                   1,1,
                                   0,0,
                                   0,0,
                                   0,0,
                                   0,0)) %>%
  
  # computing comcol 
  dplyr::mutate (comcol = ifelse (dest %in% c("EST", "LVA", "LTU", "MDA"), 1, 0)) %>%
  
  # computing comrelig
  dplyr::mutate (comrelig = ifelse (dest %in% c("ROU", "MDA"), 1, 
                                    ifelse (dest %in% c("LTU", 'LVA', 'SVN', 'EST'), 0.5, 
                                            ifelse (dest %in% c("TUR"), 0.1, 
                                            0)))) %>%
  
  # add political terror
  dplyr::mutate (political_terror_o = 4.5) %>%
  
  # add state fragility index
  dplyr::mutate (state_fragility_o = 68.6) %>%
  
  # add conflict intensity 
  dplyr::mutate (conflict_intensity_o = 10) %>%
  
  # adding lags
  dplyr::group_by (orig_dest) %>%
  dplyr::mutate (gdp_o_lag = Hmisc::Lag (gdp_o, 1), 
                 gdp_d_lag = Hmisc::Lag (gdp_d, 1), 
                 gdpcap_o_lag = Hmisc::Lag (gdpcap_o, 1), 
                 gdpcap_d_lag = Hmisc::Lag (gdpcap_d, 1), 
                 state_fragility_o_lag = Hmisc::Lag (state_fragility_o, 1), 
                 conflict_intensity_o_lag = Hmisc::Lag (conflict_intensity_o, 1),
                 political_terror_o_lag = Hmisc::Lag (political_terror_o, 1)) 

## filling in the NAs
countries = unique (ukr2$dest)
vars = c("gdp_o_lag", "gdp_d_lag", "gdpcap_o_lag", "gdpcap_d_lag", "state_fragility_o_lag", "conflict_intensity_o_lag", "political_terror_o_lag")

# computing per capita in WEC data 
wec_ukr_dest = wec_ukr_dest %>% dplyr::mutate (gdpcap = gdp / pop)
wec_ukr_orig = wec_ukr_orig %>% dplyr::mutate (gdpcap = gdp / pop)

# NAs in GDP
for (i in countries) {
    
    ukr2$gdp_d_lag = ifelse (ukr2$dest == i & ukr2$year == 2022, wec_ukr_dest$gdp [wec_ukr_dest$year == 2021 & wec_ukr_dest$dest == i], ukr2$gdp_d_lag)

}

ukr2$gdp_o_lag = ifelse (is.na (ukr2$gdp_o_lag), 383095239177, ukr2$gdp_o_lag)

# NAs in GDP per capita
for (i in countries) {
  
  ukr2$gdpcap_d_lag = ifelse (ukr2$dest == i & ukr2$year == 2022, wec_ukr_dest$gdpcap [wec_ukr_dest$year == 2021 & wec_ukr_dest$dest == i], ukr2$gdpcap_d_lag)
  
}

ukr2$gdpcap_o_lag = ifelse (is.na (ukr2$gdpcap_o_lag), 8819.008, ukr2$gdpcap_o_lag)

# NAs for conflic vars
ukr2$state_fragility_o_lag = ifelse (is.na (ukr2$state_fragility_o_lag), 69.8, ukr2$state_fragility_o_lag)
ukr2$state_fragility_o = ifelse (ukr2$year == 2023, 76.3, ukr2$state_fragility_o)
ukr2$conflict_intensity_o_lag = ifelse (is.na (ukr2$conflict_intensity_o_lag), 4, ukr2$conflict_intensity_o_lag)
ukr2$political_terror_o_lag = ifelse (is.na (ukr2$political_terror_o_lag), 3, ukr2$political_terror_o_lag)

# log transform
vars = c("totalrefugees", "ref_lag", "dist", "gdp_o", "gdp_d", "pop_o", "pop_d", "gdpcap_o", "gdpcap_d",
         "political_terror_o", "state_fragility_o", "conflict_intensity_o", "gdp_o_lag", "gdp_d_lag",
         "gdpcap_o_lag", "gdpcap_d_lag", "state_fragility_o_lag", "conflict_intensity_o_lag", "political_terror_o_lag")

for (i in vars) {
  
  ukr2 [,i] <- log1p (ukr2 [,i])
  
}

## running sim =================================================================

## creating test data
test_data_1 = ukr2 %>% filter (year == 2023)

## creating train data that excludes the test data
train_data_1 = final_input_data_imp_cart %>% 
  mutate (year = as.numeric (as.character (year))) %>% 
  bind_rows (ukr2 %>% 
               mutate (year = as.numeric (as.character (year))) %>% 
               filter (year == 2022))

## running model 
model <- gravity_fun_ethn (train_data_1)

## creating predictions of flows on test data 
test_data_1$totalrefugees_pred <- predict (model, newdata = test_data_1)

## isolating ID vars and refugee flows
ukr = test_data_1 %>% dplyr::select (orig_dest, year, totalrefugees, totalrefugees_pred)

## correlation test 
cor.test (ukr$totalrefugees, ukr$totalrefugees_pred, method = "pearson")

## creating scatter plot
ukr_plot = ggplot (data = ukr, aes (x = totalrefugees, y = totalrefugees_pred)) + 
  geom_jitter () + 
  geom_smooth (method = "lm", col = "black") + 
  theme_base() + 
  labs (x = "Actual Log Magnitude of Refugee Flows",
        y = "Predicted Log Magnitude of Refugee Flows") + 
  annotate ("text", x = min (ukr$totalrefugees), y = 23, label = "Pearson Correlation Coef: 0.4699009 \np-value = 0.01778", size = 5, hjust = 0)

ukr_plot

## creating bar plot 

# data
ukr2 = ukr %>% ungroup () %>%
  
  # keeping only relevant vars
  dplyr::select (orig_dest, year, totalrefugees, totalrefugees_pred) %>%
  
  # reversing log transform
  dplyr::mutate (totalrefugees = round (exp (totalrefugees) - 1),
                 totalrefugees_pred = round (exp (totalrefugees_pred) - 1)) %>%
  
  # renaming
  dplyr::rename (`Predicted Magnitude` = totalrefugees_pred, 
                 `Observed Magnitude` = totalrefugees) %>%
  
  # elongating
  pivot_longer (3:4, names_to = "pred", values_to = "flow")

# plot
ukr2_plot = ggplot (data = ukr2 %>% filter (!orig_dest == "UKR_ROU"), 
                    aes (x = orig_dest, y = flow, fill = pred)) + 
  geom_bar (stat = "identity", position = "dodge") + 
  theme_base() + 
  theme (axis.text.x = element_text (angle = 90)) + 
  scale_fill_manual (values = c("#808080", "#2B2B2B")) + 
  labs (x = "Dyad",
        y = "Refugee flow magnitude") + 
  theme (legend.position = c(0.5, 0.85))

ukr2_plot

## combining plots
plotlist <- list (ukr_plot, ukr2_plot)
ukr_final <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 2)
annotate_figure (ukr_final,
                 top = text_grob ("Simulation: Ukraine Refugee Crisis",
                                  color = "black", face = "bold", size = 14))








