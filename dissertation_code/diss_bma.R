
## VARIABLE SELECTION WITH BMA

## Preliminaries ===============================================================

## packages
pacman::p_load (arm)
install.packages("BAS")
library (BAS)

## changing variable names
final_input_data_imp_cart3 = final_input_data_imp_cart %>%
  dplyr::rename_with (
    ~ case_when (
      . == "totalrefugees" ~ "Flows",
      . == "ref_lag" ~ "Lag Flows",
      . == "dist" ~ "Distance",
      . == "contig" ~ "Contiguous",
      . == "gdp_o" ~ "GDP (orig)",
      . == "gdp_d" ~ "GDP (dest)",
      . == "gdp_o_lag" ~ "Lag GDP (orig)",
      . == "gdp_d_lag" ~ "Lag GDP (dest)",
      . == "pop_o" ~ "Population (orig)",
      . == "pop_d" ~ "Population (dest)",
      . == "gdpcap_o" ~ "GDP PC (orig)",
      . == "gdpcap_d" ~ "GDP PC (dest)",
      . == "gdpcap_o_lag" ~ "Lag GDP PC (orig)",
      . == "gdpcap_d_lag" ~ "Lag GDP PC (dest)",
      . == "political_terror_o" ~ "Political Terror (orig)",
      . == "state_fragility_o" ~ "State Fragility (orig)",
      . == "conflict_intensity_o" ~ "Conflict Intensity (orig)",
      . == "change_restrict" ~ "Policy Restrictiveness (dest)",
      . == "political_terror_o_lag" ~ "Lag Political Terror (orig)",
      . == "state_fragility_o_lag" ~ "Lag State Fragility (orig)",
      . == "conflict_intensity_o_lag" ~ "Lag Conflict Intensity (orig)",
      . == "change_restrict_lag" ~ "Lag Policy Restrictiveness (dest)",
      TRUE ~ .
    ) )

## fit baseline model using imputed data (CART)
fit_full <- bayesglm (`Flows` ~ `Distance` +
                        `Lag Flows` +
                        `Contiguous` +
                        `GDP (orig)` +
                        `Lag GDP (orig)` +
                        `GDP (dest)` +
                        `Lag GDP (dest)` +
                        `Population (orig)` +
                        `Population (dest)` +
                        `GDP PC (orig)` +
                        `Lag GDP PC (orig)` +
                        `GDP PC (dest)` +
                        `Lag GDP PC (dest)` +
                        `Political Terror (orig)` +
                        `Lag Political Terror (orig)` +
                        `State Fragility (orig)` +
                        `Lag State Fragility (orig)` +
                        `Conflict Intensity (orig)` +
                        `Lag Conflict Intensity (orig)`+
                        `Policy Restrictiveness (dest)`,
                      data = final_input_data_imp_cart3)


## perform Bayesian Model Selection 
fit_bms <- bas.lm (fit_full)
fit_bms_bic <- bas.lm (fit_full, prior = "BIC")

## summary 
summary (fit_bms)
summary (fit_bms_bic)

## creatng Table 2

library (stargazer)
library (knitr)
library (kableExtra)

stargazer (bms_bic_df,
           type = "html",
           out = "bma_bic.html")

bms_bic_df <- as.data.frame (summary (fit_bms_bic))

kable (bms_bic_df) %>%
  kable_material (lightable_options = "basic",
                  html_font = "times new roman", 
                  full_width = F) %>%
  save_kable (file = "bms_bic_table.html") 

kable_minimal (bms_bic_table, 
               lightable_options = "basic",
               html_font = "times new roman")

pacman::p_load (flextable, magrittr)
library(flextable)
library(magrittr)

bms_bic_df2 <- tibble::rownames_to_column(bms_bic_df, "row_names") %>% 
  dplyr::rename (Covariate = row_names) %>%
  regulartable() %>% 
  autofit()

bms_bic_df2

## visualising CART w Z
image (fit_bms, rotate = F, 
       color = "blackandwhite")

## visualising CART w BIC
image (fit_bms_bic, 
       rotate = F, 
       color = "blackandwhite")








