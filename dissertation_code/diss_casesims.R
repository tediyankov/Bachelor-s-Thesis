
## CASE SIMULATIONS (HISTORIC) =================================================

library (ggthemes)

## YUGOSLAV WARS ---------------------------------------------------------------

## creating test data
test_data_1 = final_input_data_imp_cart %>%
  
  # filtering for years of the conflicts (1991-1995)
  filter (year %in% 1990:2013) %>%
  
  # filtering for Croatia and Bosnia as origin countries
  filter (orig %in% c("BIH", "HRV"))

## creating train data that excludes the test data
train_data_1 = final_input_data_imp_cart %>% filter (!orig_dest_year %in% unique (test_data_1$orig_dest_year))

## running model 
m1 <- gravity_fun_ethn (train_data_1)

## creating predictions of flows on test data 
test_data_1$totalrefugees_pred <- predict (m1, newdata = test_data_1)

## isolating ID vars and refugee flows
yugo = test_data_1 %>% dplyr::select (orig_dest, year, totalrefugees, totalrefugees_pred)

## correlation test 
cor.test (yugo$totalrefugees, yugo$totalrefugees_pred, method = "pearson")

## creating scatter plot
yugo_plot = ggplot (data = yugo, aes (x = totalrefugees, y = totalrefugees_pred)) + 
  geom_jitter () + 
  geom_smooth (method = "lm", col = "black") + 
  theme_base() + 
  labs (x = "Actual Log Magnitude of Refugee Flows",
        y = "Predicted Log Magnitude of Refugee Flows") + 
  annotate ("text", x = 0, y = 11, label = "Pearson Correlation Coef: 0.5903109 \np-value < 2.2e-16", size = 5, hjust = 0)

yugo_plot

## creating bar plot 

# data
yugo2 = yugo %>% ungroup () %>%
  
  # keeping only relevant vars
  dplyr::select (year, totalrefugees, totalrefugees_pred) %>%
  
  # reversing log transform
  dplyr::mutate (totalrefugees = round (exp (totalrefugees) - 1),
          totalrefugees_pred = round (exp (totalrefugees_pred) - 1)) %>%
  
  # renaming
  dplyr::rename (`Predicted Magnitude` = totalrefugees_pred, 
                 `Observed Magnitude` = totalrefugees) %>%
  
  # elongating
  pivot_longer (2:3, names_to = "pred", values_to = "flow")

# plot
yugo2_plot = ggplot (data = yugo2, 
                     aes (x = year, y = flow, fill = pred)) + 
  geom_bar (stat = "identity", position = "dodge") + 
  theme_base() + 
  theme (axis.text.x = element_text (angle = 90)) + 
  scale_fill_manual (values = c("#808080", "#2B2B2B")) + 
  labs (x = "Year",
        y = "Refugee flow magnitude") + 
  theme (legend.position = c(0.78, 0.85))
  
yugo2_plot

## combining plots
plotlist <- list (yugo_plot, yugo2_plot)
yugo_final <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 2)
annotate_figure (yugo_final,
                 top = text_grob ("Simulation: Yugoslav Wars 1991-1999",
                                  color = "black", face = "bold", size = 14))


## RWANDAN WARS ----------------------------------------------------------------

## creating test data
test_data_1 = final_input_data_imp_cart %>%
  
  # filtering for years of the conflicts (1994+)
  filter (year %in% 1994:2013) %>%
  
  # filtering for Croatia and Bosnia as origin countries
  filter (orig == "RWA")

## creating train data that excludes the test data
train_data_1 = final_input_data_imp_cart %>% filter (!orig_dest_year %in% unique (test_data_1$orig_dest_year))

## running model 
m1 <- gravity_fun_ethn (train_data_1)

## creating predictions of flows on test data 
test_data_1$totalrefugees_pred <- predict (m1, newdata = test_data_1)

## isolating ID vars and refugee flows
rwa = test_data_1 %>% dplyr::select (orig_dest, year, totalrefugees, totalrefugees_pred)

## correlation test 
cor.test (rwa$totalrefugees, rwa$totalrefugees_pred, method = "pearson")

## creating scatter plot
rwa_plot = ggplot (data = rwa, aes (x = totalrefugees, y = totalrefugees_pred)) + 
  geom_jitter () + 
  geom_smooth (method = "lm", col = "black") + 
  theme_base() + 
  labs (x = "Actual Log Magnitude of Refugee Flows",
        y = "Predicted Log Magnitude of Refugee Flows") + 
  annotate ("text", x = 0, y = 11, label = "Pearson Correlation Coef: 0.8564423 \np-value < 2.2e-16", size = 5, hjust = 0)

rwa_plot

## creating bar plot 

# data
rwa2 = rwa %>% ungroup () %>%
  
  # keeping only relevant vars
  dplyr::select (year, totalrefugees, totalrefugees_pred) %>%
  
  # reversing log transform
  dplyr::mutate (totalrefugees = round (exp (totalrefugees) - 1),
                 totalrefugees_pred = round (exp (totalrefugees_pred) - 1)) %>%
  
  # renaming
  dplyr::rename (`Predicted Magnitude` = totalrefugees_pred, 
                 `Observed Magnitude` = totalrefugees) %>%
  
  # elongating
  pivot_longer (2:3, names_to = "pred", values_to = "flow")

# plot
rwa2_plot = ggplot (data = rwa2, 
                     aes (x = year, y = flow, fill = pred)) + 
  geom_bar (stat = "identity", position = "dodge") + 
  theme_base() + 
  theme (axis.text.x = element_text (angle = 90)) + 
  scale_fill_manual (values = c("#808080", "#2B2B2B")) + 
  labs (x = "Year",
        y = "Refugee flow magnitude") + 
  theme (legend.position = c(0.78, 0.85))

rwa2_plot

## combining plots
plotlist <- list (rwa_plot, rwa2_plot)
rwa_final <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 2)
annotate_figure (rwa_final,
                 top = text_grob ("Simulation: Rwandan Genocide 1994",
                                  color = "black", face = "bold", size = 14))


## ROHINGYA --------------------------------------------------------------------

## creating test data
test_data_1 = final_input_data_imp_cart %>%
  
  # filtering for years of the conflicts (1994+)
  filter (orig == "MMR") %>%
  
  # filtering years
  filter (!year %in% 2013)

## creating train data that excludes the test data
train_data_1 = final_input_data_imp_cart %>% filter (!orig_dest_year %in% unique (test_data_1$orig_dest_year))

## running model 
m1 <- gravity_fun_ethn (train_data_1)

## creating predictions of flows on test data 
test_data_1$totalrefugees_pred <- predict (m1, newdata = test_data_1)

## isolating ID vars and refugee flows
mmr = test_data_1 %>% dplyr::select (orig_dest, year, totalrefugees, totalrefugees_pred)

## correlation test 
cor.test (mmr$totalrefugees, mmr$totalrefugees_pred, method = "pearson")

## creating scatter plot
mmr_plot = ggplot (data = mmr, aes (x = totalrefugees, y = totalrefugees_pred)) + 
  geom_jitter () + 
  geom_smooth (method = "lm", col = "black") + 
  theme_base() + 
  labs (x = "Actual Log Magnitude of Refugee Flows",
        y = "Predicted Log Magnitude of Refugee Flows") + 
  annotate ("text", x = 0, y = 12, label = "Pearson Correlation Coef: 0.9228107 \np-value < 2.2e-16", size = 5, hjust = 0)

mmr_plot

## creating bar plot 

# data
mmr2 = mmr %>% ungroup () %>%
  
  # keeping only relevant vars
  dplyr::select (year, totalrefugees, totalrefugees_pred) %>%
  
  # reversing log transform
  dplyr::mutate (totalrefugees = round (exp (totalrefugees) - 1),
                 totalrefugees_pred = round (exp (totalrefugees_pred) - 1)) %>%
  
  # renaming
  dplyr::rename (`Predicted Magnitude` = totalrefugees_pred, 
                 `Observed Magnitude` = totalrefugees) %>%
  
  # elongating
  pivot_longer (2:3, names_to = "pred", values_to = "flow")

# plot
mmr2_plot = ggplot (data = mmr2, 
                    aes (x = year, y = flow, fill = pred)) + 
  geom_bar (stat = "identity", position = "dodge") + 
  theme_base() + 
  theme (axis.text.x = element_text (angle = 90)) + 
  scale_fill_manual (values = c("#808080", "#2B2B2B")) + 
  labs (x = "Year",
        y = "Refugee flow magnitude") + 
  theme (legend.position = c(0.5, 0.85))

mmr2_plot

## combining plots
plotlist <- list (mmr_plot, mmr2_plot)
mmr_final <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 2)
annotate_figure (mmr_final,
                 top = text_grob ("Simulation: Rohingya Refugee Crisis",
                                  color = "black", face = "bold", size = 14))












