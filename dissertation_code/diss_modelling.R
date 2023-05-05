
## DATA MODELLING ==============================================================

## building functions

gravity_fun_noethn = function (data) {
  
  data = data 
  
  # model without ethnicity vars
  m1 = glm (totalrefugees ~ dist + 
              ref_lag + 
              contig + 
              gdp_d +
              gdp_d_lag + 
              pop_o + 
              pop_d + 
              gdpcap_d +
              political_terror_o +
              state_fragility_o + 
              state_fragility_o_lag + 
              conflict_intensity_o + 
              as.factor (orig) + 
              as.factor (dest),
            data = data)
  
  # saving model objects 
  .GlobalEnv$m1 = m1
  
}

gravity_fun_ethn = function (data) {
  
  data = data 
  
  # model without ethnicity vars
  model = glm (totalrefugees ~ dist + 
                 ref_lag + 
                 contig + 
                 gdp_d +
                 gdp_d_lag + 
                 pop_o + 
                 pop_d + 
                 gdpcap_d +
                 political_terror_o +
                 state_fragility_o + 
                 state_fragility_o_lag + 
                 conflict_intensity_o +  
                 as.factor (ethnic_link) + 
                 as.factor (comlang_off) + 
                 as.factor (comlang_ethno) + 
                 as.factor (comcol) + 
                 comrelig +
                 as.factor (orig) + 
                 as.factor (dest),
               data = data)
  
  # saving model objects 
  .GlobalEnv$model = model
  
}


