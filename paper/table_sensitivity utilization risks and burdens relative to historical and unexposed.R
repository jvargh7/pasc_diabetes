rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/pasc_cardiometabolic_risk/functions/marginal_predictions_plot.R")

# Hazard ratios ----------
pdadm405_hr_historical <- read_csv("sensitivity utilization/pdsu405_difference relative to historical.csv") %>%
  dplyr::filter(modifier_var != 'Overlap' | is.na(modifier_var)) %>% 
  mutate(group = str_replace(exposure,"COHORT",""),
         facet = str_replace(modifier1,modifier_var,"")) %>% 
  mutate(facet = case_when(is.na(modifier1) & modifier_var == "hospitalization_category" ~ "Not Hospitalized",
                           facet == "hospitalization" ~ "Hospitalized",
                           is.na(facet) ~ "Overall",
                           TRUE ~ facet)) %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "Female",
                                       "Male",
                                       "18 to 39",
                                       "40 to 64",
                                       "65 plus",
                                       "Hispanic",
                                       "NH White",
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  rename(predicted = Estimate,
         conf.low = LCI,
         conf.high = UCI) %>% 
  mutate(across(one_of("predicted","conf.low","conf.high"),
                .fns = ~exp(.))) %>% 
  mutate(t = -1)


# Hazard ratios ----------
pdadm406_hr_unexposed <- read_csv("sensitivity utilization/pdsu406_difference relative to unexposed.csv") %>% 
  dplyr::filter(modifier_var != 'Overlap' | is.na(modifier_var)) %>% 
  mutate(group = str_replace(exposure,"COHORT",""),
         facet = str_replace(modifier1,modifier_var,"")) %>% 
  mutate(facet = case_when(is.na(modifier1) & modifier_var == "hospitalization_category" ~ "Not Hospitalized",
                           facet == "hospitalization" ~ "Hospitalized",
                           is.na(facet) ~ "Overall",
                           TRUE ~ facet)) %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "Female",
                                       "Male",
                                       "18 to 39",
                                       "40 to 64",
                                       "65 plus",
                                       "Hispanic",
                                       "NH White",
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  rename(predicted = Estimate,
         conf.low = LCI,
         conf.high = UCI) %>% 
  mutate(across(one_of("predicted","conf.low","conf.high"),
                .fns = ~exp(.))) %>% 
  mutate(t = -1)


# Total Burden -----------

pdadm403_burden <-   read_csv("sensitivity utilization/pdsu403_cumulative incidence at one year.csv") %>% 
  dplyr::filter(time == 365) %>% 
  mutate(
    facet = modifier) %>% 
  mutate(facet = case_when(is.na(facet) ~ "Overall",
                           TRUE ~ facet)) %>% 
  dplyr::filter(facet != 'Overlap') %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "Female",
                                       "Male",
                                       "18 to 39",
                                       "40 to 64",
                                       "65 plus",
                                       "Hispanic",
                                       "NH White",
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(COHORT,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) 


# Burden relative to Historical --------------


pdadm403_relative_historical <-   read_csv("sensitivity utilization/pdsu403_difference cumulative incidence from historical at one year.csv") %>% 
  dplyr::filter(time == 365) %>% 
  mutate(
    facet = modifier) %>% 
  mutate(facet = case_when(is.na(facet) ~ "Overall",
                           TRUE ~ facet)) %>% 
  dplyr::filter(facet != 'Overlap') %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "Female",
                                       "Male",
                                       "18 to 39",
                                       "40 to 64",
                                       "65 plus",
                                       "Hispanic",
                                       "NH White",
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(COHORT,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  mutate(across(contains("higher_surv"),.fns=function(x) case_when(!is.na(x) ~ x*-1,
                                                                 TRUE ~ 0))) 

# Relative to Unexposed ----------

pdadm403_relative_unexposed <-   read_csv("sensitivity utilization/pdsu403_difference cumulative incidence from unexposed at one year.csv") %>% 
  dplyr::filter(time == 365) %>% 
  mutate(
    facet = modifier) %>% 
  mutate(facet = case_when(is.na(facet) ~ "Overall",
                           TRUE ~ facet)) %>% 
  dplyr::filter(facet != 'Overlap') %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "Female",
                                       "Male",
                                       "18 to 39",
                                       "40 to 64",
                                       "65 plus",
                                       "Hispanic",
                                       "NH White",
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(COHORT,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  mutate(across(contains("diff_surv"),.fns=function(x) case_when(!is.na(x) ~ x*-1,
                                                                 TRUE ~ 0))) 


bind_rows(pdadm405_hr_historical %>% 
            mutate(coef_ci = paste0(round(predicted,2)," (",
                                    round(conf.low,2),", ",
                                    round(conf.high,2),")")) %>% 
            mutate(type = "HR"),
          pdadm406_hr_unexposed %>% 
            mutate(coef_ci = paste0(round(predicted,2)," (",
                                    round(conf.low,2),", ",
                                    round(conf.high,2),")")) %>% 
            mutate(type = "HR Unexposed"),
          pdadm403_burden %>% 
            mutate(coef_ci = paste0(round(cuminc_surv,1)," (",
                                    round(cuminc_ci_upper,1),", ",
                                    round(cuminc_ci_lower,1),")")) %>% 
            mutate(type = "Burden per 1000"),
          pdadm403_relative_historical %>% 
            mutate(coef_ci = paste0(round(higher_surv_est,1)," (",
                                    round(higher_surv_uci,1),", ",
                                    round(higher_surv_lci,1),")")) %>% 
            mutate(type = "Burden relative to Historical"),
          pdadm403_relative_unexposed %>% 
            mutate(coef_ci = paste0(round(diff_surv_est,1)," (",
                                    round(diff_surv_uci,1),", ",
                                    round(diff_surv_lci,1),")")) %>% 
            mutate(type = "Burden relative to Unexposed")
          
) %>% 
  dplyr::select(group,facet,coef_ci,type) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,type,Historical,Unexposed,Exposed) %>% 
  write_csv(.,"paper/table_sensitivity utilization risks and burdens relative to historical and unexposed.csv")

