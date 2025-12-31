rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/pasc_cardiometabolic_risk/functions/marginal_predictions_plot.R")

# Hazard ratios ----------
pdsnf405_hr_historical <- read_csv("sensitivity negatives/pdsnf405_difference relative to historical.csv") %>%
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
pdsnf406_hr_unexposed <- read_csv("sensitivity negatives/pdsnf406_difference relative to unexposed.csv") %>% 
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

bind_rows(pdsnf405_hr_historical %>% 
            mutate(coef_ci = paste0(round(predicted,2)," (",
                                    round(conf.low,2),", ",
                                    round(conf.high,2),")")) %>% 
            mutate(type = "HR"),
          pdsnf406_hr_unexposed %>% 
            mutate(coef_ci = paste0(round(predicted,2)," (",
                                    round(conf.low,2),", ",
                                    round(conf.high,2),")")) %>% 
            mutate(type = "HR Unexposed")) %>% 
  dplyr::select(group,facet,coef_ci,type) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,type,Historical,Unexposed,Exposed) %>% 
  write_csv(.,"paper/table_sensitivity negatives fractures risks and burdens relative to historical and unexposed.csv")