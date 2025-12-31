rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/pasc_cardiometabolic_risk/functions/marginal_predictions_plot.R")

# Hazard ratios ----------
pdsm405_hr_historical <- read_csv("sensitivity mice/pdsm405_difference relative to historical.csv") %>%
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
                        labels=c("Historical","Unexposed","Exposed"))) 


# Hazard ratios ----------
pdsm406_hr_unexposed <- read_csv("sensitivity mice/pdsm406_difference relative to unexposed.csv") %>% 
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
                        labels=c("Historical","Unexposed","Exposed"))) 

bind_rows(pdsm405_hr_historical %>% 
            mutate(type = "HR"),
          pdsm406_hr_unexposed  %>% 
            mutate(type = "HR Unexposed")) %>% 
  dplyr::select(group,facet,type,HR) %>% 
  pivot_wider(names_from=group,values_from=HR) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,type,Historical,Unexposed,Exposed) %>% 
  write_csv(.,"paper/table_sensitivity mice risks and burdens relative to historical and unexposed.csv")
