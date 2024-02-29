rm(list=ls());gc();source(".Rprofile")


# Hazard ratios ----------
pdsnf402_hr <- read_csv("sensitivity negatives/pdsnf402_difference relative to exposed.csv") %>% 
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
                .fns = ~exp(.)))


pdsno402_hr <- read_csv("sensitivity negatives/pdsno402_difference relative to exposed.csv") %>% 
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
                .fns = ~exp(.)))

pdsnf402_hr %>% 
  mutate(coef_ci = paste0(round(predicted,2)," (",
                          round(conf.low,2),", ",
                          round(conf.high,2),")")) %>% 
  mutate(type = "HR") %>% 
  dplyr::select(group,facet,coef_ci,type) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,type,Exposed,Unexposed,Historical) %>% 
  write_csv(.,"paper/table_sensitivity negatives traumatic fractures modeling results.csv")


pdsno402_hr %>% 
  mutate(coef_ci = paste0(round(predicted,2)," (",
                          round(conf.low,2),", ",
                          round(conf.high,2),")")) %>% 
  mutate(type = "HR") %>% 
  dplyr::select(group,facet,coef_ci,type) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,type,Exposed,Unexposed,Historical) %>% 
  write_csv(.,"paper/table_sensitivity negatives otitis media modeling results.csv")
