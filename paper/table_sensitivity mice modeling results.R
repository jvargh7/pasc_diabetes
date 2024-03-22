rm(list=ls());gc();source(".Rprofile")


# Hazard ratios ----------
pdsm402_hr <- read_csv("sensitivity mice/pdsm402_difference relative to exposed.csv") %>% 
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
  dplyr::select(group,facet,HR) %>% 
  pivot_wider(names_from=group,values_from=HR) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,Exposed,Unexposed,Historical) 

pdsm402_hr %>% 
  write_csv(.,"paper/table_sensitivity mice modeling results.csv")
