rm(list=ls());gc();source(".Rprofile")

main <- read_csv("analysis cpit2dm/pdadm402_difference relative to exposed.csv") %>% 
  dplyr::filter(modifier_var == 'Overlap')

pdss <- read_csv("sensitivity supreme/pdss402_difference relative to exposed.csv") %>% 
  dplyr::filter(modifier_var == 'Overlap')

pdsu <- read_csv("sensitivity utilization/pdsu402_difference relative to exposed.csv") %>% 
  dplyr::filter(modifier_var == 'Overlap')

pdsc <- read_csv("sensitivity covid/pdsc402_difference relative to exposed.csv") %>% 
  dplyr::filter(modifier_var == 'Overlap')

pdsm <- read_csv("sensitivity mice/pdsm402_difference relative to exposed.csv") %>% 
  dplyr::filter(modifier_var == 'Overlap') %>% 
  dplyr::select(theta_D,sqrt_T_D,dfcom,
                L, U, iv,
                exposure, exposure_value,
                modifier_value,outcome,modifier_var,modifier1,modifier1_value) %>% 
  rename(Estimate = theta_D,
         SE = sqrt_T_D,
         LCI = L,
         UCI = U,
         term = iv) %>% 
  mutate(across(one_of("LCI","UCI"),function(x) case_when(is.na(x) ~ 0,
                                                          TRUE ~ x)))
  

pdsnf <- read_csv("sensitivity negatives/pdsnf402_difference relative to exposed.csv") %>%
  dplyr::filter(modifier_var == 'Overlap')

pdsno <- read_csv("sensitivity negatives/pdsno402_difference relative to exposed.csv") %>%
  dplyr::filter(modifier_var == 'Overlap')

pd402_hr <- bind_rows(
  main %>% mutate(facet = "Overall"),
  pdsu %>% mutate(facet = "SA1 Utilization"),
  pdss %>% mutate(facet = "SA2 Supreme"),
  pdsc %>% mutate(facet = "SA3 COVID"),
  pdsm %>% mutate(facet = "SA4 MICE"),
  pdsnf %>% mutate(facet = "SA5 Fractures"),
  pdsno %>% mutate(facet = "SA5 Otitis Media")
) %>% 
  mutate(group = str_replace(exposure,"COHORT","")) %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "SA1 Utilization",
                                       "SA2 Supreme",
                                       "SA3 COVID",
                                       "SA4 MICE",
                                       "SA5 Fractures",
                                       "SA5 Otitis Media")),
         group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  rename(predicted = Estimate,
         conf.low = LCI,
         conf.high = UCI) %>% 
  mutate(across(one_of("predicted","conf.low","conf.high"),
                .fns = ~exp(.))) %>% 
  mutate(t = -1) %>% 
  mutate(coef_ci = paste0(round(predicted,2)," (",
                          round(conf.low,2),", ",
                          round(conf.high,2),")")) 





pd402_hr  %>% 
  dplyr::select(group,facet,coef_ci) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,Exposed,Unexposed,Historical) %>% 
  write_csv(.,"paper/table_overlaps weights analysis.csv")




