rm(list=ls());gc();source(".Rprofile")


pd405_hr <- bind_rows(
  read_csv("analysis cpit2dm/pdadm405_difference relative to historical.csv") %>% 
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "Overall"),
  read_csv("sensitivity utilization/pdsu405_difference relative to historical.csv") %>% 
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA1 Utilization"),
  read_csv("sensitivity supreme/pdss405_difference relative to historical.csv") %>% 
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA2 Supreme"),
  read_csv("sensitivity covid/pdsc405_difference relative to historical.csv") %>% 
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA3 COVID"),
  read_csv("sensitivity mice/pdsm405_difference relative to historical.csv") %>% 
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
                                                            TRUE ~ x))) %>% mutate(facet = "SA4 MICE"),
  read_csv("sensitivity negatives/pdsnf405_difference relative to historical.csv") %>%
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA5 Fractures"),
  
  read_csv("sensitivity negatives/pdsno405_difference relative to historical.csv") %>%
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA5 Otitis Media")
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


pd406_hr <- bind_rows(
  read_csv("analysis cpit2dm/pdadm406_difference relative to unexposed.csv") %>% 
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "Overall"),
  read_csv("sensitivity utilization/pdsu406_difference relative to unexposed.csv") %>% 
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA1 Utilization"),
  read_csv("sensitivity supreme/pdss406_difference relative to unexposed.csv") %>% 
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA2 Supreme"),
  read_csv("sensitivity covid/pdsc406_difference relative to unexposed.csv") %>% 
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA3 COVID"),
  read_csv("sensitivity mice/pdsm406_difference relative to unexposed.csv") %>% 
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
                                                            TRUE ~ x))) %>% mutate(facet = "SA4 MICE"),
  read_csv("sensitivity negatives/pdsnf406_difference relative to unexposed.csv") %>%
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA5 Fractures"),
  
  read_csv("sensitivity negatives/pdsno406_difference relative to unexposed.csv") %>%
    dplyr::filter(modifier_var == 'Overlap') %>% mutate(facet = "SA5 Otitis Media")
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


bind_rows(pd405_hr %>% 
            mutate(type = "HR"),
          pd406_hr %>% 
            mutate(type = "HR Unexposed"))  %>% 
  dplyr::select(group,type, facet,coef_ci) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(type,facet,Historical,Unexposed,Exposed) %>% 
  write_csv(.,"paper/table_overlaps weights analysis.csv")




