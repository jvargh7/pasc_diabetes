
rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_diabetes_repo,"/analysis cpit2dm/pdadm001_analytic dataset for data availability.R"))
rm(anthro_followup,demographic,index_date,lab_followup); gc()

lookback_df %>% 
  dplyr::filter(!is.na(bmi)) %>%
  left_join(all_cpit2dm_df %>% 
              dplyr::select(ID, CP, t, incident_dm),
            by = "ID") %>% 
  group_by(COHORT) %>% 
  summarize(age = paste0(round(mean(age),1)," (",round(sd(age),1),")"),
            female = mean(female),
            nhwhite = mean(nhwhite),
            n = n(),
            case_count = sum(incident_dm),
            time_count = sum(t)) %>%
  mutate(case_per_100py = case_count/(time_count/(100*365)),
         time_per_100py = time_count/(100*365))