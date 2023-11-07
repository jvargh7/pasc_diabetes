rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_diabetes_repo,"/analysis cpit2dm/pdadm001_analytic dataset for data availability.R"))
rm(anthro_followup,demographic,index_date,lab_followup); gc()
# Use pcrab002_imputed lookback dataset.RDS since it is the same

# LOSS TO FOLLOW-UP -------------
source("analysis cpit2dm/pdadm001_analytic dataset for data availability.R")
source("C:/code/external/functions/causality/trim_probabilities.R")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS")) %>% 
  # Need to exclude cases with landmark_cpit2dm?
  dplyr::filter(ID %in% analytic_sample$ID) %>% 
  # analytic_sample is coming from pdadm001_analytic dataset for data availability.R
  left_join(analytic_sample %>% 
              dplyr::select(ID, in_dm_followup_ID),
            by = "ID")

predicted_probability <-read_csv(paste0(path_pasc_diabetes_folder,"/working/sensitivity utilization/pdsu206_predicted probability for loss to followup_min10_ntree2000.csv")) %>% 
  dplyr::select(available,missing) %>%
  bind_cols(lookback_processed %>% 
              dplyr::select(ID,COHORT)) %>% 
  mutate(across(contains("available"),~trim_probabilities(.),.names = "{.col}_trimmed")) %>%  
  
  mutate(ltfu_weights = 1/available_trimmed)


saveRDS(predicted_probability, paste0(path_pasc_diabetes_folder,"/working/sensitivity utilization/pdsu301_ip weights for missing outcomes.RDS"))
