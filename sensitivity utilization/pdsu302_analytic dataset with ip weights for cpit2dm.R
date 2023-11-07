# rm(list=ls());gc();source(".Rprofile")

source(paste0(path_pasc_diabetes_repo,"/sensitivity utilization/pdsu001_analytic dataset for data availability.R"))
rm(anthro_followup,demographic,index_date,lab_followup); gc()
# Do not delete all_cpit2dm_df

source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab004_matchid dataset.R"))


tx_weights_df <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm301_ip weights for cohort membership.RDS"))
mo_weights_df <- readRDS(paste0(path_pasc_diabetes_folder,"/working/sensitivity utilization/pdsu301_ip weights for missing outcomes.RDS"))

# Different from imbalanced variables in pasc_cardiometabolic_risk/analysis bmi/pcrab302_analytic dataset with ip weights for bmi.R
imbalanced_variables <- c("age","nhblack","hispanic","nhother","smoking","site","hospitalization","bmi","ldl")


before_matchid <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS")) %>% 
  dplyr::select(-matchid) %>% 
  left_join(matchid_df %>% dplyr::select(-COHORT),
            by = "ID")

analytic_dataset_lookback = before_matchid

# Use if we want to restrict to just the exposed and historical who are matched
# analytic_dataset_lookback <- bind_rows(before_matchid %>%
#                                 dplyr::filter(COHORT == "unexposed"),
#                               before_matchid %>%
#                                 dplyr::filter(COHORT %in% c("exposed","historical"), matchid %in% before_matchid$ID))

rm(before_matchid)

cpit2dm_followup_summary <- readRDS(paste0(path_pasc_diabetes_folder,"/working/sensitivity utilization/pdsu001_cpit2dm followup summary.RDS"))


cpit2dm_df = all_cpit2dm_df %>% 
  # Has lookback BMI -- this is the analytic sample of 'analysis cpit2dm'
  dplyr::filter(ID %in% analytic_sample$ID) %>% 
  
  # Has 100 days of follow-up + >= X lab encounters -- this is the analytic sample restriction for this sensitivity analysis
  dplyr::filter(ID %in% cpit2dm_followup_summary$ID) %>%
  left_join(tx_weights_df %>%
              dplyr::select(ID,sipw,sipw_sex,sipw_age,sipw_raceeth,
                            sipw_hospitalization,
                            sex_category,age_category,raceeth_category,
                            hospitalization),
            by = "ID") %>%
  left_join(mo_weights_df %>%
              dplyr::select(ID,ltfu_weights),
            by = "ID") %>%
  left_join(analytic_dataset_lookback %>% 
              dplyr::select(ID,one_of(imbalanced_variables)) %>% 
              dplyr::select(-hospitalization),
            by = "ID") %>% 
  mutate(w = sipw*ltfu_weights,
         w_sex = sipw_sex*ltfu_weights,
         w_age = sipw_age*ltfu_weights,
         w_raceeth = sipw_raceeth*ltfu_weights,
         w_hospitalization = sipw_hospitalization*ltfu_weights) %>%
  arrange(ID,t) %>% 
  mutate(ID = factor(ID),
         COHORT = factor(COHORT,levels=c("exposed","historical","unexposed")))
