rm(list=ls());gc();source(".Rprofile")

source("sensitivity utilization/pdsu001_analytic dataset for data availability.R")
rm(anthro_followup,demographic,index_date,lab_followup); gc()

source("analysis cpit2dm/pdadm102_selecting high dimensional variables based on fdr.R")
source("analysis cpit2dm/pdadm202_selecting ltfu high dimensional variables based on fdr.R")

# Use the dataset created for pasc_cardiometabolic_risk 
lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS")) %>% 
  # Same as using dplyr::filter(!ID %in% landmark_cpit2dm$ID)
  # This is because pcrab002 only filters on lookback_cpit2dm$ID 
  # while analytic_sample (pdadm001) uses both...
  dplyr::filter(ID %in% analytic_sample$ID) %>% 
  # analytic_sample is coming from pcrab003_analytic dataset for data availability.R
  left_join(analytic_sample %>% 
              dplyr::select(ID, in_sensitivity_utilization_ID),
            by = "ID")

# Outcome variable distribution
table(lookback_processed$in_sensitivity_utilization_ID)

# Loads HD dataset + restricts based on a cutoff
source("analysis cpit2dm/pdadm103_restricting high dimensional covariates based on frequency.R")
source("analysis cpit2dm/pdadm203_restricting ltfu high dimensional covariates based on frequency.R")



outcome_df <- lookback_processed %>% 
  # dplyr::filter(ID %in% bmi_ID) %>% 
  left_join(hd_dataset_COHORT %>% 
              dplyr::select(ID, one_of(restricted_hdvars)),
            by = "ID") %>% 
  left_join(hd_dataset_censoring %>% 
              dplyr::select(ID, one_of(restricted_hdvars_censoring)),
            by = "ID")
write_parquet(outcome_df,paste0(path_pasc_diabetes_folder,"/working/sensitivity utilization/pdsu204_ipw for loss to followup data.parquet"))
