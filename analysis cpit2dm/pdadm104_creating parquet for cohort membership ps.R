rm(list=ls());gc();source(".Rprofile")

source("analysis cpit2dm/pdadm001_analytic dataset for data availability.R")
# Identify variables associated with outcome among those observations with lookback BMI
# Filter 1: Lookback BMI available >> pdadm001_analytic dataset for data availability.R >> lookback_df (after filtering based on bmi availability)
# Filter 2: Outcome available >> pdadm001_analytic dataset for data availability.R >> cpit2dm_df


rm(demographic,index_date,all_cpit2dm_df); gc()


source("analysis cpit2dm/pdadm102_selecting high dimensional variables based on fdr.R")

# Use the dataset created for pasc_cardiometabolic_risk 
lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS")) %>% 
  mutate(EXPOSED = case_when(COHORT == "exposed" ~ 1,
                             TRUE ~ 0),
         UNEXPOSED = case_when(COHORT == "unexposed" ~ 1,
                               TRUE ~ 0),
         HISTORICAL = case_when(COHORT == "historical" ~ 1,
                                TRUE ~ 0))

# Loads HD dataset + restricts based on a cutoff
# >> Filters HD dataset to observations that have lookback BMI
source("analysis cpit2dm/pdadm103_restricting high dimensional covariates based on frequency.R")



outcome_df <- lookback_processed %>% 
  # dplyr::filter(ID %in% bmi_ID) %>% 
  inner_join(hd_dataset_COHORT %>% 
              dplyr::select(ID, one_of(restricted_hdvars)),
            by = "ID")

write_parquet(outcome_df,paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm104_ipw for cohort membership data.parquet"))
