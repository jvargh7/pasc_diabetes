rm(list=ls());gc();source(".Rprofile")


source(paste0(path_pasc_cmr_repo,"/analysis/pcra_processing before imputation of lookback covariates.R"))

library(tidymodels)
tidymodels_prefer()



incident_dm_df = bind_rows(readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/cpit2dm new onset diabetes.RDS")) %>% 
            dplyr::select(ID,criterion2_date) %>% rename(last_followup_date = criterion2_date) %>% 
            mutate(incident_dm = 1),
            readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/noncpit2dm last followup.RDS")) %>% 
            dplyr::select(ID,last_followup_date) %>% 
            mutate(incident_dm = 0)) 

lookback_df <- lookback_df %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::filter(ID %in% incident_dm_df$ID) %>% 
 
  dplyr::filter(nhwhite == 1 | nhblack == 1 | hispanic == 1)

lookback_processed <- recipe(COHORT ~ .,
                             data = lookback_df) %>% 
  
  update_role(ID,matchid,index_date, new_role="ID") %>% 
  # https://recipes.tidymodels.org/reference/step_impute_knn.html
  # step_impute_knn creates a specification of a recipe step that will impute missing data using nearest neighbors.
  # step_impute_knn(all_predictors(),neighbors = 5) %>% 
  step_impute_mode(all_nominal()) %>%
  # https://github.com/tidymodels/recipes/issues/756 
  # There might be scenarios where it is more reasonable to impute missing values per group/subsample.
  step_impute_linear(all_numeric(),impute_with = imp_vars(one_of(c("female","nhblack","hispanic","site","age")))) %>%
  # prep(): For a recipe with at least one preprocessing operation, estimate the required parameters from a training set that can be later applied to other data sets.
  prep(.,training = lookback_df) %>% 
  # bake(): For a recipe with at least one preprocessing operation that has been trained by prep(), apply the computations to new data.
  bake(.,new_data=lookback_df) %>% 
  mutate(EXPOSED = case_when(COHORT == "exposed" ~ 1,
                             TRUE ~ 0),
         UNEXPOSED = case_when(COHORT == "unexposed" ~ 1,
                               TRUE ~ 0),
         HISTORICAL = case_when(COHORT == "historical" ~ 1,
                                TRUE ~ 0)) 


saveRDS(lookback_processed,paste0(path_pasc_diabetes_folder,"/dom abstract/pddabs_imputed lookback dataset.RDS"))


source(paste0(path_pasc_cmr_repo,"/analysis/pcra_selecting high dimensional variables based on fdr.R"))

hd_dataset_COHORT <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_")) %>% 
  dplyr::select(ID,one_of(selected_hdvars %>% 
                            # dplyr::filter(outcome == "bmi") %>% 
                            dplyr::select(variable) %>% 
                            pull() %>% unique()))

outcome_df <- lookback_processed %>% 
  # dplyr::filter(ID %in% bmi_ID) %>% 
  left_join(hd_dataset_COHORT,
            by = "ID")

write_parquet(outcome_df,paste0(path_pasc_diabetes_folder,"/dom abstract/pddabs_ipw for cohort membership data.parquet"))
