

source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab004_matchid dataset.R"))

tx_weights_df <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm301_ip weights for cohort membership.RDS"))
mo_weights_df <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm301_ip weights for missing outcomes.RDS"))

# Different from imbalanced variables in pasc_cardiometabolic_risk/analysis bmi/pcrab302_analytic dataset with ip weights for bmi.R
imbalanced_variables <- c("age","nhblack","hispanic","nhother","smoking","site","hospitalization","bmi","ldl")


before_matchid <- imputed_dataset %>% 
  left_join(matchid_df %>% dplyr::select(-COHORT),
            by = "ID")

analytic_dataset_lookback = before_matchid %>% 
  rename(nhblack = raceeth_category_2,
         hispanic = raceeth_category_3,
         nhother = raceeth_category_4) %>% 
  mutate(site = case_when(is.na(site) ~ "Source 1",
                          TRUE ~ site))

rm(before_matchid)

cpit2dm_df = all_cpit2dm_df %>% 
  dplyr::filter(ID %in% analytic_sample$ID) %>% 
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
