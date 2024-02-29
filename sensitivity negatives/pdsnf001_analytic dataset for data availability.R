
rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R"))
# lookback_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre209_cpit2dm diabetes during lookback period.RDS"))
landmark_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre208_cpit2dm new onset diabetes during period till origin date.RDS"))


lookback_df <- lookback_df %>% 
  dplyr::filter(!is.na(bmi),
                !ID %in% c(
                           # lookback_cpit2dm$ID,
                           landmark_cpit2dm$ID)
                )

death <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre102_death.RDS"))


index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))
demographic <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre101_demographic.RDS"))
all_cpitf_df = bind_rows(readRDS(paste0(path_pasc_diabetes_folder,"/working/cleaned/pdsn001_cp traumatic fractures.RDS")) %>% 
                         dplyr::select(ID,criterion1_date) %>% 
                           mutate(last_followup_date = criterion1_date) %>% 
                         mutate(incident_tf = 1),
                       readRDS(paste0(path_pasc_diabetes_folder,"/working/cleaned/pdsn_noncpitf last followup.RDS")) %>% 
                         dplyr::select(ID,last_followup_date) %>% 
                         mutate(incident_tf = 0))  %>% 
  dplyr::filter(!ID %in% c(
                           # lookback_cpit2dm$ID,
                           landmark_cpit2dm$ID)) %>% 
  left_join(index_date %>% 
              dplyr::select(ID, COHORT, origin_date),
            by = "ID") %>% 
  mutate(t = as.numeric(last_followup_date - origin_date))



outcome_availability = demographic %>% 
  dplyr::select(ID, COHORT,matchid) %>% 
  mutate(in_bmi_lookback_ID = case_when(ID %in% lookback_df$ID ~ 1,
                                        TRUE ~ 0),
         in_tf_followup_ID = case_when(ID %in% all_cpitf_df$ID ~ 1,
                                        TRUE ~ 0))

analytic_sample = outcome_availability %>% 
  dplyr::filter(in_bmi_lookback_ID == 1)

table(analytic_sample$ID %in% death$ID) %>% prop.table()

