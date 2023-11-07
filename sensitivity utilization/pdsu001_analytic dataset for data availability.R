
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

lab_followup_wide_ID <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre405_standard labs during followup_wide.RDS")) %>% 
  group_by(ID) %>% 
  tally() %>% 
  dplyr::filter(n >= 1) %>% 
  dplyr::select(ID) %>% 
  pull()


death <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre102_death.RDS"))


index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))
demographic <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre101_demographic.RDS"))
all_cpit2dm_df = bind_rows(readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre202_cpit2dm new onset diabetes.RDS")) %>% 
                         dplyr::select(ID,criterion2_date,CP) %>% rename(last_followup_date = criterion2_date) %>% 
                         mutate(incident_dm = 1),
                       readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre202_noncpit2dm last followup.RDS")) %>% 
                         dplyr::select(ID,last_followup_date) %>% 
                         mutate(incident_dm = 0))  %>% 
  dplyr::filter(!ID %in% c(
                           # lookback_cpit2dm$ID,
                           landmark_cpit2dm$ID)) %>% 
  left_join(index_date %>% 
              dplyr::select(ID, COHORT, origin_date),
            by = "ID") %>% 
  mutate(t = as.numeric(last_followup_date - origin_date))


cpit2dm_followup_summary = all_cpit2dm_df %>% 
  dplyr::filter(t >= 100, ID %in% lab_followup_wide_ID)

saveRDS(cpit2dm_followup_summary,paste0(path_pasc_diabetes_folder,"/working/sensitivity utilization/pdsu001_cpit2dm followup summary.RDS"))


# # qc for death --------
# 
# all_cpit2dm_df %>% 
# left_join(death,by= "ID") %>% 
#   dplyr::filter(!is.na(DEATH_DATE)) %>% 
#   dplyr::filter(DEATH_DATE < last_followup_date) %>% 
#   mutate(difference = difftime(DEATH_DATE,last_followup_date,units="days")) %>% 
#   write_csv(.,paste0(path_pasc_diabetes_folder,"/working/QC pdadm001_death before last follow-up.csv"))
# saveRDS(all_cpit2dm_df,paste0(path_pasc_diabetes_folder,"/working/QC padadm001_all cpit2dm df before death exclusion.RDS"))

outcome_availability = demographic %>% 
  dplyr::select(ID, COHORT,matchid) %>% 
  mutate(in_bmi_lookback_ID = case_when(ID %in% lookback_df$ID ~ 1,
                                        TRUE ~ 0),
         in_sensitivity_utilization_ID = case_when(ID %in% cpit2dm_followup_summary$ID ~ 1,
                                        TRUE ~ 0))

analytic_sample = outcome_availability %>% 
  dplyr::filter(in_bmi_lookback_ID == 1)

sum(analytic_sample$in_sensitivity_utilization_ID)
