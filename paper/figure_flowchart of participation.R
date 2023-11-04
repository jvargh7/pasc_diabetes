rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R"))
# lookback_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre209_cpit2dm diabetes during lookback period.RDS"))
landmark_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre208_cpit2dm new onset diabetes during period till origin date.RDS"))
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

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

# step1_bmi_df = lookback_df %>% 
#   dplyr::filter(!ID %in% c(lookback_cpit2dm$ID))
# 
s1_c = lookback_df %>%
  group_by(COHORT) %>%
  tally()

step2_bmi_df <- lookback_df %>% 
  dplyr::filter(!is.na(bmi),!ID %in% landmark_cpit2dm$ID)

s2_c = step2_bmi_df %>% 
  group_by(COHORT) %>% 
  tally()

step3_cpit2dm_df = step2_bmi_df %>% 
  inner_join(all_cpit2dm_df %>% dplyr::select(-COHORT),
             by = c("ID"))

# Unique patients
s3_p = step3_cpit2dm_df %>% 
  distinct(COHORT,ID) %>% 
  group_by(COHORT) %>% 
  tally()

# Unique cases
s3_c = step3_cpit2dm_df %>% 
  dplyr::filter(incident_dm == 1) %>% 
  group_by(COHORT) %>% 
  tally()

# Median follow-up time
s3_t = step3_cpit2dm_df %>% 
  group_by(COHORT) %>% 
  summarize(s = paste0(median(t)," [",
                       quantile(t,0.25),", ",
                       quantile(t,0.75),"]"))
