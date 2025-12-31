rm(list=ls());gc();source(".Rprofile")

# Lookback dataset -----
source("review/pdrev_lookback processed.R")
pdadm001 <- readRDS(paste0(path_pasc_diabetes_folder,"/working/cleaned/pdadm001_analytic dataset for data availability.RDS"))

# cpit2dm_dection ---------

landmark_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre208_cpit2dm new onset diabetes during period till origin date.RDS"))  %>% 
  dplyr::select(ID,criterion2_date,CP) %>% 
  rename(last_followup_date = criterion2_date)
followup_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre202_cpit2dm new onset diabetes.RDS")) %>% 
  dplyr::select(ID,criterion2_date,CP) %>% 
  rename(last_followup_date = criterion2_date)

followup_not2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre202_noncpit2dm last followup.RDS")) %>% 
  dplyr::select(ID,last_followup_date) %>% 
  dplyr::filter(!ID %in% c(landmark_cpit2dm$ID,followup_cpit2dm$ID))

cpit2dm_detection <- bind_rows(landmark_cpit2dm,
                               followup_cpit2dm,
                               followup_not2dm) %>% 
  group_by(ID) %>% 
  dplyr::filter(last_followup_date == min(last_followup_date)) %>% 
  ungroup()

# index_date -------
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS")) %>% 
  left_join(cpit2dm_detection,
            by = "ID") %>% 
  mutate(max_followup_date = case_when(!is.na(last_followup_date) ~ last_followup_date,
                                       TRUE ~ max_followup_date)) %>% 
  mutate(max_month_relative_index = interval(index_date,max_followup_date) %/% months(1),
         months_followup_cpit2dm = interval(index_date,last_followup_date) / months(1)) 


# payer_12months ---------

payer_pre <- readRDS(paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev_payer pre.RDS")) %>%
  group_by(ID) %>% 
  summarize(across(one_of(coverage_types),.fns=function(x) sum(x,na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(primary_payer = coverage_types[apply(.[,coverage_types],1,which.max)],
         secondary_payer = as.character(apply(.[,coverage_types],1,
                                              function(x) as.character(max_col(x,2)))))

payer_post <-  readRDS(paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev_payer post.RDS")) %>%
  group_by(ID) %>% 
  summarize(across(one_of(coverage_types),.fns=function(x) sum(x,na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(primary_payer = coverage_types[apply(.[,coverage_types],1,which.max)],
         secondary_payer = as.character(apply(.[,coverage_types],1,
                                              function(x) as.character(max_col(x,2)))))

payer_12months <- bind_rows(
  payer_pre %>% mutate(post = 0),
  payer_post %>% mutate(post = 1)
)

# encounters_12months

encounters_12months <- bind_rows( readRDS(paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev_encounters pre_12months.RDS")) %>% mutate(post = 0),
                                  readRDS(paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev_encounters post_12months.RDS")) %>% mutate(post = 1))
                                  



# labs_12months ---------------
labs_12months <- readRDS(paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev01_labs.RDS"))  %>% 
  arrange(ID, month_relative_index) %>% 
  dplyr::filter(month_relative_index %in% c(-12:12)) %>% 
  mutate(post = case_when(month_relative_index %in% c(-12:-1) ~ 0,
                            month_relative_index %in% c(0:12) ~ 1)) %>% 
  group_by(ID,post) %>% 
  summarize(max_lab = max(abs(month_relative_index)),
            fastingglucose = sum(fastingglucose),
            glucose = sum(glucose),
            hba1c = sum(hba1c),
            any_diabetes = sum(any_diabetes)) 
  # across(one_of("fastingglucose","glucose","hba1c","any_diabetes"),.fns = function(x) sum(x))


# analytic_dataset --------------

analytic_dataset <- expand.grid(ID = index_date$ID,
                                post = c(0,1)) %>% 
  left_join(index_date %>% 
              dplyr::select(ID,index_date,max_followup_date,months_followup_cpit2dm,max_month_relative_index),
            by = "ID") %>% 
  
  left_join(labs_12months %>% ungroup() %>% 
              dplyr::rename_at(vars(one_of("fastingglucose","glucose","hba1c","any_diabetes")),.funs = function(x) paste0("n_",x)),
            by = c("ID","post")) %>% 
  left_join(payer_12months %>% ungroup,
            by = c("ID","post")) %>% 
  mutate(max_lab_imputed = case_when(post == 1 & (is.na(max_lab) | max_lab < 12) & max_month_relative_index >=12 ~ 12,
                             post == 0 ~ 12,
                             is.na(max_lab) ~ 12,
                             TRUE ~ max_lab)) %>% 
  
  left_join(encounters_12months %>% ungroup() %>% rename(n_encounters = n),
            by = c("ID","post")) %>% 
  
  mutate(across(one_of("n_fastingglucose","n_glucose","n_hba1c","n_any_diabetes","n_encounters"),.fns=function(x) case_when(is.na(x) ~ 0,
                                                                                                     TRUE ~ x))) %>% 
  mutate(across(one_of("primary_payer"),.fns=function(x) case_when(is.na(x) ~ "None",
                                                                   TRUE ~ x))) %>% 
  
  
  left_join(lookback_processed %>% dplyr::select(-index_date),
            by = "ID") %>% 
  # Exclude those whose post-detection happened in month of index date
  # Only month they had something was when they were in index
  dplyr::filter(max_lab_imputed > 0)  %>% 
  group_by(ID) %>% 
  mutate(post_count = n()) %>% 
  ungroup() %>% 
  dplyr::filter(post_count == 2) %>% 
  dplyr::select(-post_count) %>% 
  mutate(year_index = year(index_date),
         n_any_diabetes_rate = case_when(max_lab_imputed %in% c(0,1) ~ n_any_diabetes/1,
                                         TRUE ~ n_any_diabetes/max_lab_imputed)) %>% 
  data.frame()

analytic_dataset %>% 
  inner_join(pdadm001 %>% dplyr::select(ID),
             by="ID") %>% 
saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev02_analytic_dataset.RDS"))  
