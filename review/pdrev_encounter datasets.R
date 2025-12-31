rm(list=ls());gc();source(".Rprofile")


pdadm001 <- readRDS(paste0(path_pasc_diabetes_folder,"/working/cleaned/pdadm001_analytic dataset for data availability.RDS"))

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS")) %>% 
  inner_join(pdadm001,
             by=c("ID","COHORT"))
# encounters_pre -----------

encounters_pre <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,
                ADMIT_DATE,DISCHARGE_DATE)  %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus365,COHORT),
             by = c("ID"="ID")) %>% 
  dplyr::filter(ENC_TYPE %in% permissible_enc_type) %>% 
  dplyr::filter(ADMIT_DATE >= index_date_minus365, ADMIT_DATE < index_date)  %>% 
  group_by(ID) %>% 
  tally()  %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)

saveRDS(encounters_pre,paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev_encounters pre_12months.RDS"))

# encounters_post -----------

encounters_post <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ADMIT_DATE,FACILITY_LOCATION,ENC_TYPE,FACILITYID,FACILITY_TYPE,PAYER_TYPE_PRIMARY,PAYER_TYPE_SECONDARY,site) %>% 
  right_join(index_date %>% 
               mutate(origin_date_plus65 = origin_date + days(365)) %>% 
               dplyr::select(ID,origin_date,origin_date_plus65,COHORT),
             by = c("ID"="ID")) %>% 
  dplyr::filter(ENC_TYPE %in% permissible_enc_type) %>% 
  dplyr::filter(ADMIT_DATE >= origin_date, ADMIT_DATE <= origin_date_plus65)  %>% 
  group_by(ID) %>% 
  tally()  %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)

saveRDS(encounters_post,paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev_encounters post_12months.RDS"))
