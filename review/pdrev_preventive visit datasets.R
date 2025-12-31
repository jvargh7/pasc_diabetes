
# preventive_visit_pre ----------

pdadm001 <- readRDS(paste0(path_pasc_diabetes_folder,"/working/cleaned/pdadm001_analytic dataset for data availability.RDS"))

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS")) %>% 
  inner_join(pdadm001,
             by=c("ID","COHORT"))

open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/procedures_",version,".parquet"))  %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,origin_date,index_date,index_date_minus365,index_date_minus730, COHORT),
             by = c("ID"))


preventive_visit_pre <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/procedures_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,
                ADMIT_DATE,DISCHARGE_DATE)  %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus365,COHORT),
             by = c("ID"="ID")) %>% 
  dplyr::filter(ENC_TYPE %in% permissible_enc_type) %>% 
  dplyr::filter(ADMIT_DATE >= index_date_minus365, ADMIT_DATE < index_date)  %>% 
  mutate(m = month(ADMIT_DATE),
         y = year (ADMIT_DATE)) %>% 
  group_by(ID,y,m) %>% 
  tally()  %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)