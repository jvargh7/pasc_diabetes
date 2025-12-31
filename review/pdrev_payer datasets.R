rm(list=ls());gc();source(".Rprofile")


pdadm001 <- readRDS(paste0(path_pasc_diabetes_folder,"/working/cleaned/pdadm001_analytic dataset for data availability.RDS"))

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS")) %>% 
  inner_join(pdadm001,
             by=c("ID","COHORT"))
# payer_pre -----------


payer_pre <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ADMIT_DATE,FACILITY_LOCATION,ENC_TYPE,FACILITYID,FACILITY_TYPE,PAYER_TYPE_PRIMARY,PAYER_TYPE_SECONDARY,site) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus365,COHORT),
             by = c("ID"="ID")) %>% 
  dplyr::filter(ADMIT_DATE >= index_date_minus365, ADMIT_DATE < index_date) %>% 
  # Insurance Type (None, Medicare, Medicaid, Private + Bluecross + Other) -----------
mutate(across(one_of("PAYER_TYPE_PRIMARY"), ~case_when(. %in% c("NI","UN","") ~ "No Information",
                                                       . %in% c("1","11","111","119") ~ "Medicare",
                                                       . %in% c("2","21","29") ~ "Medicaid",
                                                       . %in% c("3","311","32","349","382") ~ "Government",
                                                       . %in% c("8","81","82","821","822") ~ "None",
                                                       . %in% c("5","51","511",
                                                                "512","52","521",
                                                                "529","6","623") ~ "Private",
                                                       is.na(.) ~ "Missing",
                                                       TRUE ~ "Private"))) %>% 
  rename(payer_type_primary = PAYER_TYPE_PRIMARY) %>% 
  group_by(ID,ADMIT_DATE,index_date,index_date_minus365,payer_type_primary) %>% 
  tally()  %>% 
  collect()  %>% 
  pivot_wider(names_from = payer_type_primary,values_from=n)  %>% 
  mutate(admit_month = paste0(year(ADMIT_DATE),"-",month(ADMIT_DATE))) %>% 
  group_by(ID,admit_month) %>% 
  summarize(across(one_of(coverage_types),.fns=function(x) sum(x,na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(primary_payer = coverage_types[apply(.[,coverage_types],1,which.max)],
         secondary_payer = as.character(apply(.[,coverage_types],1,
                                              function(x) as.character(max_col(x,2)))))
saveRDS(payer_pre,paste0(paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev_payer pre.RDS")))

payer_post <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ADMIT_DATE,FACILITY_LOCATION,ENC_TYPE,FACILITYID,FACILITY_TYPE,PAYER_TYPE_PRIMARY,PAYER_TYPE_SECONDARY,site) %>% 
  right_join(index_date %>% 
               mutate(origin_date_plus65 = origin_date + days(365)) %>% 
               dplyr::select(ID,origin_date,origin_date_plus65,COHORT),
             by = c("ID"="ID")) %>% 
  dplyr::filter(ADMIT_DATE >= origin_date, ADMIT_DATE <= origin_date_plus65) %>% 
  # Insurance Type (None, Medicare, Medicaid, Private + Bluecross + Other) -----------
mutate(across(one_of("PAYER_TYPE_PRIMARY"), ~case_when(. %in% c("NI","UN","") ~ "No Information",
                                                       . %in% c("1","11","111","119") ~ "Medicare",
                                                       . %in% c("2","21","29") ~ "Medicaid",
                                                       . %in% c("3","311","32","349","382") ~ "Government",
                                                       . %in% c("8","81","82","821","822") ~ "None",
                                                       . %in% c("5","51","511",
                                                                "512","52","521",
                                                                "529","6","623") ~ "Private",
                                                       is.na(.) ~ "Missing",
                                                       TRUE ~ "Private"))) %>% 
  rename(payer_type_primary = PAYER_TYPE_PRIMARY) %>% 
  group_by(ID,ADMIT_DATE,origin_date,origin_date_plus65,payer_type_primary) %>% 
  tally()  %>% 
  collect()  %>% 
  pivot_wider(names_from = payer_type_primary,values_from=n)  %>% 
  mutate(admit_month = paste0(year(ADMIT_DATE),"-",month(ADMIT_DATE))) %>% 
  group_by(ID,admit_month) %>% 
  summarize(across(one_of(coverage_types),.fns=function(x) sum(x,na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(primary_payer = coverage_types[apply(.[,coverage_types],1,which.max)],
         secondary_payer = as.character(apply(.[,coverage_types],1,
                                              function(x) as.character(max_col(x,2)))))
saveRDS(payer_post,paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev_payer post.RDS"))
