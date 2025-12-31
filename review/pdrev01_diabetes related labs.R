rm(list=ls());gc();source(".Rprofile")



pdadm001 <- readRDS(paste0(path_pasc_diabetes_folder,"/working/cleaned/pdadm001_analytic dataset for data availability.RDS"))

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS")) %>% 
  inner_join(pdadm001,
             by=c("ID","COHORT"))

labs <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  mutate(ID = as.character(ID)) %>% 
  
  right_join(index_date %>% 
               dplyr::select(ID,index_date,COHORT),
             by = c("ID")) %>% 
  ungroup() %>% 
  # -0.1 --> -1, 0.1 --> 0, 0.9 --> 0, 1.1 --> 1
  mutate(month_relative_index = floor((as.numeric(as.integer(LAB_ORDER_DATE)) - as.numeric(as.integer(index_date)))/30.4),
         fastingglucose = case_when(LAB_LOINC %in% fastingglucose_loinc ~ 1,
                                    TRUE ~ 0),
         glucose = case_when(LAB_LOINC %in% glucose_loinc ~ 1,
                             TRUE ~ 0),
         hba1c = case_when(str_detect(RAW_LAB_NAME,"TROPONIN") ~ 0,
                           LAB_LOINC %in% hba1c_loinc ~ 1,
                           TRUE ~ 0),
         any_diabetes = case_when(fastingglucose == 1 | glucose == 1 | hba1c == 1 ~ 1,
                                  TRUE ~ 0)) %>% 
  # dplyr::filter(any_diabetes == 1) %>% 
  group_by(ID,COHORT, month_relative_index) %>% 
  summarize(fastingglucose = sum(fastingglucose),
            glucose = sum(glucose),
            hba1c = sum(hba1c),
            any_diabetes = sum(any_diabetes)) %>% 
  collect() %>% 
  # Many individuals have 2 rows for month_relative_index == 0
  group_by(ID,COHORT, month_relative_index) %>% 
  summarize(fastingglucose = sum(fastingglucose),
            glucose = sum(glucose),
            hba1c = sum(hba1c),
            any_diabetes = sum(any_diabetes))

View(labs %>% group_by(ID,month_relative_index) %>% tally() %>% dplyr::filter(n >= 2))

saveRDS(labs,paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev01_labs.RDS"))  
