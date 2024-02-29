rm(list=ls());gc();source(".Rprofile")
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))
death <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre102_death.RDS"))  %>% 
  group_by(ID) %>%
  summarize(DEATH_DATE = max(DEATH_DATE)) %>% 
  ungroup()
traumatic_fractures <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(DX_DATE >= origin_date,DX_DATE  <= max_followup_date)  %>% 
  dplyr::filter(str_detect(DX,paste0("(",paste0(c(icd10_traumatic_fractures),collapse="|"),")")),ENC_TYPE %in% permissible_enc_type) %>%
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)

otitis_media <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(DX_DATE >= origin_date,DX_DATE  <= max_followup_date)  %>% 
  dplyr::filter(str_detect(DX,paste0("(",paste0(c(icd10_otitis_media),collapse="|"),")")),ENC_TYPE %in% permissible_enc_type) %>%
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)


# CP: Traumatic Fractures
cpitf <- traumatic_fractures %>% 
  rename(criterion1_date = DX_DATE) %>% 
  group_by(ID) %>% 
  dplyr::filter(criterion1_date == min(criterion1_date)) %>% 
  slice(1) %>% 
  ungroup() 

# CP: Otitis Media

cpiom <- otitis_media %>% 
  rename(criterion1_date = DX_DATE) %>% 
  group_by(ID) %>% 
  dplyr::filter(criterion1_date == min(criterion1_date)) %>% 
  slice(1) %>% 
  ungroup() 


cpitf %>% 
  # dplyr::select() %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/cleaned/pdsn001_cp traumatic fractures.RDS"))

cpiom %>% 
  # dplyr::select() %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/cleaned/pdsn001_cp otitis media.RDS"))

# Non-CP: Traumatic Fractures

# Remaining individuals duration Traumatic Fractures --------------

cpitf_ID = cpitf$ID

# Last value before death if it is available

labs_max_cpitf = open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(!ID %in% cpitf_ID) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID"))  %>% 
  dplyr::filter(SPECIMEN_DATE >= origin_date,SPECIMEN_DATE <= max_followup_date) %>% 
  distinct(ID,SPECIMEN_DATE) %>% 
  left_join(death,
            by = "ID") %>% 
  dplyr::filter(is.na(DEATH_DATE) | (SPECIMEN_DATE < DEATH_DATE)) %>% 
  collect() %>% 
  group_by(ID) %>% 
  dplyr::filter(SPECIMEN_DATE == max(SPECIMEN_DATE)) %>% 
  ungroup() %>% 
  dplyr::filter(ID %in% included_patients$ID)

diagnosis_max_cpitf <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(!ID %in% cpitf_ID,ENC_TYPE %in% permissible_enc_type) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(DX_DATE >= origin_date,DX_DATE <= max_followup_date) %>%
  distinct(ID,DX_DATE) %>% 
  left_join(death,
            by = "ID") %>% 
  dplyr::filter(is.na(DEATH_DATE) | (DX_DATE < DEATH_DATE)) %>% 
  collect() %>% 
  group_by(ID) %>% 
  dplyr::filter(DX_DATE == max(DX_DATE)) %>% 
  ungroup() %>% 
  dplyr::filter(ID %in% included_patients$ID)

medication_max_cpitf <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(!ID %in% cpitf_ID) %>% 
  # Limit to permissible encounters
  left_join(open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
              dplyr::select(ID,ENCOUNTERID, ENC_TYPE),
            by = c("ID","ENCOUNTERID")) %>% 
  dplyr::filter(ENC_TYPE %in% permissible_enc_type) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(RX_ORDER_DATE >= origin_date,RX_ORDER_DATE <= max_followup_date) %>% 
  distinct(ID,RX_ORDER_DATE) %>% 
  left_join(death,
            by = "ID") %>% 
  dplyr::filter(is.na(DEATH_DATE) | (RX_ORDER_DATE < DEATH_DATE)) %>% 
  collect() %>% 
  group_by(ID) %>% 
  dplyr::filter(RX_ORDER_DATE == max(RX_ORDER_DATE)) %>% 
  ungroup() %>% 
  dplyr::filter(ID %in% included_patients$ID)

noncpitf <- bind_rows(
  labs_max_cpitf %>% 
    mutate(last_followup_date = SPECIMEN_DATE),
  diagnosis_max_cpitf %>% 
    mutate(last_followup_date = DX_DATE),
  medication_max_cpitf %>% 
    mutate(last_followup_date = RX_ORDER_DATE)
)  %>% 
  group_by(ID) %>% 
  dplyr::filter(last_followup_date == max(last_followup_date)) %>% 
  slice(1) %>% 
  ungroup()

noncpitf %>% 
  # dplyr::select() %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/cleaned/pdsn_noncpitf last followup.RDS"))


# Remaining individuals duration Traumatic Fractures --------------

cpiom_ID = cpiom$ID

# Last value before death if it is available

labs_max_cpiom = open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(!ID %in% cpiom_ID) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID"))  %>% 
  dplyr::filter(SPECIMEN_DATE >= origin_date,SPECIMEN_DATE <= max_followup_date) %>% 
  distinct(ID,SPECIMEN_DATE) %>% 
  left_join(death,
            by = "ID") %>% 
  dplyr::filter(is.na(DEATH_DATE) | (SPECIMEN_DATE < DEATH_DATE)) %>% 
  collect() %>% 
  group_by(ID) %>% 
  dplyr::filter(SPECIMEN_DATE == max(SPECIMEN_DATE)) %>% 
  ungroup() %>% 
  dplyr::filter(ID %in% included_patients$ID)

diagnosis_max_cpiom <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(!ID %in% cpiom_ID,ENC_TYPE %in% permissible_enc_type) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(DX_DATE >= origin_date,DX_DATE <= max_followup_date) %>%
  distinct(ID,DX_DATE) %>% 
  left_join(death,
            by = "ID") %>% 
  dplyr::filter(is.na(DEATH_DATE) | (DX_DATE < DEATH_DATE)) %>% 
  collect() %>% 
  group_by(ID) %>% 
  dplyr::filter(DX_DATE == max(DX_DATE)) %>% 
  ungroup() %>% 
  dplyr::filter(ID %in% included_patients$ID)

medication_max_cpiom <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(!ID %in% cpiom_ID) %>% 
  # Limit to permissible encounters
  left_join(open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
              dplyr::select(ID,ENCOUNTERID, ENC_TYPE),
            by = c("ID","ENCOUNTERID")) %>% 
  dplyr::filter(ENC_TYPE %in% permissible_enc_type) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(RX_ORDER_DATE >= origin_date,RX_ORDER_DATE <= max_followup_date) %>% 
  distinct(ID,RX_ORDER_DATE) %>% 
  left_join(death,
            by = "ID") %>% 
  dplyr::filter(is.na(DEATH_DATE) | (RX_ORDER_DATE < DEATH_DATE)) %>% 
  collect() %>% 
  group_by(ID) %>% 
  dplyr::filter(RX_ORDER_DATE == max(RX_ORDER_DATE)) %>% 
  ungroup() %>% 
  dplyr::filter(ID %in% included_patients$ID)

noncpiom <- bind_rows(
  labs_max_cpiom %>% 
    mutate(last_followup_date = SPECIMEN_DATE),
  diagnosis_max_cpiom %>% 
    mutate(last_followup_date = DX_DATE),
  medication_max_cpiom %>% 
    mutate(last_followup_date = RX_ORDER_DATE)
)  %>% 
  group_by(ID) %>% 
  dplyr::filter(last_followup_date == max(last_followup_date)) %>% 
  slice(1) %>% 
  ungroup()

noncpiom %>% 
  # dplyr::select() %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/cleaned/pdsn_noncpiom last followup.RDS"))


