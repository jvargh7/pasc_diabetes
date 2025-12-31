# Inpatient diagnosis

rm(list=ls());gc();source(".Rprofile")
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

death <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre102_death.RDS"))  %>% 
  group_by(ID) %>%
  summarize(DEATH_DATE = max(DEATH_DATE)) %>% 
  ungroup()

# SUPREME-DM lookback of 18 months
source(paste0(path_pasc_diabetes_repo,"/functions/encounter_check_supreme.R"))

ip_diagnosis <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(DX_DATE >= origin_date,DX_DATE  <= max_followup_date)  %>% 
  dplyr::filter(!str_detect(DX,paste0("(",paste0(c(icd10_otherdm_excluding,
                                                   icd10_t1dm,icd10_gdm),collapse="|"),")"))) %>% 
  dplyr::filter(DX %in% icd10_dm_qualifying,ENC_TYPE %in% c("IP")) %>%
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)

# HbA1c
hba1c <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(str_detect(RAW_LAB_NAME,"(A1C|A1c)")) %>% 
  dplyr::filter(LAB_LOINC %in% hba1c_loinc |
                  # The below have LAB_LOINC == ""
                  RAW_LAB_NAME %in% c("(HEMOGLOBIN A1C|Hemoglobin A1c|Hemoglobin A1C|POCT HBA1C|HM HBA1C)")) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID"))  %>% 
  dplyr::filter(SPECIMEN_DATE >= origin_date,SPECIMEN_DATE <= max_followup_date)  %>% 
  # Correction
  mutate(value = case_when(RESULT_NUM > 20 ~ NA_real_,
                           TRUE ~ RESULT_NUM)) %>% 
  
  # NI: No information
  mutate(high_hba1c = case_when(
    # RESULT_QUAL %in% c("HIGH")  --> cannot use this since it counts >=6% as HIGH 
    value >= 6.5 ~ 1,
    RESULT_QUAL %in% c("LOW","NEGATIVE","NI","OT","UNDETECTABLE") | (value>0 & value < 6.5) ~ 0,
    TRUE ~ NA_real_)) %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)

# Fasting plasma glucose
fpg <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(str_detect(RAW_LAB_NAME,"(glucose|Glucose)")) %>% 
  dplyr::filter(LAB_LOINC %in% fastingglucose_loinc) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID"))  %>% 
  dplyr::filter(SPECIMEN_DATE >= origin_date,SPECIMEN_DATE <= max_followup_date)  %>%
  # NI: No information
  mutate(high_fpg = case_when(
    # RESULT_QUAL %in% c("HIGH")  --> cannot use this since it counts >=6% as HIGH 
    RESULT_NUM >= 126 ~ 1,
    RESULT_QUAL %in% c("LOW","NEGATIVE","NI","OT","UNDETECTABLE") | (RESULT_NUM> 50 & RESULT_NUM < 126) ~ 0,
    TRUE ~ NA_real_)) %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)


# Random plasma glucose
rpg <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(str_detect(RAW_LAB_NAME,"(glucose|Glucose)")) %>% 
  dplyr::filter(LAB_LOINC %in% glucose_loinc) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID"))  %>% 
  dplyr::filter(SPECIMEN_DATE >= origin_date,SPECIMEN_DATE <= max_followup_date)  %>%
  # NI: No information
  mutate(high_rpg = case_when(
    # RESULT_QUAL %in% c("HIGH")  --> cannot use this since it counts >=6% as HIGH 
    RESULT_NUM >= 200 ~ 1,
    RESULT_QUAL %in% c("LOW","NEGATIVE","NI","OT","UNDETECTABLE") | (RESULT_NUM> 50 & RESULT_NUM < 200) ~ 0,
    TRUE ~ NA_real_)) %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)


# Outpatient diagnosis code
non_ip_enc_type = permissible_enc_type[!permissible_enc_type %in% c("IP")]

op_diagnosis <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(DX_DATE >= origin_date,DX_DATE  <= max_followup_date)  %>% 
  dplyr::filter(!str_detect(DX,paste0("(",paste0(c(icd10_otherdm_excluding,
                                                   icd10_t1dm,icd10_gdm),collapse="|"),")"))) %>% 
  dplyr::filter(DX %in% icd10_dm_qualifying,
                ENC_TYPE %in% non_ip_enc_type) %>%
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)

# Medication (filled prescription - not available)
rxcui_list <- readxl::read_excel(paste0(path_pasc_cmr_repo,"/data/PASC CMR Variable List.xlsx"),sheet="medication") %>% 
  rename(drug_class = 'Drug class',
         drug_name = 'Drug name') %>% 
  # AMYLIN ANALOG added which was not there in Wiese 2018
  dplyr::filter(drug_class %in% c("INSULIN","METFORMIN","SGLT2 INHIBITORS",
                                  "GLP1 RA","DPP4 INHIBITOR","THIAZOLIDINEDIONES",
                                  "SULFONYLUREAS","MEGLITINIDES","AGI","AMLYLIN ANALOG")) %>% 
  dplyr::select(RXCUI) %>% 
  pull() %>% 
  na.omit()

dm_medication <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  # Limit to permissible encounters
  left_join(open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
              dplyr::select(ID,ENCOUNTERID, ENC_TYPE),
            by = c("ID","ENCOUNTERID")) %>% 
  dplyr::filter(ENC_TYPE %in% permissible_enc_type) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(RX_ORDER_DATE >= origin_date,RXNORM_CUI %in% rxcui_list,RX_ORDER_DATE <= max_followup_date) %>% 
  collect()  %>% 
  dplyr::filter(ID %in% included_patients$ID) %>% 
  left_join(readxl::read_excel(paste0(path_pasc_cmr_repo,"/data/PASC CMR Variable List.xlsx"),sheet="medication") %>% 
              rename(drug_class = 'Drug class',
                     drug_name = 'Drug name') %>% 
              # AMYLIN ANALOG added which was not there in Wiese 2018
              dplyr::filter(drug_class %in% c("INSULIN","METFORMIN","SGLT2 INHIBITORS",
                                              "GLP1 RA","DPP4 INHIBITOR","THIAZOLIDINEDIONES",
                                              "SULFONYLUREAS","MEGLITINIDES","AGI","AMLYLIN ANALOG")) %>% 
              dplyr::select(RXCUI,drug_class) %>% 
              distinct(RXCUI,.keep_all=TRUE),
            by = c("RXNORM_CUI" = "RXCUI"))

# 2 prescriptions of same metformin or TZD without any other indication of diabetes not counted



# CP1: Inpatient ------------

cp1 <- ip_diagnosis %>% 
  group_by(ID) %>% 
  dplyr::summarize(n_dm_diagnosis = n(),
                   criterion1_date = min(DX_DATE))  %>% 
  mutate(
         criterion2_date = NA_Date_) %>% 
  mutate(criterion1_date_minus549 = criterion1_date - days(549)) %>% 
  dplyr::filter(ID %in% included_patients$ID)
  
cp1_encounter_check = encounter_check_supreme(cp1) %>% 
  dplyr::filter(n>=1)

cp1_valid <- cp1 %>%
  right_join(cp1_encounter_check,
             by="ID")

# CP2: Any of others ------------

cp2 <- bind_rows(
  
  hba1c %>% 
    dplyr::filter(high_hba1c == 1) %>% 
    dplyr::select(ID, SPECIMEN_DATE) %>% 
    rename(included_date = SPECIMEN_DATE) %>% 
    mutate(type = "hba1c"),
  
  rpg %>% 
    dplyr::filter(high_rpg == 1) %>% 
    dplyr::select(ID, SPECIMEN_DATE) %>% 
    rename(included_date = SPECIMEN_DATE) %>% 
    mutate(type = "rpg"),
  
  fpg %>%
    dplyr::filter(high_fpg == 1) %>%
    dplyr::select(ID, SPECIMEN_DATE) %>%
    rename(included_date = SPECIMEN_DATE) %>%
    mutate(type = "fpg"),
  # 
  op_diagnosis %>%
    dplyr::select(ID, DX_DATE) %>%
    rename(included_date = DX_DATE) %>%
    mutate(type = "op"),
  
  dm_medication %>%
    dplyr::select(ID, RX_ORDER_DATE,drug_class) %>%
    rename(included_date = RX_ORDER_DATE) %>%
    mutate(type = "prescribing")
  
) %>% 
# One unique row for a combination of patient, date, type of flag and drug class
  distinct(ID,included_date,type,drug_class) %>% 
  
  mutate(score = 1, # Every row is imputed with 1
  
		# For non-med flags, impute a blank character -- optional
         drug_class = case_when(is.na(drug_class) ~ "",
                                TRUE ~ drug_class)) %>% 
	# Convert dataset to wide to identify if more than 1 observation occurs on a date -- automatically identified as a T2DM case
  pivot_wider(names_from=c("type","drug_class"),values_from=score,values_fill = 0) %>% 
  mutate(others_true = rowSums(.[,c("hba1c_","rpg_","fpg_","op_",
                                 "prescribing_INSULIN","prescribing_SGLT2 INHIBITORS",
                                 "prescribing_SULFONYLUREAS","prescribing_GLP1 RA",
                                 "prescribing_DPP4 INHIBITOR","prescribing_MEGLITINIDES",
                                 "prescribing_AMLYLIN ANALOG","prescribing_AGI")]),
		 procedures_true = rowSums(.[,c("hba1c_","rpg_","fpg_","op_")]),
		 otherrx_true = rowSums(.[,c(
                                 "prescribing_INSULIN","prescribing_SGLT2 INHIBITORS",
                                 "prescribing_SULFONYLUREAS","prescribing_GLP1 RA",
                                 "prescribing_DPP4 INHIBITOR","prescribing_MEGLITINIDES",
                                 "prescribing_AMLYLIN ANALOG","prescribing_AGI")]),
         metformin_tzd = rowSums(.[,c("prescribing_METFORMIN","prescribing_THIAZOLIDINEDIONES")]),
         
         any_true = rowSums(.[,c("hba1c_","rpg_","fpg_","op_",
                                 "prescribing_INSULIN","prescribing_SGLT2 INHIBITORS",
                                 "prescribing_SULFONYLUREAS","prescribing_GLP1 RA",
                                 "prescribing_DPP4 INHIBITOR","prescribing_MEGLITINIDES",
                                 "prescribing_AMLYLIN ANALOG","prescribing_AGI",
                                 "prescribing_METFORMIN","prescribing_THIAZOLIDINEDIONES")])
         ) %>% 
	# Sort by date 
  arrange(ID,included_date) %>% 
  # Label each date for a patient from 1 to max number of rows
  group_by(ID) %>% 
  mutate(event_index = 1:n()) %>% 
  ungroup() %>% 
  # Merge the table with itself on the patient identifier
  left_join(.,
            {.} %>% 
              dplyr::select(ID,included_date,others_true,metformin_tzd,any_true) %>% 
              # rename columns to avoid duplication
              rename(criterion2_date = included_date,
                     c2_others_true = others_true,
                     c2_metformin_tzd = metformin_tzd,
                     c2_any_true = any_true),
            by = c("ID")) %>% 
  # Restrict to instances when different encounters occur on separate days within 2 years (730 days) of each other
  dplyr::filter(included_date < criterion2_date,criterion2_date <= (included_date + days(730))) %>% 
  # Define T2DM
  mutate(incident_dm = case_when(
    # others_true >= 1 & c2_others_true >= 1 ~ 1, -- This will count each medication class as a flag. Identifies 5x more cases.
    # Same day criteria (included_date) ---
    procedures_true > 1 ~ 10, # Any combination of A1c, FPG, RPG or Dx on same day 
    procedures_true == 1 & otherrx_true >= 1 ~ 10,
    metformin_tzd >= 1 & others_true >= 1 ~ 10,
    # Later day criteria (included_date + criterion2_date) ----
    others_true >= 1 & c2_others_true >= 1 ~ 20,
    metformin_tzd >= 1 & c2_others_true >= 1 ~ 20,
    others_true >= 1 & c2_metformin_tzd >= 1 ~ 20,
    TRUE ~ 0)) %>% 
  # Take the earliest date of displaying an incident_dm
  group_by(ID) %>% 
  dplyr::filter(included_date == min(included_date),
                incident_dm %in% c(10,20)) %>% 
  ungroup() %>% 
  # Remove duplicates based on patient ID and earliest date
  distinct(ID,included_date,.keep_all=TRUE) %>% 
  # 
  rename(criterion1_date = included_date) %>%
  mutate(diagnosis_date = case_when(incident_dm == 10 ~ criterion1_date,
                                    TRUE ~ criterion2_date)) %>% 
  mutate(incident_dm = 1) %>% 
  # Are there encounters in the previous 1.5 years?
  mutate(criterion1_date_minus549 = criterion1_date - days(549)) %>% 
  dplyr::filter(ID %in% included_patients$ID)

# Check ~pasc_diabetes/functions/encounter_check_supreme
# Are there 
cp2_encounter_check = encounter_check_supreme(cp2) %>% 
  dplyr::filter(n>=1)

cp2_valid <- cp2 %>%
  right_join(cp2_encounter_check,
             by="ID")

# New onset diabetes ---------
# Diagnosis code assigned to latter date of pair of events

supremedm <- bind_rows(cp1_valid %>%
					    mutate(diagnosis_date = criterion1_date) %>% 
                       mutate(
                         CP = "CP1"),
                     
                     cp2_valid %>% 
                       mutate(
                         CP = "CP2")) %>% 
  group_by(ID) %>% 
  dplyr::filter(diagnosis_date == min(diagnosis_date)) %>% 
  slice(1) %>% 
  ungroup() 


supremedm %>% 
  # dplyr::select() %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/cleaned/pdpre202_supremedm new onset diabetes.RDS"))


# Remaining individuals duration --------------

supremedm_ID = supremedm$ID

# Last value before death if it is available

labs_max = open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(!ID %in% supremedm_ID) %>% 
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

diagnosis_max <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(!ID %in% supremedm_ID,ENC_TYPE %in% permissible_enc_type) %>% 
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

medication_max <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(!ID %in% supremedm_ID) %>% 
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

non_supremedm <- bind_rows(
  labs_max %>% 
    mutate(last_followup_date = SPECIMEN_DATE),
  diagnosis_max %>% 
    mutate(last_followup_date = DX_DATE),
  medication_max %>% 
    mutate(last_followup_date = RX_ORDER_DATE)
)  %>% 
  group_by(ID) %>% 
  dplyr::filter(last_followup_date == max(last_followup_date)) %>% 
  slice(1) %>% 
  ungroup()

non_supremedm %>% 
  # dplyr::select() %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/cleaned/pdpre202_nonsupremedm last followup.RDS"))

