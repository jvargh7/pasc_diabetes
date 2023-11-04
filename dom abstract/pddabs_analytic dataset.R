lookback_processed <-readRDS(paste0(path_pasc_diabetes_folder,"/dom abstract/pddabs_imputed lookback dataset.RDS")) %>% 
  dplyr::filter(!is.na(bmi))

source(paste0(path_pasc_cmr_repo,"/analysis archive/pcra_matchid dataset.R"))

incident_dm_df = bind_rows(readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/cpit2dm new onset diabetes.RDS")) %>% 
                             dplyr::select(ID,criterion2_date) %>% rename(last_followup_date = criterion2_date) %>% 
                             mutate(incident_dm = 1),
                           readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/noncpit2dm last followup.RDS")) %>% 
                             dplyr::select(ID,last_followup_date) %>% 
                             mutate(incident_dm = 0)) 

bmi_ID <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/anthro followup.RDS")) %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::select(ID) %>% 
  pull()

# names(lookback_processed)
# [1] "ID"                    "female"                "nhwhite"               "nhblack"               "hispanic"              "nhother"               "age"                  
# [8] "matchid"               "index_date"            "site"                  "payer_type_primary"    "payer_type_secondary"  "hospitalization"       "n_hospitalized"       
# [15] "n_not_hospitalized"    "p_hyperglycemia"       "bmi"                   "HT"                    "SYSTOLIC"              "smoking"               "obesity"              
# [22] "cardiovascular"        "cerebrovascular"       "hypertension"          "pulmonary"             "hyperlipidemia"        "antidepressants"       "antipsychotics"       
# [29] "antihypertensives"     "statins"               "immunosuppresants"     "hba1c"                 "glucose"               "alt"                   "ast"                  
# [36] "serum_creatinine"      "hdl"                   "ldl"                   "lb_hospitalized"       "lb_telehealth"         "lb_outpatient"         "lb_n_glucose"         
# [43] "lb_n_hdl"              "lb_n_ldl"              "lb_n_serum_creatinine" "lb_n_alt"              "lb_n_ast"              "lb_n_hba1c"            "lb_n_labvisits"       
# [50] "calendar_month"        "COHORT"   

tx_weights_df <- readRDS(paste0(path_pasc_diabetes_folder,"/dom abstract/ip weights for COHORT.RDS"))
# names(tx_weights_df)
# [1] "ID"                  "COHORT"              "raceeth_category"    "sex_category"        "age_category"        "raceeth_numerator"   "sex_numerator"      
# [8] "age_numerator"       "exposed"             "historical"          "unexposed"           "numerator"           "denominator"         "trimmed_denominator"
# [15] "sipw"                "overlap_weight"      "sipw_raceeth"        "sipw_sex"            "sipw_age" 

# Not everyone has lab/prescribing/diagnosis data during the follow-up period
# Only cases (3751) + lastfu_noncases (339001) 
before_matchid <- lookback_processed %>% 
  # Restrict to population with followup BMI
  # dplyr::filter(ID %in% bmi_ID) %>%
  left_join(incident_dm_df,
            by = "ID") %>% 
  inner_join(tx_weights_df %>% 
               mutate(raceeth_category = factor(raceeth_category,levels=c("NH White","NH Black","Hispanic","NH Other"))) %>% 
              dplyr::select(ID, sipw, overlap_weight, sipw_raceeth, sipw_sex, sipw_age,sex_category,age_category,raceeth_category),
            by = "ID") %>% 
  mutate(t = as.numeric(last_followup_date - index_date)-30) %>% 
  left_join(matchid_df %>% dplyr::select(-COHORT),
            by = "ID")


# analytic_dataset <- bind_rows(before_matchid %>%
#                                 dplyr::filter(COHORT == "unexposed"),
#                               before_matchid %>%
#                                 dplyr::filter(COHORT %in% c("exposed","historical"), matchid %in% before_matchid$ID))

analytic_dataset = before_matchid

rm(lookback_processed,tx_weights_df,lastfu_noncases)
