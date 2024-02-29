encounter_check_supreme <- function(cp_df){
  
  cp_encounter_check <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
    dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,
                  ADMIT_DATE,DISCHARGE_DATE)  %>% 
    dplyr::filter(ENC_TYPE %in% permissible_enc_type) %>% 
    left_join(cp_df %>% 
                dplyr::select(ID,criterion1_date,criterion1_date_minus549),
              by = c("ID")) %>% 
    dplyr::filter(ADMIT_DATE <= criterion1_date) %>% 
    group_by(ID) %>% 
    # How many ADMIT_DATE encounters are there before criterion1_date_minus549 (18 months before criterion 1)
    summarize(n = sum(ADMIT_DATE <= criterion1_date_minus549)) %>% 
    collect()
  
  return(cp_encounter_check)
}
