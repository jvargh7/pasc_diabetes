
# Called by pdadm104_creating parquet for cohort membership ps.R
# Requires analytic_sample$ID that comes from pdadm001_analytic dataset for data availability.R
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))


hd_dataset_COHORT <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre403_high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_")) %>% 
  dplyr::select(ID,one_of(selected_hdvars %>% 
                            dplyr::filter(outcome == "cpit2dm") %>%
                            dplyr::select(variable) %>% 
                            pull() %>% unique())) %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT),
            by = "ID") %>% 
  dplyr::filter(ID %in% analytic_sample$ID)

hdvars_frequency <- hd_dataset_COHORT %>% 
  group_by(COHORT) %>% 
  summarize(across(matches("(gtOne|gtMedian|gtQ3)"),~sum(.,na.rm=TRUE))) %>% 
  mutate(across(matches("(gtOne|gtMedian|gtQ3)"), ~case_when(. >= 100 ~ 1,
                                                             TRUE ~ 0) )) %>% 
  summarize(across(-one_of("COHORT"), ~sum(.)))

restricted_hdvars = names(hdvars_frequency)[hdvars_frequency[1,]==3]


