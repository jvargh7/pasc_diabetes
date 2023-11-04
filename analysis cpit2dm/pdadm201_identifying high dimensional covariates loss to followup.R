rm(list=ls());gc();source(".Rprofile")
# library(glmmLasso)
# library(glmnet)
# library(PGEE)



source("analysis cpit2dm/pdadm001_analytic dataset for data availability.R")
rm(anthro_followup,demographic,index_date,lab_followup); gc()


source("analysis cpit2dm/pdadm102_selecting high dimensional variables based on fdr.R")

hd_dataset_COHORT <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre403_high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_")) %>% 
  dplyr::select(-one_of(selected_hdvars %>% 
                          # dplyr::filter(outcome == "bmi") %>% 
                          dplyr::select(variable) %>% 
                          pull() %>% unique())) %>% 
  left_join(outcome_availability,
            by="ID") %>% 
  dplyr::filter(ID %in% analytic_sample$ID)

var_names = hd_dataset_COHORT %>% 
  dplyr::select(-names(outcome_availability)) %>% 
  names(.)

gc()

t0 = Sys.time()
cpit2dm_c0 <- glm(as.formula(paste0("in_dm_followup_ID ~ 1")),data=hd_dataset_COHORT,family = binomial())
t1 = Sys.time()
t1 - t0

cpit2dm0_dev = deviance(cpit2dm_c0) %>% as.numeric()

lrt_est_censoring = map_dfr(var_names,
                            function(v_n){
                              # c[]: censoring, m[]: model 
                              cpit2dm_c1 <- glm(as.formula(paste0("in_dm_followup_ID ~ ",v_n)),data=hd_dataset_COHORT,family = binomial())

                              cpit2dm1_dev = deviance(cpit2dm_c1) %>% as.numeric()

                              lambda_cpit2dm = cpit2dm0_dev - cpit2dm1_dev

                              rm(cpit2dm_c1); gc();
                              
                              data.frame(variable = v_n,
                                         
                                         cpit2dm1_dev = cpit2dm1_dev,
                                         lambda_cpit2dm = lambda_cpit2dm,
                                         pval_cpit2dm = (1-pchisq(lambda_cpit2dm,1))
                              ) %>% 
                                return(.)
                              
                              
                            })
saveRDS(lrt_est_censoring,paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm201_additional high dimensional covariates with loss to followup.RDS"))
