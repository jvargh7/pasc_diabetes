source(".Rprofile")
source(paste0(path_pasc_diabetes_repo,"/analysis cpit2dm/pdadm001_analytic dataset for data availability.R"))


cpit2dm_df = all_cpit2dm_df %>% 
  dplyr::filter(ID %in% analytic_sample$ID)

# Identify variables associated with outcome among those observations with lookback BMI
# Filter 1: Lookback BMI available >> pdadm001_analytic dataset for data availability.R >> lookback_df 
# (after filtering based on bmi availability + excluding cases with CPIT2DM in lookback period and landmark period)
# Filter 2: Outcome available >> pdadm001_analytic dataset for data availability.R >> cpit2dm_df

hd_dataset <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre403_high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_"))


var_names = hd_dataset %>% 
  dplyr::select(-ID) %>% 
  names(.)

cpit2dm_df %>% 
  group_by(COHORT,incident_dm) %>% 
  tally()

library(survival)
t0 = Sys.time()
cpit2dm_m0 <- coxph(Surv(t,incident_dm) ~ 1,data=cpit2dm_df, method='breslow')
t1 = Sys.time()
t1 - t0

t0 = Sys.time()
lrt_est = map_dfr(var_names,
                  function(v_n){
                    d_df <- cpit2dm_df %>% 
                      left_join(hd_dataset %>% dplyr::select(ID,one_of(v_n)),
                                by = c("ID")) 
                    
                    cpit2dm_m1 <- coxph(as.formula(paste0("Surv(t,incident_dm) ~ 1 + ",v_n)),data=d_df, method='breslow')
                    
                    lrt_m1_m0 = anova(cpit2dm_m1,cpit2dm_m0)
                    
                    rm(bmi_m1,
                       b_df); gc();
                    
                    data.frame(variable = v_n,
                               
                               cpit2dm1_loglik = lrt_m1_m0[2,"loglik"],
                               cpit2dm1_dof = lrt_m1_m0[2,"Df"],
                               pval_cpit2dm = lrt_m1_m0[2,"Pr(>|Chi|)"]) %>% 
                      return(.)
                    
                    
                  })
t1 = Sys.time()
t1 - t0

saveRDS(lrt_est,paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm101_high dimensional covariates with cpit2dm.RDS"))

# rm(hd_dataset); gc();