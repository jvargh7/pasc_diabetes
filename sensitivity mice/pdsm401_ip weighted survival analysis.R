
rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_diabetes_repo,"/analysis cpit2dm/pdadm001_analytic dataset for data availability.R"))
rm(anthro_followup,demographic,index_date,lab_followup); gc()
# source("analysis bmi/pcrabaux_ipw formula and variables.R")

# Different from imbalanced variables in pasc_cardiometabolic_risk/analysis bmi/pcrab302_analytic dataset with ip weights for bmi.R
imbalanced_variables <- c("age","nhblack","hispanic","nhother","smoking","site","hospitalization","bmi","ldl")

library(mice)
mi_dfs <- readRDS(paste0(path_pasc_diabetes_folder,"/working/sensitivity mice/pdsm002_mi_dfs.RDS"))

library(survival)

ipw_cox_fit <- overlap_cox_fit <- ipw_cox_sex <- ipw_cox_raceeth <- ipw_cox_age <- ipw_cox_hospitalization <- list()

for (i in 1:5){
  imputed_dataset = complete(mi_dfs,i)
  source("sensitivity mice/pdsm302_analytic dataset with ip weights for cpit2dm.R")
  # IP weighted Hazard Ratios ------------
  ipw_cox_fit[[i]] <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT + ",paste0(imbalanced_variables,collapse="+"))), 
                       data = cpit2dm_df, method='efron',weights = w,cluster = ID)
  
  overlap_cox_fit[[i]] <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT + ",paste0(imbalanced_variables,collapse="+"))), 
                       data = cpit2dm_df, method='efron',weights = w_overlap,cluster = ID)

  ipw_cox_sex[[i]] <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT*sex_category + ",paste0(imbalanced_variables,collapse="+"))), 
                       data = cpit2dm_df, method='efron',weights = w_sex,cluster = ID)

  
  ipw_cox_raceeth[[i]] <-  coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT*raceeth_category + ",
                                              paste0(imbalanced_variables[!imbalanced_variables %in% 
                                                                            c("nhwhite","nhblack","hispanic","nhother")],
                                                     collapse=" + "))
  ), 
  data = cpit2dm_df %>% dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")), 
  method='efron',weights = w_raceeth,cluster = ID)
  
  ipw_cox_age[[i]] <-  coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT*age_category + ",paste0(imbalanced_variables,collapse="+"))), 
                        data = cpit2dm_df, method='efron',weights = w_age,cluster = ID)

  
  ipw_cox_hospitalization[[i]] <-  coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT*hospitalization + ",paste0(imbalanced_variables,collapse="+"))), 
                                    data = cpit2dm_df, method='efron',weights = w_hospitalization,cluster = ID)

  
}


# Saving coxph output ----------
source("C:/code/external/functions/imputation/save_mi_coxph.R")

save_mi_coxph(ipw_cox_fit) %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/sensitivity mice/pdsm401_ipw cox fit.RDS"))

save_mi_coxph(overlap_cox_fit) %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/sensitivity mice/pdsm401_overlap cox fit.RDS"))

save_mi_coxph(ipw_cox_sex) %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/sensitivity mice/pdsm401_ipw cox sex.RDS"))

save_mi_coxph(ipw_cox_raceeth) %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/sensitivity mice/pdsm401_ipw cox raceeth.RDS"))

save_mi_coxph(ipw_cox_age) %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/sensitivity mice/pdsm401_ipw cox age.RDS"))

save_mi_coxph(ipw_cox_hospitalization) %>% 
  saveRDS(.,paste0(path_pasc_diabetes_folder,"/working/sensitivity mice/pdsm401_ipw cox hospitalization.RDS"))


# Coefficients -------
source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")


bind_rows(
  clean_mi_conditionalregression(ipw_cox_fit,link="coxph") %>% mutate(model = "Overall"),
  clean_mi_conditionalregression(overlap_cox_fit,link="coxph") %>% mutate(model = "Overlap"),
  clean_mi_conditionalregression(ipw_cox_sex,link="coxph") %>% mutate(model = "Sex"),
  clean_mi_conditionalregression(ipw_cox_age,link="coxph") %>% mutate(model = "Age"),
  clean_mi_conditionalregression(ipw_cox_raceeth,link="coxph") %>% mutate(model = "Race-Ethnicity"),
  clean_mi_conditionalregression(ipw_cox_hospitalization,link="coxph") %>% mutate(model = "Hospitalization"),
) %>% 
  write_csv(.,"sensitivity mice/pdsm401_coefficients from ip weighted survival analysis.csv")

# Contrasts ----------

