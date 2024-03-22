
source("sensitivity negatives/pdsno302_analytic dataset with ip weights for cpiom.R")

# https://github.com/eleanormurray/CausalSurvivalAnalysisWorkshop/blob/master/R/workshop_v6_tidy.R
library(survival)

# Code Section 3a - Unadjusted Hazard Ratios ------------------------------

# Calculate the unadjusted hazard ratio from a Cox PH model
cox_fit <- coxph(Surv(t,incident_om) ~ COHORT,data=cpiom_df, method='efron')
summary(cox_fit)

# IP weighted Hazard Ratios ------------
ipw_cox_fit <- coxph(as.formula(paste0("Surv(t, incident_om) ~ COHORT + ",paste0(imbalanced_variables,collapse="+"))), 
                     data = cpiom_df, method='efron',weights = w,cluster = ID)
summary(ipw_cox_fit)

overlap_cox_fit <- coxph(as.formula(paste0("Surv(t, incident_om) ~ COHORT + ",paste0(imbalanced_variables,collapse="+"))), 
                     data = cpiom_df, method='efron',weights = w_overlap,cluster = ID)
summary(overlap_cox_fit)

ipw_cox_sex <- coxph(as.formula(paste0("Surv(t, incident_om) ~ COHORT*sex_category + ",paste0(imbalanced_variables,collapse="+"))), 
                     data = cpiom_df, method='efron',weights = w_sex,cluster = ID)
summary(ipw_cox_sex)


ipw_cox_raceeth <-  coxph(as.formula(paste0("Surv(t, incident_om) ~ COHORT*raceeth_category + ",
                                            paste0(imbalanced_variables[!imbalanced_variables %in% 
                                                                          c("nhwhite","nhblack","hispanic","nhother")],
                                                   collapse=" + "))
                                     ), 
                          data = cpiom_df %>% dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")), 
                          method='efron',weights = w_raceeth,cluster = ID)
summary(ipw_cox_raceeth)


ipw_cox_age <-  coxph(as.formula(paste0("Surv(t, incident_om) ~ COHORT*age_category + ",paste0(imbalanced_variables,collapse="+"))), 
                      data = cpiom_df, method='efron',weights = w_age,cluster = ID)
summary(ipw_cox_age)


ipw_cox_hospitalization <-  coxph(as.formula(paste0("Surv(t, incident_om) ~ COHORT*hospitalization + ",paste0(imbalanced_variables,collapse="+"))), 
                      data = cpiom_df, method='efron',weights = w_hospitalization,cluster = ID)
summary(ipw_cox_hospitalization)

saveRDS(ipw_cox_fit,paste0(path_pasc_diabetes_folder,"/working/sensitivity negatives/pdsno401_ipw cox fit.RDS"))
saveRDS(overlap_cox_fit,paste0(path_pasc_diabetes_folder,"/working/sensitivity negatives/pdsno401_overlap cox fit.RDS"))
saveRDS(ipw_cox_sex,paste0(path_pasc_diabetes_folder,"/working/sensitivity negatives/pdsno401_ipw cox sex.RDS"))
saveRDS(ipw_cox_raceeth,paste0(path_pasc_diabetes_folder,"/working/sensitivity negatives/pdsno401_ipw cox raceeth.RDS"))
saveRDS(ipw_cox_age,paste0(path_pasc_diabetes_folder,"/working/sensitivity negatives/pdsno401_ipw cox age.RDS"))
saveRDS(ipw_cox_hospitalization,paste0(path_pasc_diabetes_folder,"/working/sensitivity negatives/pdsno401_ipw cox hospitalization.RDS"))

bind_rows(
  broom::tidy(ipw_cox_fit) %>% mutate(model = "Overall"),
  broom::tidy(overlap_cox_fit) %>% mutate(model = "Overlap"),
  broom::tidy(ipw_cox_sex) %>% mutate(model = "Sex"),
  broom::tidy(ipw_cox_age) %>% mutate(model = "Age"),
  broom::tidy(ipw_cox_raceeth) %>% mutate(model = "Race-Ethnicity"),
  broom::tidy(ipw_cox_hospitalization) %>% mutate(model = "Hospitalization"),
) %>% 
  write_csv(.,"sensitivity negatives/pdsno401_coefficients from ip weighted survival analysis.csv")
