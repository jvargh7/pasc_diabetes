
source("sensitivity covid/pdsc302_analytic dataset with ip weights for cpit2dm.R")

# https://github.com/eleanormurray/CausalSurvivalAnalysisWorkshop/blob/master/R/workshop_v6_tidy.R
library(survival)

# Code Section 3a - Unadjusted Hazard Ratios ------------------------------

# Calculate the unadjusted hazard ratio from a Cox PH model
cox_fit <- coxph(Surv(t,incident_dm) ~ COHORT,data=cpit2dm_df, method='efron')
summary(cox_fit)

# IP weighted Hazard Ratios ------------
ipw_cox_fit <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT + ",
                                       paste0(imbalanced_variables,collapse="+"),
                                       "+",
                                       paste0(covid_variables,collapse=" + "))), 
                     data = cpit2dm_df, method='efron',weights = sipw,cluster = ID)
summary(ipw_cox_fit)

overlap_cox_fit <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT + ",
                                       paste0(imbalanced_variables,collapse="+"),
                                       "+",
                                       paste0(covid_variables,collapse=" + "))), 
                     data = cpit2dm_df, method='efron',weights = overlap_weight,cluster = ID)
summary(overlap_cox_fit)

ipw_cox_sex <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT*sex_category + ",
                                       paste0(imbalanced_variables,collapse="+"),
                                       "+",
                                       paste0(covid_variables,collapse=" + "))), 
                     data = cpit2dm_df, method='efron',weights = sipw_sex,cluster = ID)
summary(ipw_cox_sex)


ipw_cox_raceeth <-  coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT*raceeth_category + ",
                                            paste0(imbalanced_variables[!imbalanced_variables %in% 
                                                                          c("nhwhite","nhblack","hispanic","nhother")],
                                                   collapse=" + "),
                                            "+",
                                            paste0(covid_variables,collapse=" + "))
), 
data = cpit2dm_df %>% dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")), 
method='efron',weights = sipw_raceeth,cluster = ID)
summary(ipw_cox_raceeth)


ipw_cox_age <-  coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT*age_category + ",
                                        paste0(imbalanced_variables,collapse="+"),
                                        "+",
                                        paste0(covid_variables,collapse=" + "))), 
                      data = cpit2dm_df, method='efron',weights = sipw_age,cluster = ID)
summary(ipw_cox_age)


ipw_cox_hospitalization <-  coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT*hospitalization + ",
                                                    paste0(imbalanced_variables,collapse="+"),
                                                    "+",
                                                    paste0(covid_variables,collapse=" + "))), 
                                  data = cpit2dm_df, method='efron',weights = w_hospitalization,cluster = ID)
summary(ipw_cox_hospitalization)

saveRDS(ipw_cox_fit,paste0(path_pasc_diabetes_folder,"/working/sensitivity covid/pdsc407_ipw cox fit.RDS"))
saveRDS(overlap_cox_fit,paste0(path_pasc_diabetes_folder,"/working/sensitivity covid/pdsc407_overlap cox fit.RDS"))
saveRDS(ipw_cox_sex,paste0(path_pasc_diabetes_folder,"/working/sensitivity covid/pdsc407_ipw cox sex.RDS"))
saveRDS(ipw_cox_raceeth,paste0(path_pasc_diabetes_folder,"/working/sensitivity covid/pdsc407_ipw cox raceeth.RDS"))
saveRDS(ipw_cox_age,paste0(path_pasc_diabetes_folder,"/working/sensitivity covid/pdsc407_ipw cox age.RDS"))
saveRDS(ipw_cox_hospitalization,paste0(path_pasc_diabetes_folder,"/working/sensitivity covid/pdsc407_ipw cox hospitalization.RDS"))

bind_rows(
  broom::tidy(ipw_cox_fit) %>% mutate(model = "Overall"),
  broom::tidy(overlap_cox_fit) %>% mutate(model = "Overlap"),
  broom::tidy(ipw_cox_sex) %>% mutate(model = "Sex"),
  broom::tidy(ipw_cox_age) %>% mutate(model = "Age"),
  broom::tidy(ipw_cox_raceeth) %>% mutate(model = "Race-Ethnicity"),
  broom::tidy(ipw_cox_hospitalization) %>% mutate(model = "Hospitalization"),
) %>% 
  write_csv(.,"sensitivity covid/pdsc407_coefficients from ip weighted survival analysis.csv")
