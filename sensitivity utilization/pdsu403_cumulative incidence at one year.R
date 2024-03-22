rm(list=ls());gc();source(".Rprofile")
source("sensitivity utilization/pdsu302_analytic dataset with ip weights for cpit2dm.R")

# ipw_cox_fit <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm401_ipw cox fit.RDS"))

library(survival)
library(adjustedCurves)
library(riskRegression)

cpit2dm_df <- cpit2dm_df %>% 
  mutate(hospitalization = case_when(hospitalization == 1 ~ "Hospitalized",
                                     TRUE ~ "Not Hospitalized")) %>% 
  mutate(COHORT_sex_category = paste0(COHORT,"_",sex_category),
         COHORT_raceeth_category = paste0(COHORT,"_",raceeth_category),
         COHORT_age_category = paste0(COHORT,"_",age_category),
         COHORT_hospitalization = paste0(COHORT,"_",hospitalization))

ipw_cox_fit <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT + ",paste0(imbalanced_variables,collapse="+"))), 
                     data = cpit2dm_df, method='efron',weights = sipw,cluster = ID,x=TRUE)

overlap_cox_fit <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT + ",paste0(imbalanced_variables,collapse="+"))), 
                     data = cpit2dm_df, method='efron',weights = overlap_weight,cluster = ID,x=TRUE)

ipw_cox_sex <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT_sex_category + ",paste0(imbalanced_variables,collapse="+"))), 
                     data = cpit2dm_df, method='efron',weights = sipw_sex,cluster = ID,x=TRUE)


ipw_cox_raceeth <-  coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT_raceeth_category + ",
                                            paste0(imbalanced_variables[!imbalanced_variables %in% 
                                                                          c("nhwhite","nhblack","hispanic","nhother")],
                                                   collapse=" + "))
), 
data = cpit2dm_df %>% dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")), 
method='efron',weights = sipw_raceeth,cluster = ID,x=TRUE)


ipw_cox_age <-  coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT_age_category + ",paste0(imbalanced_variables,collapse="+"))), 
                      data = cpit2dm_df, method='efron',weights = sipw_age,cluster = ID,x=TRUE)


ipw_cox_hospitalization <-  coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT_hospitalization + ",
                                                    paste0(imbalanced_variables[!imbalanced_variables %in% 
                                                                                  c("hospitalization")],
                                                           collapse=" + "))), 
                                  data = cpit2dm_df, method='efron',weights = sipw_hospitalization,cluster = ID,x=TRUE)

adj_survival_fit_overall = surv_direct(outcome_model = ipw_cox_fit,data=cpit2dm_df,
                               variable = "COHORT",times = c(180,365),
                               conf_int = TRUE)

adj_survival_fit_overlap = surv_direct(outcome_model = overlap_cox_fit,data=cpit2dm_df,
                                       variable = "COHORT",times = c(180,365),
                                       conf_int = TRUE)

adj_survival_fit_sex = surv_direct(outcome_model = ipw_cox_sex,data=cpit2dm_df,
                               variable = "COHORT_sex_category",times = c(180,365),
                               conf_int = TRUE)

adj_survival_fit_raceeth = surv_direct(outcome_model = ipw_cox_raceeth,
                                       data=cpit2dm_df %>% 
                                         dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")),
                               variable = "COHORT_raceeth_category",times = c(180,365),
                               conf_int = TRUE)

adj_survival_fit_age = surv_direct(outcome_model = ipw_cox_age,data=cpit2dm_df,
                               variable = "COHORT_age_category",times = c(180,365),
                               conf_int = TRUE)

adj_survival_fit_hospitalization = surv_direct(outcome_model = ipw_cox_hospitalization,
                                               data=cpit2dm_df,
                                   variable = "COHORT_hospitalization",times = c(180,365),
                                   conf_int = TRUE)


surv_probs = bind_rows(adj_survival_fit_overall$plotdata,
          adj_survival_fit_sex$plotdata,
          adj_survival_fit_raceeth$plotdata,
          adj_survival_fit_age$plotdata,
          adj_survival_fit_hospitalization$plotdata
          ) %>% 
  separate(group,sep = "_",into=c("COHORT","modifier"))  %>% 
  bind_rows(adj_survival_fit_overlap$plotdata %>% 
              dplyr::rename(COHORT = group) %>% 
              mutate(modifier = "Overlap"))

surv_probs %>%
  mutate(across(one_of(c("surv","ci_lower","ci_upper")),.fns=function(x) (1-x)*1000,.names="cuminc_{col}")) %>% 
  write_csv(.,"sensitivity utilization/pdsu403_cumulative incidence at one year.csv")

(diff_with_exposed = surv_probs %>% 
  left_join(.,
            {.} %>% 
              dplyr::filter(COHORT == "exposed") %>% 
              dplyr::select(time,surv,modifier,se) %>% 
              rename(exposed_surv = surv,
                     exposed_se = se),
            by = c("time","modifier")) %>% 
  mutate(reduced_surv_est = surv - exposed_surv,
         reduced_surv_se = sqrt(se^2 + exposed_se^2)) %>% 
  mutate(reduced_surv_lci = case_when(COHORT == "exposed" ~ NA_real_,
                                      TRUE ~ reduced_surv_est - 1.96*reduced_surv_se),
         reduced_surv_uci = case_when(COHORT == "exposed" ~ NA_real_,
                                      TRUE ~ reduced_surv_est + 1.96*reduced_surv_se)) %>% 
  mutate(across(one_of(c("reduced_surv_est","reduced_surv_lci","reduced_surv_uci")),.fns=function(x) (x)*1000))) %>% 
  write_csv(.,"sensitivity utilization/pdsu403_difference cumulative incidence from exposed at one year.csv")

