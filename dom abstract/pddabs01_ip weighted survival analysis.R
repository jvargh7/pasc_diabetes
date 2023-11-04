rm(list=ls());gc();source(".Rprofile")

source("dom abstract/pddabs_analytic dataset.R")


analytic_dataset %>% 
  group_by(COHORT) %>% 
  summarize(age = paste0(round(mean(age),1)," (",round(sd(age),1),")"),
            female = mean(female),
            nhwhite = mean(nhwhite),
            n = n(),
            case_count = sum(incident_dm),
            time_count = sum(t)) %>%
  mutate(case_per_100py = case_count/(time_count/(100*365)),
         time_per_100py = time_count/(100*365))



# https://github.com/eleanormurray/CausalSurvivalAnalysisWorkshop/blob/master/R/workshop_v6_tidy.R
library(survival)

# Code Section 3a - Unadjusted Hazard Ratios ------------------------------

# Calculate the unadjusted hazard ratio from a Cox PH model
cox_fit <- coxph(Surv(t,incident_dm) ~ COHORT,data=analytic_dataset, method='breslow')
summary(cox_fit)

# Code Section 3b - Conditional Hazard Ratios ------------------------------

# Calculate the baseline covariate-adjusted hazard ratio from a Cox PH model
adj_cox_fit <- coxph(Surv(t, incident_dm) ~ COHORT + age + bmi + site, 
                     data = analytic_dataset, method='breslow')
summary(adj_cox_fit)

# https://stats.stackexchange.com/questions/608391/inverse-probability-weighting-for-cox-proportional-hazards-model-to-control-for
# IP weighted Hazard Ratios ------------
ipw_cox_fit <- coxph(Surv(t, incident_dm) ~ COHORT + age + bmi + site, 
                     data = analytic_dataset, method='breslow',weights = sipw)
summary(ipw_cox_fit)

ipw_cox_sex <- coxph(Surv(t, incident_dm) ~ COHORT*sex_category + age + bmi + site, 
                     data = analytic_dataset, method='breslow',weights = sipw_sex)
summary(ipw_cox_sex)


ipw_cox_raceeth <-  coxph(Surv(t, incident_dm) ~ COHORT*raceeth_category + age + bmi + site, 
        data = analytic_dataset, method='breslow',weights = sipw_raceeth)
summary(ipw_cox_raceeth)


ipw_cox_age <-  coxph(Surv(t, incident_dm) ~ COHORT*age_category + bmi + site, 
                          data = analytic_dataset, method='breslow',weights = sipw_age)
summary(ipw_cox_age)
