rm(list=ls());gc();source(".Rprofile")
source("analysis cpit2dm/pdadm302_analytic dataset with ip weights for cpit2dm.R")

# ipw_cox_fit <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm401_ipw cox fit.RDS"))

library(survival)
ipw_cox_fit <- coxph(as.formula(paste0("Surv(t, incident_dm) ~ COHORT + ",paste0(imbalanced_variables,collapse="+"))), 
                     data = cpit2dm_df, method='efron',weights = sipw,cluster = ID,x=TRUE)

library(adjustedCurves)
library(riskRegression)
adj_survival_fit = surv_direct(outcome_model = ipw_cox_fit,data=cpit2dm_df,
            variable = "COHORT",times = seq(0,250,by=10),
            conf_int = TRUE)

label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")

fig_adj_curve <- adj_survival_fit$plotdata %>% 
  mutate(group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=label_order)) %>% 
  ggplot(data=.,aes(color=group,x=time,y=surv,ymin=ci_lower,ymax=ci_upper)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width=0.2) +
  xlab("Time (days)") +
  ylab("Probability of incidence") +
  theme_bw() +
  # scale_fill_discrete(name="",labels= label_order,type = color_order) +
  scale_color_discrete(name="",labels= label_order,type = color_order)   +
  theme(legend.position = "bottom")  +
  theme(text = element_text(size=14),
        legend.text = element_text(size = 14))

fig_adj_curve %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/adjusted survival curves overall direct standardization.jpg"),width=6,height=6)


# Cumulative incidence ---------
fig_adj_curve_cuminc <- adj_survival_fit$plotdata %>% 
  mutate(group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=label_order)) %>%
  mutate(across(one_of(c("surv","ci_lower","ci_upper")),.fns=function(x) (1-x)*1)) %>% 
  ggplot(data=.,aes(color=group,x=time,y=surv,ymin=ci_lower,ymax=ci_upper)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width=0.2) +
  xlab("Time (days)") +
  ylab("Probability of incidence") +
  theme_bw() +
  # scale_fill_discrete(name="",labels= label_order,type = color_order) +
  scale_color_discrete(name="",labels= label_order,type = color_order)   +
  theme(legend.position = "bottom")  +
  theme(text = element_text(size=14),
        legend.text = element_text(size = 14))

fig_adj_curve_cuminc %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/adjusted cumulative incidence curves overall direct standardization.jpg"),width=6,height=6)

# Generate a scatterplot with blue color for historical, green for unexposed, and red for exposed