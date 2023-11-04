rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_diabetes_repo,"/analysis cpit2dm/pdadm001_analytic dataset for data availability.R"))
rm(anthro_followup,demographic,index_date,lab_followup); gc()

cpit2dm_df = all_cpit2dm_df %>% 
  # Intersection of patients with follow-up and patients with look-back
  dplyr::filter(ID %in% lookback_df$ID)

label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")

library(survival)
library(ggfortify)
# https://dk81.github.io/dkmathstats_site/rvisual-kaplan-meier.html
# https://cran.r-project.org/web/packages/survminer/readme/README.html
model_fit <-  survfit(Surv(t, incident_dm) ~ COHORT, data = cpit2dm_df %>% 
                                    mutate(COHORT = factor(COHORT,levels=c("historical","unexposed","exposed"),labels=label_order)))
out = survminer::ggsurvplot(model_fit, pval = TRUE, conf.int = F,
                            ylim=c(0.90,1),
                            xlim = c(0,250),
                            break.time.by = 50,
                            surv.scale = 'percent',
                            xlab = "Time in days since origin date (T0 + 30)",
                            title = "",palette = color_order,
                            legend.labs = c("Historical","Unexposed","Exposed"),
                            risk.table = TRUE, risk.table.y.text.col = TRUE)
ggsave(out$plot,
       filename=paste0(path_pasc_diabetes_folder,"/figures/km curve for exposure groups.png"),width=6,height=4)

out$data.survtable

# Using ggsurvfit -----
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
library(ggsurvfit)
out2 = ggsurvfit::survfit2(Surv(t, incident_dm) ~ COHORT, data = cpit2dm_df %>% 
                             mutate(COHORT = factor(COHORT,levels=c("historical","unexposed","exposed"),labels=label_order)))
label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")

out2 %>% 
  ggsurvfit() +
  labs(x = "Time in days since origin date (T0 + 30)",
       y = "Survival probability") + 
  add_confidence_interval() +
  scale_fill_discrete(name="",labels= label_order,type = color_order) +
  scale_color_discrete(name="",labels= label_order,type = color_order)


# 1-year survival probability
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
library(gtsummary)
survfit(Surv(t, incident_dm) ~ COHORT, data = cpit2dm_df) %>% 
  summary(.,times=365.25)

survdiff(Surv(t, incident_dm) ~ COHORT, data = cpit2dm_df)
