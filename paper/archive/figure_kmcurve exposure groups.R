rm(list=ls());gc();source(".Rprofile")

source("dom abstract/pddabs_analytic dataset.R")

library(survival)
library(ggfortify)
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_surv.html

fit <- survfit(Surv(t,incident_dm) ~ COHORT,data=analytic_dataset)

autoplot(fit, fun = 'event')
autoplot(fit)

# http://www.sthda.com/english/wiki/survminer-r-package-survival-data-analysis-and-visualization
out = survminer::ggsurvplot(fit, pval = TRUE, conf.int = F,
                            ylim=c(0.98,1),
                            xlim = c(0,250),
                            break.time.by = 50,
                            surv.scale = 'percent',
                            xlab = "Time in days",
                            title = "KM Curve showing proportion free of diabetes",
                            legend.labs = c("Exposed","Historical","Unexposed"),
           risk.table = TRUE, risk.table.y.text.col = TRUE)

ggsave(out$plot,
       filename=paste0(path_pasc_diabetes_folder,"/figures/km curve for exposure groups.png"),width=6,height=4)
out$data.survtable