rm(list=ls());gc();source(".Rprofile")


analytic_dataset <- readRDS(paste0(path_pasc_diabetes_folder,"/working/utilization/pdrev02_analytic_dataset.RDS")) %>% 
  mutate(n_hba1c_rate = case_when(max_lab_imputed %in% c(0,1) ~ n_hba1c/1,
                                  TRUE ~ n_hba1c/max_lab_imputed)) 


analytic_dataset %>% 
  group_by(COHORT, post) %>% 
  summarize(count = mean(n_hba1c), 
            rate = mean(n_hba1c_rate), 
            t = mean(max_lab_imputed),
            encounters = mean(n_encounters))


m0_hba1c <- glm(n_hba1c ~ offset(log(max_lab_imputed)) + COHORT*post,
                data = analytic_dataset,
                family=poisson())

library(emmeans)

crude_marginalplot_hba1c <- emmip(m0_hba1c,~ COHORT|post,type="response")
crude_marginalplot_hba1c$data


# # https://stackoverflow.com/questions/75823264/how-do-i-obtain-rate-estimates-with-95-cis-for-each-level-of-a-factor-from-a-po

# HbA1c ---------
m1_hba1c <- glm(n_hba1c ~ offset(log(max_lab_imputed)) + COHORT*post + age + female + 
                  raceeth_category + calendar_month + year_index + bmi + primary_payer,
                data = analytic_dataset,
                family=poisson())


overall_marginalplot_hba1c <- emmip(m1_hba1c,~ COHORT|post,type="response")
overall_marginalplot_hba1c$data


m2_hba1c <- glm(n_hba1c ~ offset(log(max_lab_imputed)) + COHORT*raceeth_category*post + age + female + 
                  calendar_month + year_index + bmi + primary_payer,
                data = analytic_dataset,
                family=poisson())

bind_rows(broom::tidy(m1_hba1c,exponentiate=FALSE) %>% mutate(model = "M1"),
          broom::tidy(m2_hba1c, exponentiate = FALSE) %>% mutate(model = "M2")) %>% 
  mutate(coef_ci = paste0(round(exp(estimate),2), "(",
                          round(exp(estimate - 1.96*std.error),2),", ",
                          round(exp(estimate + 1.96*std.error),2),")")) %>% 
  write_csv(.,"review/pdrev03_coefficients.csv")

raceeth_marginalplot_hba1c <- emmip(m2_hba1c,raceeth_category ~ COHORT|post,type="response")

# ALTERANTIVE FORMULATIONS --------

m1_hba1c_hcadj <- glm(n_hba1c ~ offset(log(max_lab_imputed)) + COHORT*post + age + female + 
                        raceeth_category + calendar_month + year_index + bmi + n_encounters + primary_payer,
                      data = analytic_dataset,
                      family=poisson())

overall_marginalplot_hba1c_hcadj <- emmip(m1_hba1c_hcadj,~ COHORT|post,type="response")
overall_marginalplot_hba1c_hcadj$data

m1_hba1c_rate <- glm(n_hba1c_rate ~ COHORT*post + age + female + 
                       raceeth_category + calendar_month + year_index + bmi + primary_payer,
                     data = analytic_dataset,
                     family=poisson())

overall_marginalplot_hba1c_rate <- emmip(m1_hba1c_rate,~ COHORT|post,type="response")
overall_marginalplot_hba1c_rate$data



# FIGURES -------------
figA_df = bind_rows(overall_marginalplot_hba1c$data %>% mutate(raceeth_category = "Overall"),
                    raceeth_marginalplot_hba1c$data) %>% 
  dplyr::select(-df) %>% 
  pivot_wider(values_from=c("yvar","SE"),names_from="post") %>% 
  mutate(diff_tests = (yvar_1 - yvar_0),
         se_diff_tests = sqrt(SE_1^2 + SE_0^2)) %>% 
  
  
  mutate(COHORT = factor(COHORT,levels=c("historical","unexposed","exposed"),labels=c("Historical","Unexposed","Exposed")),
         raceeth_category = factor(raceeth_category,levels=c("Overall","NH White","NH Black","Hispanic","NH Other"))) %>% 
  mutate(lci_diff_tests = diff_tests - 1.96*se_diff_tests,
         uci_diff_tests = diff_tests + 1.96*se_diff_tests)

figA_df %>% 
  write_csv(.,"review/pdrev03_marginal estimates.csv")

fig_A <- figA_df %>% 
  ggplot(data=.,aes(x=raceeth_category,
                    y = diff_tests,
                    ymin = lci_diff_tests,
                    ymax = uci_diff_tests,
                    fill = COHORT)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9),width=0.2) +
  xlab("") +
  ylab("Excess HbA1c tests \nfollowing SARS-CoV-2 infection") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(name="",values=c("#77DD77","#00008b","#ff964f")) 

fig_A %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/excess hba1c tests following infection.jpg"),width=8,height=4,dpi = 400)


figB_df = bind_rows(overall_marginalplot_hba1c$data %>% mutate(raceeth_category = "Overall"),
                    raceeth_marginalplot_hba1c$data) %>% 
  dplyr::select(-df) %>% 
  mutate(lci = yvar - 1.96*SE,
         uci = yvar + 1.96*SE) %>% 
  mutate(COHORT = factor(COHORT,levels=c("historical","unexposed","exposed"),labels=c("Historical","Unexposed","Exposed")),
         raceeth_category = factor(raceeth_category,levels=c("Overall","NH White","NH Black","Hispanic","NH Other")))

fig_B <- figB_df %>% 
  ggplot(data=.,aes(x=yvar,y=raceeth_category,col=COHORT)) +
  geom_point(position = position_dodge(width=0.9)) +
  geom_path(size=0.2,position = position_dodge(width = 0.9),
            arrow = arrow(type = "closed")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(name="",values=c("#77DD77","#00008b","#ff964f")) +
  ylab("") +
  xlab("HbA1c tests following \n SARS-CoV-2 infection")

fig_B

fig_B %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/gcdtr change in hba1c tests.jpg"),width=8,height=4,dpi = 400)



# Fig C ---------

fig_C <- overall_marginalplot_hba1c$data %>% 
  dplyr::select(-df) %>% 
  mutate(lci = yvar - 1.96*SE,
         uci = yvar + 1.96*SE) %>% 
  mutate(COHORT = factor(COHORT,levels=c("historical","unexposed","exposed"),labels=c("Historical","Unexposed","Exposed")),
         post = factor(post,levels=c(0,1),labels=c("12 months \n before index date","12 months \n after index date"))) %>% 
  ggplot(data=.,aes(x=post,
                    y = yvar,
                    ymin = lci,
                    ymax = uci,
                    col = COHORT,
                    group = COHORT)) +
  geom_point() +
  geom_errorbar(width=0.1) +
  geom_path() +
  xlab("") +
  ylab("HbA1c tests (covariate adjusted)") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(name="",values=c("#77DD77","#00008b","#ff964f")) 


library(ggpubr)

ggarrange(
  fig_C + scale_y_continuous (limits = c(0,1.1),breaks=seq(0,1.0,by=0.1)),
  fig_A + scale_y_continuous (limits = c(0,1.1),breaks=seq(0,1.0,by=0.1)),
  nrow = 2,
  ncol = 1,labels = LETTERS[1:2], common.legend = TRUE
  
) %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/gcdtr combined did change in hba1c tests.jpg"),width=8,height=8,dpi = 400)


# Per 1000 population
fig_A_1000 <- figA_df %>% 
  ggplot(data=.,aes(x=raceeth_category,
                    y = diff_tests*1000,
                    ymin = lci_diff_tests*1000,
                    ymax = uci_diff_tests*1000,
                    fill = COHORT)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9),width=0.2) +
  xlab("") +
  ylab("Excess HbA1c tests \nfollowing SARS-CoV-2 infection") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(name="",values=c("#77DD77","#00008b","#ff964f")) 

fig_C_1000 <- overall_marginalplot_hba1c$data %>% 
  dplyr::select(-df) %>% 
  mutate(lci = yvar - 1.96*SE,
         uci = yvar + 1.96*SE) %>%
  mutate(across(one_of("yvar","lci","uci"), .fns = function(x) x*1000)) %>% 
  mutate(COHORT = factor(COHORT,levels=c("historical","unexposed","exposed"),labels=c("Historical","Unexposed","Exposed")),
         post = factor(post,levels=c(0,1),labels=c("12 months \n before index date","12 months \n after index date"))) %>% 
  ggplot(data=.,aes(x=post,
                    y = yvar,
                    ymin = lci,
                    ymax = uci,
                    col = COHORT,
                    group = COHORT)) +
  geom_point() +
  geom_errorbar(width=0.1) +
  geom_path() +
  xlab("") +
  ylab("HbA1c tests (covariate adjusted) \n per 1000 individuals") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(name="",values=c("#77DD77","#00008b","#ff964f")) 

ggarrange(
  fig_C_1000 + scale_y_continuous (limits = c(0,1100),breaks=seq(0,1000,by=100)),
  fig_A_1000 + scale_y_continuous (limits = c(0,1100),breaks=seq(0,1000,by=100)),
  nrow = 2,
  ncol = 1,labels = LETTERS[1:2], common.legend = TRUE
  
) %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/gcdtr combined did change in hba1c tests per 1000 individuals.jpg"),width=8,height=8,dpi = 400)
