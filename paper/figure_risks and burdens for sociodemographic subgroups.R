rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/pasc_cardiometabolic_risk/functions/marginal_predictions_plot.R")

# Hazard ratios ----------
pdadm402_hr <- read_csv("analysis cpit2dm/pdadm402_difference relative to exposed.csv") %>% 
  mutate(group = str_replace(exposure,"COHORT",""),
         facet = str_replace(modifier1,modifier_var,"")) %>% 
  mutate(facet = case_when(is.na(modifier1) & modifier_var == "hospitalization_category" ~ "Not Hospitalized",
                           facet == "hospitalization" ~ "Hospitalized",
                           is.na(facet) ~ "Overall",
                           TRUE ~ facet)) %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "Female",
                                       "Male",
                                       "18 to 39",
                                       "40 to 64",
                                       "65 plus",
                                       "Hispanic",
                                       "NH White",
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  rename(predicted = Estimate,
         conf.low = LCI,
         conf.high = UCI) %>% 
  mutate(across(one_of("predicted","conf.low","conf.high"),
                .fns = ~exp(.))) %>% 
  mutate(t = -1)

label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")

(fig_hr = pdadm402_hr %>% 
    dplyr::filter(outcome == "CPIT2DM") %>% 
    marginal_predictions_plot(.,x_lab="Hazard Ratio \n(95% CI)",type="point_ci",axis_text_y=TRUE) +
    coord_cartesian(xlim = c(0,2)) +
    geom_vline(xintercept=1,col="red",linetype=2) +
    theme(legend.text = element_text(size = 14))) 

fig_hr %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/hazard ratios for sociodemographic groups.jpg"),width=6,height=6)

# Burden per 1000 people at 12 months --------

pdadm403_burden <-   read_csv("analysis cpit2dm/pdadm403_cumulative incidence at one year.csv") %>% 
  dplyr::filter(time == 365) %>% 
  mutate(
         facet = modifier) %>% 
  mutate(facet = case_when(is.na(facet) ~ "Overall",
                           TRUE ~ facet)) %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "Female",
                                       "Male",
                                       "18 to 39",
                                       "40 to 64",
                                       "65 plus",
                                       "Hispanic",
                                       "NH White",
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(COHORT,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) 

(fig_burden = pdadm403_burden %>% 
    ggplot(data=.,aes(x=cuminc_surv,xmin=cuminc_ci_lower,xmax=cuminc_ci_upper,y=facet,fill=group)) +
    geom_col(position = position_dodge(width=0.9)) +
    geom_errorbarh(position = position_dodge(width=0.9),height=0.2) +
    scale_fill_discrete(name="",labels= label_order,type = color_order) +
    scale_color_discrete(name="",labels= label_order,type = color_order) +
    scale_y_discrete(limits=rev) +
    theme_bw() +
    xlab("Burden per 1000 people \nat 12 months (95% CI)") +
    ylab("") +
    theme(legend.text = element_text(size = 14),
          axis.text.y = element_blank())) 

# Relative burden to Exposed --------

pdadm403_relative <-   read_csv("analysis cpit2dm/pdadm403_difference cumulative incidence from exposed at one year.csv") %>% 
  dplyr::filter(time == 365) %>% 
  mutate(
    facet = modifier) %>% 
  mutate(facet = case_when(is.na(facet) ~ "Overall",
                           TRUE ~ facet)) %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "Female",
                                       "Male",
                                       "18 to 39",
                                       "40 to 64",
                                       "65 plus",
                                       "Hispanic",
                                       "NH White",
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(COHORT,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  mutate(across(contains("reduced_surv"),.fns=function(x) case_when(!is.na(x) ~ x*-1,
                                                                    TRUE ~ 0))) 

(fig_relative = pdadm403_relative %>% 
    ggplot(data=.,aes(x=reduced_surv_est,xmin=reduced_surv_lci,xmax=reduced_surv_uci,y=facet,fill=group)) +
    geom_col(position = position_dodge(width=0.9)) +
    geom_errorbarh(position = position_dodge(width=0.9),height=0.2) +
    scale_fill_discrete(name="",labels= label_order,type = color_order) +
    scale_color_discrete(name="",labels= label_order,type = color_order) +
    scale_y_discrete(limits=rev) +
    theme_bw() +
    xlab("Burden relative to Exposed \nat 12 months (95% CI)") +
    ylab("") +
    theme(legend.text = element_text(size = 14),
          axis.text.y = element_blank())) 

# Putting them together -------

library(ggpubr)
ggarrange(fig_hr,
          fig_burden,
          fig_relative,
          nrow=1,ncol=3,
          labels=LETTERS[1:3],
          widths = c(2.2,1.5,1.5),
          common.legend=TRUE) %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/risks and burdens for sociodemographic subgroups.jpg"),width=11,height=8)


bind_rows(pdadm402_hr %>% 
            mutate(coef_ci = paste0(round(predicted,2)," (",
                                    round(conf.low,2),", ",
                                    round(conf.high,2),")")) %>% 
            mutate(type = "HR"),
          pdadm403_burden %>% 
            mutate(coef_ci = paste0(round(cuminc_surv,1)," (",
                                    round(cuminc_ci_upper,1),", ",
                                    round(cuminc_ci_lower,1),")")) %>% 
            mutate(type = "Burden per 1000"),
          pdadm403_relative %>% 
            mutate(coef_ci = paste0(round(reduced_surv_est,1)," (",
                                    round(reduced_surv_uci,1),", ",
                                    round(reduced_surv_lci,1),")")) %>% 
            mutate(type = "Burden relative to Exposed")
          ) %>% 
  dplyr::select(group,facet,coef_ci,type) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,type,Exposed,Unexposed,Historical) %>% 
  write_csv(.,"paper/table_risks and burdens for sociodemographic subgroups.csv")
