rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/pasc_cardiometabolic_risk/functions/marginal_predictions_plot.R")

# Hazard ratios ----------
pdadm405_hr <- read_csv("analysis cpit2dm/pdadm405_difference relative to historical.csv") %>% 
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

(fig_hr = pdadm405_hr %>% 
    dplyr::filter(outcome == "CPIT2DM") %>% 
    marginal_predictions_plot(.,x_lab="Hazard Ratio \n(95% CI)",type="point_ci",axis_text_y=TRUE) +
    coord_cartesian(xlim = c(0,2.5)) +
    geom_vline(xintercept=1,col="red",linetype=2) +
    theme(legend.text = element_text(size = 14))) 

fig_hr %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/hazard ratios relative to historical for sociodemographic groups.jpg"),width=6,height=6)


pdadm405_hr %>% 
  dplyr::filter(outcome == "CPIT2DM") %>% 
  mutate(coef_ci = paste0(round(predicted,2)," (",
                          round(conf.low,2),", ",
                          round(conf.high,2),")")) %>% 
  dplyr::select(group,facet,coef_ci) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,Historical,Unexposed,Exposed) %>%  
  write_csv(.,"paper/table_risks relative to historical for sociodemographic subgroups.csv")


# Total Burden -----------

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
    xlab("New cases per 1000 person-years (95% CI)") +
    ylab("") +
    theme(legend.text = element_text(size = 14),
          axis.text.y = element_blank())) 


# Relative burden to Unexposed --------

pdadm403_relative <-   read_csv("analysis cpit2dm/pdadm403_difference cumulative incidence from unexposed at one year.csv") %>% 
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
  mutate(across(contains("diff_surv"),.fns=function(x) case_when(!is.na(x) ~ x*-1,
                                                                    TRUE ~ 0))) 

(fig_relative = pdadm403_relative %>% 
    ggplot(data=.,aes(x=diff_surv_est,xmin=diff_surv_lci,xmax = diff_surv_uci,y=facet,fill=group)) +
    geom_col(position = position_dodge(width=0.9)) +
    geom_errorbarh(position = position_dodge(width=0.9),height=0.2) +
    scale_fill_discrete(name="",labels= label_order,type = color_order) +
    scale_color_discrete(name="",labels= label_order,type = color_order) +
    scale_y_discrete(limits=rev) +
    theme_bw() +
    xlab("New cases relative to Unexposed \nper 1000 person-years (95% CI)") +
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
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/risks and burdens for sociodemographic subgroups relative to historical and unexposed.jpg"),width=11,height=8)



