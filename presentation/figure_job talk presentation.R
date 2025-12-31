


label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("#77DD77","#00008b","#ff964f")

pdadm403_relative <-   read_csv(paste0(path_pasc_diabetes_repo,"/analysis cpit2dm/pdadm403_difference cumulative incidence from exposed at one year.csv")) %>% 
  dplyr::filter(time == 365) %>% 
  mutate(
    facet = modifier) %>% 
  mutate(facet = case_when(is.na(facet) ~ "Overall",
                           TRUE ~ facet)) %>% 
  dplyr::filter(facet != 'Overlap') %>%
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
    geom_col(position = position_dodge(width=1)) +
    geom_errorbarh(position = position_dodge(width=1),height=0.2) +
    scale_fill_discrete(name="",labels= label_order,type = color_order) +
    scale_color_discrete(name="",labels= label_order,type = color_order) +
    scale_y_discrete(limits=rev) +
    theme_bw() +
    xlab("Burden relative to Exposed \nat 12 months (95% CI)") +
    ylab("") +
    theme(legend.text = element_text(size = 14),
          axis.text.y = element_blank())) 


difference0 <- read_csv(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab406_difference relative to exposed for time 0.csv")) %>% 
  dplyr::filter(modifier2_value %in% c(0)) %>% 
  rename(t = modifier2_value) %>% 
  mutate(group = str_replace(exposure,"COHORT",""),
         facet = str_replace(modifier1,modifier_var,"")) %>% 
  mutate(facet = case_when(is.na(modifier1) & modifier_var == "hospitalization_category" ~ "Not Hospitalized",
                           facet == "hospitalization" ~ "Hospitalized",
                           is.na(facet) ~ "Overall",
                           TRUE ~ facet),
         t = paste0("Time = ",t)) %>% 
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
                        labels=label_order)) %>% 
  rename(predicted = Estimate,
         conf.low = LCI,
         conf.high = UCI)

source(paste0(path_pasc_cmr_repo,"/functions/marginal_predictions_plot.R"))

(fig_bmi_D = difference0 %>% 
    dplyr::filter(outcome == "bmi") %>% 
    marginal_predictions_plot(.,x_lab="Difference relative to Exposed\nBody mass index (kg/m2)",type="point_ci",axis_text_y=TRUE) +
    coord_cartesian(xlim = c(-0.5, 0.4)) +
    geom_vline(xintercept=0,col="red",linetype=2) +
    theme(legend.text = element_text(size=14),
          axis.text.x = element_text(size=14)))

library(ggpubr)
ggarrange(fig_relative,
          fig_bmi_D,
          nrow=1,ncol=2,
          labels=LETTERS[1:2],
          legend = "bottom",
          widths = c(2.2,1.5),
          common.legend=TRUE) %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/job talk presentation.jpg"),width=10,height=5)
