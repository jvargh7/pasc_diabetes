rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/pasc_cardiometabolic_risk/functions/marginal_predictions_plot.R")

# Hazard ratios ----------
pdadm405_hr <- read_csv("analysis cpit2dm/pdadm406_difference relative to unexposed.csv") %>% 
  dplyr::filter(modifier_var != 'Overlap' | is.na(modifier_var)) %>% 
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
    coord_cartesian(xlim = c(0,2)) +
    geom_vline(xintercept=1,col="red",linetype=2) +
    theme(legend.text = element_text(size = 14))) 

fig_hr %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/hazard ratios relative to unexposed for sociodemographic groups.jpg"),width=6,height=6)


pdadm405_hr %>% 
  dplyr::filter(outcome == "CPIT2DM") %>% 
  mutate(coef_ci = paste0(round(predicted,2)," (",
                          round(conf.low,2),", ",
                          round(conf.high,2),")")) %>% 
  dplyr::select(group,facet,coef_ci) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,Historical,Unexposed,Exposed) %>%  
  write_csv(.,"paper/table_risks relative to unexposed for sociodemographic subgroups.csv")
