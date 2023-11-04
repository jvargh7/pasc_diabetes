rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_diabetes_repo,"/analysis cpit2dm/pdadm001_analytic dataset for data availability.R"))
rm(anthro_followup,demographic,index_date,lab_followup); gc()

cpit2dm_df = all_cpit2dm_df %>% 
  # Intersection of patients with follow-up and patients with look-back
  dplyr::filter(ID %in% lookback_df$ID)

label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")

(fig_followup_times <- cpit2dm_df %>% 
  mutate(incident_dm = factor(incident_dm,levels=c(0,1),labels=c("No CPiT2DM","CPiT2DM")),
         COHORT = factor(COHORT,levels=c("historical","unexposed","exposed"),labels=label_order)) %>% 
  ggplot(data= .,aes(x = t,fill=COHORT,group=COHORT)) +
  geom_histogram(alpha=0.4, position="identity") +
    facet_grid(incident_dm~.,scales="free_y") +
    scale_fill_discrete(name="",labels= label_order,type = color_order)+
    theme_bw() +
    xlab("Time since origin date (T0 + 30) in days")+
    ylab("Count") +
    theme(legend.position = "bottom")) %>% 
  ggsave(.,filename=paste0(path_pasc_diabetes_folder,"/figures/distribution of followup times.png"),width=8,height=5)
