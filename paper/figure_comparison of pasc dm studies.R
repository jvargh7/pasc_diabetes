
other_studies <- readxl::read_excel(paste0(path_pasc_diabetes_folder,"/working/Comparison of PASC DM Studies_2024-03-01.xlsx")) %>% 
  rename(control = 'Type of Control',
         hr = 'Relative Risk (HR, RR, OR etc) and 95% CI') %>% 
  dplyr::filter(Population == "All",!is.na(hr)) %>% 
  distinct(Author, Year, hr,.keep_all=TRUE) %>% 
  dplyr::mutate(label = paste0(Author," ", Year," - ", control)) %>% 
  dplyr::select(label,hr) %>% 
  bind_rows(
    data.frame(label = c("This Study - Contemporary",
                         "This Study - Historical"),
               hr = c("1.28 (1.20, 1.37)",
                      "1.64 (1.50, 1.80)"))
  ) %>% 
  separate(hr,into = c("estimate","lci","uci"),sep = c("(\\s\\(|\\,)")) %>% 
  mutate(uci = str_replace(uci,"\\)","")) %>% 
  mutate(across(estimate:uci,~as.numeric(.)))


fig_comparison <- ggplot(data=other_studies,aes(x = estimate,xmin = lci,xmax = uci,y=label)) +
  geom_point() +
  geom_errorbarh() +
  scale_x_continuous(limits=c(0,4),breaks=c(0:4)) +
  geom_vline(xintercept = 0,linetype = 2, color = "red") +
  xlab("Hazard Ratio/Rate Ratio") +
  ylab("") +
  theme_bw()

fig_comparison %>% 
  ggsave(.,filename = paste0(path_pasc_diabetes_folder,"/figures/comparison of pasc dm studies.jpg"),width = 5, height = 4)
