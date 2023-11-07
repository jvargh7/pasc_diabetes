rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/pasc_cardiometabolic_risk/functions/marginal_predictions_plot.R")

# Hazard ratios ----------
pdsu402_hr <- read_csv("sensitivity utilization/pdsu402_difference relative to exposed.csv") %>% 
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


# Burden per 1000 people at 12 months --------

pdsu403_burden <-   read_csv("sensitivity utilization/pdsu403_cumulative incidence at one year.csv") %>% 
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


# Relative burden to Exposed --------

pdsu403_relative <-   read_csv("sensitivity utilization/pdsu403_difference cumulative incidence from exposed at one year.csv") %>% 
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
  mutate(across(contains("reduced_surv"),.fns=function(x) x*-1)) 




bind_rows(pdsu402_hr %>% 
            mutate(coef_ci = paste0(round(predicted,2)," (",
                                    round(conf.low,2),", ",
                                    round(conf.high,2),")")) %>% 
            mutate(type = "HR"),
          pdsu403_burden %>% 
            mutate(coef_ci = paste0(round(cuminc_surv,1)," (",
                                    round(cuminc_ci_lower,1),", ",
                                    round(cuminc_ci_upper,1),")")) %>% 
            mutate(type = "Burden per 1000"),
          pdsu403_relative %>% 
            mutate(coef_ci = paste0(round(reduced_surv_est,1)," (",
                                    round(reduced_surv_lci,1),", ",
                                    round(reduced_surv_uci,1),")")) %>% 
            mutate(type = "Burden relative to Exposed")
) %>% 
  dplyr::select(group,facet,coef_ci,type) %>% 
  pivot_wider(names_from=group,values_from=coef_ci) %>% 
  arrange(facet) %>% 
  dplyr::select(facet,type,Exposed,Unexposed,Historical) %>% 
  write_csv(.,"paper/table_sensitivity utilization modeling results.csv")
