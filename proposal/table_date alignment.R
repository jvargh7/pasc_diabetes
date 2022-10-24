require(lubridate)
require(tidyverse)

d_start = ymd("2020-03-01")
d_stop = ymd("2022-01-29")

date_df <- data.frame(
  group = c("Exposed","Unexposed"),
  min_test = rep(d_start,2),
  max_test = rep(d_stop,2)
) %>% 
  bind_rows(data.frame(group = "Historical",
            min_test = d_start - 365*2,
            max_test = d_stop - 365*2)) %>% 
  
  mutate(diff_test = max_test - min_test,
         min_checkdm = min_test - 365*2,
         max_checkdm = max_test - 365*2,
         min_alive = min_test + 30,
         max_alive = max_test + 30,
         min_covariates = min_test - 365,
         max_covariates = max_test - 365) %>% 
  mutate(min_followup = min_alive,
         max_followup = max_alive) %>% 
  mutate(diff_followup = max_followup - min_followup)

