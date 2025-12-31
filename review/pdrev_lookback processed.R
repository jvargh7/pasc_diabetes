source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R"))
library(tidymodels)
tidymodels_prefer()
lookback_processed <- recipe(COHORT ~ .,
                             data = lookback_df) %>% 
  
  update_role(ID,matchid,index_date, new_role="ID") %>% 
  # https://recipes.tidymodels.org/reference/step_impute_knn.html
  # step_impute_knn creates a specification of a recipe step that will impute missing data using nearest neighbors.
  # step_impute_knn(all_predictors(),neighbors = 5) %>% 
  step_impute_mode(all_nominal()) %>%
  # https://github.com/tidymodels/recipes/issues/756 
  # There might be scenarios where it is more reasonable to impute missing values per group/subsample.
  step_impute_linear(all_numeric(),impute_with = imp_vars(one_of(c("female","nhblack","hispanic","site","age","COHORT")))) %>%
  # prep(): For a recipe with at least one preprocessing operation, estimate the required parameters from a training set that can be later applied to other data sets.
  prep(.,training = lookback_df) %>% 
  # bake(): For a recipe with at least one preprocessing operation that has been trained by prep(), apply the computations to new data.
  bake(.,new_data=lookback_df) %>% 
  mutate(EXPOSED = case_when(COHORT == "exposed" ~ 1,
                             TRUE ~ 0),
         UNEXPOSED = case_when(COHORT == "unexposed" ~ 1,
                               TRUE ~ 0),
         HISTORICAL = case_when(COHORT == "historical" ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(raceeth_category = case_when(nhwhite == 1 ~ "NH White",
                                      nhblack == 1 ~ "NH Black",
                                      hispanic == 1 ~ "Hispanic",
                                      TRUE ~ "NH Other"),
         sex_category = case_when(female == 1 ~ "Female",
                                  TRUE ~ "Male"),
         age_category = case_when(age %in% c(18:39) ~ "18 to 39",
                                  age %in% c(40:64) ~ "40 to 64",
                                  age >= 65 ~ "65 plus",
                                  TRUE ~ NA_character_))