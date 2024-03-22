rm(list=ls());gc();source(".Rprofile")

ipw_cox_fit <- readRDS(paste0(path_pasc_diabetes_folder,"/working/sensitivity supreme/pdss401_ipw cox fit.RDS"))
overlap_cox_fit <- readRDS(paste0(path_pasc_diabetes_folder,"/working/sensitivity supreme/pdss401_overlap cox fit.RDS"))
ipw_cox_sex <- readRDS(paste0(path_pasc_diabetes_folder,"/working/sensitivity supreme/pdss401_ipw cox sex.RDS"))
ipw_cox_raceeth <- readRDS(paste0(path_pasc_diabetes_folder,"/working/sensitivity supreme/pdss401_ipw cox raceeth.RDS"))
ipw_cox_age <- readRDS(paste0(path_pasc_diabetes_folder,"/working/sensitivity supreme/pdss401_ipw cox age.RDS"))
ipw_cox_hospitalization <- readRDS(paste0(path_pasc_diabetes_folder,"/working/sensitivity supreme/pdss401_ipw cox hospitalization.RDS"))





source("C:/code/external/functions/imputation/contrasts_coxph.R")
source("C:/code/external/functions/preprocessing/prepare_contrasts.R")
source("analysis cpit2dm/pdadmaux_difference grids.R")

# fit = ipw_cox_fit
# # fit = ipw_cox_age
# x = "COHORTunexposed"
# y = ""

# Derived from pcrab406_difference relative to exposed at time 0.R
pdadm405_contrast_fit <- function(fit,x,y){
  names_het = names(fit$coefficients)
  
  mm = matrix(c(
    rep(c(0),each=length(names_het)),
    rep(c(0),each=length(names_het)),
    rep(c(0),each=length(names_het))),
    nrow=3,
    byrow=TRUE
  )
  
  
  
  x_ref = "COHORThistorical"
  
  if(!is.null(y) & y != ""){
    
    # b0 + b1 COHORTexposed + b2 sexMale + b3 t + b4 COHORTexposed.sexMale + b5 COHORTexposed.t + b6 sexMale.t + b7 COHORTexposed.sexMale.t
    # Assume x_ref = "COHORTexposed"
    # Change from t = 0 to t = 100 for x_ref --> similar to pcra305
    mm[1,which(names_het %in% x_ref)] <- -1 # b1
    mm[1,which(names_het %in% paste0(x_ref,":",y))] <- -1*1 #b4
    
    mm[2,which(names_het %in% x)] <- 1 #b1'
    mm[2,which(names_het %in% paste0(x,":",y))] <- 1*1 #b4
    
    # Difference between the above 2 (x - x_ref)
    mm[3,which(names_het %in% x_ref)] <- -1 # b1
    mm[3,which(names_het %in% paste0(x_ref,":",y))] <- -1*1 #b4
    mm[3,which(names_het %in% x)] <- 1 #b1'
    mm[3,which(names_het %in% paste0(x,":",y))] <- 1*1 #b4
    
    
  } else{
    # Change from t = 0 to t = 100 for x_ref
    mm[1,which(names_het %in% x_ref)] <- -1 # b1
    mm[2,which(names_het %in% x)] <- 1 #b1'
    mm[3,which(names_het %in% x_ref)] <- -1 # b1
    mm[3,which(names_het %in% x)] <- 1 #b1'
  }
  
  if(x_ref == x){
    
    mm[3,] <- 0
  }
  
  contrasts_coxph(
    model_matrix = mm,
    vcov_coxph = fit$var,
    coef_coxph = fit$coefficients,
    dfcom_coxph = fit$n
  )   %>% 
    return(.)
}


# Overall Contrasts -------------
contrast_overall <- map_dfr(1:nrow(difference_grid_overall),
                            function(i){
                              print(i)
                              x_name = difference_grid_overall$cohort[i]
                              y_name = difference_grid_overall$modifier1[i]
                              # z_value = difference_grid_overall$modifier2_value[i]
                              bind_rows(
                                pdadm405_contrast_fit(ipw_cox_fit,x=x_name,y="") %>% 
                                  mutate(exposure = x_name,
                                         exposure_value = 1,
                                         modifier_value = NA_real_,
                                         outcome = "CPIT2DM")) %>% 
                                return(.)
                            })

# Overlap Contrasts -------------
contrast_overlap <- map_dfr(1:nrow(difference_grid_overall),
                            function(i){
                              print(i)
                              x_name = difference_grid_overall$cohort[i]
                              y_name = difference_grid_overall$modifier1[i]
                              # z_value = difference_grid_overall$modifier2_value[i]
                              bind_rows(
                                pdadm405_contrast_fit(overlap_cox_fit,x=x_name,y="") %>% 
                                  mutate(exposure = x_name,
                                         exposure_value = 1,
                                         modifier_value = NA_real_,
                                         outcome = "CPIT2DM")) %>% 
                                return(.)
                            })



# Sex Contrasts -------------
contrast_sex <- map_dfr(1:nrow(difference_grid_sex),
                        function(i){
                          print(i)
                          x_name = difference_grid_sex$cohort[i]
                          y_name = difference_grid_sex$modifier1[i]
                          bind_rows(
                            pdadm405_contrast_fit(ipw_cox_sex,x_name,y_name) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     outcome = "CPIT2DM")) %>% 
                            return(.)
                        })

# Age contrasts ----------
contrast_age <- map_dfr(1:nrow(difference_grid_age),
                        function(i){
                          print(i)
                          x_name = difference_grid_age$cohort[i]
                          y_name = difference_grid_age$modifier1[i]
                          bind_rows(
                            pdadm405_contrast_fit(ipw_cox_age,x_name,y_name) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     outcome = "CPIT2DM")) %>% 
                            return(.)
                        })

# Race-Ethnicity Contrasts -------------


contrast_raceeth <- map_dfr(1:nrow(difference_grid_raceeth),
                            function(i){
                              print(i)
                              x_name = difference_grid_raceeth$cohort[i]
                              y_name = difference_grid_raceeth$modifier1[i]
                              bind_rows(
                                pdadm405_contrast_fit(ipw_cox_raceeth,x_name,y_name) %>% 
                                  mutate(exposure = x_name,
                                         modifier1 = y_name,
                                         exposure_value = 1,
                                         modifier1_value = 1,
                                         outcome = "CPIT2DM")) %>% 
                                return(.)
                            })

# Hospitalization Contrasts -------------


contrast_hospitalization <- map_dfr(1:nrow(difference_grid_hospitalization),
                                    function(i){
                                      print(i)
                                      x_name = difference_grid_hospitalization$cohort[i]
                                      y_name = difference_grid_hospitalization$modifier1[i]
                                      bind_rows(
                                        pdadm405_contrast_fit(ipw_cox_hospitalization,x_name,y_name) %>% 
                                          mutate(exposure = x_name,
                                                 modifier1 = y_name,
                                                 exposure_value = 1,
                                                 modifier1_value = 1,
                                                 outcome = "CPIT2DM")) %>% 
                                        return(.)
                                    })

bind_rows(contrast_overall,
          contrast_overlap %>% mutate(modifier_var = "Overlap"),
          contrast_sex %>% mutate(modifier_var = "sex_category"),
          contrast_age %>% mutate(modifier_var = "age_category"),
          contrast_raceeth %>% mutate(modifier_var = "raceeth_category"),
          contrast_hospitalization %>% mutate(modifier_var = "hospitalization_category")
) %>% 
  dplyr::filter(term %in% c("Contrast 3")) %>% 
  write_csv(.,"sensitivity supreme/pdss404_difference relative to historical.csv")
