rm(list=ls());gc();source(".Rprofile")

ipw_cox_fit <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm401_ipw cox fit.RDS"))
ipw_cox_sex <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm401_ipw cox sex.RDS"))
ipw_cox_raceeth <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm401_ipw cox raceeth.RDS"))
ipw_cox_age <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm401_ipw cox age.RDS"))
ipw_cox_hospitalization <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm401_ipw cox hospitalization.RDS"))

library(survival)



