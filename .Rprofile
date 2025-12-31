source("C:/code/external/pasc_cardiometabolic_risk/.Rprofile")

path_pasc_diabetes_folder <- "C:/Cloud/OneDrive - Emory University/Papers/_Published/PASC Diabetes Incidence" 
path_pasc_diabetes_repo <- "C:/code/external/pasc_diabetes"


icd10_traumatic_fractures <- c("S02\\.", # Head https://www.icd10data.com/ICD10CM/Codes/S00-T88/S00-S09/S02-
                               "S12\\.", # Neck https://www.icd10data.com/ICD10CM/Codes/S00-T88/S10-S19
                               "S22\\.", # Rib-sterum-thorax: https://www.icd10data.com/ICD10CM/Codes/S00-T88/S20-S29
                               "S32\\.", # Abdomen-lower back-spine: https://www.icd10data.com/ICD10CM/Codes/S00-T88/S30-S39
                               "S42\\.", # Shoulder and upper arm: https://www.icd10data.com/ICD10CM/Codes/S00-T88/S40-S49
                               "S52\\.", # Forearm: https://www.icd10data.com/ICD10CM/Codes/S00-T88/S50-S59
                               "S62\\.", # Wrist and hand: https://www.icd10data.com/ICD10CM/Codes/S00-T88/S60-S69
                               "S72\\.", # Femur: https://www.icd10data.com/ICD10CM/Codes/S00-T88/S70-S79
                               "S82\\.", # Lower leg: https://www.icd10data.com/ICD10CM/Codes/S00-T88/S80-S89
                               "S92\\." # Foot: https://www.icd10data.com/ICD10CM/Codes/S00-T88/S90-S99
                               ) # https://www.icd10data.com/ICD10CM/Index/F/Fracture%2c_traumatic

icd10_otitis_media <- c("H65\\.",
                        "H66\\.",
                        "H67\\.")

coverage_types = c("Medicare","Medicaid","Government","Private","None")
max_col = function(x,n){
  
  value = sort(x,decreasing = TRUE)[n]
  if(value > 0){
    if(sum(x %in% value) == 1){
      column_name = coverage_types[which(x == value,arr.ind = TRUE)]
    } else{
      column_name = coverage_types[which(x == value,arr.ind = TRUE)[2]]
    }
  } else{column_name = NA}
  
  return(column_name)
  
}
