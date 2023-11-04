
lrt_est <- readRDS(paste0(path_pasc_diabetes_folder,"/working/models pdadm/pdadm101_high dimensional covariates with cpit2dm.RDS")) %>% 
  mutate(across(starts_with("pval"),.f=list(padj = ~p.adjust(.,method="BH")))) %>% 
  mutate(var_group = str_extract(variable,"^(ipd|opd|ipp|opp|pre|lab)")) %>% 
  mutate(var_group = case_when(str_detect(variable,"LOINC") ~ "lab",
                               is.na(var_group) ~ "pre",
                               TRUE ~ var_group))

selected_hdvars = lrt_est %>%
  dplyr::filter(pval_cpit2dm_padj < fdr_hd_pvalue) %>% 
  dplyr::select(var_group,variable,pval_cpit2dm_padj) %>% 
  mutate(outcome = "cpit2dm")
