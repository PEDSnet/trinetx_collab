
  #' `Execute Function`
  
  site_list <- c('seattle', 'stanford', 'lurie', 'nemours', 'national',
                 'nationwide', 'chop', 'colorado', 'cchmc', 'texas')
  
  ###' *All Patients*
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    site_nm <- site_list[i]
    
    casemix_output <- compute_case_mix(dx_tbl = cdm_tbl('condition_occurrence') %>%
                                         filter(site == site_nm) %>%
                                         filter(condition_start_date >= '2014-01-01',
                                                condition_start_date <= '2023-12-31'))
    
    output_tbl_append(casemix_output, 'case_mix_10yr')
    
  }
  
  ###' *SCD Cohort*
  scd_codes <- load_codeset('dx_sickle_cell')
  scd_full <- list()
  
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    site_nm <- site_list[i]
    
    scd_pts <- cdm_tbl('condition_occurrence') %>% 
      filter(site == site_nm) %>% 
      inner_join(select(scd_codes,
                        concept_id),
                 by=c('condition_concept_id'='concept_id')) %>% 
      filter(condition_start_date >= '2014-01-01',
             condition_start_date <= '2023-12-31') %>%
      distinct(person_id) %>% compute_new()
    
    casemix_output <- compute_case_mix(cohort = scd_pts,
                                       dx_tbl = cdm_tbl('condition_occurrence') %>%
                                         filter(site == site_nm) %>%
                                         filter(condition_start_date >= '2014-01-01',
                                                condition_start_date <= '2023-12-31'))
    
    scd_full[[i]] <- casemix_output
    
  }
  
  scd_full_reduce <- reduce(.x=scd_full,
                            .f=dplyr::union)
  
  output_tbl(scd_full_reduce, 'case_mix_scd')
  
  
  ###' *SCD Cohort - P Branch Breakdown*
  scd_codes <- load_codeset('dx_sickle_cell')
  scd_full_p <- list()
  
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    site_nm <- site_list[i]
    
    scd_pts <- cdm_tbl('condition_occurrence') %>% 
      filter(site == site_nm) %>% 
      inner_join(select(load_codeset('dx_sickle_cell'),
                        concept_id),
                 by=c('condition_concept_id'='concept_id')) %>% 
      filter(condition_start_date >= '2014-01-01',
             condition_start_date <= '2023-12-31') %>%
      distinct(person_id) %>% compute_new()
    
    casemix_output <- compute_case_mix_p_deep_dive(cohort = scd_pts,
                                                   dx_tbl = cdm_tbl('condition_occurrence') %>%
                                                     filter(site == site_nm) %>%
                                                     filter(condition_start_date >= '2014-01-01',
                                                            condition_start_date <= '2023-12-31'))
    
    output_tbl_append(casemix_output %>% collect(), 'case_mix_scd_p')
    
  }

  
  ###' *Metformin Cohort*
  metformin_codes_scdf <- load_codeset('rx_metformin_scdf')
  metformin_codes <- get_descendants(metformin_codes_scdf)
  metformin_full <- list()
  
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    config('site_filter', site_list[i])
    
    site_nm <- site_list[i]
    
    metformin_pts <- site_cdm_tbl('drug_exposure') %>% 
      inner_join(select(metformin_codes,
                        concept_id),
                 by=c('drug_concept_id'='concept_id')) %>% 
      filter(drug_exposure_start_date >= '2014-01-01',
             drug_exposure_start_date <= '2023-12-31') %>%
      select(person_id) %>% compute_new()
    
    casemix_output <- compute_case_mix(cohort = metformin_pts,
                                       dx_tbl = site_cdm_tbl('condition_occurrence') %>%
                                         filter(condition_start_date >= '2014-01-01',
                                                condition_start_date <= '2023-12-31'))
    
    output_tbl_append(casemix_output, 'case_mix_metformin')
    
  }
  
  ###' *Visit Counts*
  
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    site_nm <- site_list[i]
    
    casemix_visits <- compute_case_mix_visits(dx_tbl = cdm_tbl('condition_occurrence') %>%
                                             filter(site == site_nm) %>%
                                               filter(condition_start_date >= '2014-01-01',
                                                      condition_start_date <= '2023-12-31'))
    
    output_tbl_append(casemix_visits, 'case_mix_visits')
    
  }
  
  
  #' `Combined Data Cleaning`
  
  mix_trinetx <- read_csv('results/case_mix_trinetx_sept.csv') %>% mutate(cohort='full') %>%
    inner_join(read_csv('results/site_map.csv')) %>% select(-c(site, sitenum, siteletter))
  mix_trinetx_scd <- read_csv('results/case_mix_trinetx_scd.csv') %>% mutate(cohort='scd') %>%
    inner_join(read_csv('results/site_map.csv')) %>% select(-c(site, sitenum, siteletter))
  mix_chop <- read_csv('results/case_mix_chop.csv') %>% mutate(cohort='full') %>%
    select(-site_anon) %>% inner_join(read_csv('results/site_map.csv')) %>% 
    select(-c(site, sitenum, siteletter))
  mix_chop_scd <- read_csv('results/case_mix_chop_scd.csv') %>% 
    select(-site_anon) %>% inner_join(read_csv('results/site_map.csv')) %>% 
    select(-c(site, sitenum, siteletter)) %>%
    select(site_anon,icd_header,prop_pt_site) %>% mutate(cohort='scd')
  mix_chop_metformin <- read_csv('results/case_mix_chop_metformin.csv') %>% 
    select(-site_anon) %>% inner_join(read_csv('results/site_map.csv')) %>% 
    select(-c(site, sitenum, siteletter)) %>%
    select(site_anon,icd_header,prop_pt_site) %>% mutate(cohort='metformin')
  mix_chop_scd_p <- read_csv('results/case_mix_chop_scd_p.csv') %>% 
    select(-site_anon) %>% inner_join(read_csv('results/site_map.csv')) %>% 
    select(-c(site, sitenum, siteletter)) %>%
    select(site_anon,icd_header,prop_pt_site) %>% mutate(cohort='scd_p')
  
  mix_trinetx_clean <- mix_trinetx %>%
    union(mix_trinetx_scd) %>%
    mutate(pct_pts = str_remove(pct_pts, '%'),
           pct_pts = as.numeric(pct_pts),
           prop_pts = pct_pts/100) %>%
    select(-c(description, pct_pts))
  
  mix_chop_clean <- mix_chop %>%
    select(site_anon, icd_header, prop_pt_site,cohort) %>%
    dplyr::union(mix_chop_scd) %>% 
    dplyr::union(mix_chop_metformin) %>% 
    dplyr::union(mix_chop_scd_p) %>% 
    rename(prop_pts = prop_pt_site,
           branch = icd_header) %>%
    mutate(branch = str_replace_all(branch, ' ', ''))
  
  mix_final <- mix_trinetx_clean %>%
    union(mix_chop_clean) %>%
    group_by(cohort,
             branch) %>%
    filter(site_anon != 'All HCOs') %>%
    mutate(allsite_median = median(prop_pts)) %>%
    left_join(mix_trinetx %>% distinct(branch, description)) %>%
    mutate(description = ifelse(grepl('U', branch), 'Codes for special purposes', description))
  
  write.csv(mix_final, file = 'results/COMBINED_case_mix_sept.csv')
  