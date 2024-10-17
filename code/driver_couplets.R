
  #' `Execute Function`
  
  source(paste0(config('base_dir'), '/code/dcon_execute.R'))
  
  dcon_pts <- check_dcon(conc_tbls = dcon_pts_list,
                         check_string = 'dcon_pts')
  
  dcon_reduce <- reduce(.x = dcon_pts,
                        .f = dplyr::union)
  
  output_tbl(dcon_reduce, 'dcon_output')
  
  dcon_pp <- apply_dcon_pp(dcon_tbl = results_tbl('dcon_output'),
                           byyr = FALSE)
  
  output_tbl(dcon_pp, 'dcon_output_pp')
  
  
  #' `Combined Data Cleaning`
  
  ## Read in data
  couplets_chop <- read_csv('results/couplets_chop.csv') %>%
    select(-site_anon) %>% inner_join(read_csv('results/site_map.csv')) %>% 
    select(-c(site, sitenum, siteletter))
  couplets_trinetx <- read_csv('results/couplets_trinetx_sept.csv') %>%
    inner_join(read_csv('results/site_map.csv')) %>% select(-c(site, sitenum, siteletter))
  
  ## Clean trinetx
  trinetx_cleaned <- 
    couplets_trinetx %>% 
    select(-c(c1_in_c2, c2_in_c1)) %>%
    mutate(check_desc_new=
             case_when(str_detect(check_desc, 'Sickle cell') ~ 'scd_dx_hydrox_rx',
                       str_detect(check_desc, 'Asthma') ~ 'asthma_dx_broncho_rx',
                       str_detect(check_desc, 'T2DM') ~ 't2d_dx_metformin_rx',
                       str_detect(check_desc, 'Anxiety') ~ 'anxiety_dx_depression_dx',
                       str_detect(check_desc, 'Edema') ~ 'edema_dx_loop_rx',
                       str_detect(check_desc, 'Imaging') ~ 'frac_dx_img_px',
                       str_detect(check_desc, 'T1DM') ~ 't1d_dx_insulin_rx',
                       TRUE ~ check_desc)) %>% 
    filter(! str_detect(check_desc, 'Oncology')) %>% 
    rename(couplet_name = check_desc,
           check_desc = check_desc_new) %>% 
    mutate(tot_pats = cohort_1_only + cohort_2_only + combined) %>% 
    mutate(cohort_1_denom_prop=combined/cohort_1,
           cohort_2_denom_prop=combined/cohort_2,
           cohort_1_only_prop = cohort_1_only / cohort_1,
           cohort_2_only_prop = cohort_2_only / cohort_2) %>% 
    pivot_longer(cols=c('cohort_1','cohort_1_only',
                        'combined', 'cohort_2_only',
                        'cohort_2', 'cohort_1_denom_prop', 'cohort_2_denom_prop',
                        'cohort_1_only_prop', 'cohort_2_only_prop'),
                 names_to = 'cohort',
                 values_to = 'value') %>% 
    mutate(prop=
             case_when(value >= 1.001 ~ round(value/tot_pats, 2),
                       TRUE ~ value)) 
  
  ## Using Trinetx check descriptions
  check_desc_lookup <- 
    trinetx_cleaned %>% select(couplet_name,
                               check_desc) %>% distinct()
  
  ## Clean CHOP
  chop_cleaned <- 
    couplets_chop %>% 
    select(-c(prop, sitenum)) %>% 
    pivot_wider(names_from = 'cohort',
                values_from = 'value') %>% 
    mutate(cohort_1 = cohort_1_only + combined,
           cohort_2 = cohort_2_only + combined) %>% 
    mutate(cohort_1_denom_prop=combined/cohort_1,
           cohort_2_denom_prop=combined/cohort_2,
           cohort_1_only_prop = cohort_1_only / cohort_1,
           cohort_2_only_prop = cohort_2_only / cohort_2) %>% 
    pivot_longer(cols=cohort_1_only:cohort_2_only_prop,
                 names_to='cohort',
                 values_to='value') %>% 
    mutate(prop=
             case_when(value >= 1.001 ~ round(value/tot_pats, 2),
                       TRUE ~ value)) %>% 
    inner_join(check_desc_lookup) %>% 
    select(-c('check_name','check_type'))
  
  ## Combined CHOP and Trinetx data
  couplets_combined <- dplyr::union(trinetx_cleaned,
                                    chop_cleaned)
  
  write.csv(couplets_combined, file = 'results/COMBINED_couplets_sept.csv')
  