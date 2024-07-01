.run{
  
  #' `Execute Function`
  
  site_list <- c('seattle', 'stanford', 'lurie', 'nemours', 'national',
                 'nationwide', 'chop', 'colorado', 'cchmc', 'texas')
  
  for(i in 1:length(site_list)){
    
    config('site_filter', site_list[i])
    
    source(paste0(base_dir, '/code/fot_execute.R'))
    
    num_mnths <- (interval(mdy(01012013), mdy(12312023)) %/% months(1)) + 1
    
    time_span_list_output <-
      as.character(seq(as.Date('2013-02-01'), length = num_mnths, by='months')-1)
    time_span <- c(time_span_list_output)
    
    fot <- check_fot(time_tbls = fot_tbl_list,
                     time_frame = time_span,
                     visits_only = FALSE)
    
    fot_reduce <- reduce(.x=fot,
                         .f=dplyr::union)
    
    output_tbl_append(fot_reduce, 'fot_output')
    
  }
  

  #' `Process Raw Output`
  
  fot_list <- fot_check('row_pts',tblx=results_tbl('fot_output'))
  output_list_to_db(fot_list, append = FALSE)
  
  fot_output_distance <- check_fot_all_dist(fot_list$fot_heuristic)
  output_tbl(fot_output_distance,
             'factsovertime_distance',
             indexes=list('check_name'))
  
  #' `Combined Data Cleaning`
  
  ## Read in data from both networks
  fot_trinetx <- read_csv('results/factsovertime_trinetx.csv')
  fot_chop <- read_csv('results/factsovertime_rawcts.csv') %>%
    left_join(read_csv('results/factsovertime_normalized.csv')) %>%
    left_join(read_csv('results/factsovertime_distance.csv'))
  
  
  ## Clean data and apply common structure
  fot_trinetx_clean <- fot_trinetx %>%
    rename(check_desc = query,
           month_end = date,
           row_pts = patients) %>%
    mutate(month_end = as.Date(month_end, format = '%d %b %Y')) %>%
    mutate(check_desc = case_when(grepl('anxiety', check_desc) ~ 'anxiety',
                                  grepl('asthma', check_desc) ~ 'asthma',
                                  grepl('hypertension', check_desc) ~ 'hypertension',
                                  grepl('resp', check_desc) ~ 'respiratory_infection',
                                  grepl('emergency', check_desc) ~ 'emergency_visits',
                                  grepl('emergency vitals', check_desc) ~ 'emergency_visit_vitals',
                                  grepl('inpatients', check_desc) ~ 'inpatient_visits',
                                  grepl('meds', check_desc) ~ 'inpatient_administration',
                                  grepl('outpatients', check_desc) ~ 'outpatient_visits',
                                  grepl('procedures', check_desc) ~ 'outpatient_procedure'))
  
  ## Apply normalization heuristic to TriNetX data
  fot_trinetx_new <- compute_at_cross_join(cj_tbl = fot_trinetx_clean %>% 
                                             mutate(time_start = month_end, time_increment = 'month'), 
                                           cj_var_names = c('site_anon', 'check_desc'))
  
  fot_list_trinetx <- fot_check('row_pts',
                                tblx= fot_trinetx_new %>% 
                                  filter(site_anon != 'All HCOs') %>% 
                                  mutate(check_name = check_desc, site = site_anon))
  
  fot_trinetx_final <- fot_list_trinetx$fot_heuristic %>% 
    rename(site_anon = site) %>%
    select(site_anon, month_end, check_desc, row_pts, check)
  
  ## Clean trinetx data
  fot_chop_clean <- fot_chop %>%
    select(site_anon, month_end, check_desc, row_pts, row_visits) %>%
    mutate(month_end = as.Date(month_end, format = '%m/%d/%y'))
  
  ## combined
  fot_final <- fot_trinetx_final %>%
    union(fot_chop_clean) %>%
    filter(site_anon != 'All HCOs' & site_anon != 'all')
  
  
  
}