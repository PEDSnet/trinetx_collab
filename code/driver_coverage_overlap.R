.run <- function(){
  
  #' `Execute function`
  
  site_map <- read_csv(paste0(base_dir, '/results/site_mapping.csv'))
  
  source(paste0(base_dir, '/code/coverage_overlap_execute.R'))
  overlap_output <- check_coverage_overlap(fact_tbls = cohort_tbls)
  
  output_tbl(overlap_output %>% left_join(site_map), 'coverage_overlap')
  
  
  #' `Process Raw Output`
  
  overlap_final <- trinetx_check_pp(dat = results_tbl('coverage_overlap'), group = 'fact_group') %>%
    select(site, fact_group, n_pts_site, n_pts_all, prop_pts_site, prop_pts_all,
           total_pts_site, total_pts_all)
  
  output_tbl(overlap_final, 'coverage_overlap_chop')
  
  
  #' `Combined Data Cleaning`
  
  ## Read in data from both networks
  cvg_trinetx <- read_csv('results/coverage_trinetx_sept.csv') #%>%
    #left_join(read_csv('results/coverage_trinetx_n.csv'))
  cvg_chop <- read_csv('results/coverage_overlap_chop.csv')
  
  ## Clean data and apply common structure
  cvg_trinetx_clean <- cvg_trinetx %>%
    pivot_longer(cols = !c(site_anon, total_n),
                 names_to = 'fact_group',
                 values_to = 'pct_pts') %>%
    mutate(pct_pts = str_remove(pct_pts, '%'),
           pct_pts = as.numeric(pct_pts),
           prop_pts = pct_pts/100,
           n_pts_site = round(total_n * prop_pts)) %>%
    select(-c(pct_pts, total_n))
  
  cvg_chop_clean <- cvg_chop %>%
    select(site_anon, fact_group, prop_pts_site, n_pts_site) %>%
    rename(prop_pts = prop_pts_site)
  
  cvg_final <- cvg_trinetx_clean %>%
    union(cvg_chop_clean) %>%
    filter(site_anon != 'all') %>%
    group_by(fact_group) %>%
    mutate(allsite_median = median(prop_pts)) %>% ungroup() %>%
    mutate(fact_long = str_replace_all(fact_group, c(dx='Diagnosis', px = 'Procedure',
                                                     lab = 'Lab', med = 'Medication', '_' = ' and ')),
           fact_long = ifelse(!grepl('and', fact_long), paste0(fact_long, ' Only'), 
                              fact_long))
  
  ## Output cleaned & combined data
  write.csv(cvg_final, file = 'results/COMBINED_cvg_overlap_sept.csv')
  
  
}