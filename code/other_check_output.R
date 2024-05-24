
#' Visualization List
#' 
#' COUPLETS -- Hanieh's output (heatmap & anomaly detection dot/star plot)
#' 
#' CASE MIX -- use dot/star plot for anomaly detection; try some non-venn diagram exploratory outputs
#' 
#' COVERAGE OVERLAP -- use dot/star exploratory graph from PF; same anomaly detection
#' 
#' FOT -- run euclidean distance

################### CASE MIX ##################################

# Read in & clean data
mix_trinetx <- read_csv('results/case_mix_trinetx.csv') %>% mutate(cohort='full')
mix_chop <- read_csv('results/case_mix_alltime.csv') %>% mutate(cohort='full')
mix_chop_scd <- read_csv('results/case_mix_chop_scd.csv') %>% 
  select(site_anon,icd_header,prop_pt_site) %>% mutate(cohort='scd')
mix_chop_metformin <- read_csv('results/case_mix_chop_metformin.csv') %>% 
  select(site_anon,icd_header,prop_pt_site) %>% mutate(cohort='metformin')

mix_trinetx_clean <- mix_trinetx %>%
  mutate(prop_pts = pct_pts/100) %>%
  select(-c(description, pct_pts))

mix_chop_clean <- mix_chop %>%
  select(site_anon, icd_header, prop_pt_site,cohort) %>%
  dplyr::union(mix_chop_scd) %>% 
  dplyr::union(mix_chop_metformin) %>% 
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

write.csv(mix_final, file = 'results/COMBINED_case_mix.csv')
  

## MULTI SITE EXPLORATORY Output graph
casemix_output <- ms_exp_nt2(process_output = mix_final %>% ungroup() %>%  filter(cohort=='full'),
                             check_string = 'Branch',
                             y_col = 'branch',
                             descriptor_col = 'description')

casemix_output

casemix_output_scd <- ms_exp_nt2(process_output = mix_final %>% filter(cohort=='scd'),
                             check_string = 'Branch',
                             y_col = 'branch',
                             descriptor_col = 'description')

casemix_output_metformin <- ms_exp_nt2(process_output = mix_final %>% filter(cohort=='metformin'),
                                 check_string = 'Branch',
                                 y_col = 'branch',
                                 descriptor_col = 'description')



  ### Exploratory of PEDSnet vs Trinetx
  casemix_twosites_tbl <- mix_final %>% 
                          mutate(site_assignment = 
                                   case_when(
                                     site_anon %in% c('HCO1','HCO2','HCO3',
                                                      'HCO4','HCO5','HCO6',
                                                      'HCO7','HCO8','HCO9',
                                                      'HCO10','HCO11','HCO12','HCO13') ~ 'Trinetx',
                                     TRUE ~ 'PEDSnet'
                                   )) %>% 
                           rename(site_original=site_anon,
                                  site_anon=site_assignment) %>% filter(cohort == 'full')
  
  casemix_output_twosites <- ms_exp_nt2(process_output=casemix_twosites_tbl,
                                        check_string = 'Branch',
                                        y_col = 'branch',
                                        descriptor_col = 'description')
  
  casemix_twocats_tbl <- casemix_twosites_tbl %>% 
                         rename(site_category=site_anon,
                                site_anon=site_original)
  
  casemix_output_twocats <- ms_exp_nt2_facet(process_output=casemix_twocats_tbl,
                                       check_string = 'Branch',
                                       y_col = 'branch',
                                       descriptor_col = 'description') 
  ### Multisite Exploratory of Rows
  mix_chop_clean_rows <- 
    mix_chop %>% select(site_anon, icd_header, prop_row_site) %>% 
    rename(prop_rows = prop_row_site,
           branch = icd_header) %>%
    mutate(branch = str_replace_all(branch, ' ', '')) %>% 
    group_by(branch) %>%
    filter(site_anon != 'All HCOs') %>%
    mutate(allsite_median = median(prop_rows)) 
  
  casemix_output_rows <- ms_exp_nt2(process_output = mix_chop_clean_rows,
                                    check_string = 'Branch',
                                    y_col = 'branch',
                                    descriptor_col = 'branch',
                                    x_col = 'prop_rows')
  

## MULTI SITE ANOMALY Output graph

#' Setting up for Anomaly Detection
casemix_anomaly_cohort <- compute_dist_anomalies(df_tbl=mix_final %>% 
                                                     rename(site=site_anon) %>% filter(cohort == 'full'), 
                                                   grp_vars=c('branch','description','allsite_median'),
                                                   var_col='prop_pts')
casemix_anomaly_final_cohort <- detect_outliers(casemix_anomaly_cohort,
                                                  column_analysis='prop_pts',
                                                  column_variable = 'branch')

casemix_anomaly_plot_combined <- ms_anom_nt(process_output=casemix_anomaly_final_cohort,comparison_col='prop_pts',variable='branch')

#' PEDSnet Only 
#' 
casemix_anomaly_cohort_pedsnet <- compute_dist_anomalies(df_tbl=casemix_twocats_tbl %>% filter(site_category == 'PEDSnet') %>% 
                                                   rename(site=site_anon), 
                                                 grp_vars=c('branch','description','allsite_median'),
                                                 var_col='prop_pts')

casemix_anomaly_final_cohort_pedsnet <- detect_outliers(casemix_anomaly_cohort_pedsnet,
                                                column_analysis='prop_pts',
                                                column_variable = 'branch')

casemix_anomaly_plot_combined_pedsnet <- ms_anom_nt(process_output=casemix_anomaly_final_cohort_pedsnet,comparison_col='prop_pts',variable='branch')



###################COVERAGE OVERLAP#####################################

## Read in and clean data
cvg_trinetx <- read_csv('results/coverage_trinetx.csv')
cvg_chop <- read_csv('results/coverage_overlap.csv')

cvg_trinetx_clean <- cvg_trinetx %>%
  pivot_longer(cols = !site_anon,
               names_to = 'fact_group',
               values_to = 'pct_pts') %>%
  mutate(prop_pts = pct_pts / 100) %>%
  select(-pct_pts)

cvg_chop_clean <- cvg_chop %>%
  select(site_anon, fact_group, prop_pts_site) %>%
  rename(prop_pts = prop_pts_site)

cvg_final <- cvg_trinetx_clean %>%
  union(cvg_chop_clean) %>%
  filter(site_anon != 'All HCOs') %>%
  group_by(fact_group) %>%
  mutate(allsite_median = median(prop_pts)) %>% ungroup() %>%
  mutate(fact_long = str_replace_all(fact_group, c(dx='Diagnosis', px = 'Procedure',
                                                   lab = 'Lab', med = 'Medication', '_' = ' and ')))

write.csv(cvg_final, file = 'results/COMBINED_cvg_overlap.csv')

## Output graph
cvg_output <- ms_exp_nt2(process_output = cvg_final,
                         check_string = 'Fact Type',
                         y_col = 'fact_group',
                         descriptor_col = 'fact_long')

cvg_output

cvg_overlap_anom_tbl <- compute_dist_anomalies(df_tbl=cvg_final %>% rename(site=site_anon), 
                                               grp_vars=c('fact_group','fact_long', 'allsite_median'),
                                               var_col='prop_pts')

cvg_overlap_anom_tbl2 <- detect_outliers(cvg_overlap_anom_tbl,
                                         column_analysis='prop_pts',
                                         column_variable = 'fact_group')

cvg_overlap_anom_plot <- ms_anom_nt(process_output=cvg_overlap_anom_tbl2,
                                    comparison_col='prop_pts',
                                    variable='fact_group')

#############################################################

## Read in data
fot_trinetx <- read_csv('results/factsovertime_trinetx.csv')
fot_chop <- read_csv('results/factsovertime_rawcts.csv') %>%
  left_join(read_csv('results/factsovertime_normalized.csv')) %>%
  left_join(read_csv('results/factsovertime_distance.csv'))
fot_chop_additional <- read_csv('results/fot_output_new_chop.csv')

## Clean trinetx data
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



## Rerun heuristic based on patient counts to make sure both sets of output are aligned
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
  select(site_anon, month_end, check_desc, row_pts, check) %>%
  mutate(month_end = as.Date(month_end, format = '%m/%d/%y'))

## combined
fot_final <- fot_trinetx_final %>%
  union(fot_chop_clean) %>%
  filter(site_anon != 'All HCOs' & site_anon != 'all')


## Run eucildean distance on CHOP, TriNetX, and combined data
## Using normalized patient count (i.e. weighted average value) - those values are centered on or around 0
norm_pts_euc_t <- ms_anom_euclidean(fot_input_tbl = fot_final %>% mutate(time_start = month_end,
                                                                         time_increment = 'month') %>%
                                      filter(site_anon %in% c('HCO1','HCO2','HCO3',
                                                         'HCO4','HCO5','HCO6',
                                                         'HCO7','HCO8','HCO9',
                                                         'HCO10','HCO11','HCO12','HCO13')),
                                 grp_vars = c('site_anon', 'check_desc'),
                                 var_col = 'check')

write.csv(norm_pts_euc_t, file = 'results/FOT_trinetx.csv')

norm_pts_euc_c <- ms_anom_euclidean(fot_input_tbl = fot_final %>% mutate(time_start = month_end,
                                                                         time_increment = 'month') %>%
                                      filter(! site_anon %in% c('HCO1','HCO2','HCO3',
                                                         'HCO4','HCO5','HCO6',
                                                         'HCO7','HCO8','HCO9',
                                                         'HCO10','HCO11','HCO12','HCO13')),
                                    grp_vars = c('site_anon', 'check_desc'),
                                    var_col = 'check')

write.csv(norm_pts_euc_c, file = 'results/FOT_chop.csv')

norm_pts_euc_all <- ms_anom_euclidean(fot_input_tbl = fot_final %>% mutate(time_start = month_end,
                                                                           time_increment = 'month'),
                                      grp_vars = c('site_anon', 'check_desc'),
                                      var_col = 'check')

write.csv(norm_pts_euc_all, file = 'results/FOT_combined.csv')

## Generate output -- testing separate and combined

check_type <- 'inpatient_administration'

fot_all_output <- euclidean_output(process_output = norm_pts_euc_all,
                         output_var = 'check',
                         filter_variable = check_type)

fot_all_output[[1]]
fot_all_output[[2]]
fot_all_output[[3]]


fot_chop_output <- euclidean_output(process_output = norm_pts_euc_c,
                       output_var = 'check',
                       filter_variable = check_type)

fot_chop_output[[1]]
fot_chop_output[[2]]
fot_chop_output[[3]]


fot_tnx_output <- euclidean_output(process_output = norm_pts_euc_t,
                       output_var = 'check',
                       filter_variable = check_type)

fot_tnx_output[[1]]
fot_tnx_output[[2]]
fot_tnx_output[[3]]

## Outpatient Visits as a Proportion of all Visits 

fot_chop_trinetx_rawcts <- 
  dplyr::union(fot_trinetx_new %>% select(site_anon,month_end,check_desc,row_pts) %>% mutate(cohort='trinetx',
                                                                                             row_visits=0),
               fot_chop_additional %>% select(site_anon,month_end,check_desc,row_pts,row_visits) %>% 
                 mutate(month_end=mdy(month_end),
                        cohort='pedsnet'))

fot_compute_proportions <- function(fot_tbl = fot_chop_trinetx_rawcts %>% filter(cohort=='pedsnet'),
                                    var_col = 'row_visits',
                                    denom_groups = c('all_visits'),
                                    num_groups = c('outpatient_visits')) {
  
  denom_pts <- 
    fot_tbl %>% 
    filter(check_desc %in% denom_groups) %>% 
    group_by(site_anon,
             month_end) %>% summarise(var_denom=sum(!!sym(var_col))) 
   
  
  num_pts <- 
    fot_tbl %>% 
    filter(check_desc %in% num_groups) %>% 
    group_by(site_anon,
             month_end) %>% summarise(var_num=sum(!!sym(var_col))) 
  
  num_denom_prop <- 
    denom_pts %>% 
    left_join(
      num_pts
    ) %>% 
    mutate(prop=round(var_num/var_denom, 2))
  
  
}

test <- fot_compute_proportions(var_col = 'row_visits')

  
test2 <- compute_dist_mean_median(test,
                                  c('month_end'),
                                  c('prop'),
                                  num_sd=2,
                                  num_mad=2)

outpt_visits_euclidean <- ms_anom_euclidean(fot_input_tbl = test2 %>% mutate(time_start = month_end,
                                                                         time_increment = 'month'),
                                    grp_vars = c('site_anon'),
                                    var_col = 'prop')

fot_outpatient_output <- euclidean_output(process_output = outpt_visits_euclidean,
                                          output_var = 'prop',
                                          filter_variable = NULL)

fot_outpatient_output[[1]]
fot_outpatient_output[[2]]
fot_outpatient_output[[3]]

## Asthma Inpatients as a Proportion of All Inpatients


ip_asthma_all <- fot_compute_proportions(fot_tbl = fot_chop_trinetx_rawcts %>% filter(cohort=='pedsnet'),
                                         var_col = 'row_visits',
                                         denom_groups = c('inpatient_visits'),
                                         num_groups = c('asthma_inpatient'))


asthma_ip_medians <- compute_dist_mean_median(ip_asthma_all,
                                  c('month_end'),
                                  c('prop'),
                                  num_sd=2,
                                  num_mad=2)

asthma_ip_euclidean <- ms_anom_euclidean(fot_input_tbl = asthma_ip_medians %>% mutate(time_start = month_end,
                                                                             time_increment = 'month'),
                                            grp_vars = c('site_anon'),
                                            var_col = 'prop')

fot_asthma_ip_output <- euclidean_output(process_output = asthma_ip_euclidean,
                                          output_var = 'prop',
                                          filter_variable = NULL)

fot_asthma_ip_output[[1]]
fot_asthma_ip_output[[2]]
fot_asthma_ip_output[[3]]


## Single Site Anom

# raw cts -- trinetx
anomalize_hco9_raw <- anomalize_ss_anom_at(fot_input_tbl = fot_chop_trinetx_rawcts %>% 
                                    mutate(time_increment = 'month', 
                                           time_start = month_end) %>% 
                                    filter(site_anon == 'HCO9'), 
                                  grp_vars = 'check_desc', 
                                  time_var = 'time_start', 
                                  var_col = 'row_pts')

anomalize_hco9_output <- ss_anom_at(process_output = anomalize_hco9_raw,
                                    filtered_var = 'emergency_visits',
                                    var_col = 'check_desc')

anomalize_hco9_output

# Asthma -- pedsnet
anomalize_asthma <- anomalize_ss_anom_at(fot_input_tbl = ip_asthma_all %>% 
                                             mutate(time_increment = 'month', 
                                                    time_start = month_end,
                                                    check_desc = 'inpatient_asthma') %>% 
                                             filter(site_anon == 'chop'), 
                                           grp_vars = 'check_desc', 
                                           time_var = 'time_start', 
                                           var_col = 'prop')

anomalize_asthma_output <- ss_anom_at(process_output = anomalize_asthma,
                                    filtered_var = 'inpatient_asthma',
                                    var_col = 'check_desc')

anomalize_asthma_output


# outpatient -- pedsnet

anomalize_op <- anomalize_ss_anom_at(fot_input_tbl = test %>% 
                                           mutate(time_increment = 'month', 
                                                  time_start = month_end,
                                                  check_desc = 'outpatient_visits') %>% 
                                           filter(site_anon == 'chop'), 
                                         grp_vars = 'check_desc', 
                                         time_var = 'time_start', 
                                         var_col = 'prop')

anomalize_op_output <- ss_anom_at(process_output = anomalize_op,
                                      filtered_var = 'outpatient_visits',
                                      var_col = 'check_desc')

anomalize_op_output
