
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
mix_trinetx <- read_csv('results/case_mix_trinetx.csv')
mix_chop <- read_csv('results/case_mix_alltime.csv')

mix_trinetx_clean <- mix_trinetx %>%
  mutate(prop_pts = pct_pts/100) %>%
  select(-c(description, pct_pts))

mix_chop_clean <- mix_chop %>%
  select(site_anon, icd_header, prop_pt_site) %>%
  rename(prop_pts = prop_pt_site,
         branch = icd_header) %>%
  mutate(branch = str_replace_all(branch, ' ', ''))

mix_final <- mix_trinetx_clean %>%
  union(mix_chop_clean) %>%
  group_by(branch) %>%
  filter(site_anon != 'All HCOs') %>%
  mutate(allsite_median = median(prop_pts)) %>%
  left_join(mix_trinetx %>% distinct(branch, description)) %>%
  mutate(description = ifelse(grepl('U', branch), 'Codes for special purposes', description))

write.csv(mix_final, file = 'results/COMBINED_case_mix.csv')

## Output graph
casemix_output <- ms_exp_nt2(process_output = mix_final,
                             check_string = 'Branch',
                             y_col = 'branch',
                             descriptor_col = 'description')

casemix_output

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

#############################################################

## Read in data
fot_trinetx <- read_csv('results/factsovertime_trinetx.csv')
fot_chop <- read_csv('results/factsovertime_rawcts.csv') %>%
  left_join(read_csv('results/factsovertime_normalized.csv')) %>%
  left_join(read_csv('results/factsovertime_distance.csv'))

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

fot_all <- euclidean_output(process_output = norm_pts_euc_all,
                         output_var = 'check',
                         filter_variable = check_type)

fot_all[[1]]
fot_all[[2]]
fot_all[[3]]


fot_chop <- euclidean_output(process_output = norm_pts_euc_c,
                       output_var = 'check',
                       filter_variable = check_type)

fot_chop[[1]]
fot_chop[[2]]
fot_chop[[3]]


fot_tnx <- euclidean_output(process_output = norm_pts_euc_t,
                       output_var = 'check',
                       filter_variable = check_type)

fot_tnx[[1]]
fot_tnx[[2]]
fot_tnx[[3]]
