
#' Libraries used

library(dplyr)
library(ggplot2)
library(hotspots)
library(readr)
library(stringr)
library(tidyr)
library(ggiraph)

#' Set working directory to point to files

setwd("/Users/razzaghih/one_offs/trinetx")

#' Name of Files
#' 
couplets_chop <- read_csv('results/couplets_chop.csv')
couplets_trinetx <- read_csv('results/couplets_trinetx.csv')

#' Trinetx cleaning to be more standardized
trinetx_cleaned_structure <- 
  couplets_trinetx %>% 
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
  mutate(cohort_1_denom_prop=round(combined/cohort_1, 2),
         cohort_2_denom_prop=round(combined/cohort_2,2)) %>% 
  pivot_longer(cols=c('cohort_1','cohort_1_only',
                      'combined', 'cohort_2_only',
                      'cohort_2', 'cohort_1_denom_prop', 'cohort_2_denom_prop'),
               names_to = 'cohort',
               values_to = 'value') %>% 
  mutate(prop=
           case_when(value >= 1.001 ~ round(value/tot_pats, 2),
                     TRUE ~ value)) 

#' Trinetx fix cohort order eof procedure and fracture
#'
trinetx_cleaned <- 
  trinetx_cleaned_structure %>% 
  mutate(cohort = 
           case_when(couplet_name == 'Imaging procedure + fracture' & cohort == 'cohort_1' ~ 'cohort_2',
                     couplet_name == 'Imaging procedure + fracture' & cohort == 'cohort_1_only' ~ 'cohort_2_only',
                     couplet_name == 'Imaging procedure + fracture' & cohort == 'cohort_2' ~ 'cohort_1',
                     couplet_name == 'Imaging procedure + fracture' & cohort == 'cohort_2_only' ~ 'cohort_1_only',
                     TRUE ~ cohort)) %>% 
  mutate(couplet_name = 
           case_when(couplet_name == 'Imaging procedure + fracture' ~ 'Fracture + imaging procedure',
                     TRUE ~ couplet_name))

#' Using Trinetx check descriptions
check_desc_lookup <- 
  trinetx_cleaned %>% select(couplet_name,
                             check_desc) %>% distinct()

#' Cleaning CHOP data
chop_cleaned <- 
  couplets_chop %>% 
  select(-c(prop, sitenum)) %>% 
  pivot_wider(names_from = 'cohort',
              values_from = 'value') %>% 
  mutate(cohort_1 = cohort_1_only + combined,
         cohort_2 = cohort_2_only + combined) %>% 
  mutate(cohort_1_denom_prop=round(combined/cohort_1, 2),
         cohort_2_denom_prop=round(combined/cohort_2,2)) %>% 
  pivot_longer(cols=cohort_1_only:cohort_2_denom_prop,
               names_to='cohort',
               values_to='value') %>% 
  mutate(prop=
           case_when(value >= 1.001 ~ round(value/tot_pats, 2),
                     TRUE ~ value)) %>% 
  inner_join(check_desc_lookup) %>% 
  select(-c('check_name','check_type', 'site'))

#' Combined CHOP and Trinetx data
couplets_combined <- dplyr::union(trinetx_cleaned,
                                  chop_cleaned)

write.csv(couplets_combined, file = 'results/COMBINED_couplets.csv')

#' Cohort Overlap of Both Cohorts
couplets_prop_combined <- 
  ms_exp_nt(couplets_combined %>% filter(cohort=='combined') %>% 
              filter(! str_detect(site_anon,'all|ALL')), 'prop') +
  ggtitle('Proportion of Combined Over Total in Both')

#' Proportion of Patients in Cohort 2 who are in Cohort 1
couplets_prop_cohort_1 <- 
  ms_exp_nt(couplets_combined %>% filter(cohort=='cohort_1_denom_prop') %>% 
              filter(! str_detect(site_anon,'all|ALL')), 'prop') +
  ggtitle('Proportion of Cohort 2 in Cohort 1')


#' Setting up for Anomaly Detection
couplet_anomaly_cohort_1 <- compute_dist_anomalies(df_tbl=couplets_combined %>% filter(cohort=='cohort_1_denom_prop') %>% 
                                                      filter(! str_detect(site_anon,'all|ALL')) %>% 
                                                      select(site_anon,couplet_name,check_desc,prop) %>% 
                                                      rename(site=site_anon), 
                                                  grp_vars=c('couplet_name','check_desc'),
                                                  var_col='prop')

#' Detection of outliers
couplet_anomaly_final_cohort_1 <- detect_outliers(couplet_anomaly_cohort_1,
                                                  column_analysis='prop',
                                                  column_variable = 'couplet_name')

write.csv(couplet_anomaly_final_cohort_1, file = 'results/COMBINED_couplets_anom.csv')

#' Visualization  
couplet_anomaly_plot_trinetx <- ms_anom_nt(process_output=couplet_anomaly_final_cohort_1 %>% 
                                             filter(site %in% c('HCO1','HCO2','HCO3',
                                                                'HCO4','HCO5','HCO6',
                                                                'HCO7','HCO8','HCO9',
                                                                'HCO10','HCO11','HCO12','HCO13')),comparison_col='prop',variable='couplet_name')

