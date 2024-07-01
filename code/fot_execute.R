

dx_anxiety <- load_codeset('mhcc_codes_v4') %>%
  filter(cluster == 'Anxiety Disorder') %>% compute_new()

op_proc <- site_cdm_tbl('visit_occurrence') %>%
  select(site, person_id, visit_occurrence_id, visit_concept_id) %>%
  filter(visit_concept_id == 9202L) %>%
  inner_join(select(site_cdm_tbl('procedure_occurrence'), site, person_id, visit_occurrence_id,
                    procedure_date)) %>%
  compute_new()

ed_vital <- site_cdm_tbl('visit_occurrence') %>%
  select(site, person_id, visit_occurrence_id, visit_concept_id) %>%
  filter(visit_concept_id == 9203L) %>%
  inner_join(select(site_cdm_tbl('measurement_vitals'), site, person_id, visit_occurrence_id,
                    measurement_date)) %>%
  compute_new()


fot_tbl_list <- list(
  
  'fot_vo' = list(site_cdm_tbl('visit_occurrence') %>%
                    filter(visit_concept_id == 9202L), 'outpatient_visits'),
  'fot_vip' = list(site_cdm_tbl('visit_occurrence') %>%
                    filter(visit_concept_id == 9201L), 'inpatient_visits'),
  'fot_ved' = list(site_cdm_tbl('visit_occurrence') %>%
                    filter(visit_concept_id == 9203L), 'emergency_visits'),
  'fot_dr_ip' = list(site_cdm_tbl('drug_exposure') %>%
                       filter(drug_type_concept_id == 38000180), 'inpatient_administration'),
  'fot_px_op' = list(op_proc, 'outpatient_procedure'),
  'fot_ed_vital' = list(ed_vital, 'emergency_visit_vitals'),
  'fot_asthma' = list(site_cdm_tbl('condition_occurrence') %>% 
                        inner_join(load_codeset('dx_asthma'), by = c('condition_concept_id' = 'concept_id')),
                      'asthma'),
  'fot_htn' = list(site_cdm_tbl('condition_occurrence') %>% 
                        inner_join(load_codeset('dx_htn_comb'), by = c('condition_concept_id' = 'concept_id')),
                      'hypertension'),
  'fot_anxiety' = list(site_cdm_tbl('condition_occurrence') %>%
                        inner_join(dx_anxiety, by = c('condition_concept_id' = 'concept_id')),
                      'anxiety'),
  'fot_resp' = list(site_cdm_tbl('condition_occurrence') %>% 
                        inner_join(load_codeset('dx_respiratory_infections'), 
                                   by = c('condition_concept_id' = 'concept_id')),
                      'respiratory_infection'),
  'fot_all_visits' = list(site_cdm_tbl('visit_occurrence'), 'all_visits'),
  'fot_asthma_ip' = list(site_cdm_tbl('condition_occurrence') %>% 
                           inner_join(load_codeset('dx_asthma'), by=c('condition_concept_id'='concept_id')) %>% 
                           inner_join(select(site_cdm_tbl('visit_occurrence'), visit_occurrence_id, visit_concept_id) %>% 
                                        filter(visit_concept_id %in% c(9201L,2000000048L,2000000088L))), 
                         'asthma_inpatient')
)








