
## Assemble and filter codesets

all_antihtn <- load_codeset('rx_ace_inhibitor') %>%
  union(load_codeset('rx_arb')) %>%
  union(load_codeset('rx_bb')) %>%
  union(load_codeset('rx_ccb')) %>%
  union(load_codeset('rx_diuretics_thiazides')) %>%
  union(load_codeset('rx_other_antihtn'))

nephrotic_syndrome <- load_codeset('dx_glomerular_snomed') %>%
  filter(codeset_category_name == 'nephrotic') %>% compute_new()

dx_anxiety <- load_codeset('mhcc_codes_v4') %>%
  filter(cluster == 'Anxiety Disorder') %>% compute_new()

dx_depression <- load_codeset('mhcc_codes_v4') %>%
  filter(cluster == 'Major Depression' | cluster == 'Minor Depression') %>% compute_new()

## Count measurements

three_bp <- site_cdm_tbl('measurement_vitals') %>%
  select(site, person_id, visit_occurrence_id, measurement_concept_id) %>%
  filter(measurement_concept_id %in% c(3018586L,3035856L,3009395L,3004249L)) %>%
  # distinct(site, person_id, visit_occurrence_id) %>%
  group_by(site, person_id) %>%
  mutate(n_bp = n()) %>%
  ungroup() %>%
  filter(n_bp >= 3) %>%
  compute_new()

three_scr <- site_cdm_tbl('measurement_labs') %>%
  select(site, person_id, visit_occurrence_id, measurement_concept_id) %>%
  inner_join(load_codeset('lab_serum_creatinine'), by = c('measurement_concept_id' = 'concept_id')) %>%
  #distinct(site, person_id, visit_occurrence_id) %>%
  group_by(site, person_id) %>%
  mutate(n_scr = n()) %>%
  ungroup() %>%
  filter(n_scr >= 3) %>%
  compute_new()

dcon_pts_list <- list(
  
  'antihtn_rx_three_bp' = list(site_cdm_tbl('drug_exposure') %>%
                                 inner_join(all_antihtn, by = c('drug_concept_id' = 'concept_id')),
                               three_bp,
                               'antihtn_rx_three_bp'),
  
  'scd_dx_hydrox_rx' = list(site_cdm_tbl('condition_occurrence') %>% 
                              inner_join(load_codeset('dx_sickle_cell'), by = c('condition_concept_id' = 'concept_id')),
                            site_cdm_tbl('drug_exposure') %>%
                              inner_join(load_codeset('rx_hydroxyurea'), by = c('drug_concept_id' = 'concept_id')),
                            'scd_dx_hydrox_rx'),
  
  'ckd_dx_three_scr' = list(site_cdm_tbl('condition_occurrence') %>%
                              inner_join(load_codeset('dx_ckd_allstages_comb'), by = c('condition_concept_id' = 'concept_id')),
                            three_scr,
                            'ckd_dx_three_scr'),
  
  'asthma_dx_broncho_rx' = list(site_cdm_tbl('condition_occurrence') %>% 
                                  inner_join(load_codeset('dx_asthma'), by = c('condition_concept_id' = 'concept_id')),
                                site_cdm_tbl('drug_exposure') %>%
                                  inner_join(load_codeset('rx_albuterol'), by = c('drug_concept_id' = 'concept_id')),
                                'asthma_dx_broncho_rx'),
  
  'leukemia_dx_onco_spec' = list(site_cdm_tbl('condition_occurrence') %>%
                                   inner_join(load_codeset('dx_leukemia_comb'), by = c('condition_concept_id' = 'concept_id')),
                                 site_cdm_tbl('visit_occurrence') %>% inner_join(site_cdm_tbl('provider'), by = 'provider_id') %>%
                                   inner_join(load_codeset('oncology'), by = c('specialty_concept_id' = 'concept_id')),
                                 'leukemia_dx_onco_spec'),
   
  't2d_dx_metformin_rx' = list(site_cdm_tbl('condition_occurrence') %>%
                                  inner_join(load_codeset('T2D_SNOMED_codes'), by = c('condition_concept_id' = 'concept_id')),
                                site_cdm_tbl('drug_exposure') %>%
                                  inner_join(load_codeset('metformin'), by = c('drug_concept_id' = 'concept_id')),
                                't2d_dx_metformin_rx'),

  'anxiety_dx_depression_dx' = list(site_cdm_tbl('condition_occurrence') %>%
                                      inner_join(dx_anxiety, by = c('condition_concept_id' = 'concept_id')),
                                    site_cdm_tbl('condition_occurrence') %>%
                                      inner_join(dx_depression, by = c('condition_concept_id' = 'concept_id')),
                                    'anxiety_dx_depression_dx'),
  
  'edema_dx_loop_rx' = list(site_cdm_tbl('condition_occurrence') %>% 
                              inner_join(load_codeset('dx_edema_comb'), by = c('condition_concept_id' = 'concept_id')),
                            site_cdm_tbl('drug_exposure') %>%
                              inner_join(load_codeset('rx_loop_diuretic'), by = c('drug_concept_id' = 'concept_id')),
                            'edema_dx_loop_rx'),
  
  'nephsyn_dx_neph_spec' = list(site_cdm_tbl('condition_occurrence') %>%
                                  inner_join(nephrotic_syndrome, by = c('condition_concept_id' = 'concept_id')),
                                site_cdm_tbl('visit_occurrence') %>% inner_join(site_cdm_tbl('provider'), by = 'provider_id') %>%
                                  inner_join(load_codeset('nephrology'), by = c('specialty_concept_id' = 'concept_id')),
                                'nephsyn_dx_neph_spec'),

  'frac_dx_img_px' = list(site_cdm_tbl('condition_occurrence') %>% 
                            inner_join(load_codeset('dx_fracture'), by = c('condition_concept_id' = 'concept_id')),
                          site_cdm_tbl('procedure_occurrence') %>%
                            inner_join(load_codeset('px_radiologic'), by = c('procedure_concept_id' = 'concept_id')),
                          'frac_dx_img_px')
)