

all_antihtn <- load_codeset('rx_ace_inhibitor') %>%
  union(load_codeset('rx_arb')) %>%
  union(load_codeset('rx_bb')) %>%
  union(load_codeset('rx_ccb')) %>%
  union(load_codeset('rx_diuretics_thiazides')) %>%
  union(load_codeset('rx_other_antihtn'))


# three_bp <- site_cdm_tbl('measurement_vitals') %>%
#   select(site, person_id, visit_occurrence_id, measurement_concept_id) %>%
#   filter(measurement_concept_id %in% c(3018586L,3035856L,3009395L,3004249L,
#                                        3034703L,3019962L,3013940L,3012888L)) %>%
#   distinct(site, person_id, visit_occurrence_id) %>%
#   group_by(site, person_id) %>%
#   summarise(n_bp = n()) %>% 
#   filter(n_bp >= 3) %>%
#   compute_new()



dcon_pts_list <- list(
  
  #'antihtn_rx_three_bp',
  
  'scd_dx_hydrox_rx' = list(site_cdm_tbl('condition_occurrence') %>% 
                              inner_join(load_codeset('dx_sickle_cell'), by = c('condition_concept_id' = 'concept_id')),
                            site_cdm_tbl('drug_exposure') %>%
                              inner_join(load_codeset('rx_hu_scdf'), by = c('drug_concept_id' = 'concept_id')),
                            'scd_dx_hydrox_rx'),
  
  #'ckd_dx_three_scr',
  
  'asthma_dx_broncho_rx' = list(site_cdm_tbl('condition_occurrence') %>% 
                                  inner_join(load_codeset('dx_asthma'), by = c('condition_concept_id' = 'concept_id')),
                                site_cdm_tbl('drug_exposure') %>%
                                  inner_join(load_codeset('rx_albuterol'), by = c('drug_concept_id' = 'concept_id')),
                                'asthma_dx_broncho_rx'),
  
  # 'leukemia_dx_onco_spec' = list(site_cdm_tbl('condition_occurrence') %>% 
  #                                  inner_join(load_codeset('dx_leukemia_comb'), by = c('condition_concept_id' = 'concept_id')),
  #                                site_cdm_tbl('visit_occurrence') %>% inner_join(site_cdm_tbl('provider'), by = 'provider_id') %>%
  #                                  inner_join(load_codeset(), by = c('specialty_concept_id' = 'concept_id')),
  #                                'leukemia_dx_onco_spec'),
  # 
  # 't2d_dx_metaformin_rx',
  # 
  # 'anxiety_dx_depression_dx',
  
  'edema_dx_loop_rx' = list(site_cdm_tbl('condition_occurrence') %>% 
                              inner_join(load_codeset('dx_edema_comb'), by = c('condition_concept_id' = 'concept_id')),
                            site_cdm_tbl('drug_exposure') %>%
                              inner_join(load_codeset('rx_loop_diuretic'), by = c('drug_concept_id' = 'concept_id')),
                            'edema_dx_loop_rx')
  
  #'nephsyn_dx_neph_spec',
  
  #'frac_dx_img_px'
)