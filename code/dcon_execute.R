
## Assemble and filter codesets

# nephrotic_syndrome <- load_codeset('dx_glomerular_snomed') %>%
#   filter(codeset_category_name == 'nephrotic') %>% compute_new()

dx_anxiety <- load_codeset('mhcc_codes_v4') %>%
  filter(cluster == 'Anxiety Disorder') %>% compute_new()

dx_depression <- load_codeset('mhcc_codes_v4') %>%
  filter(cluster == 'Major Depression' | cluster == 'Minor Depression') %>% compute_new()

asthma <- cdm_tbl('condition_occurrence') %>%
  select(site, person_id, condition_concept_id, condition_start_date) %>%
  inner_join(load_codeset('dx_asthma'), by = c('condition_concept_id' = 'concept_id')) %>%
  filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
  rename('date1' = condition_start_date)

albuterol <- cdm_tbl('drug_exposure') %>%
  select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
  inner_join(load_codeset('rx_albuterol'), by = c('drug_concept_id' = 'concept_id')) %>%
  filter(drug_exposure_start_date >= '2014-01-01' & drug_exposure_start_date <= '2023-12-31') %>%
  rename('date2' = drug_exposure_start_date) 

## primary list

dcon_pts_list <- list(
   
  'scd_dx_hydrox_rx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                              inner_join(load_codeset('dx_sickle_cell'), by = c('condition_concept_id' = 'concept_id')) %>%
                              filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
                              rename('date1' = condition_start_date),
                            cdm_tbl('drug_exposure') %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                              inner_join(load_codeset('rx_hydroxyurea'), by = c('drug_concept_id' = 'concept_id')) %>%
                              filter(drug_exposure_start_date >= '2014-01-01' & drug_exposure_start_date <= '2023-12-31') %>%
                              rename('date2' = drug_exposure_start_date),
                            'scd_dx_hydrox_rx'),
   
  'asthma_dx_broncho_rx' = list(asthma,
                                albuterol,
                                'asthma_dx_broncho_rx'),
  
  # 'leukemia_dx_onco_spec' = list(site_cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
  #                                  inner_join(load_codeset('dx_leukemia_comb'), by = c('condition_concept_id' = 'concept_id')) %>%
  #                                  filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
  #                                  rename('date1' = condition_start_date),
  #                                site_cdm_tbl('visit_occurrence') %>% inner_join(site_cdm_tbl('provider'), by = 'provider_id') %>%
  #                                  inner_join(load_codeset('oncology_edit'), by = c('specialty_concept_id' = 'concept_id')) %>%
  #                                  filter(visit_start_date >= '2014-01-01' & visit_start_date <= '2023-12-31') %>%
  #                                  rename('date2' = visit_start_date),
  #                                'leukemia_dx_onco_spec'),

  't2d_dx_metformin_rx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                  inner_join(load_codeset('T2D_SNOMED_codes'), by = c('condition_concept_id' = 'concept_id')) %>%
                                 filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
                                 rename('date1' = condition_start_date),
                                cdm_tbl('drug_exposure') %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                                  inner_join(load_codeset('metformin'), by = c('drug_concept_id' = 'concept_id')) %>%
                                 filter(drug_exposure_start_date >= '2014-01-01' & drug_exposure_start_date <= '2023-12-31') %>%
                                 rename('date2' = drug_exposure_start_date),
                                't2d_dx_metformin_rx'),

  'anxiety_dx_depression_dx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                      inner_join(dx_anxiety, by = c('condition_concept_id' = 'concept_id')) %>%
                                      filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
                                      rename('date1' = condition_start_date),
                                    cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                      inner_join(dx_depression, by = c('condition_concept_id' = 'concept_id')) %>%
                                      filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
                                      rename('date2' = condition_start_date),
                                    'anxiety_dx_depression_dx'),

  'edema_dx_loop_rx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                              inner_join(load_codeset('dx_edema_comb'), by = c('condition_concept_id' = 'concept_id')) %>%
                              filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
                              rename('date1' = condition_start_date),
                            cdm_tbl('drug_exposure') %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                              inner_join(load_codeset('rx_loop_diuretic'), by = c('drug_concept_id' = 'concept_id')) %>%
                              filter(drug_exposure_start_date >= '2014-01-01' & drug_exposure_start_date <= '2023-12-31') %>%
                              rename('date2' = drug_exposure_start_date),
                            'edema_dx_loop_rx'),

  # 'nephsyn_dx_neph_spec' = list(site_cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
  #                                 inner_join(nephrotic_syndrome, by = c('condition_concept_id' = 'concept_id')) %>%
  #                                 filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
  #                                 rename('date1' = condition_start_date),
  #                               site_cdm_tbl('visit_occurrence') %>% inner_join(site_cdm_tbl('provider'), by = 'provider_id') %>%
  #                                 inner_join(load_codeset('nephrology'), by = c('specialty_concept_id' = 'concept_id')) %>%
  #                                 filter(visit_start_date >= '2014-01-01' & visit_start_date <= '2023-12-31') %>%
  #                                 rename('date2' = visit_start_date),
  #                               'nephsyn_dx_neph_spec'),

  'frac_dx_img_px' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                            inner_join(load_codeset('dx_fracture'), by = c('condition_concept_id' = 'concept_id')) %>%
                            filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
                            rename('date1' = condition_start_date),
                          cdm_tbl('procedure_occurrence') %>% select(site, person_id, procedure_concept_id, procedure_date) %>%
                            inner_join(load_codeset('px_radiologic'), by = c('procedure_concept_id' = 'concept_id')) %>%
                            filter(procedure_date >= '2014-01-01' & procedure_date <= '2023-12-31') %>%
                            rename('date2' = procedure_date),
                          'frac_dx_img_px'),
  
  't1d_dx_insulin_rx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                               inner_join(load_codeset('T1D_SNOMED_codes'), by = c('condition_concept_id' = 'concept_id')) %>%
                               filter(condition_start_date >= '2014-01-01' & condition_start_date <= '2023-12-31') %>%
                               rename('date1' = condition_start_date),
                             cdm_tbl('drug_exposure') %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                               inner_join(load_codeset('insulin'), by = c('drug_concept_id' = 'concept_id')) %>%
                               filter(drug_exposure_start_date >= '2014-01-01' & drug_exposure_start_date <= '2023-12-31') %>%
                               rename('date2' = drug_exposure_start_date),
                             't1d_dx_insulin_rx')
)