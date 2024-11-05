
##' Assemble and filter codesets

dx_anxiety <- load_codeset('mhcc_codes_v4') %>%
  filter(cluster == 'Anxiety Disorder') %>% compute_new()

dx_depression <- load_codeset('mhcc_codes_v4') %>%
  filter(cluster == 'Major Depression' | cluster == 'Minor Depression') %>% compute_new()

##' Build list of inputs for DCON function
##' 
##' Each element of the parent list should be a list, named after the check being applied,
##' with the following sub-elements:
##' 
##' 1. The table for cohort 1; should minimally contain a person_id and date column
##' 2. The table for cohort 2; should minimally contain a person_id and date column
##' 3. A string identifier for the check; can be the same as the name of the list element
##' 4. An integer indicating how many *days* should separate the two events of interest at a maximum
##' 

dcon_pts_list <- list(
   
  'scd_dx_hydrox_rx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                              inner_join(load_codeset('dx_sickle_cell'), by = c('condition_concept_id' = 'concept_id')) ,
                            cdm_tbl('drug_exposure') %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                              inner_join(load_codeset('rx_hydroxyurea'), by = c('drug_concept_id' = 'concept_id')) ,
                            'scd_dx_hydrox_rx',
                            730.5),
   
  'asthma_dx_broncho_rx' = list(cdm_tbl('condition_occurrence') %>%
                                  select(site, person_id, condition_concept_id, condition_start_date) %>%
                                  inner_join(load_codeset('dx_asthma'), by = c('condition_concept_id' = 'concept_id')) ,
                                cdm_tbl('drug_exposure') %>%
                                  select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                                  inner_join(load_codeset('rx_albuterol'), by = c('drug_concept_id' = 'concept_id')) ,
                                'asthma_dx_broncho_rx',
                                730.5),

  't2d_dx_metformin_rx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                  inner_join(load_codeset('T2D_SNOMED_codes'), by = c('condition_concept_id' = 'concept_id')),
                                cdm_tbl('drug_exposure') %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                                  inner_join(load_codeset('metformin'), by = c('drug_concept_id' = 'concept_id')),
                                't2d_dx_metformin_rx',
                               730.5),

  'anxiety_dx_depression_dx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                      inner_join(dx_anxiety, by = c('condition_concept_id' = 'concept_id')),
                                    cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                                      inner_join(dx_depression, by = c('condition_concept_id' = 'concept_id')),
                                    'anxiety_dx_depression_dx',
                                    730.5),

  'edema_dx_loop_rx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                              inner_join(load_codeset('dx_edema_comb'), by = c('condition_concept_id' = 'concept_id')),
                            cdm_tbl('drug_exposure') %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                              inner_join(load_codeset('rx_loop_diuretic'), by = c('drug_concept_id' = 'concept_id')),
                            'edema_dx_loop_rx',
                            90),

  'frac_dx_img_px' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                            inner_join(load_codeset('dx_fracture'), by = c('condition_concept_id' = 'concept_id')),
                          cdm_tbl('procedure_occurrence') %>% select(site, person_id, procedure_concept_id, procedure_date) %>%
                            inner_join(load_codeset('px_radiologic'), by = c('procedure_concept_id' = 'concept_id')),
                          'frac_dx_img_px',
                          30),
  
  't1d_dx_insulin_rx' = list(cdm_tbl('condition_occurrence') %>% select(site, person_id, condition_concept_id, condition_start_date) %>%
                               inner_join(load_codeset('T1D_SNOMED_codes'), by = c('condition_concept_id' = 'concept_id')),
                             cdm_tbl('drug_exposure') %>% select(site, person_id, drug_concept_id, drug_exposure_start_date) %>%
                               inner_join(load_codeset('insulin'), by = c('drug_concept_id' = 'concept_id')),
                             't1d_dx_insulin_rx',
                             730.5)
)