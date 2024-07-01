# Vector of additional packages to load before executing the request
config_append('extra_packages', c('stringr', 'tidyr', 'purrr', 'lubridate',
                                  'plotly','ggiraph'))

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function() {

    setup_pkgs() # Load runtime packages as specified above
  
  #' `Coverage Overlap`
  
  site_map <- read_csv(paste0(base_dir, '/results/site_mapping.csv'))
  
  source(paste0(base_dir, '/code/coverage_overlap_execute.R'))
  overlap_output <- check_coverage_overlap(fact_tbls = cohort_tbls)

  output_tbl(overlap_output %>% left_join(site_map), 'coverage_overlap')
  
  #site_map <- overlap_output %>% ungroup() %>% site_anon() 
  
  #write_csv(site_map, '/results/site_mapping.csv')
  
  #' `Case Mix`
  
  site_list <- c('seattle', 'stanford', 'lurie', 'nemours', 'national',
                 'nationwide', 'chop', 'colorado', 'cchmc', 'texas')
    
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    site_nm <- site_list[i]
    
  casemix_output <- compute_case_mix(dx_tbl = cdm_tbl('condition_occurrence') %>%
                                       filter(site == site_nm) %>%
                                       filter(condition_start_date >= '2014-01-01',
                                              condition_start_date <= '2023-12-31'))
  
  output_tbl_append(casemix_output %>% left_join(site_map), 'case_mix_10yr')
  
  }
  
  ### CASE MIX SCD
  scd_codes <- load_codeset('dx_sickle_cell')
  scd_full <- list()
  
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    site_nm <- site_list[i]
    
    scd_pts <- cdm_tbl('condition_occurrence') %>% 
      filter(site == site_nm) %>% 
      inner_join(select(scd_codes,
                        concept_id),
                 by=c('condition_concept_id'='concept_id')) %>% 
      filter(condition_start_date >= '2014-01-01',
             condition_start_date <= '2023-12-31') %>%
      distinct(person_id) %>% compute_new()
    
    casemix_output <- compute_case_mix(cohort = scd_pts,
                                       dx_tbl = cdm_tbl('condition_occurrence') %>%
                                         filter(site == site_nm) %>%
                                         filter(condition_start_date >= '2014-01-01',
                                                condition_start_date <= '2023-12-31'))
    
    scd_full[[i]] <- casemix_output
    #output_tbl_append(casemix_output, 'case_mix_scd')
    
  }
  
  scd_full_reduce <- reduce(.x=scd_full,
                            .f=dplyr::union)
  
  output_tbl(scd_full_reduce %>% left_join(site_map), 'case_mix_scd')
  
  ### CASE MIX SCD DEEP DIVE
  scd_codes <- load_codeset('dx_sickle_cell')
  scd_full_p <- list()
  
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    site_nm <- site_list[i]
    
    scd_pts <- cdm_tbl('condition_occurrence') %>% 
      filter(site == site_nm) %>% 
      inner_join(select(load_codeset('dx_sickle_cell'),
                        concept_id),
                 by=c('condition_concept_id'='concept_id')) %>% 
      filter(condition_start_date >= '2014-01-01',
             condition_start_date <= '2023-12-31') %>%
      distinct(person_id) %>% compute_new()
    
    casemix_output <- compute_case_mix_p_deep_dive(cohort = scd_pts,
                                                   dx_tbl = cdm_tbl('condition_occurrence') %>%
                                                     filter(site == site_nm) %>%
                                                     filter(condition_start_date >= '2014-01-01',
                                                            condition_start_date <= '2023-12-31'))
    
    output_tbl_append(casemix_output %>% collect() %>% left_join(site_map), 'case_mix_scd_p')
    
    #scd_full_p[[i]] <- casemix_output
    #output_tbl_append(casemix_output, 'case_mix_scd')
    
  }
  
  scd_full_p_reduce <- reduce(.x=scd_full_p,
                              .f=dplyr::union)
  
  output_tbl(scd_full_p_reduce %>% left_join(site_map), 'case_mix_scd_p')
  
  ### CASE MIX METFORMIN
  metformin_codes_scdf <- load_codeset('rx_metformin_scdf')
  metformin_codes <- get_descendants(metformin_codes_scdf)
  metformin_full <- list()
  
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    config('site_filter', site_list[i])
    
    site_nm <- site_list[i]
    
    metformin_pts <- site_cdm_tbl('drug_exposure') %>% 
      #filter(site == site_nm) %>% 
      inner_join(select(metformin_codes,
                        concept_id),
                 by=c('drug_concept_id'='concept_id')) %>% 
      filter(drug_exposure_start_date >= '2014-01-01',
             drug_exposure_start_date <= '2023-12-31') %>%
      select(person_id) %>% compute_new()
    
    #metformin_pts2 <- metformin_pts %>% distinct()
    
    casemix_output <- compute_case_mix(cohort = metformin_pts,
                                       dx_tbl = site_cdm_tbl('condition_occurrence') %>%
                                         #filter(site == site_nm) %>%
                                         filter(condition_start_date >= '2014-01-01',
                                                condition_start_date <= '2023-12-31'))
    
    output_tbl_append(casemix_output %>% left_join(site_map), 'case_mix_metformin')
    
    #metformin_full[[i]] <- casemix_output
    #output_tbl_append(casemix_output, 'case_mix_scd')
    
  }
  
  metformin_full_reduce <- reduce(.x=metformin_full,
                                  .f=dplyr::union)
  
  output_tbl(metformin_full_reduce %>% left_join(site_map), 'case_mix_metformin')
  
  #' `FOT` 
  
  ## Core Function
  
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
  
  fot_additional <- list()
  
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    config('site_filter', site_list[i])
    
    site_nm <- site_list[i]
    
    fot_tbl_list_additional <- list(
      'fot_all_visits' = list(site_cdm_tbl('visit_occurrence'), 'all_visits'),
      'fot_asthma_ip' = list(site_cdm_tbl('condition_occurrence') %>% 
                               inner_join(load_codeset('dx_asthma'), by=c('condition_concept_id'='concept_id')) %>% 
                               inner_join(select(site_cdm_tbl('visit_occurrence'), visit_occurrence_id, visit_concept_id) %>% 
                                            filter(visit_concept_id %in% c(9201L,2000000048L,2000000088L))), 'asthma_inpatient')
    )
    
    fot_output <- check_fot(time_tbls = fot_tbl_list_additional,
                            time_frame = time_span,
                            visits_only = FALSE)
    
    fot_output_reduce <- reduce(.x=fot_output,
                                .f=dplyr::union)
    
    output_tbl_append(fot_output_reduce, 'fot_output_additional')
    
    fot_additional[[i]] <- fot_output_reduce %>% mutate(site=site_nm)
    
  }
  
  # fot_additional_test <- 
  #   list(fot_additional[[1]] %>%  mutate(site='seattle'),
  #        fot_additional[[2]] %>%  mutate(site='stanford'),
  #        fot_additional[[3]] %>%  mutate(site='lurie'),
  #        fot_additional[[4]] %>%  mutate(site='nemours'),
  #        fot_additional[[5]] %>%  mutate(site='national'),
  #        fot_additional[[6]] %>%  mutate(site='nationwide'),
  #        fot_additional[[7]] %>%  mutate(site='chop'),
  #        fot_additional[[8]] %>%  mutate(site='colorado'),
  #        fot_additional[[9]] %>%  mutate(site='cchmc'),
  #        fot_additional[[10]] %>%  mutate(site='texas'))
  
  fot_reduce_new <- reduce(.x=fot_additional,
                           .f=dplyr::union)
  
  fot_combined <- dplyr::union(fot_reduce,
                               fot_reduce_new) %>% left_join(site_map)
  
  output_tbl(fot_combined, 'fot_output_new')
  
 #output_tbl_append(fot_reduce, 'fot_output')

  #' `Domain Concordance`
  
  source(paste0(base_dir, '/code/dcon_execute.R'))
  
  dcon_pts <- check_dcon(conc_tbls = dcon_pts_list,
                         check_string = 'dcon_pts')
  
  dcon_reduce <- reduce(.x = dcon_pts,
                        .f = dplyr::union) %>% left_join(site_map)
  
  output_tbl(dcon_reduce, 'dcon_output')
  
  #' `Domain Concordance -- Conservative`
  
  # source(paste0(base_dir, '/code/dcon_execute.R'))
  # 
  # dcon_pts_cons <- check_dcon_cons(conc_tbls = dcon_pts_list,
  #                             check_string = 'dcon_pts')
  # 
  # dcon_reduce_cons <- reduce(.x = dcon_pts,
  #                            .f = dplyr::union)
  # 
  # output_tbl(dcon_reduce_cons, 'dcon_output_cons')
  
  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()

  message('Done.')

  invisible(rslt)

}
