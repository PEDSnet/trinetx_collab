# Vector of additional packages to load before executing the request
config_append('extra_packages', c('stringr', 'tidyr', 'purrr', 'lubridate'))

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
  
  source(paste0(base_dir, '/code/coverage_overlap_execute.R'))
  overlap_output <- check_coverage_overlap(fact_tbls = cohort_tbls)

  output_tbl(overlap_output, 'coverage_overlap')
  
  #' `Case Mix`
  
  site_list <- c('seattle', 'stanford', 'lurie', 'nemours', 'national',
                 'nationwide', 'chop', 'colorado', 'cchmc', 'texas')
    
  for(i in 1:length(site_list)){
    
    message('Starting ', site_list[i])
    
    site_nm <- site_list[i]
    
  casemix_output <- compute_case_mix(dx_tbl = cdm_tbl('condition_occurrence') %>%
                                       filter(site == site_nm))
  
  output_tbl_append(casemix_output, 'case_mix_full')
  
  }
  
  
  #' `FOT` 
  
  ## Core Function
    
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

  #' `Domain Concordance`
  
  source(paste0(base_dir, '/code/dcon_execute.R'))
  
  dcon_pts <- check_dcon(conc_tbls = dcon_pts_list,
                         check_string = 'dcon_pts')
  
  dcon_reduce <- reduce(.x = dcon_pts,
                        .f = dplyr::union)
  
  output_tbl(dcon_reduce, 'dcon_output')
  
  #' `Domain Concordance -- Conservative`
  
  source(paste0(base_dir, '/code/dcon_execute.R'))
  
  dcon_pts_cons <- check_dcon_cons(conc_tbls = dcon_pts_list,
                              check_string = 'dcon_pts')
  
  dcon_reduce_cons <- reduce(.x = dcon_pts,
                        .f = dplyr::union)
  
  output_tbl(dcon_reduce_cons, 'dcon_output_cons')
  
  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()

  message('Done.')

  invisible(rslt)

}
