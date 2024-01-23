# Vector of additional packages to load before executing the request
config_append('extra_packages', c('stringr', 'tidyr', 'purrr'))

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
  
  site_list <- c('chop', 'cchmc', 'colorado', 'nemours', 'nationwide', 'national',
                 'seattle', 'stanford', 'lurie', 'texas')
  
  ## Coverage Overlap
    
  overlap_output <- check_coverage_overlap(fact_tbls = cohort_tbls)
    
  #output_tbl(overlap_output, 'coverage_overlap')
  
  ## Case Mix
  
  casemix_list <- list()
  
  for(i in 1:length(site_list)){
    
    message(paste0('Starting ', site_list[[i]]))
    
    config('site_filter', site_list[[i]])
    
    casemix_output <- compute_case_mix(dx_tbl = site_cdm_tbl('condition_occurrence'))
    
    casemix_list[[i]] <- casemix_output
  }
  
  casemix_reduce <- purrr:reduce(.x = casemix_list,
                                 .f = dplyr::union)
  
  casemix_final <- casemix_reduce %>% casemix_pp()
  
  #output_tbl(casemix_final, 'case_mix')
  

  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()

  message('Done.')

  invisible(rslt)

}
