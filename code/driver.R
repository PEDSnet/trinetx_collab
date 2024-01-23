# Vector of additional packages to load before executing the request
config_append('extra_packages', c('stringr', 'tidyr'))

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


  site_list <- c('chop', 'colorado', 'cchmc', 'seattle', 'stanford', 'nemours',
                 'nationwide', 'lurie')
  
  ## Coverage Overlap
  
  for(i in 1:length(site_list)){
    
    message(paste0('Starting ', site_list[[i]]))
    
    config('site_filter', site_list[[i]])
    
    output <- check_coverage_overlap(fact_tbls = cohort_tbls)
    
    #output_tbl_append(output, 'coverage_overlap')
  }
  
  ## Case Mix
  
  for(i in 1:length(site_list)){
    
    message(paste0('Starting ', site_list[[i]]))
    
    config('site_filter', site_list[[i]])
    
    output <- compute_case_mix(dx_tbl = site_cdm_tbl('condition_occurrence'))
    
    #output_tbl_append(output, 'case_mix')
    
  }
  
  

  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()

  message('Done.')

  invisible(rslt)

}
