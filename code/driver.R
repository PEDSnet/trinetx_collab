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

  
  #' `Coverage Overlap`
  
  source(paste0(base_dir, '/code/coverage_overlap_execute.R')) 
  overlap_output <- check_coverage_overlap(fact_tbls = cohort_tbls)
  
  overlap_final <- trinetx_check_pp(dat = overlap_output, group = 'fact_group') %>%
    select(site, fact_group, n_pts_site, n_pts_all, prop_pts_site, prop_pts_all,
           total_pts_site, total_pts_all)
  
    
  #output_tbl(overlap_final, 'coverage_overlap')
  
  
  #' `Case Mix`
  
  site_list <- c('chop', 'cchmc', 'colorado', 'nemours', 'nationwide', 'national',
                 'seattle', 'stanford', 'lurie', 'texas')
  casemix_list <- list()
  
  for(i in 1:length(site_list)){
    
    message(paste0('Starting ', site_list[[i]]))
    
    config('site_filter', site_list[[i]])
    
    casemix_output <- compute_case_mix(dx_tbl = site_cdm_tbl('condition_occurrence'))
    
    casemix_list[[i]] <- casemix_output
  }
  
  casemix_reduce <- purrr::reduce(.x = casemix_list,
                                  .f = dplyr::union)
  
  casemix_final <- trinetx_check_pp(dat = casemix_reduce, group = 'icd_header')
  
  #output_tbl(casemix_final, 'case_mix')
  

  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()

  message('Done.')

  invisible(rslt)

}
