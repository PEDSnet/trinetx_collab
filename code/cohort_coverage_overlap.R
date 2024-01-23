

#' Check patient fact overlap between domains
#'
#' @param fact_tbls list of fact tables for the domains of interest. each list item should include the table in the first position
#'                  and a string label for that patient group in the second position. 
#'                  example can be found in `coverage_overlap_execute.R`
#' @param check_string string to identify the check type
#'
#' @return one dataframe that summarises the count and proportion of patients in each overlapping group from the list of
#'         fact tables. a patient is only counted in each group ONCE.
#'         
#'         denominator used to compute the proportion is the total number of patients that have at least one of the 
#'         provided fact types (i.e. fact 1 OR fact 2 OR fact 3... etc)
#'

check_coverage_overlap <- function(fact_tbls,
                                   check_string = 'cvg'){
  
  grp_list <- list()
  
  for(i in 1:length(fact_tbls)){
    
    #tbl <- fact_tbls[[i]][[1]]
    
    label <- fact_tbls[[i]][[2]]
    
    tbl_meta <- fact_tbls[[i]][[1]] %>% 
      distinct(site, person_id) %>% 
      mutate(group = label) %>%
      collect_new()
    
    grp_list[[i]] <- tbl_meta
  }
  
  grp_reduce <- purrr::reduce(.x = grp_list,
                              .f = dplyr::union)
  
  total_pts <- grp_reduce %>% group_by(site) %>% summarise(denom = n_distinct(person_id))
  
  grp_collapse <- grp_reduce %>% 
    arrange(site, group) %>%
    group_by(site, person_id) %>% 
    summarise(group = str_c(group, collapse = '_')) %>%
    group_by(site, group) %>%
    summarise(n_grp = n())
  
  final_tbl <- grp_collapse %>%
    left_join(total_pts) %>%
    mutate(prop_pts = round(n_grp / denom, 2))
    
  return(final_tbl)
}
