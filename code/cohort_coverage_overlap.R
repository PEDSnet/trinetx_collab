

#' Check patient fact overlap between domains
#'
#' @param fact_tbls list of fact tables for the domains of interest. each list item should include the 
#'                  table in the first position and a string label for that patient group in the second 
#'                  position. example can be found in `coverage_overlap_execute.R`
#' @param check_string string to identify the check type
#'
#' @return one dataframe that summarises the count and proportion of patients in each overlapping group from the list of
#'         fact tables. a patient is only counted in each group ONCE.
#'         
#'         denominator used to compute the proportion is the total number of patients that have at 
#'         least one of the provided fact types (i.e. fact 1 OR fact 2 OR fact 3... etc)
#'

check_coverage_overlap <- function(fact_tbls,
                                   check_string = 'cvg'){
  
  grp_list <- list()
  
  ## Loop through fact tables to identify patient groups
  for(i in 1:length(fact_tbls)){
    
    label <- fact_tbls[[i]][[2]]
    
    tbl_meta <- fact_tbls[[i]][[1]] %>% 
      distinct(site, person_id) %>% 
      mutate(temp = label) %>%
      rename_with(~label, temp) %>%
      compute_new()
    
    grp_list[[i]] <- tbl_meta
  }
  
  grp_reduce <- purrr::reduce(.x = grp_list,
                              .f = dplyr::full_join) %>%
    collect_new()
  
  ## Site Total Counts
  
  total_site_pts <- grp_reduce %>% group_by(site) %>% 
    summarise(total_pts_site = n_distinct(person_id)) %>%
    collect_new()
  
  ## Collapse group labels per person_id
  
  ncol <- ncol(grp_reduce)
  
  grp_collapse <- grp_reduce %>% 
    #summarise(fact_group = str_c(fact_group, collapse = '_')) %>%
    mutate(fact_group = apply(grp_reduce[3:ncol], 1, function(x) paste(x[!is.na(x)], collapse = "_"))) %>%
    select(site, person_id, fact_group)
  
  ## Site Group Counts
  summary_site <- grp_collapse %>%
    group_by(site, fact_group) %>%
    summarise(n_pts_site = n()) 
  
  ## Final Summary Table
  final_tbl <- summary_site %>%
    left_join(total_site_pts) %>%
    arrange(site, fact_group) %>%
    mutate(prop_pts_site = round(as.numeric(n_pts_site) / as.numeric(total_pts_site), 2),
           total_row_site = 0,
           n_row_site = 0) #%>%
    #collect_new()
    
  return(final_tbl)
}
