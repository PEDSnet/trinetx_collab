
#' Compute domain concordance between 2 cohorts
#'
#' @param conc_tbls list of inputs from `dcon_execute.R` with each element containing 
#'                  the following:
#'                  list name: the description of the concordant domains
#'                  first element: table with at least person_id OR visit_occurrence_id
#'                                 that represents all members of the first cohort
#'                  second element: table with at least person_id OR visit_occurrence_id
#'                                 that represents all members of the second cohort
#'                  third element: the check name/application
#' @param check_string a string that denotes the level at which the analysis should
#'                     take place 
#'                     
#'                     if it is `dcon_visits`, the analysis will take place
#'                     at the visit level; otherwise it will take place at the
#'                     person level
#'
#' @return one dataframe with counts for the patients/visits in the first cohort, 
#'         the patients/visits in the second cohort, and the patients/visits in both
#'         
#'         contains the columns: value, cohort, yr (set to 9999), check_type, 
#'                               database_version, site, check_name, check_desc 
#'         
#' 
check_dcon<- function(conc_tbls,
                      check_string='dcon_visits'){
  
  
  
  final <- list()
  
  for(k in 1:length(conc_tbls)) {
    
    c1_date <- colnames(conc_tbls[[k]][[1]]) %>% str_subset(pattern = 'date')
    c2_date <- colnames(conc_tbls[[k]][[2]]) %>% str_subset(pattern = 'date')
    
    cohort_1 <- conc_tbls[[k]][[1]] %>%
      filter(!!sym(c1_date) >= '2014-01-01' & !!sym(c1_date) <= '2023-12-31') %>%
      mutate(date1 = !!sym(c1_date))
    cohort_2 <- conc_tbls[[k]][[2]] %>%
      filter(!!sym(c2_date) >= '2014-01-01' & !!sym(c2_date) <= '2023-12-31') %>%
      mutate(date2 = !!sym(c2_date))
    
    
    if(check_string=='dcon_visits'){
      col_nm <- sym('visit_occurrence_id')
    } else{col_nm <- sym('person_id')}
    
    if(conc_tbls[[k]][[3]] == 'edema_dx_loop_rx'){
      
      combined <- 
        cohort_1 %>% select(site, all_of(col_nm), date1) %>% 
        inner_join(
          select(cohort_2, site, all_of(col_nm), date2)
        ) %>%
        mutate(date_diff = abs((date1 - date2))) %>%
        filter(date_diff <= 90)
      
    }else if(conc_tbls[[k]][[3]] == 'frac_dx_img_px'){
      
      combined <- 
        cohort_1 %>% select(site, all_of(col_nm), date1) %>% 
        inner_join(
          select(cohort_2, site, all_of(col_nm), date2)
        ) %>%
        mutate(date_diff = abs((date1 - date2))) %>%
        filter(date_diff <= 30)
      
    }else{
      combined <- 
        cohort_1 %>% select(site, all_of(col_nm), date1) %>% 
        inner_join(
          select(cohort_2, site, all_of(col_nm), date2)
        ) %>%
        mutate(date_diff = abs((date1 - date2)/365.25)) %>%
        filter(date_diff <= 2) #%>% compute_new()
    }
    
    cohort_list <- list('cohort_1' = cohort_1,
                        'cohort_2' = cohort_2,
                        'combined' = combined)
    cohort_list_cts <- list()
    
    for(i in 1:length(cohort_list)) {
      
      string_nm <- names(cohort_list[i])
      
      final_cts <- cohort_list[[i]] %>% 
        group_by(site) %>%
        summarise(value=n_distinct(col_nm)) %>% 
        collect() %>% 
        mutate(cohort = string_nm)
      
      cohort_list_cts[[i]] <- final_cts
      
    }
    
    final_tbls <- 
      reduce(.x=cohort_list_cts,
             .f=dplyr::union) %>% 
      #mutate(yr=9999) %>% 
      # add_meta(check_lib = check_string) %>%
      mutate(check_name=conc_tbls[[k]][[3]],
             check_type = check_string) %>%
      mutate(check_desc=names(conc_tbls[k]))
    
    final[[k]] <- final_tbls
    
  }
  
  final
  
}

#' Function to add a proportion column
#'     when counts are computed for each domain and for combined
#'     and to add total as a site summarizing cohorts from all sites
#' @param dcon_tbl output from the dcon check, expected to have the cols:
#'     check_name, check_desc, site, check_type, database_version, yr, cohort, value
#' @param byyr boolean indicator of whether the dcon_tbl output is by year or not
#' @return dcon_tbl with additional columns with totals and proportions for the checks

apply_dcon_pp <- function(dcon_tbl,
                          byyr,
                          strict){
  dcon_tbl <- collect_new(dcon_tbl)
  if(byyr){
    dcon_overall <- dcon_tbl %>%
      group_by(check_type, #database_version, 
               check_name, check_desc, yr, cohort)%>%
      summarise(value_pts=sum(value_pts,na.rm=TRUE),
                value_visits=sum(value_visits, na.rm=TRUE))%>%
      ungroup()%>%
      mutate(site='total')
    
    dcon_tbl_pp<- bind_rows(dcon_tbl, dcon_overall) %>%
      group_by(site, yr, check_name, check_type, check_desc) %>%
      mutate(tot_pats=sum(value_pts, na.rm = TRUE),
             tot_vis=sum(value_visits, na.rm=TRUE)) %>%
      ungroup()%>%
      mutate(pats_prop=value_pts/tot_pats,
             visits_prop=value_visits/tot_vis)
  }else{
    dcon_overall <- dcon_tbl %>%
      group_by(check_type, #database_version, 
               check_name, check_desc, cohort) %>%
      summarise(value=sum(value,na.rm=TRUE))%>%
      ungroup()%>%
      mutate(site='total')
    
    if(!strict){
    dcon_tbl_pp<-dcon_tbl %>%
      bind_rows(dcon_overall) %>%
      pivot_wider(values_from = value,
                  names_from=cohort)%>%
      mutate(tot_pats=cohort_1+cohort_2-combined,
             cohort_1_only=cohort_1-combined,
             cohort_2_only=cohort_2-combined)%>%
      pivot_longer(cols=c(cohort_1_only, cohort_2_only, combined),
                   names_to="cohort",
                   values_to="value")%>%
      mutate(prop=value/tot_pats)%>%
      select(-c(cohort_1, cohort_2))%>%
      distinct()
    }else{
      dcon_tbl_pp<-dcon_tbl %>%
        bind_rows(dcon_overall) %>%
        pivot_wider(values_from = value,
                    names_from=cohort)%>%
        mutate(tot_pats=cohort_1+cohort_2+combined,
               cohort_1_only=cohort_1,
               cohort_2_only=cohort_2)%>%
        pivot_longer(cols=c(cohort_1_only, cohort_2_only, combined),
                     names_to="cohort",
                     values_to="value")%>%
        mutate(prop=value/tot_pats)%>%
        select(-c(cohort_1, cohort_2))%>%
        distinct()
    }
  }
  return(dcon_tbl_pp)
}
