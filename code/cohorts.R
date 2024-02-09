#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.
#'


#' Add site to the cdm_tbl
#' 
#' @param name the name of the table, as a string
#' @param site_filter the name of the site to filter by, if 
#' filtering by a site; defaults to `config('site_filter')`.
#' If pointing to a site_specific schema and no filter is needed,
#' `config('site_filter')` should be set to NA
#' 
#' @return the cdm_tbl name with site as a grouper
#' 

site_cdm_tbl <- function(name,
                         site_filter = config('site_filter'),
                         ...) {
  
  my_tbl <- cdm_tbl(name, ...)
  
  if (!is.na(site_filter)) {
    my_tbl_new <- filter(my_tbl,site == site_filter)
  } else {my_tbl_new <- my_tbl}
  
  my_tbl_new
  
}

#' output table to database if it does not exist, or
#' append it to an existing table with the same name if it does
#' 
#' @param data the data to output
#' @param name the name of the table to output 
#' 
#' Parameters are the same as `output_tbl`
#' 
#' @return The table as it exists on the databse, with the new data
#' appended, if the table already existts.
#' 

output_tbl_append <- function(data, name = NA, local = FALSE,
                              file = ifelse(config('execution_mode') !=
                                              'development', TRUE, FALSE),
                              db = ifelse(config('execution_mode') !=
                                            'distribution', TRUE, FALSE),
                              results_tag = TRUE, ...) {
  
  if (is.na(name)) name <- quo_name(enquo(data))
  
  if(db_exists_table(config('db_src'),intermed_name(name,temporary=FALSE))) {
    
    tmp <- results_tbl(name) %>% collect_new 
    new_tbl <- 
      dplyr::union(tmp,
                   data)
    output_tbl(data=new_tbl,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  } else {
    output_tbl(data=data,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  }
  
  
}


#' Post-processing of case-mix output
#'
#' @param dat table output by `compute_case_mix` or `check_coverage_overlap`
#' @param group grouping variable for group-specific counts
#'              `icd_header` for case mix results or `fact_group` for coverage overlap results
#'
#' @return one dataframe with additional columns that compute all-site counts & proportions to
#'         supplement site specific counts and proportions
#' 

trinetx_check_pp <- function(dat,
                             group){
  
  all_site_total <- dat %>%
    ungroup() %>%
    distinct(site, total_row_site, total_pts_site) %>%
    mutate(total_row_all = sum(total_row_site),
           total_pts_all = sum(total_pts_site))
  
  all_site_grp <- dat %>%
    group_by(!!sym(group)) %>%
    mutate(n_row_all = sum(n_row_site),
           n_pts_all = sum(n_pts_site))
  
  all_site_final <- all_site_total %>%
    left_join(all_site_grp) %>%
    mutate(prop_row_all = round(as.numeric(n_row_all) / as.numeric(total_row_all), 2),
           prop_pts_all = round(as.numeric(n_pts_all) / as.numeric(total_pts_all), 2))
  
  return(all_site_final)
}


#' output a list of tables to the database
#'
#' @param output_list list of tables to output
#' @param append logical to determine if you want to append if the table exists
#'
#' @return tables output to the database; if
#' table already exists, it will be appended
#'

output_list_to_db <- function(output_list,
                              append=TRUE) {
  
  
  if(append) {
    
    for(i in 1:length(output_list)) {
      
      output_tbl_append(data=output_list[[i]],
                        name=names(output_list[i]))
      
    }
    
  } else {
    
    for(i in 1:length(output_list)) {
      
      output_tbl(data=output_list[[i]],
                 name=names(output_list[i]))
      
    }
    
  }
  
}


#' add check name, db version, and site name to a given table
#' 
#' @param tbl_meta the table to add meta information to 
#' @param check_lib the name of the check
#' @param version the version of the database; defaults to 
#' `config('current_version')`;
#' @param site_nm the name of the site; defaults to
#' `config('site')`
#' 

add_meta <- function(tbl_meta,
                     check_lib,
                     version=config('current_version'),
                     site_nm=config('site_filter')) {
  
  tbl_meta %>%
    mutate(check_type = check_lib,
           database_version=version,
           site=site_nm) 
  
  
}

#' Site Anonymization
#'
#' @param df data frame with a site column that contains every site
#'           that needs to be anonymized
#'
#' @return a data frame with the site's original name, its anonymized name, and
#'         the number associated with the anonymized name
#' 
site_anon <- function(df){
  
  distinct_sites <- df %>%
    distinct(site) %>% collect()
  site_nums <- distinct_sites[sample(1:nrow(distinct_sites)),]%>%
    mutate(sitenum=row_number(),
           site_anon=paste0("site ", sitenum))
}
