
#' *Normalization of Stability Over Time results*
#' 
#' Our full 3-function process is included below for full visibility into how we execute this check.
#' We run the normalization formula for the site-specific counts, run it again for the OVERALL patient count
#' for a given month/check application (i.e. network wide, ignoring site specificity), then use the 
#' final function  `check_fot_all_dist` to check the distance between the site specific normalized value 
#' and the overall normalized value
#' 

#' Function that iterates through a list, computing monthly visits
#' 
#' @param time_tbls a list with the following requirements:
#' `element name`: a short description of the monthly computed visits being measured
#' `first element`: the expression being evaluated (e.g., `cdm_tbl('visit_occurrernce') %>% group_by(visit_concept_id`)
#' @param meta_tbls: a list containing a description of each output from time_tbls. Element names *must* match.
#' 
#' @param time_fame a list that contains the end date of every month to iterate through
#' @param lookback_weeks if lookback is in weeks instead of months, this should be set to a non-zero integer. 
#' Defaults to 0, for lookback to be in months.
#' @param lookback_months the number of monoths to look back
#' @param check_string the name of the check, used in metadata table 
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#' 
#' @return table with the following rows (if `distinct_visits` = `TRUE`:
#'  `month_end` | `check_name` | `database_version` | `site` | `time_desc` | `row_cts` | `row_pts` | `row_visits`
#'  
#'  ** if `time_tbls` contains fields that are grouped, the output will contain the grouped variables
#' @return a metadata table summarizing all the table names produced
#' 
#' *** table names are derived from `time_tbls` element names.
#' 

check_fot <- function(time_tbls,
                      #meta_tbls,
                      time_frame = time_span,
                      lookback_weeks=0,
                      lookback_months=1,
                      check_string = 'fot',
                      visits_only = TRUE,
                      distinct_visits = TRUE) {
  
  
  final_results <- list()
  
  for(i in 1:length(time_tbls)) {
    
    message(paste0('Starting ',i))
    
    temp_results <- list()
    
    for(k in time_frame) {
      
      message(paste0('Starting ',k))
      
      target <- ymd(k)
      
      baseline_end_date <- target
      if(lookback_weeks == 0) {
        baseline_start_date <- target %m-% months(lookback_months)
      } else {baseline_start_date <- target - weeks(x=lookback_weeks)}
      
      
      date_cols <- 
        time_tbls[[i]][[1]] %>% ungroup() %>%
        select(ends_with('_date')) %>% select(- contains('end')) %>% 
        select(-contains(c('result','order'))) 
      
      order_cols <- ncol(date_cols)
      
      date_cols_unmapped <- 
        date_cols %>% 
        select(all_of(order_cols))
      
      colname_string <- as.character(colnames(date_cols_unmapped)[1])
      
      visits_narrowed <-
        time_tbls[[i]][[1]] %>%
        filter(!! sym(colname_string) <= baseline_end_date &
                 !! sym(colname_string) > baseline_start_date)  
      
      if(visits_only) {
        visit_cts <-
          visits_narrowed %>%
          summarise(row_visits = n_distinct(visit_occurrence_id)) %>%
          collect() %>%  ungroup()  %>% 
          add_meta(check_lib=check_string) %>%
          mutate(check_name = names(time_tbls[i])) %>% 
          mutate(check_desc = time_tbls[[i]][[2]])
      } else if(distinct_visits & !visits_only) {
        visit_cts <-
          visits_narrowed %>%
          summarise(row_cts = n(),
                    row_visits = n_distinct(visit_occurrence_id),
                    row_pts = n_distinct(person_id)) %>%
          collect() %>%  ungroup()  %>% 
          add_meta(check_lib=check_string) %>%
          mutate(check_name = names(time_tbls[i])) %>% 
          mutate(check_desc = time_tbls[[i]][[2]])
        #mutate(check_name = time_tbls[[i]][[2]]) %>%
        #mutate(check_desc = names(time_tbls[i]))
      } else if(!distinct_visits & !visits_only) {
        visit_cts <-
          visits_narrowed %>%
          summarise(row_cts = n(),
                    row_pts = n_distinct(person_id)) %>%
          collect() %>%  ungroup()  %>% 
          add_meta(check_lib=check_string) %>%
          mutate(check_name = names(time_tbls[i])) %>% 
          mutate(check_desc = time_tbls[[i]][[2]])
        #mutate(check_name = time_tbls[[i]][[2]]) %>%
        #mutate(check_desc = names(time_tbls[i]))
      }
      
      
      
      if(! lookback_weeks) {
        this_round <- visit_cts %>%
          mutate(month_end = date(k)) 
      } else {this_round <- this_round_pre %>%
        mutate(week_end = date(i)) }
      
      temp_results[[k]] <- this_round
      
    }
    
    final_results[[paste0(check_string, '_', names(time_tbls[i]))]] = reduce(.x=temp_results, .f=union)
    
  }
  
  # meta <- compute_meta_tbl(meta_tbls=meta_tbls,
  #   versions_tbl_list=final_results,
  #  check_string=check_string)
  
  # final_results[[paste0(check_string,'_meta')]] <- meta
  
  final_results
  
}


#' This function calculates the normalization heuristic used in the FOT dq check
#' 
#' The heuristic is:
#' month / ((month-1)*.25 +
#'          (month+1)*.25 +
#'          (month-12)*.5)
#'
#' In plain words, its the current month divided by the weighted average of the
#' previous month, the next month, and the value from the current month in the
#' previous year
#' 
#' @param tblx table with counts over time with: a site column, a time column, a column with the relevant
#'             statistic to be summarized (i.e. patient counts), and descriptive columns with 
#'             the name of the check applications (i.e. asthma_pts; Asthma Patients)
#' @param site_col this column has the name of the sites
#' @param time_col this column contains the date value for each month in the time period
#' @param target_col this column has the relevant statistic (i.e. patient counts) for each site/check/month
#' 

fot_check_calc <- function(tblx, site_col,time_col, target_col) {
  tblx %>%
    arrange(!!sym(site_col),!!sym(time_col)) %>%
    mutate(
      lag_1 = lag(!!sym(target_col)),
      lag_1_plus = lead(!!sym(target_col)),
      lag_12 = lag(!!sym(target_col),12),
      check_denom = (lag(!!sym(target_col))*.25 +
                              lead(!!sym(target_col))*.25 +
                              lag(!!sym(target_col),12)*.5)) %>%
    filter(check_denom!=0) %>%
    mutate(check = !!sym(target_col)/check_denom-1)
}

#' Main function that does the FOT checks
#' 
#' @param target_col this column has the relevant statistic (i.e. patient counts) for each site/check/month
#' @param tblx table with all output: a site column, a time column, a column with the relevant
#'             statistic to be summarized (i.e. patient counts), and optionally a column with 
#'             the name of the check applications (i.e. asthma_pts)
#' @param check_col descriptive column with an abbreviated name of the check applications 
#'                  included in the table (i.e. asthma_pts)
#' @param check_desc descriptive column that contains a fuller description of the check applications included
#'                   in the table (i.e. Asthma Patients)
#' @param site_col column with site names
#' @param time_col column with date value for each month in the specified time period
#' 
#' For each check application (i.e. asthma_pts) it runs the heuristic (using `fot_check_calc`) for the 
#' user selected statistic (@target_col). This happens for each site AND for the overall cohort 
#' (`agg_check` - site = all). Then, it will summarize the SD, Q1, Q3, mean, and median for both the 
#' site specific and overall output. 
#' 
#' The site specific and total summary tables are unioned together, and then the distance function 
#' (`check_fot_all_dist`) is run to determine how far from the all site normalized value each site 
#' falls for a specific check & month.
#' 
fot_check <- function(target_col,
                      tblx=results_tbl('fot_output'),
                      check_col='check_name',
                      check_desc='check_desc',
                      site_col='site',
                      time_col='month_end') {
  
  cols_to_keep <- c(#'domain',
                    eval(site_col),eval(check_col),eval(check_desc),eval(time_col),'check',
                    'check_denom', eval(target_col))
  
  rv <- FALSE
  rv_agg <- FALSE
  
  #base tbl to make a network wide version of the check
  agg_check <- tblx %>% group_by(#domain, 
                                 !!sym(time_col),!!sym(check_col), !!sym(check_desc)) %>%
    summarise({{target_col}} := sum(!!sym(target_col))) %>%
    ungroup() %>%
    mutate({{site_col}}:='all') 
  
  for (target_check in tblx %>% select(!!sym(check_col)) %>% distinct() %>% pull()) {
    for (target_site in tblx %>% select(!!sym(site_col)) %>% distinct() %>% pull()) {
      foo <- fot_check_calc(tblx %>%
                              filter(check_name==target_check & site==target_site),
                            site_col='site',
                            time_col,
                            target_col) %>% collect()
      if(!is.logical(rv)){
        rv <- union(rv, foo)
      } else {
        rv <- foo
      }
    }
    bar <- fot_check_calc(agg_check %>% filter(check_name==target_check),
                          site_col,time_col,target_col) %>%
      select(cols_to_keep) %>% collect()
    if(!is.logical(rv_agg)){
      rv_agg <- union(rv_agg, bar)
    } else {
      rv_agg <- bar
    }
  }
  
  # summarise the checks across sites
  rv_summary <- rv %>% group_by(#domain, 
                                !!sym(check_col), !!sym(site_col)) %>%
    summarise(std_dev = sd(check,na.rm=TRUE),
              pct_25 = quantile(check,.25),
              pct_75 = quantile(check,.75),
              med = median(check),
              m = mean(check)) %>% ungroup() %>% collect()
  
  rv_summary_allsites <- rv_agg %>%
    filter(site=='all') %>% group_by(#domain, 
                                     !!sym(check_col), !!sym(site_col)) %>%
    summarise(std_dev = sd(check,na.rm=TRUE),
              pct_25 = quantile(check,.25),
              pct_75 = quantile(check,.75),
              med = median(check),
              m = mean(check)) %>% ungroup() %>% collect() %>%
    mutate(site='all')
  
  
  return(list(fot_heuristic= dplyr::union(rv %>% select(cols_to_keep),
                                          rv_agg),
              fot_heuristic_summary=dplyr::union(rv_summary,
                                                 rv_summary_allsites)))
}


#' fot table computing distance from "all" check
#'
#' @param fot_check_output first element of list output from `fot_check`
#'
#' @return tbl with the following columns:
#' domain | check_name | month_end | centroid | site | check | distance
#'
#' The `distance` column measures, for each site/domain/check/month combination,
#' the distance between the site's normalized `check` output compared to
#' all sites combined. This is the statistic used for the visualization.
#'

check_fot_all_dist <- function(fot_check_output) {
  
  # select the all site values
  just_all <-
    fot_check_output %>%
    filter(site=='all') %>%
    select(-c(check_denom, row_pts)) %>%
    rename(centroid=check) %>%
    select(-c(site))
  
  # join in the site specific computation and check distance between
  # the overall and site specific values for each check/month
  combined <-
    just_all %>%
    inner_join(
      fot_check_output %>% select(-c(check_denom, row_pts)),
      by=c(#'domain',
           'check_name',
           'month_end',
           'check_desc')
    ) %>% mutate(
      distance=round(check,3)-round(centroid,3)
    )
}


