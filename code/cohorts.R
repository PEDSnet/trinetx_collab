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



#' Create a cross-joined master table for variable reference
#'
#' @param cj_tbl multi-site, over time output from check_code_dist_csd function
#' @param cj_var_names a vector with the names of variables that should be used as the "anchor"
#'                     of the cross join where all combinations of the variables should be
#'                     present in the final table
#' @param join_type the type of join that should be performed at the end of the function
#'                  left is used for multi-site anomaly (euclidean distance) while full
#'                  is used for single site anomaly (timetk package)
#'
#' @return one data frame with all combinations of the variables from cj_var_names with their
#'         associated facts from the original cj_tbl input
#' 
compute_at_cross_join <- function(cj_tbl,
                                  cj_var_names = c('site','concept_id'),
                                  join_type = 'left') {
  
  
  cj_tbl <- ungroup(cj_tbl)
  blah <- list()
  
  date_first <- cj_tbl %>% arrange(time_start) %>% filter(!is.na(time_start)) %>% distinct(time_start) %>% first() %>% pull()
  date_last <- cj_tbl %>% arrange(time_start) %>% filter(!is.na(time_start)) %>% distinct(time_start) %>% last() %>% pull()
  time_increment_var <- cj_tbl %>% distinct(time_increment) %>% pull()
  
  all_months <- seq.Date(from=ceiling_date(date_first, unit = time_increment_var),
                         to=ceiling_date(date_last, unit = time_increment_var),
                         by=time_increment_var) - 1
  all_months_tbl <- as_tibble(all_months) %>% rename(time_start=value)
  
  for(i in 1:length(cj_var_names)) {
    
    cj_var_name_i <- (cj_var_names[[i]])
    
    cj_tbl_narrowed <- cj_tbl %>% distinct(!! sym(cj_var_name_i))
    
    blah[[i]] <- cj_tbl_narrowed
    
  }
  
  cj_tbl_cjd <- reduce(.x=blah,
                       .f=cross_join)
  
  cj_tbl_cjd_time <- 
    all_months_tbl %>% cross_join(cj_tbl_cjd)
  
  if(join_type == 'left'){
    cj_tbl_full <- 
      cj_tbl_cjd_time %>% 
      left_join(cj_tbl) %>% 
      mutate(across(where(is.numeric), ~ replace_na(.x,0)))
  }else{
    cj_tbl_full <- 
      cj_tbl_cjd_time %>% 
      full_join(cj_tbl) %>% 
      mutate(across(where(is.numeric), ~ replace_na(.x,0)))
  }
  
  
}

#' Compute Euclidean Distance
#'
#' @param ms_tbl output from compute_dist_mean_median where the cross-joined table from
#'               compute_at_cross_join is used as input
#' @param output_var the output variable that should be used to compute the Euclidean distance
#'                   i.e. a count or proportion
#'
#' @return one dataframe with all variables from ms_tbl with the addition of columns with a site Loess
#'         value and a site Euclidean distance value
#' 
compute_euclidean <- function(ms_tbl,
                              output_var,
                              grp_vars = c('site', 'concept_id')) {
  
  grp_tbls <- group_split(ms_tbl %>% unite(facet_col, !!!syms(grp_vars), sep = '_', remove = FALSE) %>%
                            group_by(facet_col))
  
  euclidean_dist <- function(x, y) sqrt(sum((x - y)^2)) 
  
  overall <- list()
  
  for(i in 1:length(grp_tbls)) {
    
    site_datenumeric <- 
      grp_tbls[[i]] %>%  
      mutate(date_numeric = as.numeric(time_start),
             output_var = !!sym(output_var))
    site_loess <- loess(output_var ~ date_numeric, data=site_datenumeric)
    site_loess_df <- as_tibble(predict(site_loess)) %>% rename(site_loess=1) 
    euclidean_site_loess <- euclidean_dist(predict(site_loess), site_datenumeric$mean_allsiteprop)
    ms_witheuclidean <- 
      cbind(site_datenumeric,site_loess_df) %>% 
      mutate(dist_eucl_mean=euclidean_site_loess) #%>% 
    # mutate(loess_predicted=predict(site_loess)) 
    
    overall[[i]] <- ms_witheuclidean
    
  }
  
  overall_reduce <- reduce(.x=overall,
                           .f=dplyr::union) %>% as_tibble() %>% 
    mutate(dist_eucl_mean=round(dist_eucl_mean,2),
           site_loess=round(site_loess,2)) %>%
    select(-facet_col)
  
}

#' Euclidean Distance for *_ms_anom_at output
#'
#' @param fot_input_tbl table output by compute_fot where the check of interest
#'                      is used as the check_func
#' @param grp_vars the variables that should be preserved in the cross join
#' @param var_col the column with the numerical statistic of interest for the euclidean
#'                distance computation
#' @param time_period a string denoting the period of time that separates each date value
#'                    (i.e. month, year, etc)
#'
#' @return data frame with mean and median values for the user provided variable column
#'         and the euclidean distance value from the all site mean
#' 
ms_anom_euclidean <- function(fot_input_tbl,
                              grp_vars,
                              var_col) {
  
  
  ms_at_cj <- compute_at_cross_join(cj_tbl=fot_input_tbl,
                                    cj_var_names = grp_vars)
  
  allsite_grps <- grp_vars %>% append('time_start')
  allsite_grps <- allsite_grps[! allsite_grps %in% c('site_anon')]
  
  ms_at_cj_avg <- compute_dist_mean_median(tbl=ms_at_cj,
                                           grp_vars=allsite_grps,
                                           var_col=var_col,
                                           num_sd = 2,
                                           num_mad = 2)  %>% 
    rename(mean_allsiteprop=mean) 
  
  euclidiean_tbl <- compute_euclidean(ms_tbl=ms_at_cj_avg,
                                      output_var=var_col,
                                      grp_vars = grp_vars)
  
  final <- 
    euclidiean_tbl %>% 
    select(site_anon,time_start, grp_vars, var_col,
           mean_allsiteprop, median, date_numeric,
           site_loess,dist_eucl_mean
    )
  
  return(final)
  
}

#' *Computing Distance From Mean*
#' Should be able to use this for other checks,
#' but naming this way to differentiate from
#' the existing `compute_dist_mean` function
#' @param tbl table with at least the vars specified in `grp_vars` and `var_col`
#' @param grp_vars variables to group by when computing summary statistics
#' @param var_col column to compute summary statistics on
#' @param num_sd (integer) number of standard deviations away from the mean
#'               from which to compute the sd_lower and sd_upper columns
#' @return a table with the `grp_vars` ** | mean | sd | sd_lower | sd_upper | **
#'                                     ** anomaly_yn: indicator of whether data point is +/- num_sd from mean **
#'                                     ** abs_diff_mean: absolute value of difference between mean for group and observation **
compute_dist_mean_median <- function(tbl,
                                     grp_vars,
                                     var_col,
                                     num_sd,
                                     num_mad){
  
  site_rows <-
    tbl %>% ungroup() %>% select(site_anon) %>% distinct()
  grpd_vars_tbl <- tbl %>% ungroup() %>% select(!!!syms(grp_vars)) %>% distinct()
  
  tbl_new <- 
    cross_join(site_rows,
               grpd_vars_tbl) %>% 
    left_join(tbl) %>% 
    mutate(across(where(is.numeric), ~replace_na(.x,0)))
  
  stats <- tbl_new %>%
    group_by(!!!syms(grp_vars))%>%
    summarise(mean=mean(!!!syms(var_col)),
              median=median(!!!syms(var_col)),
              sd=sd(!!!syms(var_col), na.rm=TRUE),
              mad=mad(!!!syms(var_col),center=median),
              `90th_percentile`=quantile(!!!syms(var_col), 0.95)) %>%
    ungroup() %>%
    mutate(sd_lower=mean-num_sd*sd,
           sd_upper=mean+num_sd*sd,
           mad_lower=median-num_mad*mad,
           mad_upper=median+num_mad*mad)
  
  final <- tbl_new %>%
    inner_join(stats)%>%
    mutate(anomaly_yn=case_when(!!sym(var_col)<sd_lower|!!sym(var_col)>sd_upper|!!sym(var_col)>`90th_percentile`~TRUE,
                                TRUE~FALSE),
           abs_diff_mean=abs(!!sym(var_col)-mean),
           abs_diff_median=abs(!!sym(var_col)-median),
           n_mad=abs_diff_median/mad)
  
  return(final)
}



#' Multi-Site comparison against all site median
#' 
#' pulled from PF check in SSDQA with some edits

ms_exp_nt2 <- function(process_output,
                       check_string = 'Branch',
                       y_col = 'branch',
                       descriptor_col = 'description'){
  
  data_format <- process_output %>%
    mutate(text = paste0('Site: ', site_anon,
                         '\n', check_string, ': ', !!sym(descriptor_col),
                         '\nProportion: ', prop_pts))
  
  r <- ggplot(data_format, 
              aes(y=!!sym(y_col),x=prop_pts, colour=site_anon))+
    geom_point_interactive(aes(tooltip = text), size=3)+
    geom_point(aes(y=!!sym(y_col), x=allsite_median), shape=8, size=4, color="black")+
    scale_color_ssdqa(discrete = TRUE) +
    scale_y_discrete(limits=rev) +
    #facet_wrap((facet), scales="free_x", ncol=2)+
    theme_minimal() + 
    labs(y = check_string,
         x = 'Proportion Patients',
         color = 'Site',
         title = paste0('Proportion of Patients with Each ', check_string),
         subtitle = 'Star represents All-Site Median')
  
  girafe(ggobj = r)
  
}

#' Euclidean distance function for testing

euclidean_output <- function(process_output,
                             output_var,
                             filter_variable) {
  
  filt_op <- process_output %>% filter(check_desc == filter_variable) %>%
    mutate(prop_col = !!sym(output_var))
  
  allsites <- 
    filt_op %>% 
    select(time_start,check_desc,mean_allsiteprop) %>% distinct() %>% 
    rename(prop_col=mean_allsiteprop) %>% 
    mutate(site_anon='all site average') %>% 
    mutate(text_smooth=paste0("Site: ", site_anon,
                              "\n","Proportion: ",prop_col),
           text_raw=paste0("Site: ", site_anon,
                           "\n","Proportion: ",prop_col)) 
  
  dat_to_plot <- 
    filt_op %>% 
    mutate(text_smooth=paste0("Site: ", site_anon,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site_anon,
                           "\n","Site Proportion: ",prop_col,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean)) 
  
  p <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site_anon, group = site_anon, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    scale_color_ssdqa() +
    labs(y = 'Proportion (Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion of ', filter_variable, ' Across Time'))
  
  q <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site_anon,
               group=site_anon, text=text_raw)) +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    scale_color_ssdqa() +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion of ', filter_variable, ' Across Time'))
  
  t <- dat_to_plot %>% 
    distinct(site_anon, dist_eucl_mean, site_loess) %>% 
    group_by(site_anon, dist_eucl_mean) %>% 
    summarise(mean_site_loess = mean(site_loess)) %>%
    ggplot(aes(x = site_anon, y = dist_eucl_mean, fill = mean_site_loess)) + 
    geom_col() + 
    geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
              show.legend = FALSE) +
    coord_radial(r_axis_inside = FALSE, rotate_angle = TRUE) + 
    guides(theta = guide_axis_theta(angle = 0)) +
    theme_minimal() + 
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    theme(legend.position = 'bottom',
          legend.text = element_text(angle = 45, vjust = 0.9, hjust = 1),
          axis.text.x = element_text(face = 'bold')) + 
    labs(fill = 'Avg. Proportion \n(Loess)', 
         y ='Euclidean Distance', 
         x = '', 
         title = paste0('Euclidean Distance for ', filter_variable))
  
  plotly_p <- ggplotly(p,tooltip="text")
  plotly_q <- ggplotly(q,tooltip="text")
  
  output <- list(plotly_p,
                 plotly_q,
                 t)
  
  return(output)
  
}

