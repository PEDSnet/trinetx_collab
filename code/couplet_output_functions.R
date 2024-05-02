library(ggplot2)
library(scales)
library(ggpubr)
# tutorial: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
#' Function to extract colors as hex codes
#'
#' @param ... Character names of ssdqa_colors_standard
#' @return name and hex code/s for specified color
#' example usage: extract_color() [returns all] or extract_color("rust) [just returns rust]
#'
extract_color <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ssdqa_colors_standard)
  
  ssdqa_colors_standard[cols]
}

#' Return function to interpolate a color palette
#'
#' @param palette Character name of palette in ssdqa_palettes_standard
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @return color palettes, interpolated if necessary, with specified scheme and number of colors
#' example usage: ssdqa_pal("beachy")(10)
ssdqa_pal <- function(palette, reverse = FALSE, ...) {
  pal <- ssdqa_palettes_standard[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Setting standard colors
ssdqa_colors_standard<-c(`brightpink`="#FF4D6FFF", 
                         `lightblue`="#579EA4FF", 
                         `burntorange`="#DF7713FF", 
                         `yellow`="#F9C000FF", 
                         `lightgreen`="#86AD34FF", 
                         `dustblue`="#5D7298FF", 
                         `seagreen`="#81B28DFF", 
                         `rust`="#7E1A2FFF", 
                         `violet`="#2D2651FF", 
                         `redorange`="#C8350DFF", 
                         `rosypink`="#BD777AFF",
                         `grey=`="#E2D8D6FF")

ssdqa_palettes_standard<-list(
  `dark` = extract_color("rust", "violet", "redorange"),
  `fun` = extract_color("brightpink", "lightblue", "yellow"),
  `beachy`=extract_color("lightblue","dustblue","seagreen"),
  `diverging`=extract_color("dustblue", "grey", "rust"),
  `sequential`=extract_color("grey", "rosypink", "rust"),
  `main`=extract_color("brightpink", "lightblue", "burntorange", "yellow",
                       "lightgreen","dustblue", "seagreen", "rust",
                       "violet", "redorange", "rosypink")
)

# usage: ssdqa_pal("dark")(10)


#' Fill scale constructor for ssdqa colors
#'
#' @param palette Character name of palette in ssdqa_palettes_standard
#'                  If no palette specified, defaults to "main" palette
#' @param discrete Boolean indicating whether fill aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
scale_fill_ssdqa <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ssdqa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("ssdqa_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' Color scale constructor for ssdqa colors
#'
#' @param palette Character name of palette in ssdqa_palettes_standard.
#'                  If no palette specified, defaults to "main" palette
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
scale_color_ssdqa <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ssdqa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ssdqa_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#### Functions for detecting anomalies


#' * Multi Site, Exploratory, No Time *
#' 
#' @param process_output the output provided by the `evp_process` function
#' @param output_level the level of output to be displayed: `patient` or `row`
#'
#' @return a heat map displaying the proportion of patients/rows that meet criteria for each
#'         of the variables found in process_output at each of site
#' 

ms_exp_nt <- function(process_output,
                      output_col){
  
  
  process_output %>%
    mutate(colors = ifelse(!!sym(output_col) < 0.2 | !!sym(output_col) > 0.8, 'group1', 'group2')) %>%
    ggplot(aes(y = site_anon, x = couplet_name, fill = !!sym(output_col))) +
    geom_tile() +
    geom_text(aes(label = !!sym(output_col), color = colors), #size = 2, 
              show.legend = FALSE) +
    scale_color_manual(values = c('white', 'black')) +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    theme_minimal() +
    labs(title = paste0('Proportion per Couplet'),
         y = 'Site',
         x = 'Couplet') + 
    theme(axis.text.x=element_text(angle=45,hjust=1))
  
}

#' * Multi Site, Anomaly, No Time *

#' @param df_tbl output from the computation of a particular function for anomaly detection
#' @param grp_vars the columns to group by to compute the summary statistics for
#' @param var_col column to perform summary statistics for, to detect an anomaly 
#' 
#' @return the `df_tbl` with the following computed: 
#'  `mean_val`, `median_val`, `sd_val`, `mad_val`, `cov_val`, `max_val`, 
#'  `min_val`, `range_val`, `total_ct`, `analysis_eligible`
#'  the `analysis_eligible` will indicate whether the group for which the user
#'  wishes to detect an anomaly for is eligible for analysis.
#'  
#'  The following conditions will disquality a group from the anomaly detection analysis:
#'  (1) Sample size < 5 in group
#'  (2) Mean < 0.02 or Median < 0.01
#'  (3) Mean value < 0.05 and range <0.01
#'  (4) Coefficient of variance <0.01 and sample size <11
#'  


compute_dist_anomalies <- function(df_tbl,
                                   grp_vars,
                                   var_col){
  
  site_rows <-
    df_tbl %>% ungroup() %>% select(site) %>% distinct()
  grpd_vars_tbl <- df_tbl %>% ungroup() %>% select(!!!syms(grp_vars)) %>% distinct()
  
  tbl_new <- 
    cross_join(site_rows,
               grpd_vars_tbl) %>% 
    left_join(df_tbl) %>% 
    mutate(across(where(is.numeric), ~replace_na(.x,0)))
  
  
  stats <- tbl_new %>%
    group_by(!!!syms(grp_vars))%>%
    summarise(mean_val=mean(!!!syms(var_col)),
              median_val=median(!!!syms(var_col)),
              sd_val=sd(!!!syms(var_col), na.rm=TRUE),
              mad_val=mad(!!!syms(var_col)),
              cov_val=sd(!!!syms(var_col),na.rm=TRUE)/mean(!!!syms(var_col)),
              max_val=max(!!!syms(var_col)),
              min_val=min(!!!syms(var_col)),
              range_val=max_val-min_val,
              total_ct=n()) %>% ungroup() %>% 
    ungroup() %>% mutate(analysis_eligible = 
                           case_when(mean_val < 0.02 | median_val < 0.01 | 
                                       (mean_val < 0.05 & range_val < 0.1) | 
                                       (cov_val < 0.1 & total_ct < 11) ~ 'no',
                                     TRUE ~ 'yes'))
  final <- tbl_new %>% left_join(stats,
                                 by=c(grp_vars))
  
  return(final)
  
  
}

#' Computes anomaly detection for a group (e.g., multi-site analysis)
#' Assumes: (1) No time component; (2) Table has a column indicating 
#' whether a particular group or row is eligible for analysis; (3) column 
#' variable exists for which to compute the anomaly
#' 
#' @param df_tbl tbl for analysis; usually output from `compute_dist_anomalies`
#' @param tail_input whether to detect anomaly on right, left, or both sides; defaults to `both`
#' @param p_input the threshold for anomaly; defaults to 0.9
#' @param column_analysis a string, which the name of the column for which to compute anomaly detection;
#' @param column_variable a string, which is the name of the variable to compute summary statistics for;
#' @param column_eligible a string, which is the name of the column that indicates eligibility for analysis
#' 

detect_outliers <- function(df_tbl,
                            tail_input = 'both', 
                            p_input = 0.9,
                            column_analysis = 'prop_concept',
                            column_eligible = 'analysis_eligible',
                            column_variable = 'concept_id') {
  
  final <- list()
  
  eligible_outliers <- 
    df_tbl %>% filter(!! sym(column_eligible) == 'yes')
  
  if(nrow(eligible_outliers) == 0){
    
    output_final_all <- df_tbl %>% mutate(anomaly_yn = 'no outlier in group')
    
    cli::cli_warn('No variables were eligible for anomaly detection analysis')
    
  }else{
    
    groups_analysis <- group_split(eligible_outliers %>% unite(facet_col, !!!syms(column_variable), sep = '_', remove = FALSE) %>%
                                     group_by(facet_col))
    
    for(i in 1:length(groups_analysis)) {
      
      # filtered <- 
      #   eligible_outliers %>% filter(!!! syms(column_variable) == i) 
      
      vector_outliers <- 
        groups_analysis[[i]] %>% select(!! sym(column_analysis)) %>% pull()
      
      outliers_test <- 
        hotspots::outliers(x=vector_outliers, p=p_input, tail= tail_input)
      
      output <- groups_analysis[[i]] %>% mutate(
        lower_tail = outliers_test[[10]],
        upper_tail = outliers_test[[9]]
      ) %>% mutate(anomaly_yn = case_when(!! sym(column_analysis) < lower_tail |
                                            !! sym(column_analysis) > upper_tail ~ 'outlier',
                                          TRUE ~ 'not outlier'))
      
      final[[i]] <- output
      
      
    }
    
    final
    
    output_final_anomaly <- purrr::reduce(.x=final,
                                          .f=dplyr::union)
    
    output_final_all <- df_tbl %>% left_join(output_final_anomaly) %>% 
      mutate(anomaly_yn=case_when(
        is.na(anomaly_yn) ~ 'no outlier in group',
        TRUE ~ anomaly_yn
      )) %>% select(-facet_col)
  }
  
  return(output_final_all)
}



#' * Multi Site, Anomaly, No Time *
#' 
#' @param process_output the output provided by the `evp_process` function
#' @param output_level the level of output to be displayed: `patient` or `row`
#' @param kmeans_centers the number of centers that should be used in the k-means computations
#'                       defaults to `2`
#' @param facet columns the user would like to facet by
#'
#' @return one cluster graph per facet grouping with sites comprising the cluster elements
#'         if facet = NULL, one cluster graph will be output
#' 
# evp_ms_anom_nt <- function(process_output,
#                            output_level,
#                            kmeans_centers = 2, 
#                            facet){
#   
#   cli::cli_div(theme = list(span.code = list(color = 'blue')))
#   
#   if(output_level == 'row'){
#     prop <- 'prop_row_variable'
#     title <- 'Rows'
#   }else if(output_level == 'patient'){
#     prop <- 'prop_pt_variable'
#     title <- 'Patients'
#   }else(cli::cli_abort('Please choose an acceptable output level: {.code patient} or {.code row}'))
#   
#   process_output_prep <- process_output %>%
#     mutate(domain = variable)
#   
#   kmeans_prep <- prep_kmeans(dat = process_output_prep,
#                              output = prop,
#                              facet_vars = facet)
#   
#   kmeans_output <- produce_kmeans_output(kmeans_list = kmeans_prep,
#                                          centers = kmeans_centers)
#   
#   return(kmeans_output)
# }

ms_anom_nt<-function(process_output,
                     comparison_col,
                     variable,
                     text_wrapping_char = 60){
  
  
  
  dat_to_plot <- process_output %>%
    mutate(text=paste("Variable: ",variable,
                      "\nSite: ",site,
                      "\nProportion: ",round(!!sym(comparison_col),2),
                      "\nMean proportion:",round(mean_val,2),
                      "\nMedian proportion: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2)))
  
  
  #mid<-(max(dat_to_plot[[comparison_col]],na.rm=TRUE)+min(dat_to_plot[[comparison_col]],na.rm=TRUE))/2
  
  plt<-ggplot(dat_to_plot %>% filter(anomaly_yn != 'no outlier in group'),
              aes(x=site, y=!!sym(variable), text=text, color=!!sym(comparison_col)))+
    geom_point_interactive(aes(size=mad_val,shape=anomaly_yn, tooltip = text))+
    scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_shape_manual(values=c(20,8))+
    scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=60)) +
    labs(y = "Variable",
         size="") +
    guides(color = guide_colorbar(title = 'Proportion'),
           shape = guide_legend(title = 'Anomaly'),
           size = 'none')
  
  girafe(ggobj = plt)
}



