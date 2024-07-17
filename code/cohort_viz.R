
#' TriNetX Anomaly Detection Visualization
#'
#' @param dat the output from `trinetx_anom_detect`
#' @param var_col the target numerical column used as input to `trinetx_anom_detect`
#' @param grp_col the grouping column used as input to `trinetx_anom_detect` that should
#'                be used as the y axis value
#'
#' @return a dot plot with the grp_col along the y axis and the site along the x axis.
#' 
#'         the shape of each icon represents the anomaly type: circle is not an outlier,
#'         triangle pointing up is an upper outlier, and triangle pointing down is a 
#'         lower outlier. 
#'         
#'         the color of each icon represents the value of var_col.
#'         
#'         the size of each circle represents the IQR value for that row, while the
#'         size of each triangle represents the severity score of the outlier
#'         
#' @return an html table with the site severity scores returned by trinetx_anom_detect
#' 
trinetx_anom_viz <- function(dat,
                             var_col,
                             grp_col){
  
  dat_to_plot <- dat %>%
    mutate(text=paste("Variable: ",!!sym(grp_col),
                      "\nSite: ",site_anon,
                      "\nProportion: ",round(!!sym(var_col),2),
                      "\nSeverity Score: ", round(severity_score, 4),
                      "\nSite Outlier Score: ", round(site_score,4)))
  
  
  #mid<-(max(dat_to_plot[[comparison_col]],na.rm=TRUE)+min(dat_to_plot[[comparison_col]],na.rm=TRUE))/2
  
  plt<-ggplot(dat_to_plot, aes(x=site_anon, y=!!sym(grp_col), tooltip=text, color=!!sym(var_col),shape = anomaly_yn))+
    geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'), aes(size = iqr_val)) + 
    geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn != 'not outlier'), aes(size = severity_score,
                                                                                           tooltip = text)) + 
    scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_shape_manual(values=c('upper outlier' = 24,
                                'not outlier' = 20,
                                'lower outlier' = 25))+
    #scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 1)) +
    scale_y_discrete(labels = label_wrap_gen()) +
    labs(title = paste0('Anomaly Detection per ', grp_col)) +
    guides(color = guide_colorbar(title = 'Percent'),
           shape = guide_legend(title = 'Anomaly'),
           size = 'none')
  
  gplot <- girafe(ggobj = plt)
  
  tbl <- dat %>%
    ungroup() %>%
    distinct(site_anon, site_score) %>%
    mutate(site_score = round(site_score, 4)) %>%
    gt::gt() %>%
    opt_interactive() %>%
    tab_header('Site Outlier Scores') %>%
    cols_label(site_anon = 'Site (Anonymized)',
               site_score = 'Site Outlier Score')
  
  otpt <- list(gplot, tbl)
  
  return(otpt)
  
}



#' Couplets Exploratory Visualization
#'
#' @param process_output the cleaned, combined output of the couplets
#'                       check
#' @param output_col the name of the target numerical column 
#'
#' @return a heat map with site along the y axis and couplet type along the x axis; the
#'         color of each section of the heat map represents the value of output_col
#' 
couplets_ms_viz <- function(process_output,
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

couplets_ss_viz <- function(process_output){
  
  process_output %>%
    filter(cohort %in% c('cohort_2_only', 
                         'combined', 'cohort_1_only')) %>%
    mutate(pct = paste0(round(prop, 2) * 100, '%')) %>%
    ggplot(aes(y=couplet_name,x=prop,fill=factor(cohort, levels = c('cohort_2_only', 
                                                                 'combined', 'cohort_1_only'))),
           stat='identity') +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = pct), position = position_stack(vjust = 0.5), size = 2,
              fontface = 'bold') +
    scale_fill_manual(values = c(ssdqa_colors_standard[[2]], ssdqa_colors_standard[[4]],
                      ssdqa_colors_standard[[11]])) +
    theme_minimal()+
    labs(y="Couplet",
         x="Proportion",
         fill = '',
         title = 'Couplet Distributions per Site')+
    facet_wrap(~site_anon, ncol = 2)
  
  
}


#' Case Mix Exploratory Visualization - Multi Site
#'
#' @param process_output the cleaned, combined output of the case mix check
#'
#' @return a bar plot with site along the x axis and patient proportion along the y axis, 
#'         facetting by the ICD10CM branch. A dotted line represents the all site median
#'         for each branch
#' 
case_mix_ms_viz <- function(process_output){
  
  data_format <- process_output %>%
    mutate(text = paste0('Site: ', site_anon,
                         '\nBranch: ', description,
                         '\nProportion: ', prop_pts))
  
  r <- ggplot(data_format, aes(y=prop_pts,x=site_anon, fill=site_anon))+
    geom_col_interactive(aes(tooltip = text))+
    geom_hline(aes(yintercept = allsite_median), linetype = 'dotted') +
    #geom_point(aes(y=!!sym(y_col), x=allsite_median), shape=8, size=4, color="black")+
    scale_fill_ssdqa(discrete = TRUE) +
    #scale_y_discrete(limits=rev) +
    facet_wrap(~branch, #scales="free_y", 
               ncol = 2)+
    theme_minimal() + 
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'Proportion Patients',
         x = 'Site',
         title = paste0('Proportion of Patients with Each Branch'),
         subtitle = 'Dotted line represents All-Site Median')
  
  girafe(ggobj = r)
  
}


#' Case Mix Exploratory Visualization - Summary / Single Site
#'
#' @param process_output the cleaned, combined output from the case mix
#'                       check
#' @param site_filter the single site that is the target of the analysis
#'
#' @return a bar graph with the ICD10CM branch along the x axis and patient 
#'         proportion along the y axis. Two dots are placed on each bar representing
#'         the all site mean (pink) and all site median (brown) proportion
#'         for each ICD branch
#' 
case_mix_summ_viz <- function(process_output,
                              site_filter){
  
  dat_to_plot <- process_output %>%
    group_by(cohort, branch) %>%
    mutate(allsite_mean = mean(prop_pts)) %>%
    ungroup()
  
  data_format <- dat_to_plot %>%
    filter(site_anon %in% site_filter) %>%
    mutate(text = paste0('Site: ', site_anon,
                         '\nBranch: ', description,
                         '\nProportion: ', prop_pts))
  
  r <- ggplot(data_format, aes(y=prop_pts,x=branch))+
    geom_col_interactive(aes(tooltip = text), fill = ssdqa_colors_standard[[7]])+
    geom_point_interactive(aes(y = allsite_mean, tooltip = paste0('All Site Mean: ', 
                                                                  round(allsite_mean, 3))),
                           color = ssdqa_colors_standard[[1]]) +
    geom_point_interactive(aes(y = allsite_median, tooltip = paste0('All Site Median: ', 
                                                                  round(allsite_median, 3))),
                           color = ssdqa_colors_standard[[8]]) +
    # geom_hline(aes(yintercept = allsite_median), linetype = 'dotted') +
    facet_wrap(~site_anon, #scales="free_y", 
               ncol = 2)+
    theme_minimal() + 
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'Proportion Patients',
         x = 'ICD10CM Branch',
         color = 'Site',
         title = paste0('Proportion of Patients per Branch'),
         subtitle = 'Dots represent all-site mean (pink) and median (brown)')
  
  girafe(ggobj = r)
  
}



#' Case Mix Exploratory Visualization -- Visit and Patient Proportions
#'
#' @param process_output cleaned output with both visit and patient proportions; should at least
#'                       have the columns site_anon, prop, prop_type (indicated pt vs visit),
#'                       branch, and description
#'
#' @return a dodged bar graph with two bars per branch: yellow for visit proportions, blue
#'         for patient proportions
#' 
case_mix_visits_viz <- function(process_output){
  
  
  cmvis <- ggplot(process_output, aes(x = site_anon, y = prop, fill = prop_type)) +
    geom_col_interactive(aes(tooltip = paste0('Site: ', site_anon, '\nBranch: ', 
                                              description, '\nProportion: ', prop)),
                         position = position_dodge()) +
    facet_wrap(~branch, ncol = 2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_manual(values = c(ssdqa_colors_standard[[2]], ssdqa_colors_standard[[4]])) +
    labs(fill = 'Proportion Type',
         y = 'Proportion',
         x = 'Site',
         title = 'Proportion of Patients & Visits per Branch')
  
  girafe(ggobj = cmvis)
  
  
}

#' Stability Over Time - Raw vs Normalized Patients
#'
#' @param process_output the cleaned, combined output of the stability
#'                       over time check
#' @param site_filter the target site for the analysis
#' @param domain_filter the target domain for the analysis
#'
#' @return line plot with time along the x axis, raw patient count along the
#'         left y axis, and normalized patient count along the right y axis
#' 
fot_raw_norm_viz <- function(process_output,
                             site_filter,
                             domain_filter){
  
  ay <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = "Normalized Patients")
  
  plt <- process_output %>%
    filter(site_anon == site_filter,
           check_desc == domain_filter) %>%
    plot_ly() %>%
    add_lines(x = ~month_end, y = ~row_pts, yaxis = 'y1', name = 'Patients') %>%
    add_lines(x = ~month_end, y = ~check, yaxis = 'y2', name = 'Normalized Patients',
              line = list(color = 'navy', dash = 'dot')) %>%
    layout(
      title = paste0(site_filter, ' ', domain_filter), 
      yaxis2 = ay,
      xaxis = list(title="Month End"),
      yaxis = list(title="Patients"))
  
  return(plt)
  
}

#' Stability Over Time - Euclidean Distance
#'
#' @param process_output the cleaned, combined output from `ms_anom_euclidean`
#' @param output_var the target numerical column for the analysis
#' @param filter_variable the target domain for the analysis
#' @param title user provided title for the graph
#' 
#' @return returns 3 graphs:
#'            
#'            1. smoothed line graph applying Loess regression to the output_var 
#'               with one line per site
#'            2. line graph of the raw value of the output_var with one line per
#'               site
#'            3. radial bar plot with the euclidean distance values for each site
#'               where color represents the average loess value per site
#' 
fot_euclidean_viz <- function(process_output,
                              output_var,
                              filter_variable,
                              title) {
  
  if (! is.null(filter_variable)) {
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
    
  } else {
    filt_op <- process_output %>% 
      mutate(prop_col = !!sym(output_var))
    
    allsites <- 
      filt_op %>% 
      select(time_start,mean_allsiteprop) %>% distinct() %>% 
      rename(prop_col=mean_allsiteprop) %>% 
      mutate(site_anon='all site average') %>% 
      mutate(text_smooth=paste0("Site: ", site_anon,
                                "\n","Proportion: ",prop_col),
             text_raw=paste0("Site: ", site_anon,
                             "\n","Proportion: ",prop_col)) 
  }
  
  
  dat_to_plot <- 
    filt_op %>% 
    mutate(text_smooth=paste0("Site: ", site_anon,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site_anon,
                           "\n","Site Proportion: ",prop_col,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean)) 
  
  lvls <- stringr::str_sort(unique(dat_to_plot$site_anon), numeric = TRUE, decreasing = TRUE)
  dat_to_plot$site_anon <- factor(dat_to_plot$site_anon, levels = lvls)
  
  p <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site_anon, group = site_anon, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)
          #text = element_text(size = 25)
          ) +
    scale_color_ssdqa() +
    labs(y = 'Proportion (Loess)',
         x = 'Time',
         title = title)
  
  q <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site_anon,
               group=site_anon, text=text_raw)) +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)
          #text = element_text(size = 25)
          ) +
    scale_color_ssdqa() +
    labs(x = 'Time',
         y = 'Proportion',
         title = title)
  
  t <- dat_to_plot %>% 
    distinct(site_anon, dist_eucl_mean, site_loess) %>% 
    group_by(site_anon, dist_eucl_mean) %>% 
    summarise(mean_site_loess = mean(site_loess)) %>%
    ggplot(aes(x = site_anon, y = dist_eucl_mean, fill = mean_site_loess)) + 
    geom_col() + 
    # geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
    #           show.legend = FALSE) +
    coord_radial(r_axis_inside = FALSE, rotate_angle = TRUE) + 
    guides(theta = guide_axis_theta(angle = 0)) +
    theme_minimal() + 
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    theme(legend.position = 'none',
          legend.text = element_text(angle = 45, vjust = 0.9, hjust = 1),
          axis.text.x = element_text(face = 'bold'),
          #text = element_text(size = 20)
    ) + 
    labs(y ='Euclidean Distance', 
         x = '',
         title = title)
  
  plotly_p <- ggplotly(p,tooltip="text")
  plotly_q <- ggplotly(q,tooltip="text")
  
  output <- list(plotly_p,
                 plotly_q,
                 t)
  
  return(output)
  
}


#' Coverage Overlap Exploratory Visualization
#'
#' @param process_output the cleaned, combined output from the coverage
#'                       overlap check
#'
#' @return a dot plot with fact group along the y axis and patient proportion
#'         along the x axis. each dot represents a site, and the star icon
#'         in each row represents the all site median
#' 
coverage_overlap_ms_viz <- function(process_output){
  
  data_format <- process_output %>%
    mutate(text = paste0('Site: ', site_anon,
                         '\nFact Group', ': ', fact_long,
                         '\nProportion: ', prop_pts))
  
  r <- ggplot(data_format, 
              aes(y=fact_long,x=prop_pts, colour=site_anon))+
    geom_point_interactive(aes(tooltip = text), size=3)+
    geom_point(aes(y=fact_long, x=allsite_median), shape=8, size=4, color="black")+
    scale_color_ssdqa(discrete = TRUE) +
    scale_y_discrete(limits=rev,
                     labels = label_wrap_gen()) +
    #facet_wrap((facet), scales="free_x", ncol=2)+
    theme_minimal() + 
    labs(y = 'Fact Group',
         x = 'Proportion Patients',
         color = 'Site',
         title = paste0('Proportion of Patients with Each Fact'),
         subtitle = 'Star represents All-Site Median')
  
  girafe(ggobj = r)
  
}


coverage_overlap_venn_viz <- function(process_output,
                                      site_list,
                                      output_directory){
  
  for(i in 1:length(site_list)){
    
    cvg_venn1 <- process_output %>%
      filter(site_anon == site_list[[i]]) %>%
      mutate(fg_venn = case_when(fact_group == 'dx' ~ 'area1',
                                 fact_group == 'lab' ~ 'area2',
                                 fact_group == 'med' ~ 'area3',
                                 fact_group == 'px' ~ 'area4',
                                 fact_group == 'dx_lab' ~ 'n12',
                                 fact_group == 'dx_med' ~ 'n13',
                                 fact_group == 'dx_med_lab' ~ 'n123',
                                 fact_group == 'dx_px' ~ 'n14',
                                 fact_group == 'dx_px_lab' ~ 'n124',
                                 fact_group == 'dx_px_med' ~ 'n134',
                                 fact_group == 'dx_px_med_lab' ~ 'n1234',
                                 fact_group == 'med_lab' ~ 'n23',
                                 fact_group == 'px_lab' ~ 'n24',
                                 fact_group == 'px_med' ~ 'n34',
                                 fact_group == 'px_med_lab' ~ 'n234')) %>%
      mutate(n_pts_site = ifelse(n_pts_site < 11, 0, n_pts_site)) %>%
      mutate(n_pts_site = prettyNum(n_pts_site, big.mark = ','),
             prop_pts_pct = prop_pts * 100,
             venn_display = paste0(n_pts_site, ' \n(', prop_pts_pct, '%)')) %>%
      distinct(fg_venn, venn_display) %>%
      pivot_wider(names_from = fg_venn, values_from = venn_display)
    
    png(filename = paste0(output_directory, 'venn_diagram_', site_list[[i]], '.png'),
        height = 1000,
        width = 1000)
    draw.quad.venn(direct.area = TRUE,
                   area.vector = c(cvg_venn1$area3, cvg_venn1$n34, cvg_venn1$area4, cvg_venn1$n13,
                                   cvg_venn1$n134, cvg_venn1$n1234, cvg_venn1$n234, cvg_venn1$n24,
                                   cvg_venn1$area1, cvg_venn1$n14, cvg_venn1$n124, cvg_venn1$n123,
                                   cvg_venn1$n23, cvg_venn1$area2, cvg_venn1$n12),
                   category = c('Diagnoses', 'Labs', 'Medications', 'Procedures'),
                   fill = c('orange', 'red', 'green', 'blue'),
                   alpha = 0.3,
                   cex = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),
                   cat.cex = c(2, 2, 2, 2))
    dev.off()
    
  }
  
  
}