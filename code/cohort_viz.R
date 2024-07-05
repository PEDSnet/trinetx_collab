

couplets_viz <- function(process_output,
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


case_mix_viz <- function(process_output){
  
  data_format <- process_output %>%
    mutate(text = paste0('Site: ', site_anon,
                         '\nBranch: ', branch,
                         '\nProportion: ', prop_pts))
  
  r <- ggplot(data_format, aes(y=prop_pts,x=site_anon, fill=site_anon))+
    geom_col_interactive(aes(tooltip = text))+
    geom_hline(aes(yintercept = allsite_median), linetype = 'dotted') +
    #geom_point(aes(y=!!sym(y_col), x=allsite_median), shape=8, size=4, color="black")+
    scale_fill_ssdqa(discrete = TRUE) +
    #scale_y_discrete(limits=rev) +
    facet_wrap(~branch, scales="free_y", ncol = 2)+
    theme_minimal() + 
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'Proportion Patients',
         x = 'ICD10CM Branch',
         color = 'Site',
         title = paste0('Proportion of Patients with Each Fact'),
         subtitle = 'Dotted line represents All-Site Median')
  
  girafe(ggobj = r)
  
}


fot_raw_norm_viz <- function(process_output,
                             site_filter,
                             domain_filter){
  
  ay <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = "Normalized Patients")
  
  plt <- process_output %>%
    filter(site == site_filter,
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

#' Euclidean distance function for testing

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
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
          #text = element_text(size = 25),
          legend.position = 'none') +
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
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
          #text = element_text(size = 25),
          legend.position = 'none') +
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


coverage_overlap_viz <- function(process_output){
  
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