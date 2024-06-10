

trinetx_anom_detect <- function(dat,
                                var_col,
                                grp_vars = c('site')){
  
  id_outliers <- dat %>%
    group_by(!!!syms(grp_vars)) %>%
    mutate(q3 = quantile(!!sym(var_col), 0.75),
           q1 = quantile(!!sym(var_col), 0.25),
           iqr_val = q3 - q1,
           iqr_mult = iqr_val * 1.5,
           upper_lim = q3 + iqr_mult,
           lower_lim = q1 - iqr_mult,
           anomaly_yn = case_when(!!sym(var_col) > upper_lim ~ 'upper outlier',
                                    !!sym(var_col) < lower_lim ~ 'lower outlier',
                                  TRUE ~ 'not outlier'))
  
  severity_score <- id_outliers %>%
    filter(anomaly_yn == 'upper outlier' | anomaly_yn == 'lower outlier') %>%
    mutate(lim_range = upper_lim - lower_lim,
           dist_range = ifelse(anomaly_yn == 'upper outlier', !!sym(var_col) - upper_lim,
                               lower_lim - !!sym(var_col)),
           severity_score = dist_range / lim_range)
  
  total_scores <- id_outliers %>%
    left_join(severity_score) %>%
    group_by(!!!syms(grp_vars)) %>%
    mutate(weight_val = mean(!!sym(var_col)) / 100,
           weighted_score = severity_score * weight_val) %>%
    ungroup() %>%
    group_by(site_anon) %>%
    mutate(site_score = sum(weighted_score, na.rm = TRUE)) %>%
    select(!!!syms(grp_vars), !!sym(var_col), iqr_val, upper_lim, lower_lim, anomaly_yn,
           severity_score, weighted_score, site_score)
  
  
}



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
  
  plt<-ggplot(dat_to_plot, aes(x=site_anon, y=!!sym(grp_col), text=text, color=!!sym(var_col),shape = anomaly_yn))+
    geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'), aes(size = iqr_val)) + 
    geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn != 'not outlier'), aes(size = severity_score)) + 
    scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_shape_manual(values=c('upper outlier' = 24,
                                'not outlier' = 20,
                                'lower outlier' = 25))+
    #scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=60)) +
    labs(title = paste0('Anomaly Detection per ', grp_col)) +
    guides(color = guide_colorbar(title = 'Proportion'),
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