

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