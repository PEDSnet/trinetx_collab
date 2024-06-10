

#' `Case Mix`

casemix_final <- trinetx_check_pp(dat = results_tbl('case_mix_10yr'), group = 'icd_header')

output_tbl(casemix_final, 'case_mix_10yr_pp')

#' `Coverage Overlap`

overlap_final <- trinetx_check_pp(dat = results_tbl('coverage_overlap'), group = 'fact_group') %>%
  select(site, fact_group, n_pts_site, n_pts_all, prop_pts_site, prop_pts_all,
         total_pts_site, total_pts_all)

output_tbl(overlap_final, 'coverage_overlap_pp')

#' `FOT`

fot_list <- fot_check('row_pts',tblx=results_tbl('fot_output'))
output_list_to_db(fot_list, append = FALSE)

fot_output_distance <- check_fot_all_dist(fot_list$fot_heuristic)
output_tbl(fot_output_distance,
           'fot_output_distance',
           indexes=list('check_name'))

#' `Domain Concordance`
dcon_output_pp <- apply_dcon_pp(dcon_tbl=results_tbl('dcon_output'),
                                byyr=FALSE,
                                strict = FALSE)
output_tbl(dcon_output_pp,
           name='dcon_output_pp')

#' `Domain Concordance --- Conservative`
# dcon_output_cons_pp <- apply_dcon_pp(dcon_tbl=results_tbl('dcon_output_strict'),
#                                 byyr=FALSE,
#                                 strict = TRUE)
# output_tbl(dcon_output_cons_pp,
#            name='dcon_output_strict_pp')

