
#' List of table inputs for `check_coverage_overlap` function
#' 
#' The input list of the function should contain one list item per domain of interest. Each
#' list item should contain the fact table of the domain of interest in the first position, and
#' a string label for that patient group in the second position
#' 

cohort_tbls <- list(
  'dx' = list(site_cdm_tbl('condition_occurrence'), 'dx'),
  'px' = list(site_cdm_tbl('procedure_occurrence'), 'px'),
  'med' = list(site_cdm_tbl('drug_exposure'), 'med'),
  'lab' = list(site_cdm_tbl('measurement_labs'), 'lab')
)