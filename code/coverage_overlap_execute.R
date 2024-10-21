
#' List of table inputs for `check_coverage_overlap` function
#' 
#' The input list of the function should contain one list item per domain of interest. Each
#' list item should contain:
#' (1) the fact table of the domain of interest
#' (2) the date column that should be used to filter to the relevant time period
#' (3) a string label for that patient group
#' 

cohort_tbls <- list(
  'dx' = list(cdm_tbl('condition_occurrence'), 'condition_start_date', 'dx'),
  'px' = list(cdm_tbl('procedure_occurrence'), 'procedure_date', 'px'),
  'med' = list(cdm_tbl('drug_exposure'), 'drug_exposure_start_date','med'),
  'lab' = list(cdm_tbl('measurement_labs'), 'measurement_date','lab')
)