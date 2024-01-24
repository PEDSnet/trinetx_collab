
#' Summarize Case-Mix / Signatures for ICD10CM Conditions
#'
#' @param dx_tbl table with condition occurrences. defaults to site CDM condition_occurrence table
#' @param vocab_tbl concept table with ICD codes and vocabulary IDs
#' @param check_string string to identify the check type
#'
#' @return one dataframe that summarizes the count and proportion of rows & patients with 
#'         diagnoses under each ICD10CM top branch
#'          
#'         diagnoses are limited to ONLY ICD10CM -- other ICD flavors are excluded
#'         
 
compute_case_mix <- function(dx_tbl = site_cdm_tbl('condition_occurrence'),
                             vocab_tbl = vocabulary_tbl('concept'),
                             check_string = 'mix'){
  
  ## Isolate ICD10CM Codes
  icd10cm <- vocab_tbl %>% select(concept_id, concept_code, vocabulary_id) %>%
    filter(vocabulary_id == 'ICD10CM') %>% compute_new()
  
  icd_map <- dx_tbl %>%
    select(site, person_id, condition_source_concept_id, condition_source_value) %>%
    inner_join(icd10cm, by = c('condition_source_concept_id' = 'concept_id')) %>%
    compute_new()
  
  ## Site Total Counts
  total_pts_site <- icd_map %>% group_by(site) %>%
    summarise(total_row_site = n(),
              total_pts_site = n_distinct(person_id)) %>%
    compute_new()
  
  ## Sort codes into header groups
  icd_cats <- icd_map %>%
    mutate(icd_num = sql("UNNEST(REGEXP_MATCHES(concept_code, '(?<!\\.|\\d)(\\d+)'))"),
           icd_num = as.numeric(icd_num), 
           icd_header = case_when(grepl('^A', concept_code) | grepl('^B', concept_code) ~ 'A00 - B99',
                                  grepl('^C', concept_code) ~ 'C00 - D49',
                                  grepl('^D', concept_code) & icd_num < 50 ~ 'C00 - D49',
                                  grepl('^D', concept_code) & icd_num >= 50 ~ 'D50 - D89',
                                  grepl('^E', concept_code) ~ 'C00 - E89',
                                  grepl('^F', concept_code) ~ 'F01 - F99',
                                  grepl('^G', concept_code) ~ 'G00 - G99',
                                  grepl('^H', concept_code) & icd_num < 60 ~ 'H00 - H59',
                                  grepl('^H', concept_code) & icd_num >= 60 ~ 'H60 - H95',
                                  grepl('^I', concept_code) ~ 'I00 - I99',
                                  grepl('^J', concept_code) ~ 'J00 - J99',
                                  grepl('^K', concept_code) ~ 'K00 - K95',
                                  grepl('^L', concept_code) ~ 'L00 - L99',
                                  grepl('^M', concept_code) ~ 'M00 - M99',
                                  grepl('^N', concept_code) ~ 'N00 - N99',
                                  grepl('^O', concept_code) ~ 'O00 - O9A',
                                  grepl('^P', concept_code) ~ 'P00 - P96',
                                  grepl('^Q', concept_code) ~ 'Q00 - Q99',
                                  grepl('^R', concept_code) ~ 'R00 - R99',
                                  grepl('^S', concept_code) | grepl('^T', concept_code) ~ 'S00 - T88',
                                  grepl('^U', concept_code) ~ 'U00 - U85',
                                  grepl('^V', concept_code) | grepl('^W', concept_code) | 
                                    grepl('^X', concept_code) | grepl('^Y', concept_code) ~ 'V00 - Y99',
                                  grepl('^Z', concept_code) ~ 'Z00 - Z99')) %>%
    filter(!is.na(icd_header)) %>% compute_new()
  
  ## Site group counts
  summary_site <- icd_cats %>%
    group_by(site, icd_header) %>%
    summarise(n_pts_site = n_distinct(person_id),
              n_row_site = n())
    
  ## Final Summary Table
  final_tbl <- summary_site %>%
      left_join(total_pts_site) %>%
      mutate(prop_row_site = round(as.numeric(n_row_site) / as.numeric(total_row_site), 2),
             prop_pt_site = round(as.numeric(n_pts_site) / as.numeric(total_pts_site), 2)) %>%
      collect_new()
  
  return(final_tbl)
}