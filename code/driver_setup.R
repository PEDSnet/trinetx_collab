
#' `Set working directory`
setwd(Sys.getenv('PEDSNET_DATA_REQUEST_ROOT'))

#' `Source relevant files`
source(file.path(getwd(), '/code/cohorts.R'))
source(file.path(getwd(), '/code/cohort_case_mix.R'))
source(file.path(getwd(), '/code/cohort_coverage_overlap.R'))
source(file.path(getwd(), '/code/cohort_dcon.R'))
source(file.path(getwd(), '/code/cohort_fot.R'))
source(file.path(getwd(), '/code/cohort_viz.R'))
source(file.path(getwd(), '/code/cohort_trinetx_anom.R'))

#' `Load necessary packages`
library(argos)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(srcr)
library(lubridate)
library(purrr)

#' `Connect to data`
initialize_session(session_name = 'pedsnet_trinetx_collab',
                   db_conn = Sys.getenv('PEDSNET_DB_SRC_CONFIG_BASE'),
                   is_json = TRUE,
                   cdm_schema = 'dcc_pedsnet',
                   results_schema = 'trinetx_collab')

#' `Build anonymized site map`

# site_names <- read_csv(file.path(config('base_dir'), '/results/site_names.csv'))
# 
# site_map <- site_anon(df = site_names) %>%
#   mutate(siteletter = LETTERS[sitenum],
#          site_anon = paste0('site ', siteletter))
# 
# write_csv(site_map, file = file.path(config('base_dir'), '/results/site_map.csv'))
