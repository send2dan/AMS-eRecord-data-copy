# # prerequisites ----------------------------------------------------------

source("./01_src/01_startup/01_initialise.R")

source("./01_src/functions.R")

# add most recent data (single file only) ---------------------------------

source("./01_src/02_data_import/02_data_import - 01_extract eRecord data from data warehouse.R")

# create pre-wrangle database ---------------------------------------------

source("./01_src/02_data_import/02_data_import - 02_create pre-wrangle database.R")

# # read pre-wrangle database -----------------------------------------------
# 
# source("./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R")

# wrangling daisychain ----------------------------------------------------------

source("./01_src/03_wrangle/03_wrangle - data_allergies.R")

data_allergies_wrangled |>
  glimpse()

source("./01_src/03_wrangle/03_wrangle - data_cohort_admission.R")

data_cohort_admission_wrangled |>
  glimpse()

source("./01_src/03_wrangle/03_wrangle - data_obs_news2.R")

data_obs_news2 |>
  glimpse()

source("./01_src/03_wrangle/03_wrangle - data_medications_antimicrobials.R")

data_medications_antimicrobials |>
  count(mrn, order_name_c, order_name)

source("./01_src/03_wrangle/03_wrangle - data_clinical_records_wide.R")

data_clinical_records_wide |>
  glimpse()

# create database (post-wrangled) -----------------------------------------------------------

source("./01_src/02_data_import/02_data_import - create database.R")

# read database -----------------------------------------------------------

source("./01_src/02_data_import/02_data_import - read database.R")

data_medications_antimicrobials |>
  glimpse()

source("./01_src/03_wrangle/03_wrangle - merge_data_to_create_antimicrobial_pool.R")

data_antimicrobial_pool |>
  glimpse()

# render reports ----------------------------------------------------------

source("./01_src/ams_eRecord_render_reports.R")

# add reports to website --------------------------------------------------

source("./01_src/ams_eRecord_upload_HTML_files_to_datascience_website.R")

# Generate draft emails ---------------------------------------------------

# This script should not send anything, simply create drafts.
source("./01_src/ams_eRecord_compose_and_send_emails_2.R")
