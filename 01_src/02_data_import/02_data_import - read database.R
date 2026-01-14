# Packages -------------------------------------

library(tidyverse)
library(DBI)
library(RSQLite)
library(dbplyr)
library(dotenv)

# END PACKAGES
# Read functions ----------------------------------------------------------

# source("./01_src/functions.R")

# Prerequisite R scripts --------------------------------------------------

# # This script reads in the following objects using DBI, RSQLite and dbplyr by reading in a series of database files.
# data_allergies_wrangled
# data_clinical_records_wide
# data_cohort_admission_wrangled
# data_medications_antimicrobials
# data_obs_news2

# If .db files don't exist/new data has been added, run this:
# source("./01_src/02_data_import/02_data_import - create database.R")

# Allergies wrangled -------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_post_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

# read data_allergies_wrangled.db database using dbReadTable -----------------------------------------

data_allergies_wrangled <- DBI::dbReadTable(con, "data_allergies_wrangled") |>
  as_tibble() |>
  mutate(beta_lac_allergy = as.logical(beta_lac_allergy))

data_allergies_wrangled |>
  dplyr::glimpse()

# #correct format of vectors in database, e.g. use AMR::as.sir() and lubridate::receive_date()
# data_allergies_wrangled <- data_allergies_wrangled |>
#   dplyr::mutate(across(where(AMR::is_sir_eligible), AMR::as.sir),
#                 across(c(date_of_birth, receive_date), lubridate::as_date)) |>
#   dplyr::glimpse()

DBI::dbDisconnect(con)

# END ALLERGIES WRANGLED
# Clinical records wide -------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_post_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

# read data_clinical_records_wide.db database using dbReadTable -----------------------------------------

data_clinical_records_wide <- DBI::dbReadTable(con, "data_clinical_records_wide") |>
  as_tibble()

data_clinical_records_wide |>
  dplyr::glimpse()

DBI::dbDisconnect(con)

# END CLINICAL RECORDS WIDE
# Cohort admission wrangled -------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_post_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

# read data_cohort_admission_wrangled.db database using dbReadTable -----------------------------------------

data_cohort_admission_wrangled <- DBI::dbReadTable(con, "data_cohort_admission_wrangled") |>
  as_tibble()

data_cohort_admission_wrangled |>
  dplyr::glimpse()

DBI::dbDisconnect(con)

# END COHORT ADMISSION WRANGLED
# Antimicrobials -------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_post_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

# read data_medications_antimicrobials.db database using dbReadTable -----------------------------------------

data_medications_antimicrobials <- DBI::dbReadTable(con, "data_medications_antimicrobials") |>
  as_tibble() |>
  mutate(
    medication_beta_lac = as.logical(medication_beta_lac)
  )

data_medications_antimicrobials |>
  dplyr::glimpse()

data_medications_antimicrobials |>
  select(ends_with("time")) |>
  as_tibble()

DBI::dbDisconnect(con)

# END ANTIMICROBIALS
# Obs news2 -------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_post_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

# read data_obs_news2.db database using dbReadTable -----------------------------------------

data_obs_news2 <- DBI::dbReadTable(con, "data_obs_news2") |>
  as_tibble()

data_obs_news2 |>
  dplyr::glimpse()

DBI::dbDisconnect(con)

# END OBS NEWS2
# Audit period ------------------------------------------------------------

# Determine audit dates by extracting dates from all file names in folder containing data

load_dot_env()

folder_path <- str_c(Sys.getenv("SHARED_REPO_PATH"), "02_data/antimicrobial_audit/")
# folder_path <- "C:/Users/Weiandd2/Documents/AMS-eRecord-data2/02_data_REDUNDANT/antimicrobial_audit"

file_names <- list.files(folder_path)
dates <- str_extract_all(file_names, "\\d{2}-\\d{2}-\\d{2}") |> unlist()
audit_dates <- dmy(dates)

audit_dates

print(paste("Number of dates audited:", length(audit_dates)))
