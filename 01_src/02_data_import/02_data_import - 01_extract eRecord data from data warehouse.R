
# 0 Packages --------------------------------------------------------------

library(odbc)
library(DBI)
library(tidyverse)
library(janitor)
library(here)
library(dotenv)

i_am("01_src/02_data_import/02_data_import - 01_extract eRecord data from data warehouse.R")

# 1 Create connection to data warehouse -----------------------------------

dotenv::load_dot_env()

#LIMSMart
con <- dbConnect(
  drv = odbc(),
  driver = Sys.getenv("DW_DRIVER"),
  server = Sys.getenv("DW_SERVER"),
  database = Sys.getenv("DW_DATABASE"),
  trusted_connection = TRUE
)
# 
# # names of all tables -----------------------------------------------------
# 
# available_tables_to_query <- dbListTables(con) |> 
#   as_tibble()
# 
# available_tables_to_query
# 
# # To List Columns for All Tables ---------
# 
# all_columns_df <- dbGetQuery(con,
#                              "SELECT TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME 
#    FROM INFORMATION_SCHEMA.COLUMNS
#    ORDER BY TABLE_SCHEMA, TABLE_NAME, ORDINAL_POSITION"
# ) |> 
#   as_tibble()
# 
# all_columns_df |> 
#   print(n = 400)
# 
# # query to see what tables exist and are visible from connection ---------
# 
# all_tables_df <- dbGetQuery(con, "SELECT * FROM INFORMATION_SCHEMA.TABLES")
# 
# all_tables_df

# Cohort and admission ---------------------------------------------------

data_cohort_admission <- dbSendQuery(
  con,
  "SELECT 
  *
  FROM Cohort"
) |> 
  dbFetch() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(import_date = as.character(Sys.Date())) |> 
  mutate(across(everything(), as.character))

data_cohort_admission |> 
  mutate(admission_dt = ymd(admission_dt)) |> 
  count(admission_dt) |> 
  arrange(desc(admission_dt)) |> 
  ggplot() +
  geom_col(aes(admission_dt, n))

data_cohort_admission |> 
  mutate(admission_dt = ymd(admission_dt)) |> 
  count(admission_dt) |> 
  arrange(desc(admission_dt))

admission_date_range <- data_cohort_admission |> 
  mutate(admission_dt = ymd(admission_dt)) |> 
  reframe(admission_data_range = range(admission_dt))

cat("Earliest admission date")
Sys.sleep(2) #check dates are OK 

admission_date_range[[1]][1]
Sys.sleep(2) #check dates are OK 

cat("Latest admission date")
Sys.sleep(2) #check dates are OK 

admission_date_range[[1]][2]
Sys.sleep(2) #check dates are OK 

cat("Check dates are OK before proceeding")
Sys.sleep(2) #check dates are OK 

# Allergies ---------------------------------------------------------------

data_allergies <- dbSendQuery(
  con,
  "SELECT 
    ENCNTR_ID,
    MRN,
    Substance,
    Substance_Type,
    Reaction_Class,
    Reaction_Status,
    Severity,
    Source_of_Info,
    Active_Status,
    Allergy_Comment,
    CREATED_DT_TM,
    ONSET_DT_TM,
    BEG_EFFECTIVE_DT_TM,
    END_EFFECTIVE_DT_TM,
    REVIEWED_DT_TM,
    REACTION_FT_DESC -- problematic column LAST**
  FROM Allergies
  "
) |> 
  dbFetch() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(import_date = as.character(Sys.Date())) |> 
  mutate(across(everything(), as.character)) |> 
  relocate(reaction_ft_desc, .after = active_status)

# **
# RE: nanodbc/nanodbc.cpp:3182: 07009
# [Microsoft][ODBC SQL Server Driver]Invalid Descriptor Index 

# Clinical records --------------------------------------------------------

data_clinical_records <- dbSendQuery(
  con,
  "SELECT 
  *
  FROM ClinicalRecords"
) |> 
  dbFetch() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(import_date = as.character(Sys.Date())) |> 
  mutate(across(everything(), as.character)) |>
  # remove "HARS weight" and "HARS height" as these don't really feature in the dataset
  filter(!str_detect(event_type, "HAR.*"))

# Medications -------------------------------------------------------------

data_medications <- dbSendQuery(
  con,
  "SELECT 
  *
  FROM Meds"
) |> 
  dbFetch() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(import_date = as.character(Sys.Date())) |> 
  mutate(across(everything(), as.character))

# Obs ---------------------------------------------------------------------

data_obs <- dbSendQuery(
  con,
  "SELECT 
  *
  FROM Obs"
) |> 
  dbFetch() |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(import_date = as.character(Sys.Date())) |> 
  mutate(across(everything(), as.character))

# disconnect from server --------------------------------------------------

dbDisconnect(con)
