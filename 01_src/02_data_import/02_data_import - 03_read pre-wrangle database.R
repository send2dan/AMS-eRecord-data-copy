# Packages ----------------------------------------------------------------

library(DBI)
library(RSQLite)
library(dbplyr)

# END PACKAGES
# prerequisite R scripts --------------------------------------------------

source("./01_src/functions.R")

# source("./01_src/01_startup/01_initialise.R")

# If .db files don't exist/new data needs to be added, run these:
# source("./01_src/02_data_import/02_data_import - 01_test eRecord data.R")
# source("./01_src/02_data_import/02_data_import - 02_create pre-wrangle database.R")

# Allergies ---------------------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_pre_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

.GlobalEnv$data_allergies <- DBI::dbReadTable(
  conn = con,
  "data_allergies"
) |>
  as_tibble()

DBI::dbDisconnect(con)
rm(con)

# END ALLERGIES
# Clinical records --------------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_pre_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

.GlobalEnv$data_clinical_records <- DBI::dbReadTable(
  con,
  "data_clinical_records"
) |>
  as_tibble()

DBI::dbDisconnect(con)
rm(con)

# END CLINICAL RECORDS
# Cohort admission --------------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_pre_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

.GlobalEnv$data_cohort_admission <- DBI::dbReadTable(
  con,
  "data_cohort_admission"
) |>
  as_tibble()

DBI::dbDisconnect(con)
rm(con)

# END COHORT ADMISSION
# Medications -------------------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_pre_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

.GlobalEnv$data_medications <- DBI::dbReadTable(
  con,
  "data_medications"
) |>
  as_tibble()

DBI::dbDisconnect(con)
rm(con)

# END MEDICATIONS
# Obs ---------------------------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "eRecord_data_pre_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

.GlobalEnv$data_obs <- DBI::dbReadTable(
  con,
  "data_obs"
) |>
  as_tibble()

DBI::dbDisconnect(con)
rm(con)

# END OBS
