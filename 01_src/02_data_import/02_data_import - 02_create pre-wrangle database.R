# Packages ----------------------------------------------------------------

library(DBI)
library(RSQLite)
library(dbplyr)

# END PACKAGES
# prerequisite R scripts --------------------------------------------------

# source("./01_src/01_startup/01_initialise.R")

# source("./01_src/02_data_import/02_data_import - 01_test eRecord data.R")

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

DBI::dbWriteTable(con,
  "data_allergies",
  data_allergies,
  overwrite = TRUE
)

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

DBI::dbWriteTable(con,
  "data_clinical_records",
  data_clinical_records,
  overwrite = TRUE
)

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

DBI::dbWriteTable(con,
  "data_cohort_admission",
  data_cohort_admission,
  overwrite = TRUE
)

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

DBI::dbWriteTable(con,
  "data_medications",
  data_medications,
  overwrite = TRUE
)

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

DBI::dbWriteTable(con,
  "data_obs",
  data_obs,
  overwrite = TRUE
)

DBI::dbDisconnect(con)
rm(con)

# END OBS
