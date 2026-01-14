# Creating your own database ----------------------------------------------

# If you don’t already have a database, here’s some advice from my experiences setting up and running all of them. SQLite is by far the easiest to get started with. PostgreSQL is not too much harder to use and has a wide range of built-in functions. In my opinion, you shouldn’t bother with MySQL/MariaDB: it’s a pain to set up, the documentation is subpar, and it’s less featureful than Postgres. Google BigQuery might be a good fit if you have very large data, or if you’re willing to pay (a small amount of) money to someone who’ll look after your database.
# MySQL/MariaDB
# In terms of functionality, MySQL lies somewhere between SQLite and PostgreSQL. It provides a wider range of built-in functions. It gained support for window functions in 2018.
# PostgreSQL
# PostgreSQL is a considerably more powerful database than SQLite. It has a much wider range of built-in functions, and is generally a more featureful database.
# BigQuery
# BigQuery is a hosted database server provided by Google. To connect, you need to provide your project, dataset and optionally a project for billing (if billing for project isn’t enabled).
# It provides a similar set of functions to Postgres and is designed specifically for analytic workflows. Because it’s a hosted solution, there’s no setup involved, but if you have a lot of data, getting it to Google can be an ordeal (especially because upload support from R is not great currently). (If you have lots of data, you can ship hard drives!)

# Packages-------------------------------------

library(DBI)
library(RSQLite)
library(dbplyr)

# END PACKAGES
# Read functions ----------------------------------------------------------

# source("./01_src/functions.R")

# Prerequisite R scripts --------------------------------------------------

# source("./01_src/03_wrangle/03_wrangle - data_allergies.R")
# source("./01_src/03_wrangle/03_wrangle - data_clinical_records_wide.R")
# source("./01_src/03_wrangle/03_wrangle - data_cohort_admission.R")
# source("./01_src/03_wrangle/03_wrangle - data_medications_antimicrobials.R")
# source("./01_src/03_wrangle/03_wrangle - data_obs_news2.R")

# Allergies wrangled -------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(), # use SQLite function to progress
  dbname = "eRecord_data_post_wrangle.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

# create data_allergies_wrangled.db database using dbWriteTable ------------------------------------

DBI::dbWriteTable(con,
  "data_allergies_wrangled",
  data_allergies_wrangled,
  overwrite = TRUE
)

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

# create data_clinical_records_wide.db database using dbWriteTable ------------------------------------

DBI::dbWriteTable(con,
  "data_clinical_records_wide",
  data_clinical_records_wide,
  field.types,
  overwrite = TRUE
)

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

# create data_cohort_admission_wrangled.db database using dbWriteTable ------------------------------------

DBI::dbWriteTable(con,
  "data_cohort_admission_wrangled",
  data_cohort_admission_wrangled,
  field.types,
  overwrite = TRUE
)

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

# # create data_medications_antimicrobials.db database using dbWriteTable ------------------------------------

DBI::dbWriteTable(con,
  "data_medications_antimicrobials",
  data_medications_antimicrobials,
  field.types,
  overwrite = TRUE
)

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

# # create data_obs_news2.db database using dbWriteTable ------------------------------------

DBI::dbWriteTable(con,
  "data_obs_news2",
  data_obs_news2,
  field.types,
  overwrite = TRUE
)

DBI::dbDisconnect(con)

# END OBS NEWS2
# read data_allergies_wrangled.db database using dbplyr----------------------------------------------------------

# # # https://github.com/tidyverse/dbplyr
#
# # https://dbplyr.tidyverse.org/articles/dbplyr.html
#
# #All dplyr calls are evaluated lazily, generating SQL that is only sent to the database when you request the data.
#
# con <- DBI::dbConnect(RSQLite::SQLite(),
#                       ":memory:",
#                       #loadable.extensions = TRUE,
#                       #default.extensions = loadable.extensions,
#                       cache_size = NULL,
#                       synchronous = "off",
#                       flags = SQLITE_RWC,
#                       vfs = NULL,
#                       bigint = "integer64", #c("integer64", "integer", "numeric", "character")
#                       extended_types = TRUE) #When TRUE columns of type DATE, DATETIME / TIMESTAMP, and TIME are mapped to corresponding R-classes, c.f. below for details. Defaults to FALSE.) #use the special string ":memory:" which causes SQLite to make a temporary in-memory database.
#
# #check what has been created above
# con
#
# #Our temporary database has no data in it, so we’ll start by copying over data_allergies_wrangled using the convenient copy_to() function. This is a quick and dirty way of getting data into a database and is useful primarily for demos and other small jobs.
# copy_to(con, data_allergies_wrangled)
#
# #Now that we’ve copied the data, we can use tbl() to take a reference to it
# data_allergies_wrangled_test <- tbl(con, "data_allergies_wrangled") |>
#   head() |> #Because you can’t find the last few rows without executing the whole query, you can’t use tail().
#   glimpse()
#
# #The most important difference between ordinary data frames and remote database queries is that your R code is translated into SQL and executed in the database on the remote server, not in R on your local machine. When working with databases, dplyr tries to be as lazy as possible
# data_allergies_wrangled_test |>
#   show_query() #When you print it out, you’ll notice that it mostly looks like a regular tibble. The main difference is that you can see that it’s a remote source in a SQLite database.
#
# #collect() requires that database does some work, so it may take a long time to complete. Otherwise, dplyr tries to prevent you from accidentally performing expensive query operations
# data_allergies_wrangled_test_sir <- data_allergies_wrangled_test |>
#   collect() |> #need to collect() in order to collect data as an object in R
#   glimpse()
#
# DBI::dbDisconnect(con)

# server ------------------------------------------------------------------

# NB Most existing databases don’t live in a file, but instead live on another server. That means in real-life that your code will look more like this:

# con <- DBI::dbConnect(RMariaDB::MariaDB(),
#                       host = "database.rstudio.com",
#                       user = "hadley",
#                       password = rstudioapi::askForPassword("Database password")
# )

# (If you’re not using RStudio, you’ll need some other way to securely retrieve your password. You should never record it in your analysis scripts or type it into the console. Securing Credentials provides some best practices.)
# https://db.rstudio.com/best-practices/managing-credentials

# final 29/4/24
