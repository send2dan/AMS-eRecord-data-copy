# https://github.com/send2dan/AMS-eRecord-data/issues/142

# 1 Packages --------------------------------------------------------------

library(odbc)
library(DBI)
library(tidyverse)
library(janitor)
library(dotenv) # for retrieving connection information
library(bench) # for timing SQL queries

# 2 Read in data ----------------------------------------------------------

dotenv::load_dot_env()

#LIMSMart
con <- dbConnect(
  drv = odbc(),
  driver = Sys.getenv("DW_DRIVER"),
  server = Sys.getenv("DW_SERVER"),
  database = Sys.getenv("DW_DATABASE"),
  trusted_connection = TRUE
)

# Extract data ----

extract_data <- function(con, start_date, end_date) {
  
  # Validate inputs
  if (!inherits(con, "DBIConnection")) {
    stop("Invalid database connection")
  }
  
  if (start_date > end_date) {
    stop("Start date must be before end date")
  }
  
  # Build SQL query
  sql_query <- str_c(
    "SELECT 
      -- Patient and specimen information
      T1.Receive_Date,
      T1.Receive_Time,
      T1.Collection_Date,
      T1.Collection_Time,
      T1.Specimen_No,
      T1.Specimen_Type_Code,
      T1.Location_Code,
      T1.Date_of_Birth,
      T1.Patient_Full_Name,
      T1.Hospital_Number,
      T1.Consultant,
      T1.Order_Comment,
      
      -- Patient demographics
      T2.New_NHS_Number,
      T2.Sex,
      
      -- Order information
      T3.Reason_for_Req_Line1,
      T3.Reason_for_Req_Line2,
      T3.Reason_for_Req_Line3,
      
      -- Test information
      T4.Order_code,
      T4.Test_code,
      
      -- Organism information (if needed)
      T5.Organism_Code,
      T5.Quantifier,
      
      -- Antibiotic columns
      T6.Antibiotic_Code,
      T6.Interpreted_Sensitivity_Result
      
    FROM dbo.Micro_Results_File T1
    
    INNER JOIN dbo.Sub_Master_File T2 
      ON T1.Internal_Patient_No = T2.Internal_Patient_Number
    
    INNER JOIN dbo.Orders_file T3 
      ON T1.Order_No = T3.Order_Number
    
    INNER JOIN dbo.Micro_Tests T4 
      ON T1.ISRN = T4.Micro_Results_File
    
    LEFT JOIN dbo.Micro_Organisms T5 
      ON T4.Micro_Tests = T5.Micro_Tests
    
    LEFT JOIN dbo.Micro_Antibiotics T6 ON T5.Micro_Organisms = T6.Micro_Organisms
    
    WHERE T5.Quantifier IN ('I', 'NI')
    AND 
    T5.Organism_Code IN ('QMRSA', 'MRSA', 'MRSAN', 'STMRSA')
    AND
    T1.Receive_Date between {d '",
    receive_date_start,
    "'} AND {d '",
    receive_date_end,
    "'}
    
    ORDER BY T1.Receive_Date DESC, T1.Specimen_No"
  )
  
  # Execute query with error handling
  tryCatch({
    bench_time({
      rs <- dbSendQuery(con, sql_query)
      
      data_extract <- dbFetch(rs) |> 
        as_tibble() |> 
        clean_names() |> 
        distinct() # Remove duplicate rows
      
      dbClearResult(rs)
    })
    
    # Log success
    cat("Data extracted successfully:", nrow(data_extract), "records\n")
    
    return(data_extract)
    
  }, error = function(e) {
    # Clean up resources on error
    if (exists("rs") && dbIsValid(rs)) {
      dbClearResult(rs)
    }
    stop("Error extracting data: ", e$message)
  })
}

# Configuration ----

receive_date_start <- today() - days(24*7) #24 weeks ago
receive_date_end <- today() 

# Execute the function
mrsa_historical_data <- extract_data(con, receive_date_start, receive_date_end)

mrsa_historical_data |> 
  distinct(specimen_no, .keep_all = TRUE) |>
  group_by(collection_date, receive_date) |> 
  count(collection_date, receive_date) |> 
  arrange(collection_date) 

# mrsa_historical_data |> 
#   distinct(specimen_no, .keep_all = TRUE) |>
#   skimr::skim()

mrsa_historical_data |> 
  distinct(specimen_no, order_code, test_code, organism_code, quantifier, antibiotic_code, interpreted_sensitivity_result)

# keep only specimens where organism "I" ----------------------------------
# i.e. filter out/remove quantifier == "NI"

mrsa_historical_data_pos <- mrsa_historical_data |> 
  filter(!quantifier == "NI")


# write to database -------------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "mrsa_historical_data.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

DBI::dbWriteTable(con,
                  "mrsa_historical_data",
                  mrsa_historical_data,
                  overwrite = TRUE
)

DBI::dbDisconnect(con)
rm(con)

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

# read mrsa_historical_data.db ---------------------------------------------------------------

con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "mrsa_historical_data.db",
  cache_size = NULL,
  synchronous = "off",
  flags = SQLITE_RWC,
  vfs = NULL,
  bigint = "integer64",
  extended_types = TRUE
)

.GlobalEnv$mrsa_historical_data <- DBI::dbReadTable(
  conn = con,
  "mrsa_historical_data"
) |>
  as_tibble()

DBI::dbDisconnect(con)
rm(con)


