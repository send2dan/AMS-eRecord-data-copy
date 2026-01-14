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
      -- T1.Receive_Time,
      -- T1.Collection_Date,
      -- T1.Collection_Time,
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
      T4.Result,
      
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
    
    WHERE
    (T4.Order_code = 'CRIRES' OR T4.Order_code LIKE 'BCUL%')
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

receive_date_start <- today() - days(14) 
receive_date_end <- today() 

# Execute the function
data_crires_bc <- extract_data(con, receive_date_start, receive_date_end)

# write_csv(data_crires_bc, "data_crires_bc.csv")


# check output ------------------------------------------------------------

data_crires_bc |> 
  glimpse()

data_crires_bc |> 
  count(order_code, result) |> 
  arrange(desc(order_code))
# # A tibble: 16 × 3
# order_code result     n
# <chr>      <chr>  <int>
# 1 CRIRES     0          6
# 2 CRIRES     1        471
# 3 CRIRES     NA         4

crires_specimen_no <- data_crires_bc |> 
  filter(order_code == "CRIRES" & result == 1) |> 
  pull(specimen_no)

# keep only specimens where CRIRES == 1 -----------------------------------

# data_crires_bc |> 
#   filter(specimen_no %in% crires_specimen_no) |> 
#   count(order_code, result)

data_crires_bc <- data_crires_bc |> 
  filter(specimen_no %in% crires_specimen_no)


# check output ------------------------------------------------------------

data_crires_bc |> 
  glimpse()

data_crires_bc |> 
  count(order_code, test_code, result)

# disconnect from server --------------------------------------------------

dbDisconnect(con)

