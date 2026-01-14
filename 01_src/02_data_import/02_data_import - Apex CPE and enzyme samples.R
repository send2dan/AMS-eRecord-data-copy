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
      T1.Location_Code,
      T1.Date_of_Birth,
      T1.Patient_Full_Name,
      T1.Hospital_Number,
      T1.Specimen_No,
      T1.Specimen_Type_Code,
      T1.Collection_Date,
      T1.Collection_Time,
      T1.Receive_Date,
      T1.Receive_Time,
      -- T1.Consultant,
      -- T1.Order_Comment,
      
      -- Patient demographics
      T2.New_NHS_Number,
      T2.Sex,
      
      -- Order information
      -- T3.Reason_for_Req_Line1,
      -- T3.Reason_for_Req_Line2,
      -- T3.Reason_for_Req_Line3,
      
      -- Test information
      T4.Order_code,
      T4.Test_code,
      T4.Result,
      T4.Result_Expansion,
      T4.Test_Type, -- I = culture / T = molecular
      
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
    T4.Order_code IN ('CRIRES', 'CPEPC', 'CPESCR', 'KPC', 'NDM', 'OXA48', 'IMP1', 'VIM')
    AND
    T1.Receive_Date between {d '",
    receive_date_start,
    "'} AND {d '",
    receive_date_end,
    "'}
    
    ORDER BY T1.Receive_Date DESC, T1.Specimen_No"
  )
  
  # lateral fl DETECTED <- organism_code == IMA & quantity == DET
  # KPC DETECTED <- = Org == KPC & quantity == DET
  # CPE                            DETECTED <- result = DET
  
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

receive_date_start <- today() - weeks(24)
  # as_date("2025-07-01")
  # today() - days(31)
receive_date_end <- today()
  # today() 

# Execute the function
data_cpe <- extract_data(con, receive_date_start, receive_date_end)

# write_csv(data_crires_bc, "data_crires_bc.csv")

# check date range --------------------------------------------------------

data_cpe |> 
  reframe(date_range = range(receive_date, na.rm = TRUE))

# lubridate::as.duration(lubridate::interval(lubridate::date("2025-03-25"), lubridate::date("2025-09-10")))

lubridate::as.duration(lubridate::interval(lubridate::date(receive_date_start), lubridate::date(receive_date_end)))

# disconnect from server --------------------------------------------------

dbDisconnect(con)


