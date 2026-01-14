# 1 Packages --------------------------------------------------------------

library(odbc)
library(DBI)
library(tidyverse)
library(janitor)
library(dotenv)
library(bench)

# 2 Read in data ----------------------------------------------------------

dotenv::load_dot_env()

# Connect to database
tryCatch({
  con <- dbConnect(
    drv = odbc(),
    dsn = Sys.getenv('GENETICS_DSN'),
    sslmode = "require"
  )
  
  if (!dbIsValid(con)) {
    stop("Database connection is not valid")
  }
  
  print("Database connection established successfully")
}, error = function(e) {
  stop("Failed to connect to database: ", e$message)
})

# 2.1 Genetics molecular investigation data ----

tryCatch({
  bench_time({
    # Query with correct schema (emsliej)
    data_mtrnr1 <- dbGetQuery(
      con,
      "SELECT DISTINCT 
        inv.TestID, 
        inv.Investigation, 
        p.NHSnumber, 
        p.PAS_ID AS MRN, 
        s.Sample_Number, 
        st.ActivationDate, 
        s.SampleType, 
        s.HospCode, 
        st.SampleTestID, 
        st.Result, 
        st.OutcomeDate
      FROM emsliej.tblMolSampleTesting st
      INNER JOIN emsliej.tblMolSample s ON st.Sample_Number = s.Sample_Number
      INNER JOIN emsliej.tblPatient p ON s.PID = p.PID
      INNER JOIN emsliej.tblMolInvestigation_Codes inv ON st.InvestigationCode = inv.ShortCode
      WHERE inv.ShortCode IN ('MT_RNR1 A1555G R65_1', 'MT_RNR1 C1494T R65_1', 'MT_RNR1 T1095C R65_1');
      -- WHERE inv.ShortCode IN ('MT_RNR1 A1555G R65_1');
      -- WHERE inv.ShortCode IN ('MT_RNR1 C1494T R65_1');
      -- WHERE inv.ShortCode IN ('MT_RNR1 T1095C R65_1');
      "
    ) |>
      as_tibble() |>
      clean_names()
  })
  
  # Validate data extraction
  if (nrow(data_mtrnr1) == 0) {
    warning("No data returned from query - check if records exist for the specified criteria")
    
    # Let's check if the ShortCode exists at all
    shortcode_check <- dbGetQuery(con, "
      SELECT DISTINCT ShortCode 
      FROM emsliej.tblMolInvestigation_Codes 
      WHERE ShortCode LIKE '%MT_RNR1%' OR ShortCode LIKE ('%A1555G%', '%C1494T%', '%T1095C')
      -- WHERE ShortCode LIKE '%MT_RNR1%' OR ShortCode LIKE '%A1555G%'
      -- WHERE ShortCode LIKE '%MT_RNR1%' OR ShortCode LIKE '%C1494T%'
      -- WHERE ShortCode LIKE '%MT_RNR1%' OR ShortCode LIKE '%T1095C'
    ")
    
    if (nrow(shortcode_check) > 0) {
      cat("Found similar ShortCodes:\n")
      print(shortcode_check)
    } else {
      cat("No similar ShortCodes found. Checking first 10 ShortCodes:\n")
      sample_codes <- dbGetQuery(con, "
        SELECT TOP 10 ShortCode, Investigation 
        FROM emsliej.tblMolInvestigation_Codes
      ")
      print(sample_codes)
    }
    
  } else {
    print(paste("Genetics data extracted successfully -", nrow(data_mtrnr1), "records retrieved"))
    
    # Display basic info about the extracted data
    cat("\nData summary:\n")
    print(glimpse(data_mtrnr1))
  }
  
}, error = function(e) {
  stop("Error executing query: ", e$message)
})

# Disconnect from server --------------------------------------------------
tryCatch({
  dbDisconnect(con)
  print("Database connection closed successfully")
}, error = function(e) {
  warning("Error closing database connection: ", e$message)
})

# View(data_mtrnr1)

# data_mtrnr1 |>
#   skimr::skim()

glimpse(data_mtrnr1)
