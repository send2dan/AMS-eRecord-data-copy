
# Load packages -----------------------------------------------------------

library(excel.link) # use this package to open a password protected file
library(janitor) # to use clean_names()
library(tidyverse) # to use str_c() ...
library(here)

# # citedrive ---------------------------------------------------------------
# 
# download.file("https://api.citedrive.com/bib/46d6b4c4-5461-4754-9db4-1a0a26f43ed9/references.bib?x=eyJpZCI6ICI0NmQ2YjRjNC01NDYxLTQ3NTQtOWRiNC0xYTBhMjZmNDNlZDkiLCAidXNlciI6ICIyNzk0IiwgInNpZ25hdHVyZSI6ICI5NDQwN2E3ZDkzZDQ1NzU4NGFkZDY2ZTRjNmYxN2FkNzk0MTkzNTA5ZjkzOTRkNWRlMDE3ZDQzZDYxZmU3YWJiIn0=/bibliography.bib", "citedrive.bib")

# Open password protected excel file using Sys.getenv() -------------------------

open_excel_password_prot <- function(
    data,
    data_folder_name = 'antimicrobial_audit/',
    sheet, 
    password
    ) {
  excel.link::xl.read.file(filename = paste0(
    Sys.getenv('SHARED_REPO_PATH'),
    '02_data/',
    data_folder_name,
    data
  ),
  password = Sys.getenv(password),
  xl.sheet = sheet
  ) |>
    clean_names() |> 
    as_tibble()
}

# Open excel file with no password ----------------------------------------

open_excel <- function(data, is_audit = FALSE, sheet) {
  excel.link::xl.read.file(filename=paste0(Sys.getenv('SHARED_REPO_PATH'),
                               '02_data/',
                               if_else(is_audit, "antimicrobial_audit/", ""),
                               data),
               xl.sheet=sheet) |>
    clean_names() |> 
    as_tibble()
}

# # Test function
# 
# Sys.getenv("SAMPLE_DATASHEET_KEY")
#   
# test_2024_02_06 <- open_excel_password_prot(
#   data="Sample dataset output 06-02-24.xlsx",
#   password="SAMPLE_DATASHEET_KEY",
#   sheet="Cohort and admission details")
# 
# Sys.getenv("SAMPLE_DATASHEET_KEY_2")
# 
# test_2024_03_26 <- open_excel_password_prot(
#   data="Sample dataset output 26-03-24.xlsx",
#   password="SAMPLE_DATASHEET_KEY_2",
#   sheet="Cohort and admission details")


# frequency poly ----------------------------------------------------------

# Define a function to create the plot for each variable
plot_freqpoly <- function(data, variable) {
  x_var <-  variable
  
  ggplot(data, aes(.data[[x_var]])) +
    geom_freqpoly() +
    labs(title = variable, x = variable, y = "Frequency")
}

# DT:datatable ------------------------------------------------------------

# Use DT:datatable to embed an interactive table with export options in an R session or markdown document

# copied from github: jbisanz/qiime2R/R/interactive_table.R
# https://rstudio.github.io/DT/options.html ... "DataTables has a large number of initialization options, which make it very flexible to customize the table."

interactive_table <- function(table, 
                              nrow = 5, 
                              not_as_factor = c("mrn", 
                                                "patient"
                                                ),
                              autoWidth = FALSE
) {
  # mutate(across(where(is.character) using forcats::as_factor, avoiding items in "not_as_factor"
  table <- table |> 
    mutate(across(where(is.character) 
                  & !matches(not_as_factor), 
                  forcats::as_factor))
  
  # configure table settings
  dtable <- DT::datatable(table, 
                          extensions = 'Buttons', 
                          filter = "top",
                          options = list(
                            pageLength = nrow, #nrow dictates pageLength,
                            #lengthMenu = c(5, 10, 15, 20),
                            # initComplete = JS(
                            #   "function(settings, json) {",
                            #   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});", #we use the initComplete callback function in options to change the background color of the table header to black after the initialization:
                            #   "}"
                            dom = 'Bfrtip', 
                            buttons = c('copy',
                                        'csv',
                                        #'pdf',
                                        'excel'
                                        ),
                            autoWidth = autoWidth,
                            searchHighlight = TRUE)
  )
  
  return(dtable)
}


# round to the nearest 40 -------------------------------------------------

# Function to round to the nearest 40
round_to_nearest_40 <- function(number) {
  rounded_decimal <- round(number / 40)
  rounded_number <- rounded_decimal * 40
  return(rounded_number)
}

# Source a section of a script --------------------------------------------

# primarily for reading in database tables into the global environment
source_section <- function(file, start_string, end_string) {
  script_connection <- file(file,
                            open = "r"
  )
  
  script_lines <- readLines(script_connection)
  close(script_connection)
  
  section_to_source <- script_lines[grep(start_string, script_lines, fixed = TRUE):grep(end_string, script_lines, fixed = TRUE)]
  
  source(textConnection(section_to_source))
}

# General wrangling -------------------------------------------------------

add_observations_history_number <- function(data,
                                            sort_order_cols,
                                            observation_group_cols,
                                            history_col_name
                                            ) {
  data |> 
    arrange(pick({{sort_order_cols}})) |>
    group_by(across({{observation_group_cols}})) |> 
    mutate(!!history_col_name := n() - row_number() + 1) |> # Earliest records have the highest number, most recent record = 1
    ungroup()
}

# widen_date_and_dttm_columns <- function(data,
#                                         excluded_cols = NULL
# ) {
#   # Create a list of relevant columns
#   date_and_dttm_cols <- colnames(data[,sapply(data, \(x) is.Date(x) | is.POSIXct(x) | is.POSIXt(x))]) |>
#     setdiff({{excluded_cols}})
# 
#   # widen each column one at a time
#   for (col in date_and_dttm_cols) {
#     data <- data |>
#       group_by(across(-c({{col}}))) |> # using an external vector here is apparently deprecated, but I am unsure how to update the code for the same effect.
#       mutate(duplicate_number = str_c({{col}}, "_", row_number())) |>
#       pivot_wider(names_from = duplicate_number,
#                   values_from = {{col}}
#       ) |>
#       ungroup()
#   }
#   
#   # cols that don't widen by more than 1 column can be renamed back to how they were originally.
#   if("earliest_import_date" %in% date_and_dttm_cols) {
#     data <- data |> 
#       rename(earliest_import_date = earliest_import_date_1)
#     
#     return(data)
#   } else {
#     return(data)
#   }
# }

# penicillins and cephalosporins ------------------------------------------

list_of_beta_lacs <- AMR::antimicrobials |> 
  filter(stringr::str_detect(group, stringr::regex(ignore_case = TRUE, 
                                 "ceph.*|penic.*|Carbapenems"))) |> 
  pull(name) |>
  append(c("Penicillin", "Cephalosporin", "Carbapenem", "Monobactam", "Co-amoxiclav")) |> # add the classes which are missing from AMR::antimicrobials
  stringr::str_flatten("|")

#########
# create function to create charts ----------------------------------------
#########

# I want to create a function out of the above, so I can filter data_cohort_admission_mrsa_perc_compliance by:
### best performing wards (i.e. highest MRSA screening compliance, above Y percent) with >X eligible patients 
### worst performing wards (i.e. lowest MRSA screening compliance, below Y percent) with >X eligible patients

# function to create MRSA screening compliance by ward chart --------------

create_mrsa_compliance_chart <- function(data, 
                                         min_eligible_patients = 0,
                                         min_compliance_pct = 0,
                                         max_compliance_pct = 100,
                                         performance_title = " ") {
  
  filtered_data <- data |>
    filter(mrsa_screen_n >= min_eligible_patients,
           mrsa_screen_compliant_perc >= min_compliance_pct,
           mrsa_screen_compliant_perc <= max_compliance_pct)
  
  filtered_data |>
    arrange(desc(mrsa_screen_compliant_perc)) |>
    ggplot() +
    geom_col(aes(fct_reorder(current_ward, mrsa_screen_compliant_perc), 
                 mrsa_screen_compliant_perc),
             fill = "steelblue") +
    geom_point(aes(fct_reorder(current_ward, mrsa_screen_compliant_perc), 
                   mrsa_screen_n),
               color = "darkred", size = 3) +
    scale_y_continuous(
      name = "Percentage of eligible patients screened (%)",
      sec.axis = sec_axis(~., name = "Number of eligible patients (n)")
    ) +
    labs(
      x = "Ward",
      title = glue::glue("MRSA Screening Compliance by Ward",  {{performance_title}}, " (min. ", {{min_eligible_patients}}, " eligible patients)"),
      subtitle = paste0("<span style='color:steelblue'>Bars</span> show percentage screened, <span style='color:darkred'>points</span> show number of eligible patients"),
      caption = paste0("Some wards with <span style='color:darkred'>high number of patients eligible for MRSA screening</span> have <span style='color:steelblue'>low MRSA screening compliance</span>")
    ) +
    theme_minimal() +
    theme(
      axis.title.y.right = element_text(color = "darkred"),
      axis.text.y.right = element_text(color = "darkred"),
      axis.title.y.left = element_text(color = "steelblue"),
      plot.subtitle = ggtext::element_markdown(),
      plot.caption = ggtext::element_markdown()
    ) +
    coord_flip()
}


# function to comply with IG requirements for qmd reports -----------------

# function that converts name (surname, first name) into initials

#' get_initials("Smith, John")

get_initials <- function(names) {
  sapply(names, function(name) {
    parts <- strsplit(name, ", ")[[1]]
    paste0(substr(parts[2], 1, 1), substr(parts[1], 1, 1))
  })
}

# Example usage
example_name <- "Smith, John"

initials <- get_initials(example_name)

print(paste("Initials:", initials))

# create_sqlite_database ------------------------------------------------

# Here's a function that takes both the database name and table parameters as arguments:
create_sqlite_connection <- function(db_name, table_name, data) {
  # Create connection
  con <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = paste0(db_name, ".db"),
    cache_size = NULL,
    synchronous = "off",
    vfs = NULL,
    bigint = "integer64",
    extended_types = TRUE
  )
  
  # Write table
  DBI::dbWriteTable(con,
                    table_name,
                    data,
                    overwrite = TRUE
  )
  
  # Return connection object
  return(con)
}

# # You can use this function like this:
# 
# # Example usage
# create_sqlite_database(
#   db_name = "my_database",
#   table_name = "my_table",
#   data = my_dataframe
# )
#
# DBI::dbDisconnect(con)


# create read database function -------------------------------------------

read_db_file <- function(db_name) {
  con <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = paste0(db_name, ".db"),
    cache_size = NULL,
    synchronous = "off",
    vfs = NULL,
    bigint = "integer64",
    extended_types = TRUE
  )
  
  db_data <- DBI::dbReadTable(con, db_name) |>
    as_tibble() |>
    dplyr::glimpse()
  
  # Replace "D" with "R" for SIR-eligible columns
  db_data <- db_data |>
    mutate(across(where(AMR::is_sir_eligible),
                  ~ str_replace_all(.x,
                                    pattern = "D",
                                    replacement = "R")))
  
  # Format vectors in database
  db_data <- db_data |>
    dplyr::mutate(across(where(AMR::is_sir_eligible), AMR::as.sir)
                  #,
                  #across(c(date_of_birth, receive_date), lubridate::as_date)
    ) |>
    dplyr::glimpse()
  
  DBI::dbDisconnect(con)
  return(db_data)
}

# create function to look at abx Rx for Gram-negative bacteraemias --------

filter_antibiotics_for_gram_neg_orgs <- function(data, ab_name) {
  filtered_data <- data |>
    filter(
      str_detect(Rx, ab_name),
      AMR::mo_is_gram_negative(organism)
    )
  
  if (nrow(filtered_data) == 0) {
    message("no patients found in search")
  } else {
    not_as_factor <- c("mrn", "specimen_no")
    filtered_data |>
      select(
        ward, initials, mrn, specimen_no, `receive date`, organism,
        AMX, AMC, CIP, SXT, GEN, AMK, TAZ, TEM, CXM, ATM, CHL, ETP, MEM
      ) |>
      arrange(desc(ward), mrn, `receive date`) |>
      distinct() |> 
      interactive_table(nrow = 5, not_as_factor = not_as_factor)
  }
}

# Generic function to extract a pattern and date from a single HTML file -----------
extract_pattern_data <- function(file_path, 
                                 text_pattern,
                                 percentage_capture_group = 1,
                                 date_format = "([0-9]{2}-[0-9]{2}-[0-9]{4})") {
  
  # Read the HTML file
  html_content <- read_html(file_path)
  
  # Extract all text from the HTML
  full_text <- html_text(html_content)
  
  # Extract the value using the provided pattern
  match <- str_match(full_text, text_pattern)
  
  if (!is.na(match[1, 1])) {
    extracted_value <- as.numeric(match[1, percentage_capture_group + 1])
  } else {
    extracted_value <- NA
  }
  
  # Extract the date from the filename
  filename <- basename(file_path)
  
  # Extract date using provided date format
  date_match <- str_extract(filename, date_format)
  
  return(tibble(
    filename = filename,
    report_date = date_match,
    extracted_value = extracted_value
  ))
}

# Main extraction function - now with customizable parameters ------------------
extract_all_reports <- function(directory,
                                file_pattern,
                                text_pattern,
                                percentage_capture_group = 1,
                                date_format = "([0-9]{2}-[0-9]{2}-[0-9]{4})",
                                sort_by_date = TRUE) {
  
  # Check if directory exists
  if (!dir.exists(directory)) {
    stop(paste("Directory not found:", directory))
  }
  
  # Get all HTML files matching the pattern
  html_files <- list.files(
    path = directory,
    pattern = file_pattern,
    full.names = TRUE
  )
  
  # Check if any files found
  if (length(html_files) == 0) {
    warning(paste("No matching files found in:", directory))
    return(tibble(
      filename = character(),
      report_date = character(),
      extracted_value = numeric()
    ))
  }
  
  # Extract data from all files
  results <- map_df(
    html_files,
    extract_pattern_data,
    text_pattern = text_pattern,
    percentage_capture_group = percentage_capture_group,
    date_format = date_format
  )
  
  # Sort by report date if requested
  if (sort_by_date) {
    results <- results %>%
      mutate(report_date = dmy(report_date)) %>%
      arrange(desc(report_date)) %>%
      mutate(report_date = format(report_date, "%d-%m-%Y"))
  }
  
  return(results)
}


# reports_date ------------------------------------------------------------

reports_date <- lubridate::floor_date(
  lubridate::today(),
  unit = "week",
  week_start = 2
) |> 
  format("%d-%m-%Y") 
