# VWS43 ---------------

# NB depends on following script being run first:
# source("./01_src/ams_eRecord_render_reports.R")

# 3. Set source and target directories

#source directory of html files that need to be moved
source_dir <- here::here("02_data_output", "render_output_holding")

#target directory of html files that need to be moved
target_base <- "//NUTH-VWS43/datascience.xnuth.nhs.uk"

# OneDrive target directory
onedrive_base <- "C:/Users/Weiandd2/OneDrive - NHS/NUTH-MICRO - Shared Access Documents - Documents/Data science/Data science intranet site"

#Find all HTML files with names ending with reports_date
html_files <- fs::dir_ls(source_dir, regexp = paste0(reports_date, "\\.html$"))

# 4. Find all HTML files with names ending with reports_date
html_files_target_base <- fs::dir_ls(target_base)

# > html_files
# C:/Users/Weiandd2/Documents/AMS-eRecord-data2/02_data_output/render_output_holding/ams-antimicrobial-usage-report 08-07-2025.html
# C:/Users/Weiandd2/Documents/AMS-eRecord-data2/02_data_output/render_output_holding/ams-gentamicin-prescriptions-report 08-07-2025.html
# C:/Users/Weiandd2/Documents/AMS-eRecord-data2/02_data_output/render_output_holding/ams-mrsa-pos-patients-prescribed-antimicrobials-report 08-07-2025.html
# C:/Users/Weiandd2/Documents/AMS-eRecord-data2/02_data_output/render_output_holding/ams-mrsa-screening-compliance-report 08-07-2025.html
# C:/Users/Weiandd2/Documents/AMS-eRecord-data2/02_data_output/render_output_holding/ams-penicillin-allergy-delabelling-report 08-07-2025.html

# Helper function to copy files to a target location
copy_html_files <- function(html_files, target_base, reports_date, location_name) {
  for (file in html_files) {
    fname <- basename(file)
    # Remove the date and extension from the end of fname
    # Pattern: " <date>.html" at the end
    subfolder <- stringr::str_remove(fname, paste0(" ", reports_date, "\\.html$"))
    target_dir <- file.path(target_base, subfolder)
    fs::dir_create(target_dir)
    fs::file_copy(file, file.path(target_dir, "index.html"), overwrite = TRUE)
    cat("Copied", fname, "to", file.path(target_dir, "index.html"), "on", location_name, "\n")
  }
}

# Copy to VWS43
copy_html_files(html_files, target_base, reports_date, "VWS43")

# VWS44 ---------------

# Copy to VWS44
target_base <- "//NUTH-VWS44/datascience.xnuth.nhs.uk"
copy_html_files(html_files, target_base, reports_date, "VWS44")

# OneDrive/SharePoint ---------------

# Copy to OneDrive
copy_html_files(html_files, onedrive_base, reports_date, "OneDrive")

# create index.html  for https://datascience.xnuth.nhs.uk/ intranet site -------------

quarto::quarto_render(
  "index.qmd",
  output_format = "html",
  output_file = glue::glue("index.html")
)

#source directory of html files that need to be moved
source_file <- here::here("02_data_output", "render_output_holding", "index.html")

# Helper function to copy index files
copy_index_file <- function(source_file, target_base, location_name) {
  for (file in source_file) {
    fname <- basename(file)
    target_dir <- file.path(target_base)
    fs::dir_create(target_dir)
    fs::file_copy(file, file.path(target_dir, "index.html"), overwrite = TRUE)
    cat("Copied", fname, "to", file.path(target_dir, "index.html"), "on", location_name, "\n")
  }
}

# Copy to VWS43
target_base <- "//NUTH-VWS43/datascience.xnuth.nhs.uk"
copy_index_file(source_file, target_base, "VWS43")

# Copy to VWS44
target_base <- "//NUTH-VWS44/datascience.xnuth.nhs.uk"
copy_index_file(source_file, target_base, "VWS44")

# Copy to OneDrive
copy_index_file(source_file, onedrive_base, "OneDrive")

# create how-were-these-reports-created ----------

quarto::quarto_render(
  "how-were-these-reports-created.qmd",
  output_format = "html",
  output_file = glue::glue("how-were-these-reports-created.html")
)

#source directory of html files that need to be moved
source_file <- here::here("02_data_output", "render_output_holding", "how-were-these-reports-created.html")

# Helper function to copy documentation files
copy_doc_file <- function(source_file, target_base, location_name) {
  for (file in source_file) {
    fname <- basename(file)
    subfolder <- stringr::str_remove(fname, "\\.html$")
    target_dir <- file.path(target_base, subfolder)
    fs::dir_create(target_dir)
    fs::file_copy(file, file.path(target_dir, "index.html"), overwrite = TRUE)
    cat("Copied", fname, "to", file.path(target_dir, "index.html"), "on", location_name, "\n")
  }
}

# Copy to VWS43
target_base <- "//NUTH-VWS43/datascience.xnuth.nhs.uk"
copy_doc_file(source_file, target_base, "VWS43")

# Copy to VWS44
target_base <- "//NUTH-VWS44/datascience.xnuth.nhs.uk"
copy_doc_file(source_file, target_base, "VWS44")

# Copy to OneDrive
copy_doc_file(source_file, onedrive_base, "OneDrive")

# List folders on website

# To list all folders (directories) in a given network location in R, you can use the list.dirs() or list.files() 

# List all folders (directories) in the specified network location
folders_on_site_43 <- list.dirs("\\\\NUTH-VWS43\\datascience.xnuth.nhs.uk", recursive = FALSE, full.names = TRUE)

folders_on_site_44 <- list.dirs("\\\\NUTH-VWS44\\datascience.xnuth.nhs.uk", recursive = FALSE, full.names = TRUE)

folders_on_onedrive <- list.dirs(onedrive_base, recursive = FALSE, full.names = TRUE)

# Print the list of folders
print(c(folders_on_site_43, folders_on_site_44, folders_on_onedrive))