# here --------------------------------------------------------------------

if (!require("here")) {
  install.packages("here",
    repos = "https://packagemanager.posit.co/cran/latest", # "https://cran.r-project.org/"
    type = "win.binary"
  )
}

library(here)

# Package list ---------------------------------

# install {renv} if not already done so
if (!require("renv")) {
  install.packages("renv",
    repos = "https://packagemanager.posit.co/cran/latest", # "https://cran.r-project.org/"
    type = "win.binary"
  )
}

library(renv)

# list packages to download -----------------------------------------------

packages_to_download <- c(
  "tidymodels", #https://www.tidymodels.org/packages/ The core tidymodels packages work together to enable a wide variety of modeling approaches
  "dotenv", # for reading ".env" file
  "AMR", #organism ID and susceptibility data wrangling
  "cleaner", #to help analyse organism data created using AMR package
  "bookdown", # required to set knitr options
  "here", # for finding files
  "tidytext", # for text cleaning tools
  "shiny", # for viewing rendered quarto reports (?)
  "quarto", # for quarto report rendering scripts
  "flextable", # for creating pretty (non-interactive) tables
  #"reactable", # for creating interactive tables in quarto reports
  #"plotly", # for creating interactive charts in quarto reports
  "excel.link", # to read password protected excel files (xl.read.file function)
  # "dplyr",
  # "lubridate",
  # "stringr",
  # "forcats",
  # "purrr",
  "odbc", # for SQL connection to APEX
  "gt",
  "knitr",
  "DBI", # for SQL connection to APEX
  "NHSRplotthedots", # QI work
  "RSQLite",
  "skimr", # Compact and Flexible Summaries of Data
  "tidyverse",
  "networkD3", #for sankey diagrams
  "readr", #to read csv files
  "readxl", #to read excel files
  "janitor", # clean_names function
  "beepr",
  "bench", # for timing SQL queries
  "openxlsx",
  #"waffle", # for e.g. Waffle Chart Showing Survival Rates
  "ggrepel", # for geom_label_repel function... geom_label_repel draws a rectangle underneath the text, making it easier to read. The text labels repel away from each other and away from the data points.
  #"trelliscopejs" # for creating interactive dashboards in quarto reports
  "dlookr", # for creating data diagnosis reports
  "kknn", # for building k-nearest neighbour models
  "xfun",
  "corrplot", # for plotting data from correlation matrices
  "butcher", # for reducing size of model objects before saving
  "bundle" # for collecting together all info associated with a model object before saving
)

# check packages already installed vs. listed for install -----------------

packages_to_download %in% row.names(installed.packages())

row.names(installed.packages()) %in% packages_to_download

# install purrr ----------------------------------------------------------

# #install {purrr} if not already done so
if (!require("purrr")) {
  renv::install(
    "purrr",
    prompt = FALSE
  )
}

library(purrr)

# create function to install packages -------------------------------------

install_vector_of_packages <- function(x) {
  if (!x %in% row.names(installed.packages())) { # Check if already loaded without installing
    # RE: renv::install() : it's a little less typing and can install packages from GitHub, Bioconductor, and more, not just CRAN
    # NB: If you use renv for multiple projects, you'll have multiple libraries, meaning that you'll often need to install the same package in multiple places.
    renv::install(
      x,
      prompt = FALSE
    )
  }
} # [JL] used to have !any(<condition>), but the any() part is not required as purrr::map() takes each individual element of packages_to_download as input into the function i.e. x is not a vector.

# use purrr to map() function and install packages ------------------------

purrr::map(packages_to_download, install_vector_of_packages)

# Load packages --------------------------------------------------------------------

# Loop through packages_to_download and load all libraries
for (i in packages_to_download) {
  if (!require(i, character.only = TRUE)) {
    library(i, character.only = TRUE)
  }
} # [JL] Is it better practice to load in all packages at the beginning, or load in packages only when required?

# check all is installed correctly ----------------------------------------

# Open a command prompt or terminal and type R --version. If it displays the version, R is installed.

# Run quarto check to see if Quarto can find all its dependencies, including R.

# get package references for final bibliography/references --------------------------------------------------

# Get the packages references
knitr::write_bib(c(.packages(), "bookdown"), here::here("packages.bib"))

# merge the zotero references and the packages references
cat(paste("% Automatically generated", Sys.time()),
  "\n% DO NOT EDIT",
  {
    readLines(here("citedrive.bib")) |> # which .bib file is being used?
      paste(collapse = "\n")
  },
  {
    readLines(here("packages.bib")) |>
      paste(collapse = "\n")
  },
  file = here("biblio.bib"),
  sep = "\n"
)

# knitr options -----------------------------------------------------------

knitr::opts_chunk$set(
  tidy.opts = list(width.cutoff = 60),
  tidy = TRUE,
  strip.white = TRUE,
  out.width = "90%"
)

# theme minimal -----------------------------------------------------------

ggplot2::theme_set(ggplot2::theme_minimal())

# Update packages (optional) ----------------------------------------------

# renv::update(
#   "pillar",
#   prompt = FALSE
# )

# renv::snapshot()
# 
# renv::status()
