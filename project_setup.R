
cat(
  "# History files",
  ".Rhistory",
  ".Rapp.history",
  "# Session Data files",
  ".RData",
  "# User-specific files",
  ".Ruserdata",
  "# Example code in package build process",
  "*-Ex.R",
  "# Output files from R CMD build",
  "/*.tar.gz",
  "# Output files from R CMD check",
  "/*.Rcheck/",
  "# RStudio files",
  ".Rproj.user/",
  "# produced vignettes",
  "vignettes/*.html",
  "vignettes/*.pdf",
  "# OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3",
  ".httr-oauth",
  "# knitr and R markdown default cache directories",
  "*_cache/",
  "/cache/",
  "# Temporary files created by R markdown",
  "*.utf8.md",
  "*.knit.md",
  "# R Environment Variables",
  ".Renviron",
  "*.Rproj",
  "*.xls",
  "*.xlsx",
  "*.png",
  "*.pdf",
  "*.doc",
  "*.docx",
  "*.html",
  file = ".gitignore", sep="\n",append=TRUE
)


dir.create('01_src')
dir.create('02_data')
cat(
  "# History files",
  ".Rhistory",
  ".Rapp.history",
  "# Session Data files",
  ".RData",
  "# User-specific files",
  ".Ruserdata",
  "# Example code in package build process",
  "*-Ex.R",
  "# Output files from R CMD build",
  "/*.tar.gz",
  "# Output files from R CMD check",
  "/*.Rcheck/",
  "# RStudio files",
  ".Rproj.user/",
  "# produced vignettes",
  "vignettes/*.html",
  "vignettes/*.pdf",
  "# OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3",
  ".httr-oauth",
  "# knitr and R markdown default cache directories",
  "*_cache/",
  "/cache/",
  "# Temporary files created by R markdown",
  "*.utf8.md",
  "*.knit.md",
  "# R Environment Variables",
  ".Renviron",
  "*.Rproj",
  "*.xls",
  "*.xlsx",
  "*.png",
  "*.pdf",
  "*.doc",
  "*.docx",
  "*.html",
  file = "02_data/.gitignore", sep="\n",append=TRUE
)
dir.create('03_results')
cat(
  "# History files",
  ".Rhistory",
  ".Rapp.history",
  "# Session Data files",
  ".RData",
  "# User-specific files",
  ".Ruserdata",
  "# Example code in package build process",
  "*-Ex.R",
  "# Output files from R CMD build",
  "/*.tar.gz",
  "# Output files from R CMD check",
  "/*.Rcheck/",
  "# RStudio files",
  ".Rproj.user/",
  "# produced vignettes",
  "vignettes/*.html",
  "vignettes/*.pdf",
  "# OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3",
  ".httr-oauth",
  "# knitr and R markdown default cache directories",
  "*_cache/",
  "/cache/",
  "# Temporary files created by R markdown",
  "*.utf8.md",
  "*.knit.md",
  "# R Environment Variables",
  ".Renviron",
  "*.Rproj",
  "*.xls",
  "*.xlsx",
  "*.png",
  "*.pdf",
  "*.doc",
  "*.docx",
  "*.html",
  file = "03_results/.gitignore", sep="\n",append=TRUE
)
dir.create('04_doc')

cat("#KNITR setup","knitr::opts_chunk$set(echo = TRUE, warning=TRUE, message = TRUE)","options(stringsAsFactors = FALSE)"," ","#Package load",
"pacman::p_load(tidyverse, tidylog, kableExtra, lubridate, readxl, janitor, fuzzyjoin)"," ","#Function load",
file = "01_src/01_initialise.R", sep="\n",append=TRUE)

file.create('01_src/02_data_import.R')
file.create('01_src/03_wrangle.R')
file.create('01_src/04a_analyse.R')
file.create('01_src/04b_model.R')
file.create('01_src/05_figures.R')

file.edit('01_src/01_initialise.R')
file.edit('01_src/02_data_import.R')
file.edit('01_src/03_wrangle.R')
file.edit('01_src/04a_analyse.R')
file.edit('01_src/04b_model.R')
file.edit('01_src/05_figures.R')
