# wrangle

# Read functions ----------------------------------------------------------

# source("./01_src/functions.R")

# prerequisite R scripts -----------------------------------------------------

# If .db files don't exist/new data needs to be added, run this:
# source('./01_src/02_data_import/02_data_import - 01_test eRecord data.R')

# Read in (or create) Cohort admission dataset -----------------------------------------------

# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Packages",
#                end_string = "END PACKAGES")
# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Cohort admission",
#                end_string = "END COHORT ADMISSION")

# data_cohort_admission_wrangled ------------------------------------------------------------

# skimr::skim(data_cohort_admission)

data_cohort_admission_wrangled_01 <- data_cohort_admission |>
  select(-patient_record_num) |> # field serves no purpose
  mutate(across(where(is.character), \(x) na_if(x, "NULL"))) |>
  mutate(
    date_of_birth = as_date(date_of_birth),
    admission_dt = as_date(admission_dt)
  ) |>
  rename(encntr_id_cohort_admission = encntr_id) |> # rename encounter ID so files can be merged onto data_cohort_admission_wrangled without forcing awkward renaming
  rename(earliest_import_date = import_date)

data_cohort_admission_wrangled_02 <- data_cohort_admission_wrangled_01 |>
  add_observations_history_number(
    sort_order_cols = c(
      "encntr_id_cohort_admission",
      "mrn",
      "earliest_import_date"
    ), # there was one instance of admission_dt changing during one patient's admission, so did not include here.
    observation_group_cols = c("encntr_id_cohort_admission", "mrn"),
    history_col_name = "history_number"
  )

# # Check for missing values in your date columns
# sum(is.na(data_cohort_admission_wrangled_02$date_of_birth))
# sum(is.na(data_cohort_admission_wrangled_02$admission_dt))
# 
# # View rows with missing dates
# data_cohort_admission_wrangled_02 |>
#   filter(is.na(date_of_birth) | is.na(admission_dt))

#updated the following code following an error 22/07/2025 - it now uses base R rather than the AMR::age_groups() function
data_cohort_admission_wrangled_03 <- data_cohort_admission_wrangled_02 |>
  mutate(
    age_on_admission = round(as.numeric(difftime(admission_dt, date_of_birth, units = "days")) / 365.25),
    age_group = cut(age_on_admission, 
                    breaks = c(0, 17, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
                    labels = c("0-16", "17-29", "30-39", "40-49", "50-59", 
                               "60-69", "70-79", "80-89", "90-99", "100+"),
                    right = FALSE),
    paeds = if_else(age_group == "0-16", TRUE, FALSE, missing = NA)
  )

# data_cohort_admission_wrangled_03 |>
#   distinct(mrn, age_on_admission) |> 
#   slice_sample(n = 10)

data_cohort_admission_wrangled <- data_cohort_admission_wrangled_03 |>
  mutate(age_group = fct_relevel(age_group,
    "100+",
    after = Inf
  )) |>
  mutate(across(
    where(is.character) & !matches(c("fin", "patient")),
    forcats::as_factor
  )) |> # in order to search by these cols more easily in final quarto document
  mutate(age_group = fct_relevel(age_group,
    "100+",
    after = Inf
  ))

# check
fct_relevel(data_cohort_admission_wrangled$age_group)

# admission timeframe from data_cohort_admission_wrangled -----------------

# This code finds the earliest and latest admission dates to understand the data timeframe.

start_date_admission_dt <- min(data_cohort_admission_wrangled$admission_dt)
end_date_admission_dt <- max(data_cohort_admission_wrangled$admission_dt)

print(paste("Earliest admission date for patients in this cohort:", format(start_date_admission_dt, "%d %B %Y")))
print(paste("Latest admission date for patients in this cohort:", format(end_date_admission_dt, "%d %B %Y")))


# remove some locations from current_ward  -------------------
# e.g. virtual wards and discharge lounge

data_cohort_admission_wrangled <- data_cohort_admission_wrangled |> 
  filter(!str_detect(current_ward, "FH\\sFrailty\\sVirtual\\sWard|FH\\sDischarge\\sLounge|RV\\sRespiratory\\sVirtual\\sWard")) 

# data_cohort_admission_wrangled |> 
#   distinct(mrn, current_ward) |> 
#   count(current_ward) |> 
#   arrange(n)

# skimr -------------------------------------------------------------------

# data_cohort_admission_wrangled |>
#   skimr::skim()
