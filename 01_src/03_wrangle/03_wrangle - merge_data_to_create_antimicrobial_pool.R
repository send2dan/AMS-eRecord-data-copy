# wrangle

library(knitr) # for kable()

# Read functions ----------------------------------------------------------

source("./01_src/functions.R")
#
# # Read in datasets -----------------------------------------------
#
# source_section('./01_src/02_data_import/02_data_import - read database.R',
#                start_string = "Packages",
#                end_string = "END PACKAGES")
# source_section('./01_src/02_data_import/02_data_import - read database.R',
#                start_string = "Antimicrobials",
#                end_string = "END ANTIMICROBIALS")
# source_section('./01_src/02_data_import/02_data_import - read database.R',
#                start_string = "Cohort admission wrangled",
#                end_string = "END COHORT ADMISSION WRANGLED")

# analyse import_date for reporting purposes ------------------------------

import_date_report <- data_cohort_admission_wrangled |>
  distinct(earliest_import_date) |>
  pull()

import_date_report_formatted <- format(ymd(import_date_report), "%A %d %B %Y")

# # Named arguments can be used to assign temporary variables.
# glue::glue('Data for inpatients prescribed antimicrobials in the 2 days prior to {date}',
#            date = format(ymd(import_date_report), '%A %d %B %Y'))

# admission timeframe from data_cohort_admission_wrangled -----------------

# This code finds the earliest and latest admission dates to understand the data timeframe.

start_date_admission_dt <- min(data_cohort_admission_wrangled$admission_dt)
end_date_admission_dt <- max(data_cohort_admission_wrangled$admission_dt)

# #Earliest admission date for patients in this cohort:
# format(start_date_admission_dt, "%d %B %Y")
#
# #Latest admission date for patients in this cohort:
# format(end_date_admission_dt, "%d %B %Y")

# merge datasets of possible interest -------------------------------------

# data_allergies_wrangled
# data_clinical_records_wide
# data_cohort_admission_wrangled
# data_medications
# data_medications_antimicrobials
# data_obs_news2

# data_cohort_admission_wrangled |>
#   skimr::skim_without_charts()

# data_medications_antimicrobials |>
#   skimr::skim_without_charts()

# # compare col_names -------------------------------------------------------
#
# #intersect(x, y) finds all rows in both x and y.
# intersect(
#   colnames(data_cohort_admission_wrangled),
#   colnames(data_medications_antimicrobials)
# )
#
# #union(x, y) finds all rows in either x or y, excluding duplicates.
# union(
#   colnames(data_cohort_admission_wrangled),
#   colnames(data_medications_antimicrobials)
# )
#
# #union_all(x, y) finds all rows in either x or y, including duplicates.
# union_all(
#   colnames(data_cohort_admission_wrangled),
#   colnames(data_medications_antimicrobials)
# )
#
# #setdiff(x, y) finds all rows in x that aren't in y.
# setdiff(
#   colnames(data_cohort_admission_wrangled),
#   colnames(data_medications_antimicrobials)
# )

# check if all values included in "encntr_id" "mrn" are included in all datasets to be merged ----------------------

# encounter ID
data_cohort_admission_wrangled_encntr_id <- data_cohort_admission_wrangled |>
  select(encntr_id_cohort_admission) |>
  distinct() |>
  pull()

# length(data_cohort_admission_wrangled_encntr_id)

data_medications_antimicrobials_encntr_id <- data_medications_antimicrobials |>
  select(encntr_id) |>
  distinct() |>
  pull()

# length(data_medications_antimicrobials_encntr_id)
#
# #Q: check if all values included in "encntr_id" "mrn" are included in all datasets to be merged
# union(data_cohort_admission_wrangled_encntr_id,
#       data_medications_antimicrobials_encntr_id)
#
# union(data_medications_antimicrobials_encntr_id,
#       data_cohort_admission_wrangled_encntr_id)
#
# setdiff(data_cohort_admission_wrangled_encntr_id,
#         data_medications_antimicrobials_encntr_id)
#
# setdiff(data_medications_antimicrobials_encntr_id,
#         data_cohort_admission_wrangled_encntr_id)

# mrn
data_cohort_admission_wrangled_mrn <- data_cohort_admission_wrangled |>
  select(mrn) |>
  distinct() |>
  pull()

# length(data_cohort_admission_wrangled_mrn)

data_medications_antimicrobials_mrn <- data_medications_antimicrobials |>
  select(mrn) |>
  distinct() |>
  pull()

# length(data_medications_antimicrobials_mrn)
#
# # A: no, the datasets differ; e.g. data_cohort_admission_wrangled includes more records than data_medications;
#
# setdiff(data_cohort_admission_wrangled_mrn,
#         data_medications_antimicrobials_mrn)
#
# setdiff(data_medications_antimicrobials_mrn,
#         data_cohort_admission_wrangled_mrn)
#
# # compare length of encounter ID and mrn ----------------------------------
#
# length(data_cohort_admission_wrangled_encntr_id)
#
# length(data_cohort_admission_wrangled_mrn)

##### Conclusion:
# thankfully, data_medications includes largely only encntr_id or mrn that ALSO feature in data_cohort_admission_wrangled
# Thus, it's OK to left_join onto data_cohort_admission_wrangled without risking loss of data

# data_cohort_admission_wrangled |>
#   count(mrn, encntr_id_cohort_admission) |>
#   arrange(desc(n))

# merge onto data_cohort_admission_wrangled using "mrn" only and LEFT_join --------------------------------------------

data_antimicrobial_pool <- data_cohort_admission_wrangled |>
  left_join(data_medications_antimicrobials,
    by = join_by(mrn, earliest_import_date),
    multiple = "all"
  )

# #check encntr_id_cohort_admission length
# data_antimicrobial_pool |>
#   select(encntr_id_cohort_admission) |>
#   distinct() |>
#   pull() |>
#   length()
#
# #check encntr_id length (i.e. encntr_id taken from dataset merged into data_cohort_admission_wrangled)
# data_antimicrobial_pool |>
#   select(encntr_id) |>
#   distinct() |>
#   pull() |>
#   length()
#
# #check mrn length
# data_antimicrobial_pool |>
#   select(mrn) |>
#   distinct() |>
#   pull() |>
#   length()

# keep only those rows where !is.na(encntr_id)  ---------------------------

# note: patients where encntr_id == NA are order_name_c == NA as well

# data_antimicrobial_pool |>
#   filter(is.na(encntr_id)) |>
#   select(encntr_id, order_name_c, order_name, everything())

# Aim: to keep only data patients with antimicrobial prescriptions, can filter out where encntr_id == NA

data_antimicrobial_pool <- data_antimicrobial_pool |>
  filter(!is.na(encntr_id))

# #check encntr_id_cohort_admission length
# data_antimicrobial_pool |>
#   select(encntr_id_cohort_admission) |>
#   distinct() |>
#   pull() |>
#   length()
#
# #check encntr_id length (i.e. encntr_id taken from dataset merged into data_cohort_admission_wrangled)
# data_antimicrobial_pool |>
#   select(encntr_id) |>
#   distinct() |>
#   pull() |>
#   length()
#
# #check mrn length
# data_antimicrobial_pool |>
#   select(mrn) |>
#   distinct() |>
#   pull() |>
#   length()

# remove "na" column, if present ------------------------------------------------------

colnames(data_antimicrobial_pool)

# "na" column appears not to add much...
# data_antimicrobial_pool |>
#   filter(!is.na(na))

# Remove the "na" column if it exists, otherwise do nothing
if ("na" %in% colnames(data_antimicrobial_pool)) {
  data_antimicrobial_pool <- data_antimicrobial_pool %>% select(-na)
}

# create current_bay_bed (bay bed) field -----------

data_antimicrobial_pool$current_bay_bed <- str_c(
  data_antimicrobial_pool$current_bay,
  "Bed",
  data_antimicrobial_pool$current_bed,
  sep = " "
)

# change dates/times using ymd -------------------------
data_antimicrobial_pool <- data_antimicrobial_pool |>
  mutate(
    review_date = ymd(format(review_date, "%Y-%m-%d")),
    stop_date_time = ymd(format(stop_date_time, "%Y-%m-%d")),
    date_of_birth = ymd(format(date_of_birth, "%Y-%m-%d")),
    admission_dt = ymd(format(admission_dt, "%Y-%m-%d")),
    start_date_time = ymd(format(start_date_time, "%Y-%m-%d")),
    next_dose_date_time = ymd(format(next_dose_date_time, "%Y-%m-%d"))
  )

# change relevant columns to non-character fields for table ----------------

data_antimicrobial_pool <- data_antimicrobial_pool |>
  mutate(
    intended_duration = as.numeric(intended_duration),
    age_on_admission = as.numeric(age_on_admission)
  )



# antimicrobials prescribed ------------------------------------------------

# check for duplicate abx prescriptions for same patient

# data_antimicrobial_pool |>
#   distinct(mrn, order_name_c, start_date_time) |>
#   count(mrn, order_name_c) |>
#   arrange(desc(n))

# data_antimicrobial_pool |>
#   distinct(mrn, order_name_c, start_date_time, review_date, stop_date_time, .keep_all = TRUE) |>
#   skimr::skim()

# data_antimicrobial_pool |>
#   filter(is.na(dose)) #|>
# #View()


# Merge with select columns in data_obs_news2 --------

temp_data_obs_selected_columns <- data_obs_news2 |>
  # glimpse() |>
  select(mrn, starts_with("obs"), starts_with("news2_max"), oxygen_status, starts_with("temp_max"))

data_antimicrobial_pool <- data_antimicrobial_pool |>
  left_join(temp_data_obs_selected_columns,
    by = join_by(mrn),
    relationship = "many-to-many"
  )


# Change obs_done... and obs_stable... to as.logical.factor ---------------

data_antimicrobial_pool <- data_antimicrobial_pool |>
  mutate(
    obs_done_in_last_24h = as.logical(obs_done_in_last_24h),
    obs_stable_in_last_24h = as.logical(obs_stable_in_last_24h)
  )

# Merge with select columns in data_clinical_records_wide -----------------

temp_data_clinical_records_wide_selected_columns <- data_clinical_records_wide |>
  # glimpse() |>
  select(mrn, crp_max, temp_max_clin_rec_wide)

data_antimicrobial_pool <- data_antimicrobial_pool |>
  left_join(temp_data_clinical_records_wide_selected_columns,
    by = join_by(mrn),
    relationship = "many-to-many"
  )

# create DT::datatable to filter ------------------------------------------

# use function (interactive_table) from fuctions.R script to create interactive table

# data_antimicrobial_pool |>
#   skimr::skim_without_charts()

# colnames(data_antimicrobial_pool)
#
# data_antimicrobial_pool_col_names <- data_antimicrobial_pool |>
#   colnames()
#
# data_antimicrobial_pool_col_names <- str_c(data_antimicrobial_pool_col_names, ",")
#
# kable(data_antimicrobial_pool_col_names, format = "simple")

# select columns of interest, in correct order
data_antimicrobial_pool_for_table <- data_antimicrobial_pool |>
  select(
    current_ward,
    current_bay_bed,
    patient,
    sex,
    mrn,
    fin,
    age_on_admission,
    admission_dt,
    order_name_c,
    start_date_time,
    dose,
    dose_unit,
    drug_form,
    route_of_administration,
    frequency,
    indication,
    # diagnostic_confidence,
    intended_duration,
    duration_unit,
    # stop_type,
    stop_date_time,
    review_date,
    # obs_date_time,
    # obs_number,
    obs_done_in_last_24h,
    obs_stable_in_last_24h,
    news2_max,
    news2_max_last_24h,
    # oxygen_status,
    temp_max,
    temp_max_last_24h,
    # temp_max_clin_rec_wide, (??data_clinical_records_wide$temperature... is less reliable than the temperature value in data_obs_news2$temperature...)
    crp_max
  )

# filter data

# 24/9/24 Missing data review -----
# Fully complete:
# order_name_c
# current_ward
# admission_dt

# Quite complete:
# Dose: 0.957
# Route of admin: 0.991
# Indication:  0.811
# Stop date/time: 0.825

# But often missing:
# Frequency: 0.297
# Duration: 0.182
# review_date: 0.466
# intended_duration: 0.180

# #Q: is it OK to leave out Rx without frequency?
# #A: Yes, for now
# data_antimicrobial_pool |>
#   filter(!is.na(frequency)) |>
#   View() |>
#   skimr::skim()

data_antimicrobial_pool_for_table <- data_antimicrobial_pool_for_table |>
  filter(!is.na(order_name_c)) |> # where order_name_c is not missing, this outlines the actual Misc Prescription...
  # filter(!is.na(dose)) |> #where drugs don't have a documented dose, this is (usually) because the drug was given with frequency == ONCE)
  filter(!is.na(frequency)) |>
  arrange(current_ward, current_bay_bed, mrn, start_date_time, order_name_c) # order by location, mrn, then drug name

# colnames(data_antimicrobial_pool_for_table)
#
# data_antimicrobial_pool_for_table_col_names <- data_antimicrobial_pool_for_table |>
#   colnames()
#
# data_antimicrobial_pool_for_table_col_names <- str_c(data_antimicrobial_pool_for_table_col_names, ",")
#
# kable(data_antimicrobial_pool_for_table_col_names, format = "simple")

# make table IG compliant -------

data_antimicrobial_pool_for_table <- data_antimicrobial_pool_for_table |> 
  mutate(patient = get_initials(patient))

# rename
data_antimicrobial_pool_for_table <- data_antimicrobial_pool_for_table |>
  rename(
    ward = current_ward,
    `bay/bed` = current_bay_bed,
    initials = patient,
    # patient,
    # sex,
    # mrn,
    # fin,
    age = age_on_admission,
    `adm date` = admission_dt,
    `Rx` = order_name_c,
    # start_date_time,
    # indication,
    # dose,
    `dose unit` = dose_unit,
    `drug form` = drug_form,
    `drug route` = route_of_administration,
    # frequency,
    # diagnostic_confidence,
    duration = intended_duration,
    `duration unit` = duration_unit,
    # stop_type,
    `stop date/time` = stop_date_time,
    `review date` = review_date,
    `start date/time` = start_date_time,
    # obs_date_time,
    # obs_number,
    `obs done in last 24h` = obs_done_in_last_24h,
    `obs stable in last 24h` = obs_stable_in_last_24h,
    # `oxygen status` = oxygen_status,
    `NEWS2 max` = news2_max,
    `NEWS2 max in last 24h` = news2_max_last_24h,
    `Temp max` = temp_max,
    `Temp max in last 24h` = temp_max_last_24h,
    # `Temp max (clin record)` = temp_max_clin_rec_wide,
    `CRP max` = crp_max
  )

# check for duplicates:
# library(janitor)

# data_antimicrobial_pool_for_table |>
#   janitor::get_dupes() #|>
#   #View()

# Remove duplicates from the original data frame
data_antimicrobial_pool_for_table_dedup <- data_antimicrobial_pool_for_table |>
  distinct()
