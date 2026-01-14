# wrangle

# Read functions ----------------------------------------------------------

# source("./01_src/functions.R")

# prerequisite R scripts -----------------------------------------------------

# If .db files don't exist/new data needs to be added, run this:
# source('./01_src/02_data_import/02_data_import - 01_test eRecord data.R')

# Read in (or create) Clinical records dataset -----------------------------------------------

# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Packages",
#                end_string = "END PACKAGES")
# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Clinical records",
#                end_string = "END CLINICAL RECORDS")

# Read in (or create) Cohort admission ------------------------------------------------

# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Packages",
#                end_string = "END PACKAGES")
# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Cohort admission",
#                end_string = "END COHORT ADMISSION")

# Dealing with duplicate records ------------------------------------------

data_clinical_records <- data_clinical_records |>
  filter(!str_detect(event_type, "ED CC Weight measured|Weight at discharge|Weight Stones|HAR.*")) |> # remove less useful data from event_type e.g. remove "HARS weight" and "HARS height" as these don't really feature in the dataset
  select(-record_num) |>
  rename(earliest_import_date = import_date) |>
  add_observations_history_number(
    sort_order_cols = c(
      "encntr_id",
      "mrn",
      "event_type",
      "performed_dt_tm",
      "earliest_import_date"
    ),
    observation_group_cols = c("encntr_id", "mrn", "event_type"),
    history_col_name = "record_num"
  )

glimpse(data_clinical_records)

# data_clinical_records_wide ----------------------------------------------

data_clinical_records_wide_01 <- data_clinical_records |>
  pivot_wider(
    names_from = event_type,
    names_glue = "{event_type}_{.value}",
    values_from = c(performed_dt_tm, result_val, result_units)
  ) |> # to put CRP, height, weight etc. into their own columns
  clean_names() |>
  select(mrn, encntr_id, record_num, starts_with("c_reac"), starts_with("temp"), starts_with("height"), starts_with("weight"), starts_with("creat"), everything())

data_clinical_records_wide_02 <- data_clinical_records_wide_01 |>
  filter(!if_all(matches("dt_tm$|val$|units$"), is.na)) |>
  mutate(
    height_length_measured_result_val =
      as.numeric(height_length_measured_result_val),
    weight_measured_result_val =
      as.numeric(weight_measured_result_val),
  ) |>
  mutate(c_reactive_protein_result_val = case_match(
    c_reactive_protein_result_val,
    c("<1", "Less than 5 mg/L       (     0 to 5   )") ~ "1",
    "*" ~ NA,
    .default = c_reactive_protein_result_val
  ))

data_clinical_records_wide_03 <- data_clinical_records_wide_02 |>
  mutate(height_length_measured_result_val = case_when(
    height_length_measured_result_val == 0 ~ NA,
    height_length_measured_result_val < 2.5 & !is.na(height_length_measured_result_val) ~ height_length_measured_result_val * 100, # change values in meters to values in cm where val < 3
    height_length_measured_result_val < 10 & !is.na(height_length_measured_result_val) ~ NA, # remaining < 10 are likely errors in data entry
    height_length_measured_result_val > 300 & !is.na(height_length_measured_result_val) ~ NA, # remaining > 300 are likely errors in data entry
    .default = height_length_measured_result_val
  ))

# Age vs Height -------------------------------------------------------------

age_data <- data_cohort_admission |>
  select(mrn, date_of_birth) |>
  mutate(
    age = AMR::age(date_of_birth),
    age_group = AMR::age_groups(age)
  ) |>
  distinct()

# check min heights recorded vs age_group
data_clinical_records_wide_03 |>
  left_join(age_data) |>
  count(height_length_measured_result_val, age_group) |>
  filter(!age_group == "0-11") |>
  arrange((height_length_measured_result_val)) |>
  print(n = 10)

# check max heights recorded vs age_group
data_clinical_records_wide_03 |>
  left_join(age_data) |>
  count(height_length_measured_result_val, age_group) |>
  filter(!age_group == "0-11") |>
  arrange(desc(height_length_measured_result_val)) |>
  print(n = 10)

# CONTINUE ----------------------------------------------------------------

data_clinical_records_wide_04 <- data_clinical_records_wide_03 |>
  mutate(weight_measured_result_val = case_when(
    weight_measured_result_val == 0 ~ NA,
    weight_measured_result_val > 300 & !is.na(weight_measured_result_val) ~ NA,
    .default = weight_measured_result_val
  ))

# Bimodal distributions -------------------------------------------------------------

# for weight
data_clinical_records_wide_04 |>
  select(weight_measured_result_val) |>
  ggplot(aes(weight_measured_result_val)) +
  geom_histogram()

# not so much for height
data_clinical_records_wide_04 |>
  select(height_length_measured_result_val) |>
  ggplot(aes(height_length_measured_result_val)) +
  geom_histogram()

# but for age, too
age_data |>
  ggplot(aes(age)) +
  geom_histogram()

# check age_group vs. weight (children)
data_clinical_records_wide_04 |>
  left_join(age_data) |>
  count(weight_measured_result_val, age_group) |>
  filter(age_group == "0-11") |> # check young people data
  arrange((weight_measured_result_val))

# check age_group vs. weight (older)
data_clinical_records_wide_04 |>
  left_join(age_data) |>
  count(weight_measured_result_val, age_group) |>
  filter(!age_group == "0-11") |>
  arrange(desc(weight_measured_result_val))

# looks like the bimodal distribution is correct, and may reflect bimodal distribution of age of patients in dataset

# CONTINUE ----------------------------------------------------------------

data_clinical_records_wide_05 <- data_clinical_records_wide_04 |>
  mutate(
    temperature_result_val = case_match(
      temperature_result_val,
      c(" ", "0") ~ NA,
      .default = temperature_result_val
    ),
    temperature_result_val = case_when(
      temperature_result_val < "25" | temperature_result_val > "45" ~ NA,
      .default = temperature_result_val
    )
  )

data_clinical_records_wide_06 <- data_clinical_records_wide_05 |>
  mutate(creatinine_result_val = case_match(
    creatinine_result_val,
    c("*", "Old sample - Unsuitable for analysis") ~ NA,
    "<5" ~ "1",
    .default = creatinine_result_val
  ))


# Add max CRP, creat, weight, height for each patient  --------------------

# updated code handles groups in data where all values in the corresponding columns are missing... To handle these cases gracefully, you can modify your code to use a conditional statement and set the maximum value to NA if there are no non-missing values.

data_clinical_records_wide <- data_clinical_records_wide_06 |>
  mutate(across(ends_with("_val"), as.numeric)) |>
  group_by(mrn) |>
  mutate(
    crp_max = ifelse(any(!is.na(c_reactive_protein_result_val)), max(c_reactive_protein_result_val, na.rm = TRUE), NA),
    creat_max = ifelse(any(!is.na(creatinine_result_val)), max(creatinine_result_val, na.rm = TRUE), NA),
    weight_max = ifelse(any(!is.na(weight_measured_result_val)), max(weight_measured_result_val, na.rm = TRUE), NA),
    height_max = ifelse(any(!is.na(height_length_measured_result_val)), max(height_length_measured_result_val, na.rm = TRUE), NA),
    temp_max_clin_rec_wide = ifelse(any(!is.na(temperature_result_val)), max(temperature_result_val, na.rm = TRUE), NA)
  )

# #check data
# data_clinical_records_wide |>
#   distinct(mrn, .keep_all = TRUE) |>
#   select(mrn, ends_with("max"))
#
# #look for patient example with allegedly missing CRP
# missing_crp_patient <- data_clinical_records_wide |>
#   distinct(mrn, .keep_all = TRUE) |>
#   select(mrn, ends_with("max")) |>
#   filter(is.na(crp_max)) |>
#   pluck(1,1)
#
# #search for patient with allegedly missing CRP to confirm absence of any recorded CRP
# data_clinical_records_wide |>
#   filter(mrn == missing_crp_patient) |>
#   glimpse()
# # A: Can confirm: CRP missing for this patient

# # replace -Inf with NA_real_ ----------------------------------------------
#
# data_clinical_records_wide <- data_clinical_records_wide |>
#   mutate_if(is.numeric, ~if_else(is.infinite(.x), NA_real_, .x))

# add ideal body weight ---------------------------------------------------

# IBW (kilograms) = Constant + 0.91 (Height - 152.4)
# Where Constant = 50 for men & 45.5 for women
# Male average height = 175.3cm
# Female average height = 161.6cm

# add sex info from data_

sex_age_info <- data_cohort_admission_wrangled |>
  select(mrn, sex, age_on_admission)

data_clinical_records_wide <- data_clinical_records_wide |>
  left_join(sex_age_info) |>
  mutate(
    ibw = case_when(
      !is.na(height_max) & !is.na(sex) & age_on_admission > 16 ~
        case_when(
          sex == "Male" ~ 50 + 0.91 * (height_max - 152.4),
          sex == "Female" ~ 45.5 + 0.91 * (height_max - 152.4),
          TRUE ~ NA_real_
        ),
      TRUE ~ NA_real_
    )
  )

# check patients where ibw is negative?

data_clinical_records_wide |> 
  filter(!is.na(ibw)) |> 
  distinct(mrn, age_on_admission, ibw, sex, height_max, weight_max) |> 
  arrange((ibw))

# remove ibw where it's not plausible (arbitrary value ~ less than 40)...  a realistic minimum ideal body weight (IBW) for adults is typically around 40 kg. Values below 40 kg would be considered unusually low and worth investigating. 

str(data_clinical_records_wide$ibw)

data_clinical_records_wide |>
  filter(ibw <40) |> 
  distinct(mrn, age_on_admission, ibw, sex, height_max, weight_max)

ind <- which(data_clinical_records_wide$ibw < 40)

data_clinical_records_wide$ibw[ind] <- NA_character_

str(data_clinical_records_wide$ibw)

data_clinical_records_wide <- data_clinical_records_wide |> 
  mutate(ibw = as.numeric(ibw))

str(data_clinical_records_wide$ibw)

# Add BMI for patients where this can be relatively safely calculated -----------------------

# str(data_clinical_records_wide)

# For patients where age_on_admission >18 where weight_max and height_max are not missing, calculate BMI

data_bmi <- data_clinical_records_wide %>%
  filter(age_on_admission > 18, #calc for adults only
         height_max > 100, #min height 100cm
         weight_max > 15, #min weight 15kg
         !is.na(weight_max),
         !is.na(height_max)) %>%
  mutate(
    bmi = weight_max / ((height_max/100)^2)  # Convert height to meters and calculate BMI
  ) %>%
  select(mrn, weight_max, height_max, bmi)

data_clinical_records_wide <- data_clinical_records_wide |> 
  left_join(data_bmi,
            relationship = "many-to-many") #Joining with `by = join_by(mrn, weight_max, height_max)`

#some patients have very high recorded BMI
data_clinical_records_wide %>%
  distinct(mrn, weight_max, height_max, bmi) |> 
  filter(!is.na(bmi)) |> 
  arrange(desc(bmi))

data_clinical_records_wide %>%
  distinct(mrn, weight_max, height_max, bmi) |> 
  filter(!is.na(bmi)) |> 
  ggplot(aes(bmi)) +
  geom_histogram()

# Age vs Height -------------------------------------------------------------

# age_data <- data_cohort_admission |>
#   select(mrn, date_of_birth) |>
#   mutate(age = AMR::age(date_of_birth),
#          age_group = AMR::age_groups(age)) |>
#   distinct()
#
# # check min heights recorded vs age_group
# data_clinical_records_wide |>
#   left_join(age_data) |>
#   count(height_length_measured_result_val, age_group) |>
#   filter(!age_group == "0-11") |>
#   arrange((height_length_measured_result_val)) |>
#   print(n = 10)
#
# #check max heights recorded vs age_group
# data_clinical_records_wide |>
#   left_join(age_data) |>
#   count(height_length_measured_result_val, age_group) |>
#   filter(!age_group == "0-11") |>
#   arrange(desc(height_length_measured_result_val)) |>
#   print(n = 10)

# Bimodal distributions -------------------------------------------------------------

# # for weight
# data_clinical_records_wide |>
#   select(weight_measured_result_val) |>
#   ggplot(aes(weight_measured_result_val)) +
#   geom_histogram()
#
# # not so much for height
# data_clinical_records_wide |>
#   select(height_length_measured_result_val) |>
#   ggplot(aes(height_length_measured_result_val)) +
#   geom_histogram()
#
# # but for age, too
# age_data |>
#   ggplot(aes(age))+
#   geom_histogram()
#
# # check age_group vs. weight (children)
# data_clinical_records_wide |>
#   left_join(age_data) |>
#   count(weight_measured_result_val, age_group) |>
#   filter(age_group == "0-11") |> #check young people data
#   arrange((weight_measured_result_val))
#
# # check age_group vs. weight (older)
# data_clinical_records_wide |>
#   left_join(age_data) |>
#   count(weight_measured_result_val, age_group) |>
#   filter(!age_group == "0-11") |>
#   arrange(desc(weight_measured_result_val))
#
# # looks like the bimodal distribution is correct, and may reflect bimodal distribution of age of patients in dataset
#
# # Create frequency poly charts with patchwork using plot_freqpoly -----------------------------
# frequency_poly_plot_data_clin_rec <- data_clinical_records_wide |>
#   select(-record_num, -encntr_id, -earliest_import_date)
#
# # Use map to iterate over numeric columns and create plots
# plots_clin_rec <- lapply(names(frequency_poly_plot_data_clin_rec)[sapply(frequency_poly_plot_data_clin_rec, is.numeric)], plot_freqpoly, data = frequency_poly_plot_data_clin_rec)
#
# combined_plot_clin_rec <- patchwork::wrap_plots(plots_clin_rec, nrow = nrow(plots_clin_rec))
#
# combined_plot_clin_rec
