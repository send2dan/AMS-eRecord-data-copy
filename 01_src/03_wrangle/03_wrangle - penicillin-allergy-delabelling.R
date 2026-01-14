# wrangle

# Packages ----------------------------------------------------------------

library(tidytext) # for unnest_tokens()

# Read functions ----------------------------------------------------------

# source("./01_src/functions.R")

# Prerequisite R scripts --------------------------------------------------

# source(here("01_src", "02_data_import", "02_data_import - read database.R"))

# Which dates are imported ------------------------------------------------

data_cohort_admission_wrangled |>
  distinct(earliest_import_date)

# How many dates are imported ---------------------------------------------

data_cohort_admission_wrangled |>
  distinct(earliest_import_date) |>
  pull(earliest_import_date) |>
  length()

# How many patients were there in total? ----------------------------------

data_cohort_admission_wrangled |>
  distinct(mrn) |>
  pull(mrn) |>
  length()

# How many patients were prescribed antibiotics? --------------------------

data_medications_antimicrobials |>
  distinct(mrn) |>
  pull(mrn) |>
  length()

## check dates of antibiotic Rx ----------------------

## Variable type: POSIXct
## start_date_time has highest completion time

# data_medications_antimicrobials |>
#   skimr::skim()

# Check:
data_medications_antimicrobials |>
  distinct(mrn, order_name_c, medication_beta_lac) |>
  filter(medication_beta_lac == TRUE) |>
  count(order_name_c, medication_beta_lac) |>
  arrange(desc(n))

# Check:
# data_allergies_wrangled |>
#   filter(beta_lac_allergy == TRUE)

# Limit dataset size ------------------------------------------------------

# Restrict data_allergies_wrangled to only include patients with a penicillin allergy i.e. beta_lac_allergy == TRUE
data_allergies_beta_lac <- data_allergies_wrangled |>
  filter(beta_lac_allergy == TRUE) |>
  glimpse()

# Restrict data_medications_antimicrobials to only include patients prescribed beta_lac i.e. medication_beta_lac == TRUE
data_medications_antimicrobials_beta_lac <- data_medications_antimicrobials |>
  filter(medication_beta_lac == TRUE) |>
  glimpse()


# join data_allergies_wrangled with data_allergies_medications --------------

data_allergies_medications_beta_lac <- left_join(data_allergies_beta_lac,
  data_medications_antimicrobials_beta_lac,
  join_by(mrn),
  relationship = "many-to-many"
)


# # check completeness of fields in data_allergies_medications_beta_ --------
#
# # complete
# data_allergies_medications_beta_lac |>
#   skimr::skim() |>
#   as_tibble() |>
#   filter(complete_rate == 1)
#
# # incomplete
# data_allergies_medications_beta_lac |>
#   skimr::skim() |>
#   as_tibble() |>
#   filter(!complete_rate == 1) |>
#   print(n = 50)

# find patients with beta_lac allergy who were Rx beta_lac ----------------

data_allergies_medications_beta_lac |>
  filter(medication_beta_lac == TRUE & beta_lac_allergy == TRUE) |>
  distinct(mrn, order_name_c, concurrent_allergies, substance, active_status, severity, allergy_comment, reaction_ft_desc)

# # Work out when allergy first created OR last reviewed --------
#
# # which mrn's have substance where created_dt_tm does not == reviewed_dt_tm? A: None
#
# data_allergies_medications_beta_lac |>
#   as_tibble() |>
#   select(mrn, substance, created_dt_tm, reviewed_dt_tm) |>
#   #convert dt_tm fields to "%Y-%m-%d" format to avoid comparing the fields to the second
#   mutate(created_dt_tm_ymd = ymd(format(created_dt_tm, "%Y-%m-%d")),
#          reviewed_dt_tm_ymd = ymd(format(reviewed_dt_tm, "%Y-%m-%d"))) |>
#   #filter(!created_dt_tm == reviewed_dt_tm) |>
#   filter(created_dt_tm_ymd == reviewed_dt_tm_ymd)
#
# # are there any patients were created_dt_tm or reviewed_dt_tm are NA/missing? A: No
#
# data_allergies_medications_beta_lac |>
#   as_tibble() |>
#   select(mrn, substance, created_dt_tm, reviewed_dt_tm) |>
#   skimr::skim()
#
# # conclusion:
# # Checks show it's OK to use reviewed_dt_tm to work out/report when allergy first created OR last reviewed

# Use DT:datatable to embed an interactive table with export options in an R session or markdown document --------------

beta_lac_allergy_rx_beta_lac <- data_allergies_medications_beta_lac |>
  filter(medication_beta_lac == TRUE & beta_lac_allergy == TRUE) |>
  distinct(mrn, order_name_c, start_date_time, concurrent_allergies, substance, reviewed_dt_tm, active_status, severity, allergy_comment, reaction_ft_desc) |>
  as.data.frame() |>
  # extract the year, month, and day components from the POSIXct object and combine them into a new format.
  mutate(
    start_date_time_ymd = ymd(format(start_date_time, "%Y-%m-%d")),
    reviewed_dt_tm_ymd = ymd(format(reviewed_dt_tm, "%Y-%m-%d"))
  ) |>
  select(-start_date_time)

# Add cohort info to get current ward

## Work out where each patient was as of last earliest_import_date
data_cohort_admission_wrangled_latest_location <- data_cohort_admission_wrangled |>
  distinct(mrn, fin, patient, current_ward, current_bay, current_bed, earliest_import_date) |>
  arrange(mrn, earliest_import_date) |>
  group_by(mrn) |>
  mutate(earliest_import_date_latest = max(earliest_import_date)) |>
  filter(earliest_import_date == earliest_import_date_latest) |>
  select(-earliest_import_date_latest, -earliest_import_date)

# create current_bay_bed (bay bed) field ----------

data_cohort_admission_wrangled_latest_location$current_bay_bed <- str_c(
  data_cohort_admission_wrangled_latest_location$current_bay,
  "Bed",
  data_cohort_admission_wrangled_latest_location$current_bed,
  sep = " "
)

# Combine beta_lac_allergy_rx_beta_lac with data_cohort_admission_wrangled_latest_location to add location data to data_cohort_admission_wrangled_latest_location ----------

beta_lac_allergy_rx_beta_lac_location <- beta_lac_allergy_rx_beta_lac |>
  left_join(data_cohort_admission_wrangled_latest_location, by = join_by(mrn)) |>
  as_tibble() |>
  select(current_ward, current_bay_bed, patient, mrn, fin, order_name_c, start_date_time_ymd, substance, reviewed_dt_tm_ymd, active_status, severity, allergy_comment, reaction_ft_desc)

## Work out the most recent (based on start_date_time) antibiotic prescription (specified by order_name_c) for each patient (identified by mrn).

beta_lac_allergy_rx_beta_lac_location_latest <- beta_lac_allergy_rx_beta_lac_location |>
  group_by(mrn, order_name_c, substance) |>
  arrange(desc(start_date_time_ymd)) |> # arrange(desc(start_date_time)): Sorts the data within each group by start_date_time in descending order (latest first).
  slice(1) # slice(1): Selects the first row within each group, which will be the most recent prescription.

# make table IG compliant -------

beta_lac_allergy_rx_beta_lac_location_latest <- beta_lac_allergy_rx_beta_lac_location_latest |> 
  mutate(patient = get_initials(patient))

# Rename fields

beta_lac_allergy_rx_beta_lac_location_renamed <- beta_lac_allergy_rx_beta_lac_location_latest |>
  rename(
    ward = current_ward,
    `bay/bed` = current_bay_bed,
    initials = patient,
    allergy = substance,
    `allergy date` = reviewed_dt_tm_ymd,
    `allergy nature 1` = allergy_comment,
    `allergy nature 2` = reaction_ft_desc,
    `Rx` = order_name_c,
    `Rx date` = start_date_time_ymd,
    status = active_status,
  ) |>
  arrange(initials)

# print table

not_as_factor <- c("mrn", "initials")
interactive_table(beta_lac_allergy_rx_beta_lac_location_renamed,
  nrow = 5,
  not_as_factor = not_as_factor
)

# work out time difference between `Rx date` and `allergy date` ---------
# where time_diff is negative, this means the allergy was added AFTER treatment started

beta_lac_allergy_rx_beta_lac_location_renamed |>
  select(`Rx date`, `allergy date`) |>
  mutate(time_diff = `Rx date` - `allergy date`) |>
  distinct(mrn, .keep_all = TRUE) |>
  arrange(-desc(time_diff)) |>
  print(n = 100)

# how many distinct patients in final table? ---------

beta_lac_allergy_rx_beta_lac_location_renamed |>
  distinct(mrn) |>
  pull(mrn) |>
  length()

# check for duplicates ----------------

beta_lac_allergy_rx_beta_lac_location_renamed |>
  arrange(mrn)

# tidytext analysis of allergy reaction -----------------------------------

# This code utilizes the tidytext package to identify frequently occurring terms in the reaction descriptions after removing stop words.

data_allergies_medications_beta_lac |>
  glimpse()

data_allergies_medications_beta_lac |>
  filter(!is.na(reaction_ft_desc) | !is.na(allergy_comment)) |>
  distinct(mrn, order_name_c, substance, severity, allergy_comment, reaction_ft_desc)

# search for stop words
stop_words_c <- tidytext::stop_words |>
  pull(word)

# add further words to exclude from search of allergy_comment and reaction_ft_desc
append(stop_words_c, "and")
append(stop_words_c, "not")
append(stop_words_c, "amp")
append(stop_words_c, "comment")

# Top most common words included in allergy_comment amongst patients with a pencillin allergy (less common words get filed under "Other")

reaction_terms_allergy_comment <- data_allergies_medications_beta_lac |>
  distinct(mrn, substance, allergy_comment, .keep_all = TRUE) |>
  unnest_tokens(output = word, input = allergy_comment) |>
  filter(!word %in% stop_words_c) |>
  mutate(word_common = fct_lump_n(word, 10)) |>
  count(word_common) |>
  arrange(desc(n))

reaction_terms_allergy_comment |>
  print(n = 20)

# Top most common words included in reaction_ft_desc amongst patients with a pencillin allergy (less common words get filed under "Other")

reaction_terms_reaction_ft_desc <- data_allergies_medications_beta_lac |>
  distinct(mrn, substance, allergy_comment, .keep_all = TRUE) |>
  unnest_tokens(output = word, input = reaction_ft_desc) |>
  filter(!word %in% stop_words_c) |>
  mutate(word_common = fct_lump_n(word, 10)) |>
  count(word_common) |>
  arrange(desc(n))

reaction_terms_reaction_ft_desc |>
  print(n = 20)

# nature of concurrent allergies amongst patients with a pencillin allergy --------

# are patients with many different allergies a possible target for penicillin allergy delabelling? Particularly where the nature of the allergy is not specified?

data_allergies_medications_beta_lac |>
  filter(concurrent_allergies > 1) |>
  distinct(mrn, concurrent_allergies, substance, reaction_status, reaction_class, reaction_ft_desc) |>
  arrange(desc(concurrent_allergies), mrn) |>
  print(n = 100)

# which antimicrobial allergies do patients with multiple concurrent_allergies have?
patients_with_multiple_allergies <- data_allergies_medications_beta_lac |>
  filter(concurrent_allergies > 1 & beta_lac_allergy == TRUE) |>
  distinct(mrn) |>
  pull(mrn)

# search data_allergies_wrangled for patients_with_multiple_allergies (from data_allergies_medications_beta_lac)
data_allergies_wrangled |>
  filter(mrn %in% patients_with_multiple_allergies) |>
  distinct(mrn, substance, concurrent_allergies) |>
  arrange(desc(concurrent_allergies))

data_allergies_medications_beta_lac |>
  filter(concurrent_allergies > 1) |>
  filter(!is.na(order_name_c)) |>
  distinct(mrn, concurrent_allergies, order_name_c, start_date_time, substance, reaction_status, reaction_class, reaction_ft_desc) |>
  arrange(desc(concurrent_allergies))

# Proportion of patients with a documented beta_lac allergy --------

# all distinct patients where beta_lac_allergy == TRUE
mrn_distinct_with_beta_lac_allergy <- data_allergies_wrangled |>
  distinct(mrn, beta_lac_allergy, .keep_all = TRUE) |> # multiple rows at this point should indicate the same patient.
  filter(beta_lac_allergy == TRUE) |>
  distinct(mrn) |>
  pull(mrn)

length(mrn_distinct_with_beta_lac_allergy)

# all distinct patients where beta_lac_allergy == TRUE
mrn_distinct_any_allergy_total <- data_allergies_wrangled |>
  distinct(mrn, substance, .keep_all = TRUE) |> # multiple rows at this point should indicate the same patient.
  filter(!is.na(substance)) |>
  distinct(mrn) |>
  pull(mrn)

length(mrn_distinct_any_allergy_total)

# all distinct patients
mrn_distinct_total <- data_cohort_admission_wrangled |>
  distinct(mrn, .keep_all = TRUE) |>
  pull(mrn)

length(mrn_distinct_total)

# perc patients with a recorded allergy (to any medication)
length(mrn_distinct_any_allergy_total) / length(mrn_distinct_total) * 100

# perc patients with a recorded allergy (to beta-lactams)
length(mrn_distinct_with_beta_lac_allergy) / length(mrn_distinct_total) * 100

# Calculate percentages
percentage_allergy <- length(mrn_distinct_with_beta_lac_allergy) / length(mrn_distinct_total) * 100
percentage_no_allergy <- 100 - percentage_allergy

# Create a data frame
df <- data.frame(
  group = c("With beta-lactam allergy", "Without beta-lactam allergy"),
  value = c(percentage_allergy, percentage_no_allergy)
)

# Create the pie chart with percentages
ggplot(df, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(value, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "") +
  theme_void() +
  theme(legend.position = "right")

# Proportion of patients with a documented allergy by age group --------

age_group_data <- data_cohort_admission_wrangled |>
  select(mrn, age_group)

(data_for_join <- data_allergies_medications_beta_lac |>
  # filter(beta_lac_allergy == TRUE) |>
  left_join(age_group_data,
    relationship = "many-to-many",
    by = join_by(mrn)
  ) |>
  group_by(age_group) |>
  distinct(mrn, .keep_all = TRUE) |>
  select(age_group, beta_lac_allergy) |>
  group_by(beta_lac_allergy, age_group) |>
  add_count(name = "n_with_allergy") |>
  distinct(age_group, .keep_all = TRUE) |>
  arrange(age_group, beta_lac_allergy) |>
  filter(beta_lac_allergy == TRUE) |>
  ungroup()
)

(n_per_age_group <- data_cohort_admission_wrangled |>
  group_by(age_group) |>
  distinct(mrn, .keep_all = TRUE) |>
  count(name = "n_per_age_group") |>
  ungroup()
)

# join data_for_join with n_per_age_group to get perc for n_with_allergy

(perc_patients_with_allergy <- data_for_join |>
  left_join(n_per_age_group, by = join_by(age_group)) |>
  mutate(perc = n_with_allergy / n_per_age_group * 100) |>
  mutate(age_group = fct_relevel(age_group,
    "100+",
    after = Inf
  )) |>
  arrange(age_group)
)

perc_patients_with_allergy |>
  ggplot(aes(perc, age_group)) +
  geom_col() +
  ylab("age group") +
  xlab("% patients per a penicillin allergy")

# analyse import_date for reporting purposes ------------------------------

import_date_report <- data_cohort_admission_wrangled |>
  distinct(earliest_import_date) |>
  pull()

format(ymd(import_date_report), "%A %d %B %Y")

import_date_report_formatted <- format(ymd(import_date_report), "%A %d %B %Y")

# admission timeframe from data_cohort_admission_wrangled -----------------

# This code finds the earliest and latest admission dates to understand the data timeframe.

start_date_admission_dt <- min(data_cohort_admission_wrangled$admission_dt)
end_date_admission_dt <- max(data_cohort_admission_wrangled$admission_dt)

# Earliest admission date for patients in this cohort:
format(start_date_admission_dt, "%d %B %Y")

# Latest admission date for patients in this cohort:
format(end_date_admission_dt, "%d %B %Y")
