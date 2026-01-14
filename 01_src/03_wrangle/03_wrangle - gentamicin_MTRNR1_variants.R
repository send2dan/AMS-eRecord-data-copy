
# Dependencies ------------------------------------------------------------

# source("./01_src/03_wrangle/03_wrangle - gentamicin_dosing_weight_height_sex_2.R")

# To get: wrangled gentamicin dadata (data_antimicrobial_pool_gentamicin_clin_rec)

glimpse(data_antimicrobial_pool_gentamicin_clin_rec)


# number of patients prescribed gentamicin --------------------------------

gentamicin_mrn_n <- data_antimicrobial_pool_gentamicin_clin_rec |> 
  distinct(mrn) |> 
  count() |> 
  pull()

gentamicin_mrn_n

# source("./01_src/02_data_import/02_data_import - Read genetics database - MTRNR1 variants.R")

# To get: genetics data, too (data_mtrnr1)

glimpse(data_mtrnr1)

# Change date format ------------------------------------------------------

data_mtrnr1 <- data_mtrnr1 %>%
  mutate(
    activation_date = ymd(activation_date),
    outcome_date = ymd(outcome_date)
  )

# data exploration --------------------------------------------------------


# nhs number and MRN number -----------------------------------------------

#nhs number vs. mrn number
data_mtrnr1 |>
  count(!is.na(nh_snumber), !is.na(mrn)) |> 
  arrange(desc(n))

# date range (activation_date) i.e. request date -----------
data_mtrnr1_dates <- data_mtrnr1 |> 
  reframe(date_range = range(activation_date, na.rm = TRUE, finite = TRUE))

data_mtrnr1_dates

format(data_mtrnr1_dates[[1]][1], '%a %B %d %Y')

#dates of requests
plot_mtrnr1_tests_per_month <- data_mtrnr1 %>%
  distinct(sample_number, investigation, activation_date, .keep_all = TRUE) %>%
  filter(activation_date >= ymd("2024-01-01")) %>%
  mutate(month = floor_date(activation_date, "month")) %>%
  group_by(result) |> 
  count(month, name = "n_tests") %>%
  ggplot(aes(x = month, y = n_tests, fill = result)) +
  geom_col() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(breaks = pretty_breaks(n = 5), labels = scales::number_format(accuracy = 1)) +
  labs(
    title = "Number of MTRNR1 Test Requests per Month since Jan 2024",
    x = "Month",
    y = "Number of Tests"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_mtrnr1_tests_per_month


# result ------------------------------------------------------------------

mtrnr1_pos_n <- data_mtrnr1 |> 
  distinct(sample_test_id, .keep_all = TRUE) |> 
  count(result) |> 
  filter(result == "hom") |> 
  pull(n) 

mtrnr1_pos_n

#only one patient with an MRN (i.e. a NUTH patient) has had the mutation detected as of 16/07
data_mtrnr1 |>
  filter(result == "hom") |> 
  count(!is.na(nh_snumber), !is.na(mrn)) |> 
  arrange(desc(n))

#only one patient with an MRN (i.e. a NUTH patient) has had the mutation detected as of 16/07
mtrnr1_pos_n_with_mrn <- data_mtrnr1 |>
  distinct(mrn, result, .keep_all = TRUE) |> 
  filter(result == "hom", 
         !is.na(mrn)) |> 
  count() |> 
  pull()

mtrnr1_pos_n_with_mrn

#hospital code --------------
# The Newcastle upon Tyne Hospitals NHS Foundation Trust's registration code is RTD. This code is used for various purposes, including registration with the Care Quality Commission (CQC) and the Data Security and Protection Toolkit. It signifies the trust's legal authorization to provide healthcare services and its adherence to data security standards
data_mtrnr1 |>
  count(hosp_code) |> 
  arrange(desc(n)) 








# how many NHS numbers are there?

data_mtrnr1 |> 
  filter(!is.na(nh_snumber)) |> 
  distinct(nh_snumber) |> 
  pull(nh_snumber) |> 
  length()

# how many MRNs are there?

data_mtrnr1_n_tested_with_mrn <- data_mtrnr1 |> 
  filter(!is.na(mrn)) |> 
  distinct(mrn) |> 
  pull(mrn) |> 
  length()

data_mtrnr1_n_tested_with_mrn

# how many MRNs AND NHS numbers are there?

data_mtrnr1 |> 
  filter(!is.na(mrn),
         !is.na(nh_snumber)) |> 
  count()

# how many tests are there in the total dataset?

data_mtrnr1 |> 
  glimpse()

data_mtrnr1_n_tests <- data_mtrnr1 |> 
  distinct(sample_test_id) |> 
  pull(sample_test_id) |> 
  length()

# wrangle genetics data to facilitate joining to gentamicin data --------------------------------------------------

data_mtrnr1_wrangled <- data_mtrnr1 |> 
  # ensure nhs number column/field named appropriately, i.e. nhs_number 
  rename(nhs_number = nh_snumber) |> 
  # ensure no spaces in nhs number
  mutate(nhs_number = str_replace_all(nhs_number, " ", "")) 

data_mtrnr1_wrangled |> 
  filter(!is.na(nhs_number))

glimpse(data_mtrnr1_wrangled)

data_mtrnr1_wrangled |> 
  skimr::skim_without_charts()

data_mtrnr1_wrangled |> 
  distinct(sample_number, .keep_all = TRUE) |> 
  count(result)

# wrangle gentamicin data to ensure it can be joined onto -------------------

glimpse(data_antimicrobial_pool_gentamicin_clin_rec)

data_antimicrobial_pool_gentamicin_clin_rec <- data_antimicrobial_pool_gentamicin_clin_rec |> 
  # ensure nhs number column/field named appropriately, i.e. nhs_number 
  rename(nhs_number = `nhs number`) 

# join genetics data onto gentamicin data ---------------------------------

data_gentamicin_mtrnr1 <- data_antimicrobial_pool_gentamicin_clin_rec |> 
  left_join(data_mtrnr1_wrangled,
            by = join_by(mrn)) #join only be MRN number 
# this is to avoid the need for each dataset to have both a complete MRN number AND complete NHS number, and maximise the chances of 

data_gentamicin_mtrnr1 |> 
  glimpse()

# look at NHS numbers .x vs .y and MRNs

data_gentamicin_mtrnr1 |> 
  select(mrn, nhs_number.x, nhs_number.y) |> 
  skimr::skim_without_charts()

# how many MRNs AND NHS numbers are there?

data_gentamicin_mtrnr1 |> 
  filter(!is.na(mrn),
         !is.na(nhs_number.x)) |> 
  count()


# which patients have been tested? ----------------------------------------

data_gentamicin_mtrnr1 |> 
  count(activation_date)

data_gentamicin_mtrnr1 |> 
  count(activation_date, outcome_date, result)

data_gentamicin_mtrnr1 |> 
  skimr::skim_without_charts()

data_gentamicin_mtrnr1 |> 
  filter(!is.na(activation_date))

# summary -----------------------------------------------------------------

# Summarise number of patients tested for MTRNR1 variants,
# considering activation_date may be NA

data_gentamicin_mtrnr1 |> 
  count(activation_date)

data_gentamicin_mtrnr1_summary <- data_gentamicin_mtrnr1 %>%
  filter(!is.na(activation_date)) %>%  # Only keep records with valid activation_date
  distinct(mrn) %>%  # Count unique patients, assuming 'mrn' is the patient identifier
  summarise(n_tested = n())

mtrnr1_tested_n_with_mrn <- data_gentamicin_mtrnr1_summary |> 
  pull(n_tested)

mtrnr1_tested_n_with_mrn

# If n_tested is 0 (or data frame is empty), then no patients have been tested
if (nrow(data_gentamicin_mtrnr1_summary) == 0 || data_gentamicin_mtrnr1_summary$n_tested == 0) {
  message("No gentamicin-prescribed patients have been tested for MTRNR1 variants.")
  #n_tested = 0
} else {
  print(data_gentamicin_mtrnr1_summary)
}

# how many patients have been tested? Answer ----------
mtrnr1_tested_n_with_mrn

# details of tested patients -----------

# Filter only tested patients (activation_date not missing)
tested_mrns <- data_gentamicin_mtrnr1 %>%
  filter(!is.na(activation_date)) %>%
  distinct(mrn, activation_date, result) |> 
  rename(`request date` = activation_date)

# Check if anyone has been tested
if (nrow(tested_mrns) > 0) {
  # Create a tibble with those MRNs
  tibble_tested_mrns <- as_tibble(tested_mrns)
  print(tibble_tested_mrns)
} else {
  message("No gentamicin-prescribed patients have been tested for MTRNR1 variants.")
  tibble_tested_mrns <- as_tibble(tested_mrns)
}

tibble_tested_mrns

interactive_table(tibble_tested_mrns)

