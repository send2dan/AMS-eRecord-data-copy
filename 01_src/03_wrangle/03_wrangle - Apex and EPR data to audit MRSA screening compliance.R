# https://github.com/send2dan/AMS-eRecord-data/issues/142

# pre-req:
# 02_data_import - Apex MRSA data import.R

# # Basic structure and summary ---------------
# str(mrsa_historical_data)
# 
# 
# # mrsa_historical_data |> 
# #   skimr::skim()
# 
# # First and last few rows -------------------
# head(mrsa_historical_data)
# tail(mrsa_historical_data)
# 
# # dimensions --------------------------------
# dim(mrsa_historical_data)
# 
# # Column names ------------------------------
# colnames(mrsa_historical_data)
# 
# # Missing values check -------------------------
# colSums(is.na(mrsa_historical_data))
# 
# # Duplicate checking ------------------
# janitor::get_dupes(mrsa_historical_data)
# 
# # Remove any duplicates ---------------------------------------------------
# mrsa_historical_data <- mrsa_historical_data |> 
#   distinct()
# 
# #check
# janitor::get_dupes(mrsa_historical_data)
# 
# # # top levels for factors ---------------------
# # mrsa_historical_data |> 
# #   mutate(consultant = as_factor(location_code)) |> 
# #   reframe(across(where(is.factor), janitor::top_levels, .names = "top_levels_{.col}"))
# # # Doesn't seem to work very well
# 
# # get_one_to_one ---------------
# # Find the list of columns that have a 1:1 mapping to each other
# janitor::get_one_to_one(mrsa_historical_data)

########################
# Wrangling of LIMS data  ------------------
########################

mrsa_historical_data_original <- mrsa_historical_data

# Rename fields ----------------------------------
#This step undertaken so it gets less confusing with joining mrsa_historical_data onto data_cohort_admission_wrangled
mrsa_historical_data <- mrsa_historical_data |> 
  rename(mrn = hospital_number,
         patient_name_lims = patient_full_name)

#Remove DOB from LIMS data as this is included in EPR data already -------------------------------
mrsa_historical_data <- mrsa_historical_data |> 
  select(-date_of_birth)

########################
# Basic analysis/data checking steps ------------------
########################

# From which specimen types have we isolated MRSA?

mrsa_historical_data |> 
  distinct(specimen_no, .keep_all = TRUE) |> 
  count(specimen_type_code, order_code, organism_code, quantifier) |> 
  arrange(desc(n)) |> 
  print(n = 100)

#check specimen_type_code POL
## These are environmental screening samples... exclude
mrsa_historical_data |> 
  distinct(specimen_no, .keep_all = TRUE) |> 
  filter(specimen_type_code == "POL") |> 
  count(patient_name_lims) 

mrsa_historical_data <- mrsa_historical_data |> 
  filter(!specimen_type_code == "POL")

########################
# Join LIMS and EPR data ------------------
########################

data_cohort_admission_mrsa <- data_cohort_admission_wrangled |> 
  left_join(mrsa_historical_data,
            by = join_by(mrn))

# analyse import_date for reporting purposes (for .QMD) ------------------------------

import_date_report <- data_cohort_admission_wrangled |>
  distinct(earliest_import_date) |>
  pull()

import_date_report_formatted <- format(ymd(import_date_report), "%A %d %B %Y")

########################
# Basis analysis of https://github.com/send2dan/AMS-eRecord-data/issues/142 ------------------
########################

str(data_cohort_admission_mrsa)

data_cohort_admission_mrsa |> 
  glimpse()

# data_cohort_admission_mrsa |> 
#   skimr::skim()

# First and last few rows -------------------
head(data_cohort_admission_mrsa)
tail(data_cohort_admission_mrsa)

# dimensions --------------------------------
dim(data_cohort_admission_mrsa)

# Column names ------------------------------
colnames(data_cohort_admission_mrsa)

# Missing values check -------------------------
colSums(is.na(data_cohort_admission_mrsa))

# Duplicate checking ------------------
janitor::get_dupes(data_cohort_admission_mrsa)

# Remove duplicates from data_cohort_admission_mrsa ---------

data_cohort_admission_mrsa <- data_cohort_admission_mrsa |> 
  distinct()

# # top levels for factors ---------------------
# data_cohort_admission_mrsa |> 
#   mutate(consultant = as_factor(location_code)) |> 
#   reframe(across(where(is.factor), janitor::top_levels, .names = "top_levels_{.col}"))
# # Doesn't seem to work very well

# get_one_to_one ---------------
# Find the list of columns that have a 1:1 mapping to each other
janitor::get_one_to_one(data_cohort_admission_mrsa)

########################
# Wrangling data_cohort_admission_mrsa ------------------
########################

# remove exempted wards, using current_ward ---------------------------------------------------

### Summary Table RE: MRSA screening exemptions
#  Hospital | Ward/Area          | MRSA Screening on Admission?         |
# ----------|--------------------|--------------------------------------|
#  RVI      | GNCH 2b, 8         | No (day case areas, exempt)          |
#  RVI      | GNCH 6 PED         | No (exempt)                          |
#  RVI      | RV32/33/41         | No (unless C-section/high-risk)      |
#  RVI      | RV40               | No (day case area, exempt)           |
#  RVI      | RV21, 39           | No (day case areas, exempt)          |
#  Freeman  | [All listed wards] | Yes (no exemptions listed)           |

# #location codes (based on lab data)
# data_cohort_admission_mrsa |> 
#   count(location_code) |> 
#   print(n = 150)

#current ward data (based on EPR data)
data_cohort_admission_mrsa |> 
  count(current_ward) |> 
  print(n = 150)

data_cohort_admission_mrsa <- data_cohort_admission_mrsa |> 
  filter(!str_detect(current_ward, "GNCH02B|GNCH08|GNCH06|RV32|RV33|RV41|RV40|RV21|RV39"))

# Only included patients admitted in the 6 weeks prior to last date of EPR data import --------

epr_data_import_date <- lubridate::as_date(unique(data_cohort_admission_mrsa$earliest_import_date))

data_cohort_admission_mrsa <- data_cohort_admission_mrsa |> 
  filter(admission_dt > epr_data_import_date - weeks(6)) 

#check if any data_cohort_admission_mrsa$admission_dt == NA
data_cohort_admission_mrsa |> 
  filter(is.na(admission_dt))

#check
range(data_cohort_admission_mrsa$admission_dt)

# Enable removal of all MRSA screening results for specimens received more than 2 days after admission, where required ------------
## To be categorised as compliant with the Trust's MRSA policy, a patient's MRSA screening sample must be received by the laboratory within the period starting 18 weeks prior to admission. 
## Samples received by the laboratory one day after the date of hospital admission will be labelled as compliant. This is to account for MRSA screening specimens collected late on the date of hospital admission, when current arrangements around specimen transport of non-urgent samples may mean that the specimen isn't received until one day after hospital admission.

#compare admission_dt & receive_date where MRSA screens were collected later than 1 day after admission?
data_cohort_admission_mrsa |> 
  filter(specimen_type_code == "MRSA") |> 
  distinct(specimen_no, .keep_all = TRUE) |> 
  mutate(admission_date_plus_1 = admission_dt + days(1)) |> 
  filter(receive_date > admission_date_plus_1) |> 
  distinct(location_code, mrn, admission_dt, receive_date, specimen_type_code, specimen_no, organism_code) |> 
  head(10)

#which location_code areas account for MRSA screens collected later than 1 day after admission?
## NB this includes patients who have had multiple MRSA screens done
## i.e. Some patients with MRSA screens collected later than 1 day after admission may have been appropriately screened pre-admission
location_code_mrsa_screen_collected_later_than_1_day_after_admission <- data_cohort_admission_mrsa |> 
  filter(specimen_type_code == "MRSA") |> 
  distinct(specimen_no, .keep_all = TRUE) |> 
  mutate(admission_date_plus_1 = admission_dt + days(1)) |> 
  filter(receive_date > admission_date_plus_1) |> 
  mutate(location_code_lump = fct_lump_n(location_code, 10)) |> 
  count(location_code_lump) |> 
  ggplot(aes(fct_reorder(location_code_lump, n), n)) +
  geom_col() +
  coord_flip() 

location_code_mrsa_screen_collected_later_than_1_day_after_admission

#pull specimen_no where MRSA screens collected later than 1 day after admission
spec_no_mrsa_screen_received_after_admission <- data_cohort_admission_mrsa |> 
  filter(specimen_type_code == "MRSA") |> 
  mutate(admission_date_plus_1 = admission_dt + days(1)) |> 
  filter(receive_date > admission_date_plus_1) |> 
  pull(specimen_no)

#create new field: spec_mrsa_screen_received_after_adm
## this can be used to filter data_cohort_admission_mrsa where MRSA screens collected later than 1 day after admission
data_cohort_admission_mrsa <- data_cohort_admission_mrsa |> 
  mutate(spec_mrsa_screen_received_after_adm = specimen_no %in% spec_no_mrsa_screen_received_after_admission)

#check output
data_cohort_admission_mrsa |> 
  distinct(specimen_no, .keep_all = TRUE) |> 
  count(spec_mrsa_screen_received_after_adm)

###########################
# admission_dt timeframe from data_cohort_admission_mrsa -----------------
###########################

# This code finds the earliest and latest admission dates to understand the data timeframe.

# admission timeframe from data_cohort_admission_mrsa needs to only include patients admitted within the past 6 weeks (in order to include MRSA screening window of up to 18 weeks prior to admission)

start_date_admission_dt <- min(data_cohort_admission_mrsa$admission_dt)
end_date_admission_dt <- max(data_cohort_admission_mrsa$admission_dt)

print(paste("Earliest admission date for patients in this cohort:", format(start_date_admission_dt, "%d %B %Y")))
print(paste("Latest admission date for patients in this cohort:", format(end_date_admission_dt, "%d %B %Y")))


########################
# Earliest and latest dates for MRSA isolation ------------------
########################

# Find the earliest date
mrsa_earliest_date <- min(data_cohort_admission_mrsa$receive_date, na.rm = TRUE)
mrsa_earliest_date_formatted <- format(ymd(mrsa_earliest_date), "%A %d %B %Y")
mrsa_earliest_date_formatted

# Find the latest date
mrsa_latest_date <- max(data_cohort_admission_mrsa$receive_date, na.rm = TRUE)
mrsa_latest_date_formatted <- format(ymd(mrsa_latest_date), "%A %d %B %Y")
mrsa_latest_date_formatted

# Calculate weeks between dates
mrsa_weeks_difference <- interval(ymd(mrsa_earliest_date), ymd(mrsa_latest_date)) / weeks(1)

# Round to whole number if needed
mrsa_weeks_difference_rounded <- round(mrsa_weeks_difference)

print(paste("Number of weeks between dates:", mrsa_weeks_difference))
# or for rounded value
print(paste("Number of weeks between dates (rounded):", mrsa_weeks_difference_rounded))


###################
#how many patients in total in whole cohort (data_cohort_admission_mrsa) -------------
###################
data_cohort_admission_mrsa_mrn <- data_cohort_admission_mrsa |> 
  distinct(mrn) |> 
  pull(mrn)

data_cohort_admission_mrsa_mrn_total <- length(data_cohort_admission_mrsa_mrn)

data_cohort_admission_mrsa_mrn_total

###################
#add a column stating if patient is MRSA pos and if patient has had an MRSA bacteraemia -------------
###################

# MRSA pos (any specimen type) --------------------------------------------
mrsa_pos_pt_mrn <- data_cohort_admission_mrsa |> 
  filter(quantifier == "I") |> 
  distinct(mrn) |> 
  pull(mrn) 

mrsa_pos_pt_total <- length(mrsa_pos_pt_mrn)

mrsa_pos_pt_total

# MRSA pos and MRSA bacteraemia --------------------------------------------------------
## Can't use str_detect(specimen_type_code, "BC") & quantifier == "I" as this includes PLEFBC
## Therefore, using specimen_type_code == "BC" & quantifier == "I"

#Check all specimen_type_code
data_cohort_admission_mrsa |> 
  count(specimen_type_code) 

#which patients have had an MRSA bacteraemia? ----------
mrsa_pos_bacteraemia_mrn <- data_cohort_admission_mrsa |> 
  filter(specimen_type_code == "BC" & quantifier == "I") |> 
  distinct(mrn) |> 
  pull(mrn) 

#label patients as MRSA pos and MRSA bacteraemia ------------
data_cohort_admission_mrsa <- data_cohort_admission_mrsa |> 
  mutate(mrsa_pos = mrn %in% mrsa_pos_pt_mrn, #mrsa pos any site
         mrsa_bacteraemia = mrn %in% mrsa_pos_bacteraemia_mrn) #mrsa bacteraemia


# which patients are MRSA pos ---------------------------------------------

# table_mrsa_pos <- data_cohort_admission_mrsa |> 
#   filter(mrsa_pos == TRUE) |> 
#   distinct()

#how many patients have had an MRSA bacteraemia? ----------
mrsa_pos_bacteraemia_pt_total <- length(mrsa_pos_bacteraemia_mrn)

mrsa_pos_bacteraemia_pt_total

# what proportion of current (MRSA-pos) inpatients (i.e. MRSA isolated within period [18 weeks prior to admission - today]) have had an MRSA bacteraemia? --------
prop_mrsa_pos_mrsa_bacteraemia <- mrsa_pos_bacteraemia_pt_total / mrsa_pos_pt_total * 100

prop_mrsa_pos_mrsa_bacteraemia_rounded <- round(prop_mrsa_pos_mrsa_bacteraemia, 1)

prop_mrsa_pos_mrsa_bacteraemia_rounded

#details of distinct specimens collected from current inpatients with... MRSA isolated... from any specimen type... received for culture at NUTH in the 18 week period preceding admission
mrsa_pos_pt_mrn_specimens <- data_cohort_admission_mrsa |> 
  filter(mrsa_pos == TRUE) |> 
  distinct(specimen_no, .keep_all = TRUE) |>
  select(mrn, location_code, admission_dt, receive_date, specimen_type_code, specimen_no, quantifier, organism_code, spec_mrsa_screen_received_after_adm) |> 
  distinct() |> 
  arrange(mrn, receive_date)

mrsa_pos_pt_mrn_specimens |> 
  count(quantifier, specimen_type_code)

#####################
# Distinct patients with MRSA screen received within MRSA policy-compliant time period (n)  -------------
#####################

#confirmation it's specimen_type_code we need (and not order_code)
data_cohort_admission_mrsa |>
  count(specimen_type_code, order_code, organism_code, quantifier) |>
  print(n = 100)
## NA == patients who were not screened

#is it spec_mrsa_screen_received_after_adm == FALSE & specimen_type_code == "MRSA" we're after?
data_mrsa_screen_compliant_true_details <- data_cohort_admission_mrsa |>
  filter(spec_mrsa_screen_received_after_adm == FALSE & specimen_type_code == "MRSA") |> 
  select(mrn, location_code, admission_dt, receive_date, specimen_type_code, specimen_no, quantifier, organism_code, spec_mrsa_screen_received_after_adm) |> 
  distinct() |> 
  arrange(mrn, receive_date)

data_mrsa_screen_compliant_true_details

data_mrsa_screen_compliant_true_details |> 
  distinct(specimen_no, .keep_all = TRUE) |> 
  count()

################
# filter data into compliant vs. non-compliant ----------------------------
################

#bear in mind total patients in cohort
data_cohort_admission_mrsa_mrn_total

#how many mrsa screens are there in total?
data_cohort_admission_mrsa |> 
  filter(specimen_type_code == "MRSA") |> 
  distinct(specimen_no) |> 
  count()
#but some patients had had many mrsa screens collected...

# compliant patients ---------------------------------------------------------------
## Each patient must have:
## a) had an MRSA screen collected, i.e. specimen_type_code == "MRSA"
## b) this MRSA screen must be collected where spec_mrsa_screen_received_after_adm == FALSE

#look for compliant patients
data_cohort_admission_mrsa |> 
  filter(spec_mrsa_screen_received_after_adm == FALSE & specimen_type_code == "MRSA") |> 
  distinct(specimen_no, .keep_all = TRUE) |> 
  select(mrn, admission_dt, receive_date, specimen_no, specimen_type_code, order_code, quantifier) |> 
  arrange(mrn, receive_date)

# compliant patient MRNs ---------------------
data_mrsa_screen_compliant_mrn <- data_cohort_admission_mrsa |> 
  filter(spec_mrsa_screen_received_after_adm == FALSE & specimen_type_code == "MRSA") |> 
  distinct(mrn) |> 
  pull(mrn)

length(data_mrsa_screen_compliant_mrn)


# where did MRSA screening get undertaken for compliant patients? ---------

data_cohort_admission_mrsa |> 
  filter(spec_mrsa_screen_received_after_adm == FALSE & specimen_type_code == "MRSA") |> 
  #distinct(patient, mrn, .keep_all = TRUE) |> 
  distinct(mrn, specimen_no, .keep_all = TRUE) |> 
  mutate(location_code_lump = fct_lump(location_code, 10)) |> 
  count(location_code_lump) |> 
  arrange(desc(n)) 

######### create chart

# Prepare data: count where MRSA screening was undertaken for compliant patients
mrsa_screening_location <- data_cohort_admission_mrsa |>
  filter(
    spec_mrsa_screen_received_after_adm == FALSE,
    specimen_type_code == "MRSA"
  ) |>
  distinct(specimen_no, .keep_all = TRUE) |>
  mutate(location_code_lump = fct_lump(location_code, 20)) |>
  count(location_code_lump) |>
  filter(!location_code_lump == "Other") |> #filter out Other to leave only top 10
  arrange(desc(n))

top_location_undertaking_mrsa_screening <- as.character(mrsa_screening_location |> 
  pluck(1,1))

# ggplot: Visualise screening locations
mrsa_screening_location_chart <- ggplot(mrsa_screening_location) +
  geom_col(
    aes(
      x = fct_reorder(location_code_lump, n),
      y = n
    ),
    fill = "steelblue"
  ) +
  labs(
    x = NULL,
    y = "MRSA Screens (n)",
    title = "Where was MRSA Screening Undertaken for Compliant Patients?",
    caption = "Only the top 20 locations are shown."
  ) +
  theme_minimal() +
  coord_flip()

# mrsa_screening_location_chart
  
####################
# create new variable... mrsa_screen_compliant (true/false) ---------------
####################
data_cohort_admission_mrsa <- data_cohort_admission_mrsa |> 
  mutate(mrsa_screen_compliant = mrn %in% data_mrsa_screen_compliant_mrn)

####################
#create new df data_mrsa_screen_compliant -----------------
###################
data_mrsa_screen_compliant <- data_cohort_admission_mrsa |> 
  filter(mrn %in% data_mrsa_screen_compliant_mrn)

#check ------------
data_mrsa_screen_compliant %>%
  distinct(specimen_no, .keep_all = TRUE) |> 
  group_by(mrn) %>%
  arrange(receive_date) %>%
  mutate(
    screen_number = row_number(),
    days_from_admission = as.numeric(difftime(receive_date, admission_dt, units = "days"))
  ) |> 
  select(mrn, admission_dt, receive_date, days_from_admission, screen_number, mrsa_screen_compliant, specimen_no) |> 
  ungroup() |> 
  arrange(mrn, receive_date)

#check specimen_type_code, order_code ------------
data_mrsa_screen_compliant |> 
  count(specimen_type_code, order_code, quantifier)

#how many MRNs are MRSA screening compliant? --------------
data_mrsa_screen_compliant_mrn <- data_mrsa_screen_compliant |> 
  distinct(mrn) |> 
  pull(mrn)

length(data_mrsa_screen_compliant_mrn)

data_mrsa_screen_compliant_n_true <- data_mrsa_screen_compliant |> 
  distinct(mrn) |> 
  count() |> 
  pull(n)

data_mrsa_screen_compliant_n_true

#####################
# Distinct patients **without** MRSA screen received within MRSA policy-compliant time period (n)  -------------
#####################

####################
#create new df data_mrsa_screen_non_compliant -----------------
###################
data_mrsa_screen_non_compliant <- data_cohort_admission_mrsa |> 
  filter(!mrn %in% data_mrsa_screen_compliant_mrn)

#check -------------
data_mrsa_screen_non_compliant %>%
  distinct(specimen_no, .keep_all = TRUE) |> 
  group_by(mrn) %>%
  arrange(receive_date) %>%
  mutate(
    screen_number = row_number(),
    days_from_admission = as.numeric(difftime(receive_date, admission_dt, units = "days"))
  ) |> 
  select(mrn, admission_dt, receive_date, days_from_admission, screen_number, mrsa_screen_compliant, specimen_no) |> 
  ungroup() |> 
  arrange(mrn, receive_date)

#check specimen_type_code, order_code ------------
data_mrsa_screen_non_compliant |> 
  count(specimen_type_code, order_code, quantifier)
## where specimen_type_code == NA, this is a patient without an MRSA screen
## i.e. lots of patients without MRSA screens as of Jan 2025

#check again, concentrating on patients where a sample WAS received, but not in a timely fashion (i.e. !is.na(specimen_no))
data_mrsa_screen_non_compliant |> 
  filter(!is.na(specimen_no)) |> #i.e. sample received
  distinct(specimen_no, .keep_all = TRUE) |> 
  arrange(mrn, admission_dt, receive_date) |> 
  select(mrn, admission_dt, receive_date, quantifier, mrsa_screen_compliant, mrsa_pos, specimen_no)

#get MRNs or screening non-compliant patients
data_mrsa_screen_non_compliant_mrn <- data_mrsa_screen_non_compliant |> 
  distinct(mrn) |> 
  pull(mrn)

length(data_mrsa_screen_non_compliant_mrn)

data_mrsa_screen_compliant_n_false <- data_mrsa_screen_non_compliant |> 
  distinct(mrn) |> 
  count() |> 
  pull(n)

data_mrsa_screen_compliant_n_false

#####################
# check data   -------------
#####################
intersect(data_mrsa_screen_non_compliant_mrn, data_mrsa_screen_compliant_mrn)

setdiff(data_mrsa_screen_non_compliant_mrn, data_mrsa_screen_compliant_mrn)

#check again
# is this the same...
length(data_mrsa_screen_non_compliant_mrn) + length(data_mrsa_screen_compliant_mrn)

# .... as this
data_cohort_admission_mrsa |> 
  distinct(mrn) |> 
  pull(mrn) |> 
  length()

#####################
# Proportion of patients with MRSA screen received within MRSA policy-compliant time period (%)   -------------
#####################

# Calculate proportions --------------

#compliant
data_mrsa_screen_compliant_prop_true <- data_mrsa_screen_compliant_n_true / 
  (data_mrsa_screen_compliant_n_true + data_mrsa_screen_compliant_n_false) * 100

data_mrsa_screen_compliant_prop_true

data_mrsa_screen_compliant_prop_true_rounded <-  data_mrsa_screen_compliant_prop_true |> 
  round(1)

data_mrsa_screen_compliant_prop_true_rounded

#non-compliant
data_mrsa_screen_compliant_prop_false <- 100 - data_mrsa_screen_compliant_prop_true

data_mrsa_screen_compliant_prop_false

data_mrsa_screen_compliant_prop_false_rounded <-  data_mrsa_screen_compliant_prop_false |> 
  round(1)

data_mrsa_screen_compliant_prop_false_rounded

# Create a data frame with the values
df <- tibble(
  group = c("Compliant", "Non-compliant"),
  value = c(data_mrsa_screen_compliant_prop_true, data_mrsa_screen_compliant_prop_false)
)

# Create the pie chart with custom colors
data_mrsa_screen_compliant_prop_piechart <- ggplot(df, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = sprintf("%.1f%%", value)), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Compliant" = "green4", "Non-compliant" = "red3")) +
  labs(
    title = "MRSA Screening Compliance",
    fill = "Status"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

data_mrsa_screen_compliant_prop_piechart

#####################
# Evaluate the timeliness of MRSA screening relative to patient admission dates.   -------------
#####################

# Create analysis of **all** MRSA screens relative to admission
mrsa_screen_timing <- data_cohort_admission_mrsa %>%
  filter(specimen_type_code == "MRSA") %>%
  distinct(specimen_no, .keep_all = TRUE) |>
  group_by(mrn) %>%
  arrange(receive_date) %>%
  mutate(
    screen_number = row_number(),
    days_from_admission = as.numeric(difftime(receive_date, admission_dt, units = "days"))
  ) %>%
  ungroup()

mrsa_screen_timing

# Summary statistics
screen_summary <- mrsa_screen_timing %>%
  summarise(
    total_patients_in_cohort = length(unique(data_cohort_admission_mrsa$mrn)),
    total_patients_screened = n_distinct(mrn),
    total_specimen_no = n_distinct(specimen_no),
    avg_screens_per_patient_with_sample = n() / n_distinct(mrn),
    avg_screens_per_total_patients_in_cohort = n() / length(unique(data_cohort_admission_mrsa$mrn)),
    max_screens = max(screen_number)
  )

screen_summary

screen_summary$total_specimen_no

screen_summary$total_patients_in_cohort

#total patients in whole cohort
length(unique(data_cohort_admission_mrsa$mrn))

#total patients with an MRSA screen
screen_summary$total_patients_screened

#total patients without an MRSA screen
length(unique(data_cohort_admission_mrsa$mrn)) - screen_summary$total_patients_screened

total_patients_not_screened <- length(unique(data_cohort_admission_mrsa$mrn)) - screen_summary$total_patients_screened

#check output
mrsa_screen_timing |>
  group_by(mrn) |> 
  summarise(max = max(screen_number)) |> 
  arrange(desc(max)) 

#which MRN had the most screens (NB this could be tie)
mrn_most_screens <- mrsa_screen_timing |>
  group_by(mrn) |> 
  summarise(max = max(screen_number)) |> 
  arrange(desc(max)) |> 
  pluck(1, 1)

#look at receive_date of MRSA screens for MRN with the most screens (mrn_most_screens) 
mrsa_screen_timing |> 
  #filter(mrn == mrn_most_screens) |> 
  select(mrn, screen_number, admission_dt, receive_date, quantifier,  mrsa_screen_compliant) |> 
  group_by(mrn) |> 
  reframe(max_specim = max(screen_number)) |> 
  ungroup() |> 
  group_by(max_specim) |> 
  count()

mrsa_screen_timing |> 
  filter(mrn == mrn_most_screens) |> 
  select(mrn, screen_number, admission_dt, receive_date, quantifier,  mrsa_screen_compliant) |> 
  group_by(mrn) |> 
  reframe(max_specim = max(screen_number)) |> 
  ungroup() |> 
  group_by(max_specim) |> 
  count()

mrsa_screen_timing |> 
  filter(mrn == mrn_most_screens) |> 
  select(mrn, screen_number, admission_dt, receive_date, quantifier,  mrsa_screen_compliant) 
  
# Distribution of number of screens per patient
screens_per_patient <- mrsa_screen_timing %>%
  distinct(specimen_no, .keep_all = TRUE) |> 
  count(mrn) %>%
  count(n, name = "frequency") %>%
  mutate(
    screens = fct_lump_n(factor(n), n = 5, other_level = "6+")
  )

screens_per_patient

# add the count of patients with no MRSA screens to your screens_per_patient data
mrsa_screen_non_compliant_no_specimen_received_mrn <-  data_mrsa_screen_non_compliant |> 
  filter(!is.na(specimen_no)) |> 
  distinct(mrn) |> 
  pull(mrn) 

#total patients without an MRSA screen
mrsa_screen_non_compliant_no_specimen_received_n <- length(unique(data_cohort_admission_mrsa$mrn)) - screen_summary$total_patients_screened

mrsa_screen_non_compliant_no_specimen_received_n

# Add row for patients with zero screens
screens_per_patient_incl_zero <- tibble(
  frequency = c(as.numeric(mrsa_screen_non_compliant_no_specimen_received_n), screens_per_patient$frequency),
  screens = as.numeric(c(0, as.character(screens_per_patient$screens)))
)

screens_per_patient_incl_zero

# Visualize distribution of screens per patient
mrsa_screen_received_per_mrn <- ggplot(screens_per_patient_incl_zero, aes(x = screens, y = frequency)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Number of MRSA Screens Received per Patient",
    x = "Number of Screens",
    y = "Number of Patients"
  ) +
  theme_minimal()

mrsa_screen_received_per_mrn

# Timing of screens relative to admission - table
mrsa_screen_timing_days_from_admission_table <- mrsa_screen_timing |> 
  #only select first screen for each patient in order to determine earliest point in the window of opportunity for screening
  filter(screen_number == 1) 

mrsa_screen_timing_days_from_admission_table |> 
  select(mrn, specimen_no, admission_dt, receive_date, days_from_admission) |> 
  arrange(desc(days_from_admission))

# Timing of screens relative to admission - chart
mrsa_screen_timing_days_from_admission <- mrsa_screen_timing |> 
  #only select first screen for each patient in order to determine earliest point in the window of opportunity for screening
  filter(screen_number == 1) |> 
ggplot(aes(x = days_from_admission)) +
  geom_histogram(binwidth = 5, fill = "steelblue") +
  labs(
    title = "Timing of First MRSA Screen Received per Patient Relative to Admission",
    x = "Days from Admission",
    y = "Number of Screens",
    caption = "binwidth 5"
  ) +
  theme_minimal()

mrsa_screen_timing_days_from_admission

# In what proportion of MRSA screening non-compliant patients was specimen transit time the issue? #226 ------------

# Need:
#   
#   Date of birth +/- Date of admission
# Date of collection
# Date of receipt
# Also need:
#   In what proportion of MRSA screening non-compliant patients has no MRSA screen been received at all?
#   
#   i.e. how is it a case of specimen transit time causing non-compliance vs. no sample being taken at all?

data_mrsa_screen_non_compliant

data_mrsa_screen_non_compliant |> 
  glimpse()

# Use lubridate to parse the date+time and dplyr::mutate to create POSIXct datetimes, then take the difference. The snippet below is robust to times like "0705" or "07:05" and will produce minutes and hours between collection and receipt (NA when either side is missing).

data_mrsa_screen_non_compliant <- data_mrsa_screen_non_compliant %>%
  # normalize time strings like "0705" -> "07:05" so lubridate can parse them
  mutate(
    collection_time = case_when(
      is.na(collection_time) ~ NA_character_,
      str_detect(collection_time, "^\\d{3,4}$") ~ str_replace(collection_time, "(\\d{1,2})(\\d{2})$", "\\1:\\2"),
      TRUE ~ collection_time
    ),
    receive_time = case_when(
      is.na(receive_time) ~ NA_character_,
      str_detect(receive_time, "^\\d{3,4}$") ~ str_replace(receive_time, "(\\d{1,2})(\\d{2})$", "\\1:\\2"),
      TRUE ~ receive_time
    )
  ) %>%
  # build POSIXct datetimes from date + time (try common orders)
  mutate(
    collection_dt = case_when(
      !is.na(collection_date) & !is.na(collection_time) ~
        parse_date_time(paste(collection_date, collection_time),
                        orders = c("ymd HMS", "ymd HM", "ymd H", "ymd"),
                        tz = "UTC"),
      !is.na(collection_date) & is.na(collection_time) ~
        as.POSIXct(collection_date, tz = "UTC"),
      TRUE ~ as.POSIXct(NA)
    ),
    receive_dt = case_when(
      !is.na(receive_date) & !is.na(receive_time) ~
        parse_date_time(paste(receive_date, receive_time),
                        orders = c("ymd HMS", "ymd HM", "ymd H", "ymd"),
                        tz = "UTC"),
      !is.na(receive_date) & is.na(receive_time) ~
        as.POSIXct(receive_date, tz = "UTC"),
      TRUE ~ as.POSIXct(NA)
    )
  ) %>%
  # compute differences
  mutate(
    time_to_receive_mins  = as.numeric(difftime(receive_dt, collection_dt, units = "mins")),
    time_to_receive_hours = round(time_to_receive_mins / 60, 1)
  ) |> 
  mutate(time_to_receive_hours_2 = if_else(
    is.na(time_to_receive_hours),
    true = "No sample received",
    false = as.character(time_to_receive_hours)
  ))

#some will fail to parse because these patients didn't have an MRSA screen sent at all



## visualise data
# data_mrsa_screen_non_compliant |> 
#   select(collection_dt, receive_dt, time_to_receive_hours) |> 
#   arrange(desc(time_to_receive_hours)) |> 
#   ggplot(aes(time_to_receive_hours)) +
#   geom_histogram()

data_mrsa_screen_non_compliant_transit <- data_mrsa_screen_non_compliant |> 
  distinct(mrn, date_of_birth, mrsa_screen_compliant, specimen_no, specimen_type_code, order_code, test_code, collection_date, collection_time, receive_date, time_to_receive_hours, time_to_receive_hours_2)

#bear in mind not all specimen_no are MRSA screens... some are e.g. wound swabs
data_mrsa_screen_non_compliant_transit |> 
  count(mrsa_screen_compliant, specimen_type_code, order_code, test_code)

#so these are all non-compliant patients, but some did have specimens sent...

# which patients have never had a sample sent?
data_mrsa_screen_non_compliant_transit <- data_mrsa_screen_non_compliant_transit |> 
  mutate(mrsa_screen_received = 
           case_when(
             mrsa_screen_compliant == FALSE & specimen_type_code == "MRSA" ~ TRUE,
             .default = FALSE
           )) 

# what percentage never had a sample sent?
df_mrsa_screen_received <- data_mrsa_screen_non_compliant_transit |> 
  group_by(mrsa_screen_received) |> 
  count() |>
  ungroup() |> 
  mutate(sum = sum(n),
         perc = round(n / sum * 100, 1))

df_mrsa_screen_received

# Answer to what percentage never a sample sent:
ind <- which(df_mrsa_screen_received$mrsa_screen_received == TRUE)
mrsa_screened_but_not_compliant <- df_mrsa_screen_received$perc[ind] #percent DID have a sample sent
mrsa_screened_but_not_compliant <- as.character(mrsa_screened_but_not_compliant)


# ggplot(df_mrsa_screen_received, aes(x = "", y = perc, fill = mrsa_screen_received)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y", start = 0) +
#   geom_text(aes(label = sprintf("%.1f%%", perc)), 
#             position = position_stack(vjust = 0.5)) +
#   scale_fill_manual(values = c("TRUE" = "green4", "FALSE" = "red3")) +
#   labs(
#     title = "MRSA Screen Received",
#     fill = "Status"
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(hjust = 0.5)
#   )

# calc summary stats RE: time_to_receive_hours ---------

data_mrsa_screen_non_compliant_transit_summary_stats <- data_mrsa_screen_non_compliant_transit |> 
  reframe(mean_time_to_receive_hours = mean(time_to_receive_hours, na.rm = TRUE),
          median_time_to_receive_hours = median(time_to_receive_hours, na.rm = TRUE),
          max_time_to_receive_hours = max(time_to_receive_hours, na.rm = TRUE),
          min_time_to_receive_hours = min(time_to_receive_hours, na.rm = TRUE),
          range_time_to_receive_hours = range(time_to_receive_hours, na.rm = TRUE))

data_mrsa_screen_non_compliant_transit_summary_stats

# mean number of hours for samples to be received in the laboratory for non-compliant patients
non_compliant_but_sample_received_mean_time_to_receive_hours <- round(unique(data_mrsa_screen_non_compliant_transit_summary_stats$mean_time_to_receive_hours), 1)

non_compliant_but_sample_received_mean_time_to_receive_hours

# max number of hours for samples to be received in the laboratory for non-compliant patients
non_compliant_but_sample_received_max_time_to_receive_hours <- round(unique(data_mrsa_screen_non_compliant_transit_summary_stats$max_time_to_receive_hours), 1)

non_compliant_but_sample_received_max_time_to_receive_hours

# min number of hours for samples to be received in the laboratory for non-compliant patients
non_compliant_but_sample_received_min_time_to_receive_hours <- round(unique(data_mrsa_screen_non_compliant_transit_summary_stats$min_time_to_receive_hours), 1)

non_compliant_but_sample_received_min_time_to_receive_hours

#####################
# Interactive table featuring details of distinct patients **without** MRSA screen received within MRSA policy-compliant time period.   -------------
#####################

data_mrsa_screen_non_compliant |> 
  glimpse()

data_mrsa_screen_non_compliant_for_table <- data_mrsa_screen_non_compliant |> 
  select(current_ward,
         current_bay,
         mrn,
         fin,
         age_on_admission,
         admission_dt,
         admission_source,
         collection_date,
         time_to_receive_hours_2) |> 
  rename(ward = current_ward, 
         bay = current_bay,
         #current_bed,
         #patient,
         #mrn,
         #fin,
         #date_of_birth,
         age = age_on_admission,
         #sex,
         `adm date` = admission_dt,
         `adm source` = admission_source,
         `MRSA screen collection date/time` = collection_date,
         `MRSA screen transit time (hours)` = time_to_receive_hours_2,
         #mrsa_pos,
         #receive_date,
         #specimen_no,
         #specimen_type_code,
         #order_code
         ) |> 
  distinct() |> 
  arrange(ward, bay, mrn) 

# not_as_factor <- c("mrn", "initials", "fin")
# interactive_table(data_mrsa_screen_non_compliant_for_table, nrow = 5, not_as_factor = not_as_factor)


# calculate percentage compliance per ward ------------------------------------------

# data_mrsa_screen_compliant
# 
# data_mrsa_screen_non_compliant

#calc mrsa_screen_compliant TRUE vs FALSE per ward
data_cohort_admission_mrsa_perc_compliance <- data_cohort_admission_mrsa |> 
  distinct(mrn, current_ward, mrsa_screen_compliant, quantifier, mrsa_screen_compliant) |> 
  group_by(current_ward) |> 
  count(mrsa_screen_compliant) |> 
  pivot_wider(names_from = mrsa_screen_compliant, values_from = n) 

data_cohort_admission_mrsa_perc_compliance

#replace NA with 0
ind <- which(is.na(data_cohort_admission_mrsa_perc_compliance$`FALSE`))
data_cohort_admission_mrsa_perc_compliance$`FALSE`[ind] <- as.integer(0)
data_cohort_admission_mrsa_perc_compliance$`FALSE`[ind] 

ind <- which(is.na(data_cohort_admission_mrsa_perc_compliance$`TRUE`))
data_cohort_admission_mrsa_perc_compliance$`TRUE`[ind] <- as.integer(0)
data_cohort_admission_mrsa_perc_compliance$`TRUE`[ind] 

#work out mrsa_screen_compliant_perc
data_cohort_admission_mrsa_perc_compliance <- data_cohort_admission_mrsa_perc_compliance |> 
  mutate(mrsa_screen_n = `TRUE` + `FALSE`,
         mrsa_screen_compliant_perc = `TRUE` / mrsa_screen_n * 100)

data_cohort_admission_mrsa_perc_compliance |> 
  arrange(mrsa_screen_n)

#check ward 18, which had high number of patients eligible for MRSA screening BUT low MRSA screening compliance as of 21/01/2025
data_cohort_admission_mrsa_perc_compliance |> 
  filter(current_ward == "FH18")

data_cohort_admission_mrsa |> 
  distinct(mrn, current_ward, admission_dt, mrsa_screen_compliant, quantifier, mrsa_screen_compliant, specimen_no, receive_date) |> 
  filter(current_ward == "FH18") |> 
  arrange(desc(admission_dt), receive_date)

###########
# create charts: MRSA Screening Compliance by Ward -----------
###########

# all wards ---------------------------------------------------------------

mrsa_screening_compliance_chart <- data_cohort_admission_mrsa_perc_compliance |>
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
    title = "MRSA Screening Compliance by Ward",
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


#########
# use create_mrsa_compliance_chart() function to create charts ----------------------------------------
#########

# best performing wards (i.e. highest MRSA screening compliance, above Y percent) with >X eligible patients -------------

create_mrsa_compliance_chart(
    data = data_cohort_admission_mrsa_perc_compliance, 
    min_eligible_patients = 10,
    min_compliance_pct = 75,
    max_compliance_pct = 100,
    performance_title = ": \nWards with highest compliance rate")

# worst performing wards (i.e. lowest MRSA screening compliance, below Y percent) with >X eligible patients -------------

create_mrsa_compliance_chart(
  data = data_cohort_admission_mrsa_perc_compliance, 
  min_eligible_patients = 10,
  min_compliance_pct = 0,
  max_compliance_pct = 25,
  performance_title = ": \nWards with lowest compliance rate")


# flextable with data for every ward --------------------------------------

mrsa_compliance_all_wards_table <- data_cohort_admission_mrsa_perc_compliance |> 
  arrange(desc(mrsa_screen_compliant_perc)) |> 
  select(current_ward, everything()) |> 
  mutate(mrsa_screen_compliant_perc = round(mrsa_screen_compliant_perc, 0)) |> 
  rename(
    "current ward" = current_ward,
    "screened for MRSA \n(n)" = `TRUE`,
    "not screened for MRSA \n(n)" = `FALSE`,
    "total patients eligible for MRSA screening \n(n)" = mrsa_screen_n,
    "compliance with MRSA screening \n(%)" = mrsa_screen_compliant_perc
  ) |> 
  flextable() |> 
  set_table_properties(layout = "autofit")

mrsa_compliance_all_wards_table

# By ward - MRSA admission screening compliance  ------------------------------------------------------

#compliance data for all wards

data_cohort_admission_mrsa_perc_compliance |> 
  glimpse()

# I have some ward level data I want to report on using simple statistics initially

data_cohort_admission_mrsa_perc_compliance |> 
  glimpse()

# > data_cohort_admission_mrsa_perc_compliance |> 
#   +   glimpse()
# Rows: 75
# Columns: 5
# Groups: current_ward [75]
# $ current_ward               <chr> "FH02", "FH03", "F…
# $ `FALSE`                    <int> 6, 7, 5, 5, 6, 10,… [i.e. screen not received at all, or not received in a policy-compliant timeframe]
# $ `TRUE`                     <int> 16, 12, 13, 11, 10… [i.e. screen received in a policy-compliant timeframe]
# $ mrsa_screen_n              <int> 22, 19, 18, 16, 16… [total number of MRSA admission screening-eligible patients on ward]
# $ mrsa_screen_compliant_perc <dbl> 72.72727, 63.15789… [percentage compliance with MRSA screening policy for each ward]
# > 

# best performing wards (i.e. highest MRSA screening compliance, above Y percent) with >X eligible patients

create_mrsa_compliance_chart(
  data = data_cohort_admission_mrsa_perc_compliance, 
  min_eligible_patients = 10,
  min_compliance_pct = 75,
  max_compliance_pct = 100,
  performance_title = ": \nWards with highest compliance rate")

# worst performing wards (i.e. lowest MRSA screening compliance, below Y percent) with >X eligible patients

create_mrsa_compliance_chart(
  data = data_cohort_admission_mrsa_perc_compliance, 
  min_eligible_patients = 10,
  min_compliance_pct = 0,
  max_compliance_pct = 50,
  performance_title = ": \nWards with lowest compliance rate")

# WARD-LEVEL COMPLIANCE STATISTICS

# Overall ward-level summary
ward_summary <- data_cohort_admission_mrsa_perc_compliance |>
  summarise(
    n_wards = n(),
    mean_ward_compliance = mean(mrsa_screen_compliant_perc, na.rm = TRUE),
    sd_ward_compliance = sd(mrsa_screen_compliant_perc, na.rm = TRUE),
    median_ward_compliance = median(mrsa_screen_compliant_perc, na.rm = TRUE),
    min_ward_compliance = min(mrsa_screen_compliant_perc, na.rm = TRUE),
    max_ward_compliance = max(mrsa_screen_compliant_perc, na.rm = TRUE),
    range_compliance = paste0(round(min_ward_compliance, 1), "–", round(max_ward_compliance, 1)),
    cv_ward_compliance = (sd(mrsa_screen_compliant_perc, na.rm = TRUE) / mean(mrsa_screen_compliant_perc, na.rm = TRUE)) * 100,
    total_eligible_patients = sum(mrsa_screen_n, na.rm = TRUE),
    total_compliant_screens = sum(`TRUE`, na.rm = TRUE),
    total_non_compliant = sum(`FALSE`, na.rm = TRUE),
    overall_compliance_perc = (sum(`TRUE`, na.rm = TRUE) / sum(mrsa_screen_n, na.rm = TRUE)) * 100
  )

# Print summary
cat("===== WARD-LEVEL COMPLIANCE SUMMARY =====\n")
print(ward_summary)

# Performance categories
ward_performance <- data_cohort_admission_mrsa_perc_compliance |>
  mutate(
    performance_category = case_when(
      mrsa_screen_compliant_perc >= 75 ~ "High (≥75%)",
      mrsa_screen_compliant_perc >= 60 & mrsa_screen_compliant_perc < 75 ~ "Moderate (60–74%)",
      mrsa_screen_compliant_perc < 60 ~ "Low (<60%)"
    )
  ) |>
  group_by(performance_category) |>
  summarise(
    n_wards = n(),
    mean_compliance = mean(mrsa_screen_compliant_perc, na.rm = TRUE),
    sd_compliance = sd(mrsa_screen_compliant_perc, na.rm = TRUE),
    total_eligible = sum(mrsa_screen_n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(factor(performance_category, levels = c("High (≥75%)", "Moderate (60–74%)", "Low (<60%)")))

cat("\n===== PERFORMANCE CATEGORIES =====\n")
print(ward_performance)

# Top and bottom performers
top_performers <- data_cohort_admission_mrsa_perc_compliance |>
  arrange(desc(mrsa_screen_compliant_perc)) |>
  slice(1:5) |>
  select(current_ward, mrsa_screen_n, `TRUE`, mrsa_screen_compliant_perc)

bottom_performers <- data_cohort_admission_mrsa_perc_compliance |>
  arrange(mrsa_screen_compliant_perc) |>
  slice(1:5) |>
  select(current_ward, mrsa_screen_n, `TRUE`, mrsa_screen_compliant_perc)

cat("\n===== TOP 5 PERFORMING WARDS =====\n")
print(top_performers)

cat("\n===== BOTTOM 5 PERFORMING WARDS =====\n")
print(bottom_performers)

# Distribution of ward compliance
ward_distribution <- data_cohort_admission_mrsa_perc_compliance |>
  summarise(
    n_wards_above_mean = sum(mrsa_screen_compliant_perc > mean(mrsa_screen_compliant_perc, na.rm = TRUE), na.rm = TRUE),
    n_wards_below_mean = sum(mrsa_screen_compliant_perc < mean(mrsa_screen_compliant_perc, na.rm = TRUE), na.rm = TRUE),
    n_wards_at_or_above_80perc = sum(mrsa_screen_compliant_perc >= 80, na.rm = TRUE),
    n_wards_at_or_above_70perc = sum(mrsa_screen_compliant_perc >= 70, na.rm = TRUE),
    n_wards_below_50perc = sum(mrsa_screen_compliant_perc < 50, na.rm = TRUE),
    perc_wards_above_70 = (sum(mrsa_screen_compliant_perc >= 70, na.rm = TRUE) / n()) * 100
  )

cat("\n===== DISTRIBUTION OF WARD-LEVEL COMPLIANCE =====\n")
print(ward_distribution)

# Create visualization
ward_plot <- data_cohort_admission_mrsa_perc_compliance |>
  arrange(mrsa_screen_compliant_perc) |>
  mutate(
    current_ward = factor(current_ward, levels = unique(current_ward)),
    performance_colour = case_when(
      mrsa_screen_compliant_perc >= 75 ~ "#118C45", # Green
      mrsa_screen_compliant_perc >= 60 ~ "#FFB81C",  # Amber
      TRUE ~ "#E4011F"                                # Red
    )
  ) |>
  ggplot(aes(x = mrsa_screen_compliant_perc, y = fct_reorder(current_ward, mrsa_screen_compliant_perc))) +
  geom_col(aes(fill = performance_colour), show.legend = FALSE) +
  scale_fill_identity() +
  geom_vline(aes(xintercept = mean(data_cohort_admission_mrsa_perc_compliance$mrsa_screen_compliant_perc)),
             colour = "#003087", linewidth = 1, linetype = "dashed") +
  labs(
    title = "MRSA Admission Screening Compliance by Ward",
    subtitle = "Week ending 11 November 2025 | Dashed line = organisational mean",
    x = "Compliance (%)",
    y = "Ward"
  ) +
  xlim(0, 100) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 8)
  )

print(ward_plot)

# SIMPLER WARD-LEVEL SUMMARY STATISTICS

# First, let's extract the numbers we need as simple scalars
n_wards_total <- nrow(data_cohort_admission_mrsa_perc_compliance)

mean_compliance <- mean(data_cohort_admission_mrsa_perc_compliance$mrsa_screen_compliant_perc, na.rm = TRUE)
sd_compliance <- sd(data_cohort_admission_mrsa_perc_compliance$mrsa_screen_compliant_perc, na.rm = TRUE)
median_compliance <- median(data_cohort_admission_mrsa_perc_compliance$mrsa_screen_compliant_perc, na.rm = TRUE)
min_compliance <- min(data_cohort_admission_mrsa_perc_compliance$mrsa_screen_compliant_perc, na.rm = TRUE)
max_compliance <- max(data_cohort_admission_mrsa_perc_compliance$mrsa_screen_compliant_perc, na.rm = TRUE)
cv_compliance <- (sd_compliance / mean_compliance) * 100

# Calculate performance categories
high_perf <- data_cohort_admission_mrsa_perc_compliance |>
  filter(mrsa_screen_compliant_perc >= 75) |>
  nrow()

moderate_perf <- data_cohort_admission_mrsa_perc_compliance |>
  filter(mrsa_screen_compliant_perc >= 60 & mrsa_screen_compliant_perc < 75) |>
  nrow()

low_perf <- data_cohort_admission_mrsa_perc_compliance |>
  filter(mrsa_screen_compliant_perc < 60) |>
  nrow()

# Totals
total_eligible <- sum(data_cohort_admission_mrsa_perc_compliance$mrsa_screen_n, na.rm = TRUE)
total_compliant <- sum(data_cohort_admission_mrsa_perc_compliance$`TRUE`, na.rm = TRUE)
total_noncompliant <- sum(data_cohort_admission_mrsa_perc_compliance$`FALSE`, na.rm = TRUE)
overall_compliance_rate <- (total_compliant / total_eligible) * 100

# Top and bottom performers
top_5 <- data_cohort_admission_mrsa_perc_compliance |>
  arrange(desc(mrsa_screen_compliant_perc)) |>
  slice(1:5) |>
  pull(mrsa_screen_compliant_perc)

bottom_5 <- data_cohort_admission_mrsa_perc_compliance |>
  arrange(mrsa_screen_compliant_perc) |>
  slice(1:5) |>
  pull(mrsa_screen_compliant_perc)

top_5_best <- max(top_5)
top_5_worst_of_best <- min(top_5)
bottom_5_best <- max(bottom_5)
bottom_5_worst <- min(bottom_5)
compliance_gap <- top_5_best - bottom_5_worst

# Print as simple output
cat("===== WARD-LEVEL COMPLIANCE SUMMARY STATISTICS =====\n\n")
cat("Number of wards:", n_wards_total, "\n")
cat("Mean compliance (%):", round(mean_compliance, 1), "\n")
cat("SD (%):", round(sd_compliance, 1), "\n")
cat("Median (%):", round(median_compliance, 1), "\n")
cat("Range (%):", round(min_compliance, 1), "–", round(max_compliance, 1), "\n")
cat("Coefficient of variation (%):", round(cv_compliance, 1), "\n\n")

cat("===== PERFORMANCE DISTRIBUTION =====\n")
cat("Wards with ≥75% compliance:", high_perf, "(", round((high_perf/n_wards_total)*100, 0), "%)\n")
cat("Wards with 60–74% compliance:", moderate_perf, "(", round((moderate_perf/n_wards_total)*100, 0), "%)\n")
cat("Wards with <60% compliance:", low_perf, "(", round((low_perf/n_wards_total)*100, 0), "%)\n\n")

cat("===== TOP AND BOTTOM PERFORMERS =====\n")
cat("Top 5 wards range from:", round(top_5_worst_of_best, 1), "% to", round(top_5_best, 1), "%\n")
cat("Bottom 5 wards range from:", round(bottom_5_worst, 1), "% to", round(bottom_5_best, 1), "%\n")
cat("Compliance gap (best vs worst):", round(compliance_gap, 1), "percentage points\n\n")

cat("===== OVERALL FIGURES =====\n")
cat("Total eligible inpatients:", total_eligible, "\n")
cat("Total compliant screens:", total_compliant, "(", round(overall_compliance_rate, 1), "%)\n")
cat("Total non-compliant screens:", total_noncompliant, "(", round(100-overall_compliance_rate, 1), "%)\n")
cat("Overall compliance rate:", round(overall_compliance_rate, 1), "%\n")

# Store as list for easy reference in Quarto
ward_stats <- list(
  n_wards = n_wards_total,
  mean = mean_compliance,
  sd = sd_compliance,
  median = median_compliance,
  min = min_compliance,
  max = max_compliance,
  cv = cv_compliance,
  high_perc = high_perf,
  high_perc_pct = round((high_perf/n_wards_total)*100, 0),
  moderate_perc = moderate_perf,
  moderate_perc_pct = round((moderate_perf/n_wards_total)*100, 0),
  low_perc = low_perf,
  low_perc_pct = round((low_perf/n_wards_total)*100, 0),
  total_eligible = total_eligible,
  total_compliant = total_compliant,
  total_noncompliant = total_noncompliant,
  overall_rate = overall_compliance_rate,
  top_best = top_5_best,
  top_worst = top_5_worst_of_best,
  bottom_best = bottom_5_best,
  bottom_worst = bottom_5_worst,
  gap = compliance_gap
)


# number of- and timing of MRSA screens received per patient ----------------------------------------------

# TIMING OF FIRST MRSA SCREEN RELATIVE TO ADMISSION

# Get first screen per patient
first_screen_per_patient_screen_no <- mrsa_screen_timing_days_from_admission_table |>
  group_by(mrn) |>
  slice(1) |>  # First screen for each patient
  ungroup()

first_screen_per_patient_screen_no |> 
  select(mrn, specimen_no, admission_dt, receive_date, screen_number)

# Calculate timing statistics
timing_stats_screen_no <- first_screen_per_patient_screen_no |>
  summarise(
    n_screened_patients = n_distinct(mrn),
    mean_days_from_admission = mean(days_from_admission, na.rm = TRUE),
    sd_days_from_admission = sd(days_from_admission, na.rm = TRUE),
    median_days_from_admission = median(days_from_admission, na.rm = TRUE),
    min_days_from_admission = min(days_from_admission, na.rm = TRUE),
    max_days_from_admission = max(days_from_admission, na.rm = TRUE),
    q25_days = quantile(days_from_admission, 0.25, na.rm = TRUE),
    q75_days = quantile(days_from_admission, 0.75, na.rm = TRUE)
  )

timing_stats_screen_no |> 
  glimpse()

# Categorise timing
timing_categories_screen_no <- first_screen_per_patient_screen_no |>
  mutate(
    timing_category = case_when(
      days_from_admission < 0 ~ "Pre-admission",
      days_from_admission == 0 ~ "On admission day",
      days_from_admission > 0 & days_from_admission <= 1 ~ "Within 24 hours",
      days_from_admission > 1 & days_from_admission <= 2 ~ "1–2 days",
      days_from_admission > 2 & days_from_admission <= 7 ~ "3–7 days",
      days_from_admission > 7 & days_from_admission <= 14 ~ "8–14 days",
      days_from_admission > 14 ~ ">14 days"
    )
  ) |>
  select(mrn, specimen_no, admission_dt, receive_date, screen_number, timing_category)

#check
timing_categories_screen_no

timing_categories_screen_no <- timing_categories_screen_no |> 
  group_by(timing_category) |>
  summarise(
    n_patients = n(),
    perc_patients = round((n() / nrow(first_screen_per_patient_screen_no)) * 100, 1),
    .groups = "drop"
  ) |>
  mutate(timing_category = factor(timing_category, 
                                  levels = c("Pre-admission", "On admission day", "Within 24 hours", 
                                             "1–2 days", "3–7 days", "8–14 days", ">14 days"))) |>
  arrange(timing_category)

timing_categories_screen_no <- timing_categories_screen_no |> 
  mutate(sum = sum(n_patients)) |> 
  relocate(sum, .after = n_patients)

# Compliance window (pre-admission or within policy timeframe)
pre_admission_screen_screen_no <- first_screen_per_patient_screen_no |>
  filter(days_from_admission <= 0) |>
  nrow()

within_24hrs_screen_no <- first_screen_per_patient_screen_no |>
  filter(days_from_admission >= -999 & days_from_admission <= 1) |>  # Policy-compliant window
  nrow()

within_7days_screen_no <- first_screen_per_patient_screen_no |>
  filter(days_from_admission <= 7) |>
  nrow()

delayed_screen_no <- first_screen_per_patient_screen_no |>
  filter(days_from_admission > 14) |>
  nrow()

# Store for Quarto
timing_inline_stats_screen_no <- list(
  n_screened = nrow(first_screen_per_patient_screen_no),
  mean_days = round(timing_stats_screen_no$mean_days_from_admission, 1),
  sd_days = round(timing_stats_screen_no$sd_days_from_admission, 1),
  median_days = round(timing_stats_screen_no$median_days_from_admission, 1),
  min_days = round(timing_stats_screen_no$min_days_from_admission, 1),
  max_days = round(timing_stats_screen_no$max_days_from_admission, 1),
  q25 = round(as.numeric(timing_stats_screen_no$q25_days), 1),
  q75 = round(as.numeric(timing_stats_screen_no$q75_days), 1),
  pre_admission = pre_admission_screen_screen_no,
  pre_admission_perc = round((pre_admission_screen_screen_no / nrow(first_screen_per_patient_screen_no)) * 100, 1),
  within_24hrs = within_24hrs_screen_no,
  within_24hrs_perc = round((within_24hrs_screen_no / nrow(first_screen_per_patient_screen_no)) * 100, 1),
  within_7days = within_7days_screen_no,
  within_7days_perc = round((within_7days_screen_no / nrow(first_screen_per_patient_screen_no)) * 100, 1),
  delayed_gt14 = delayed_screen_no,
  delayed_gt14_perc = round((delayed_screen_no / nrow(first_screen_per_patient_screen_no)) * 100, 1)
)

# Print summary
cat("===== TIMING OF FIRST MRSA SCREEN =====\n\n")
cat("Total patients screened:", timing_inline_stats_screen_no$n_screened, "\n\n")
cat("Mean days from admission to first screen:", timing_inline_stats_screen_no$mean_days, "(SD", timing_inline_stats_screen_no$sd_days, ")\n")
cat("Median days:", timing_inline_stats_screen_no$median_days, "\n")
cat("Range:", timing_inline_stats_screen_no$min_days, "to", timing_inline_stats_screen_no$max_days, "days\n")
cat("IQR:", timing_inline_stats_screen_no$q25, "to", timing_inline_stats_screen_no$q75, "days\n\n")

cat("TIMING CATEGORIES:\n")
print(timing_categories_screen_no)

cat("\nKEY FINDINGS:\n")
cat("Pre-admission screens:", timing_inline_stats_screen_no$pre_admission, "(", timing_inline_stats_screen_no$pre_admission_perc, "%)\n")
cat("Within 24 hours (policy-compliant window):", timing_inline_stats_screen_no$within_24hrs, "(", timing_inline_stats_screen_no$within_24hrs_perc, "%)\n")
cat("Within 7 days:", timing_inline_stats_screen_no$within_7days, "(", timing_inline_stats_screen_no$within_7days_perc, "%)\n")
cat("Delayed (>14 days from admission):", timing_inline_stats_screen_no$delayed_gt14, "(", timing_inline_stats_screen_no$delayed_gt14_perc, "%)\n")

print(timing_inline_stats_screen_no)

# missing data ------------------------------------------------------------

# data_cohort_admission_mrsa |> 
#   skimr::skim()

# collection date

n_specimen_missing_collection_date <- data_cohort_admission_mrsa |> 
  filter(!is.na(specimen_no) & is.na(collection_date)) |> 
  distinct(specimen_no) |> 
  pull(specimen_no) |> 
  length()

n_specimen_total <- data_cohort_admission_mrsa |> 
  filter(!is.na(specimen_no)) |> 
  distinct(specimen_no) |> 
  pull(specimen_no) |> 
  length()

n_specimen_missing_collection_date_perc <- round((n_specimen_total - n_specimen_missing_collection_date) / n_specimen_total * 100, 2)

# location code

data_cohort_admission_mrsa |> 
  filter(!is.na(specimen_no) & is.na(location_code)) |> 
  distinct(specimen_no) |> 
  pull(specimen_no) |> 
  length()

data_cohort_admission_mrsa |> 
  filter(!is.na(specimen_no) & !is.na(location_code)) |> 
  distinct(specimen_no) |> 
  pull(specimen_no) |> 
  length()