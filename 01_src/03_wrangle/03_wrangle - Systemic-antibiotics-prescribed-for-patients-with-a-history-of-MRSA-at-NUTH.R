# OLD Import MRSA data --------------------------------------------------------

# #redundant workflow
# data_mrsa_all <- read_csv("I:/Git_Area/AMS-eRecord-data/02_data/antimicrobial_audit/misc/MRSAAnySpecimenTypeMicrobiology-latest.csv", 
#                           col_types = cols(`Specimen date` = col_character(), 
#                                            `Specimen Result` = col_character(), 
#                                            Result = col_character(), VANCOMYCIN = col_character())) |> 
#   janitor::clean_names()
# 
# data_mrsa_all <- data_mrsa_all |> 
#   select(-starts_with("x"))

# NEW Import MRSA data --------------------------------------------------------

## new source of info: APEX without needing ICNET --------------------------
# source("./01_src/02_data_import/02_data_import - Apex MRSA data import.R")

# mrsa_historical_data |> 
#   glimpse()
# Columns: 23
# $ receive_date                   <chr> "2025-08-03", "2025-08-03", …
# $ receive_time                   <chr> "12:05:00", "12:06:00", "12:…
# $ collection_date                <chr> "2025-08-02", "2025-08-02", …
# $ collection_time                <chr> "10:42:00", "18:58:00", "10:…
# $ specimen_no                    <chr> "MG452044R", "MG452045X", "M…
# $ specimen_type_code             <chr> "MRSA", "MRSA", "MRSA", "MRS…
# $ location_code                  <chr> "RV46", "RV15", "RV46", "FH1…
# $ date_of_birth                  <chr> "X", "Y", …
# $ patient_full_name              <chr> "X",…
# $ hospital_number                <chr> "XYZ…
# $ consultant                     <chr> "Immanuel, Arul", "Konstanti…
# $ order_comment                  <chr> NA, NA, NA, NA, NA, NA, NA, …
# $ new_nhs_number                 <chr> "X", "Y", …
# $ sex                            <chr> "F", "M", "M", "M", "F", "M"…
# $ reason_for_req_line1           <chr> "Admission Screen", "Admissi…
# $ reason_for_req_line2           <chr> NA, NA, NA, NA, NA, NA, NA, …
# $ reason_for_req_line3           <chr> NA, NA, NA, NA, NA, NA, NA, …
# $ order_code                     <chr> "MRSA", "MRSA", "MRSA", "MRS…
# $ test_code                      <chr> "MRSA", "MRSA", "MRSA", "MRS…
# $ organism_code                  <chr> "MRSA", "MRSA", "MRSA", "MRS…
# $ quantifier                     <chr> "NI", "NI", "NI", "NI", "NI"…
# $ antibiotic_code                <chr> NA, NA, NA, NA, NA, NA, NA, …
# $ interpreted_sensitivity_result <chr> NA, NA, NA, NA, NA, NA, NA, …
# > 

mrsa_historical_data_pos |> 
  distinct(specimen_no, order_code, test_code, organism_code, quantifier, antibiotic_code, interpreted_sensitivity_result)

mrsa_historical_data_pos |> 
  count(order_code, test_code, organism_code, quantifier, antibiotic_code, interpreted_sensitivity_result) 


# check susceptibility data -----------------------------------------------

mrsa_historical_data_pos |> 
  count(antibiotic_code, interpreted_sensitivity_result) |> 
  arrange(antibiotic_code, interpreted_sensitivity_result) |> 
  print(n = 50)

#check where MRSA cefox/fluclox AST result is not R 
mrsa_historical_data_pos |> 
  filter(str_detect(antibiotic_code, "FLUCLX|TAZ|PG|FOX|CZA|CXM|COAMOX|ATM|CFX")) |> 
  filter(!interpreted_sensitivity_result == "R") |> 
  select(specimen_no, organism_code, antibiotic_code, interpreted_sensitivity_result)

mrsa_historical_data_pos <- mrsa_historical_data_pos |> 
  rename(patient = patient_full_name,
         mrn = hospital_number,
         specimen_number = specimen_no,
         specimen_type = specimen_type_code,
         ward = location_code,
         specimen_date = collection_date) 

# keep only distinct rows with relevant info ------------------------------

data_mrsa_all_distinct <- mrsa_historical_data_pos |> 
  distinct(patient, mrn, specimen_number, specimen_date, specimen_type, ward, consultant)

# Rename ward and patient --------------------------
# (so it doesn't get confusing during _join() function with prescribing data)

data_mrsa_all_distinct <- data_mrsa_all_distinct |> 
  rename(ward_mrsa = ward,
         patient_mrsa = patient) |> 
  mutate(specimen_date = lubridate::ymd(specimen_date),
         specimen_date = format(specimen_date, "%d-%b-%Y"),
         specimen_date = dmy(specimen_date),
         specimen_date_plus2 = specimen_date + 2) |> 
  arrange(desc(specimen_date))

data_mrsa_all_distinct |> 
  head(n = 10) |> 
  select(specimen_date, specimen_date_plus2)

# # check data --------------------------------------------------------------
# 
# dplyr::glimpse(data_mrsa_all)
# 
# data_mrsa_all_distinct |> 
#   skimr::skim()

# earliest and latest dates when MRSA isolated ----------------------------

# Find the earliest date
mrsa_earliest_date <- min(data_mrsa_all_distinct$specimen_date, na.rm = TRUE)
mrsa_earliest_date_formatted <- format(ymd(mrsa_earliest_date), "%A %d %B %Y")
mrsa_earliest_date_formatted

# Find the latest date
mrsa_latest_date <- max(data_mrsa_all_distinct$specimen_date, na.rm = TRUE)
mrsa_latest_date_formatted <- format(ymd(mrsa_latest_date), "%A %d %B %Y")
mrsa_latest_date_formatted

# # check to make sure MRSA isolates approx. equally frequently thro --------
# 
# data_mrsa_all_distinct |> 
#   group_by(specimen_date) |> 
#   count() |> 
#   ggplot(aes(specimen_date, n))+
#   geom_col()

# totals ------------------------------------------------------------------

mrsa_pos_pt_total <- data_mrsa_all_distinct |> 
  distinct(mrn) |> 
  pull(mrn) |> 
  length()

mrsa_pos_pt_total

# Pull MRN numbers ----------------------------
# (to help filter down to only patients with a h/o MRSA in the prescribing data)

mrsa_pos_mrn <- data_mrsa_all_distinct |> 
  pull(mrn) |> 
  unique()

#### REQUIRES point_prevalence_survey data: 
# source("./01_src/03_wrangle/03_wrangle - point_prevalence_survey.R")

# how many patients in data_antimicrobial_pool_iv_im_po_not_prophy have a h/o MRSA pos at NUTH in past year? --------

mrsa_pos_mrn_rx_iv_im_antimicrobials_total <- data_antimicrobial_pool_iv_im_po_not_prophy |> 
  distinct(mrn) |> 
  filter(mrn %in% mrsa_pos_mrn) |> 
  pull(mrn) |> 
  length()

# percentage of patients Rx IV/IM/PO non-prophy antimicrobials in past week who have a history of MRSA isolation at NUTH in past year --------

mrsa_pos_mrn_rx_iv_im_antimicrobials_perc <- round(mrsa_pos_mrn_rx_iv_im_antimicrobials_total / data_antimicrobial_pool_iv_im_po_not_prophy_mrn * 100, 1)


# Line list for MRSA report -----------------------------------------------

# Line list of systemically administered (IV/IM/PO) antimicrobial prescriptions, placed within the 7 days prior to `r format(ymd(import_date_report), '%A %d %B %Y')`, for current inpatients at RVI, FH, GNCH and NCCC, who have been in hospital at least 24 hours, who have had MRSA isolated from any specimen received for culture at NUTH in the pear year.

data_antimicrobial_syst_mrsa <- data_antimicrobial_pool_iv_im_po_not_prophy |> 
  filter(mrn %in% mrsa_pos_mrn) |> 
  left_join(data_mrsa_all_distinct,
            by = join_by(mrn),
            relationship = "many-to-many") |> 
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
    #specimen_number,
    specimen_date,
    specimen_date_plus2,
    #specimen_type,
    #ward_mrsa,
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

# Create a new logical column (rx_date_after_mrsa) indicating if start_date_time is before specimen_date_plus2 -----------

data_antimicrobial_syst_mrsa$rx_date_after_mrsa <-
  data_antimicrobial_syst_mrsa$start_date_time > data_antimicrobial_syst_mrsa$specimen_date_plus2

data_antimicrobial_syst_mrsa |> 
  head(n = 10) |> 
  select(specimen_date, specimen_date_plus2, start_date_time, rx_date_after_mrsa)

data_antimicrobial_syst_mrsa |>
  count(rx_date_after_mrsa)

# Look only into Rx after MRSA pos --------------------------------------------

data_antimicrobial_syst_mrsa <- data_antimicrobial_syst_mrsa |>
  filter(rx_date_after_mrsa == TRUE)

# search Rx for beta-lactams ----------------------------------------------

data_antimicrobial_syst_mrsa <- data_antimicrobial_syst_mrsa |>
  mutate(medication_beta_lac = case_when(
    str_detect(order_name_c, regex(
      list_of_beta_lacs,
      ignore_case = TRUE
    )) ~ TRUE,
    .default = FALSE
  ))

# Look for MRNs of patients Rx beta-lactams after MRSA pos ------------

mrn_pt_rx_beta_lac_after_mrsa <- data_antimicrobial_syst_mrsa |>
  filter(medication_beta_lac == TRUE) |>
  pull(mrn) |>
  unique()

data_antimicrobial_syst_mrsa <- data_antimicrobial_syst_mrsa |>
  filter(mrn %in% mrn_pt_rx_beta_lac_after_mrsa) 

# when were they last MRSA pos? ------------------

mrsa_pos_min_max_date <- data_antimicrobial_syst_mrsa |> 
  group_by(mrn) |> 
  summarise(
    #earliest_mrsa_pos_date = min(specimen_date),
    #latest_mrsa_rx_date = max(start_date_time),
    latest_mrsa_pos_date_plus2 = max(specimen_date_plus2)
    )

mrsa_pos_min_max_date

#join mrsa_pos_min_max_date with  data_antimicrobial_syst_mrsa
# to add earliest_mrsa_pos_date and latest_mrsa_pos_date_plus2 columns
data_antimicrobial_syst_mrsa <- data_antimicrobial_syst_mrsa |> 
  left_join(mrsa_pos_min_max_date,
            by = join_by(mrn)) |> 
  glimpse() 

data_antimicrobial_syst_mrsa <- data_antimicrobial_syst_mrsa |> 
  select(current_ward:admission_dt,
         latest_mrsa_pos_date_plus2,
         medication_beta_lac,
         everything()) |> 
  select(-specimen_date,
         -rx_date_after_mrsa) |> 
  glimpse()
 

# filter data
data_antimicrobial_syst_mrsa_for_table <- data_antimicrobial_syst_mrsa |>
  filter(!is.na(order_name_c)) |> # where order_name_c is not missing, this outlines the actual Misc Prescription...
  # filter(!is.na(dose)) |> #where drugs don't have a documented dose, this is (usually) because the drug was given with frequency == ONCE)
  filter(!is.na(frequency)) |>
  arrange(current_ward, current_bay_bed, mrn, start_date_time, order_name_c) # order by location, mrn, then drug name

# make table IG compliant -------

data_antimicrobial_syst_mrsa_for_table <- data_antimicrobial_syst_mrsa_for_table |> 
  mutate(patient = get_initials(patient))

# rename
data_antimicrobial_syst_mrsa_for_table <- data_antimicrobial_syst_mrsa_for_table |>
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
    `MRSA spec. date` = specimen_date_plus2,
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
    #`earliest MRSA result date` = earliest_mrsa_pos_date,
    `most recent MRSA result date` = latest_mrsa_pos_date_plus2,
    `beta-lactam` = medication_beta_lac,
    `stop date/time` = stop_date_time,
    `review date` = review_date,
    `start date/time` = start_date_time,
    #`MRSA spec. no` = specimen_number,
    #`MRSA spec. date` = specimen_date,
    #`MRSA spec. location` = ward_mrsa,
    #`MRSA spec. type` = specimen_type,
    #`MRSA req. type` = request_types,
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

# Remove duplicates from the original data frame
data_antimicrobial_syst_mrsa_for_table_dedup <- data_antimicrobial_syst_mrsa_for_table |>
  distinct()





