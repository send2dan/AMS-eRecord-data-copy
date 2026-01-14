
# data_crires_bc_wrangled_wide_for_table to data_antimicrobial_pool_iv_im_po_not_prophy
#
data_crires_bc_wrangled_wide_for_table |>
  glimpse()
#
# skimr::skim(data_crires_bc_wrangled_wide_for_table)
#

# create All Critical-Flagged Blood Cultures - Interactive table --------------

# not_as_factor <- c("mrn", "specimen_no", "`current/intended antibiotic therapy`", "`anatomical site sampled`", "`indwelling device nature (if applicable)`")
# interactive_table(data_crires_bc_wrangled_wide_for_table |> 
#                     arrange(desc(location_code), mrn, `receive date`), nrow = 5, not_as_factor = not_as_factor)

# join --------------------------------------------------------------------

data_crires_bc_wrangled_wide_for_table |>
  glimpse()

data_antimicrobial_pool_iv_im_po_not_prophy |>
  glimpse()

data_crires_bc_antimicrobials <- data_crires_bc_wrangled_wide_for_table |> 
  left_join(data_antimicrobial_pool_iv_im_po_not_prophy,
            join_by(mrn),
            relationship = "many-to-many") 

data_crires_bc_antimicrobials |>
  glimpse()

# change patient name to initials -----------------------------------------

data_crires_bc_antimicrobials <- data_crires_bc_antimicrobials |> 
  # mutate(patient = str_replace(patient, ",", ", ")) |> #add space after comma
  mutate(initials = if_else(!is.na(patient), 
                            get_initials(patient),
                            NA_character_))

data_crires_bc_antimicrobials |> 
  select(initials, patient)

# rename columns for tables -----------------------------------------------

colnames(data_crires_bc_antimicrobials)

data_crires_bc_antimicrobials_col_names <- data_crires_bc_antimicrobials |>
  colnames()

data_crires_bc_antimicrobials_col_names <- str_c(data_crires_bc_antimicrobials_col_names, ",")

kable(data_crires_bc_antimicrobials_col_names, format = "simple")



data_crires_bc_antimicrobials <- data_crires_bc_antimicrobials |>
  rename(
    `adm date` =   admission_dt,                             
    ward = current_ward,
    age =  age_on_admission,                         
    Rx =      order_name_c,                             
    `start date/time` = start_date_time,
    `dose unit` =  dose_unit,
    `drug form` = drug_form,
    `drug route` = route_of_administration,
    `bay/bed` = current_bay_bed,
    `duration unit` = duration_unit,
    duration = intended_duration,
    # stop_type,                                
    `stop date/time` = stop_date_time,                           
    `review date` = review_date,
    `obs done in last 24h` = obs_done_in_last_24h,
    `obs stable in last 24h`= obs_stable_in_last_24h,
    `NEWS2 max` = news2_max,
    `NEWS2 max in last 24h` = news2_max_last_24h,
    `Temp max` = temp_max,
    `Temp max in last 24h` = temp_max_last_24h,
    `CRP max` = crp_max
    )

# You can prevent errors from missing columns in dplyr::select() by using the any_of() helper instead of unquoted column names or backticked names. any_of() will silently ignore any column names that are not present in your data frame. 
data_crires_bc_antimicrobials <- data_crires_bc_antimicrobials |>
  select(any_of(c("ward",
                  "bay/bed",
                  "initials",
                  "sex",
                  "mrn",
                  "fin",
                  "age",
                  "adm date",
                  "Rx",
                  "start date/time",
                  "dose",
                  "dose unit",
                  "drug form",
                  "drug route",
                  "frequency",
                  "indication",
                  "duration",
                  "duration unit",
                  "stop date/time",
                  "review date",
                  "obs done in last 24h",
                  "obs stable in last 24h",
                  "NEWS2 max",
                  "NEWS2 max in last 24h",
                  "Temp max",
                  "Temp max in last 24h",
                  "CRP max",
                  "location_code",
                  "mrn",
                  "specimen_no",
                  "receive date",
                  "spec. type",
                  "current/intended antibiotic therapy",
                  "reason for request",
                  "anatomical site sampled",
                  "indwelling device nature (if applicable)",
                  "organism",
                  "genus",
                  "short name",
                  "Gram stain",
                  "CLI", "DOX", "ERY", "FLC", "GEN", "LNZ", "MFX", "PEN", "RIF", "TMP", "SXT", "AMX", "CHL", "OPT", "TEC", "VAN", "AMK", "ATM", "CAZ", "CIP", "AMC", "CXM", "CZA", "ETP", "FOS", "MEM", "TAZ", "TEM", "TGC", "MTR", "DAP", "TCY", "CRO", "FDC", "IMR", "TOB", "DAL"
                  )))

data_crires_bc_antimicrobials |>
  glimpse()
#
# data_crires_bc_antimicrobials |> 
#   str()
# 
# data_crires_bc_antimicrobials |> 
#   print(n = 100)

# # #which column(s) contain sir data? (S/i/R)
# antibiotics_in_dataset <- colnames(data_crires_bc_antimicrobials)[is_sir_eligible(data_crires_bc_antimicrobials)] 

# antibiotics_in_dataset

# wrangle Rx (drug) column so that it's recognised by AMR -----------------
# Need to wrangle it so antibiotic columns are the same as data in Rx columns

# #get full name
# data_crires_bc_antimicrobials |> 
#   mutate(`Antibiotic Rx` = AMR::ab_name(Rx)) |> 
#   count(`Antibiotic Rx`, Rx)
# 
# #get full ATC info (codes)
# data_crires_bc_antimicrobials |> 
#   mutate(`Antibiotic Rx` = AMR::ab_atc(Rx)) |> 
#   count(`Antibiotic Rx`, Rx) |> 
#   unnest(cols = c(`Antibiotic Rx`))
# 
# #get antibiotic code
# data_crires_bc_antimicrobials |> 
#   mutate(`Antibiotic Rx` = AMR::as.ab(Rx)) |> 
#   count(`Antibiotic Rx`, Rx)

#get antibiotic code
data_crires_bc_antimicrobials <- data_crires_bc_antimicrobials |> 
  mutate(`Antibiotic Rx` = AMR::as.ab(Rx)) |> 
  relocate(`Antibiotic Rx`, .after = Rx)

# data_crires_bc_antimicrobials |> 
#   glimpse()


data_crires_bc_antimicrobials |>
  count(`Antibiotic Rx`, Rx)

data_crires_bc_antimicrobials |>
  count(`Antibiotic Rx`, Rx) |>
  filter(is.na(`Antibiotic Rx`))

data_crires_bc_antimicrobials |>
  filter(is.na(`Antibiotic Rx`)) |>
  glimpse()

# #which column(s) contain sir data? (S/i/R)
antibiotics_in_dataset <- colnames(data_crires_bc_antimicrobials)[is_sir_eligible(data_crires_bc_antimicrobials)] 

antibiotics_in_dataset

colnames(data_crires_bc_wrangled_wide_for_table)[is_sir_eligible(data_crires_bc_wrangled_wide_for_table)]


# pivot longer antimicrobials to get antibiotic disk name into col --------

# data_crires_bc_antimicrobials |> 
#   str()

data_crires_bc_antimicrobials |> 
  glimpse()

data_crires_bc_antimicrobials_longer <- data_crires_bc_antimicrobials |>
  pivot_longer(
    cols = !ward:`Gram stain`,
    names_to = "antibiotic_disk_name",
    values_to = "antibiotic_disk_result",
    values_drop_na = TRUE
  ) |> 
  glimpse()


# table showing appropriateness of current antibiotic therapy -------------

data_crires_bc_antimicrobials_longer_table <- data_crires_bc_antimicrobials_longer |> 
  select(ward, initials, mrn, specimen_no, `receive date`, organism, `Antibiotic Rx`, antibiotic_disk_name, antibiotic_disk_result) |> 
  filter(!is.na(`Antibiotic Rx`)) |> 
  filter(`Antibiotic Rx` == antibiotic_disk_name) |> 
  mutate(`Antibiotic Rx` = AMR::ab_name(`Antibiotic Rx`),
         antibiotic_disk_name = AMR::ab_name(antibiotic_disk_name),
         correct_treatment = if_else(antibiotic_disk_result == "S",
                                     TRUE,
                                     FALSE)) |> 
  distinct() 

data_crires_bc_antimicrobials_longer_table |> 
  glimpse()

# data_crires_bc_antimicrobials_longer_table |> 
#   flextable()

# not_as_factor <- c("mrn", "specimen_no")
# interactive_table(data_crires_bc_antimicrobials_longer_table |>
#                     arrange(desc(ward), mrn, `receive date`), nrow = 5, not_as_factor = not_as_factor)

# concentrate of patients with positive blood cultures prescribed  --------

# create data_crires_bc_antimicrobials_filtered
data_crires_bc_antimicrobials_filtered <- data_crires_bc_antimicrobials |> 
  filter(!is.na(Rx)) |> 
  distinct() 

data_crires_bc_antimicrobials_filtered |> 
  glimpse()

data_crires_bc_antimicrobials_filtered |> 
  print(n = 100)

# temocillin and tazocin specific analysis --------------------------------

data_crires_bc_antimicrobials_filtered |> 
  filter(str_detect(Rx, "Piper|Temo")) |> 
  select(ward, initials, mrn, specimen_no, organism, Rx, TAZ, TEM, AMR::administrable_per_os()) |> 
  distinct(organism, Rx, .keep_all = TRUE) |> 
  print(n = 100)

# resistance (non-intrinsic) to each antimicrobial ------------------------

# data_crires_bc_antimicrobials_filtered |>
#   select(-fin, -mrn) |>
#   summarise(across(not_intrinsic_resistant(), resistance)) |>
#   mutate(across(everything(), ~ ifelse(is.na(.x), NA, round(.x * 100)))) |>
#   pivot_longer(
#     everything(),
#     cols_vary = "slowest",
#     values_drop_na = TRUE,
#     names_to = "antibiotic",
#     values_to = "percentage resistance"
#   ) |>
#   arrange(desc(`percentage resistance`))

# bug drug combinations ---------------------------------------------------

# data_crires_bc_antimicrobials_filtered |> 
#   distinct(mrn, specimen_no, organism, .keep_all = TRUE) |> 
#   filter(mo_is_gram_negative(organism)) |> 
#   bug_drug_combinations() 


# Identify concurrent prescriptions for each patient by start date/time ---------
# Adjust the time window logic if needed (e.g., allowing overlap, same day, etc.)

# 1. Identify concurrent prescriptions for each patient by start date/time
combinations <- data_crires_bc_antimicrobials %>%
  distinct(mrn, Rx, .keep_all = TRUE) |> 
  group_by(mrn) %>%
  summarise(
    n_antimicrobials = n_distinct(Rx),
    antimicrobials = list(sort(unique(Rx)))
  ) %>%
  ungroup()

# 2. How many patients had >1 antimicrobial at the same time? Keep mrn in output
multi_abx <- combinations %>%
  filter(n_antimicrobials > 1) |> 
  arrange(desc(n_antimicrobials))

n_patients_multi_abx <- n_distinct(multi_abx$mrn)

# 3. Most common combinations, with patient mrns attached
multi_abx <- multi_abx %>%
  mutate(combo = map_chr(antimicrobials, ~ paste(.x, collapse = " + "))) |> 
  select(-antimicrobials)

# Outputs
cat("Number of patients prescribed more than one antimicrobial at the same time:", n_patients_multi_abx, "\n")

multi_abx <- multi_abx |> 
  left_join(data_crires_bc_antimicrobials |> #add ward information
              select(mrn, initials, ward) |> 
              distinct()) |> 
  relocate(ward, .before = "mrn") |> 
  relocate(initials, .before = "mrn") |> 
  distinct()

