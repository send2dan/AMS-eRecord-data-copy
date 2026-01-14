# Process medication data to standardise format and identify beta-lactams
medications_standardised <- data_medications |>
  # Replace "NULL" strings with NA
  mutate(across(where(is.character), \(x) na_if(x, "NULL"))) |>
  # Rename for clarity
  rename(earliest_import_date = import_date) |>
  # Convert date-time strings to proper datetime objects
  mutate(across(c(ends_with("date_time"), review_date), \(x) dmy_hms(x, tz = "GB"))) |>
  # Flag beta-lactam medications
  mutate(medication_beta_lac = case_when(
    str_detect(synonym_desc, regex(list_of_beta_lacs, ignore_case = TRUE)) |
      str_detect(order_name, regex(list_of_beta_lacs, ignore_case = TRUE)) |
      str_detect(pharmacy_order_sentence, regex(list_of_beta_lacs, ignore_case = TRUE)) ~ TRUE,
    .default = FALSE
  ))

# medications_standardised

# Extract beta-lactam names from order_name
beta_lactams_from_orders <- medications_standardised |>
  filter(str_detect(order_name, regex(list_of_beta_lacs, ignore_case = TRUE)) |
           str_detect(synonym_desc, regex(list_of_beta_lacs, ignore_case = TRUE))) |>
  mutate(extracted_names = str_extract_all(
    order_name,
    regex(list_of_beta_lacs, ignore_case = TRUE)
  ))

# beta_lactams_from_orders

# Process extracted beta-lactam names from order_name
beta_lactams_processed <- beta_lactams_from_orders |>
  unnest_wider(extracted_names, names_sep = "_") |>
  unite(
    col = consolidated_name,
    starts_with("extracted_names"),
    sep = " ",
    na.rm = TRUE
  ) |>
  mutate(consolidated_name = str_squish(consolidated_name)) |>
  mutate(consolidated_name = str_to_sentence(consolidated_name)) |>
  mutate(consolidated_name = if_else(consolidated_name == "", NA, consolidated_name)) |>
  select(
    encntr_id,
    order_id,
    mrn,
    starts_with("order"),
    everything()
  )

# Extract beta-lactam names from synonym_desc
beta_lactams_with_synonyms <- beta_lactams_processed |>
  mutate(extracted_names = str_extract_all(
    synonym_desc,
    regex(list_of_beta_lacs, ignore_case = TRUE)
  )) |>
  unnest_wider(extracted_names, names_sep = "_") |>
  unite(
    col = synonym_consolidated,
    starts_with("extracted_names"),
    sep = " ",
    na.rm = TRUE
  ) |>
  mutate(synonym_consolidated = str_squish(synonym_consolidated)) |>
  mutate(synonym_consolidated = str_to_sentence(synonym_consolidated)) |>
  mutate(synonym_consolidated = if_else(synonym_consolidated == "", NA, synonym_consolidated)) |>
  select(
    encntr_id,
    order_id,
    mrn,
    starts_with("order"),
    starts_with("syn"),
    everything()
  )

# Combine extracted names and finalise beta-lactam identification
beta_lactams_final <- beta_lactams_with_synonyms |>
  mutate(pharmacy_extracted_name = str_extract(
    pharmacy_order_sentence,
    regex(list_of_beta_lacs, ignore_case = TRUE)
  )) |>
  mutate(
    consolidated_name = if_else(is.na(consolidated_name) & !is.na(pharmacy_extracted_name),
                                true = pharmacy_extracted_name,
                                false = consolidated_name
    ),
    consolidated_name = if_else(is.na(consolidated_name) & !is.na(synonym_consolidated),
                                true = synonym_consolidated,
                                false = consolidated_name
    ),
    consolidated_name = str_to_sentence(consolidated_name)
  ) |>
  select(
    -synonym_consolidated,
    -pharmacy_extracted_name
  ) |>
  rename(beta_lac_name = consolidated_name) |>
  right_join(medications_standardised)

# Clean pharmacy order sentence by removing redundant information
# Define text patterns to remove
text_patterns_to_remove <- c(
  "(, )?-1,",
  "DOSE:?",
  "(Anticoagulant)? Indication:?",
  "REVIEW DATE:",
  "START:?",
  "(Intended )?Duration:",
  "STOP:?",
  "REVIEW DATE:?"
)

# Define date-time columns to remove from pharmacy order sentence
datetime_cols_to_remove <- c(
  "start_date_time",
  "next_dose_date_time",
  "stop_date_time",
  "review_date"
)

# Define other columns to remove from pharmacy order sentence
other_cols_to_remove <- c(
  "dose",
  "dose_unit",
  "drug_form",
  "route_of_administration",
  "frequency",
  "indication",
  "diagnostic_confidence",
  "intended_duration",
  "duration_unit",
  "stop_type",
  "special_instructions"
)

# Process pharmacy order sentence to remove redundant information
medications_with_clean_pos <- beta_lactams_final |>
  # Add formatted dose with thousand separator
  mutate(dose_with_separators = str_squish(
    as.character(
      format(
        as.numeric(dose),
        big.mark = ",",
        scientific = FALSE,
        drop0trailing = TRUE
      )
    )
  )) |>
  mutate(dose_with_separators = case_when(
    dose_with_separators == "NA" | dose_with_separators == dose ~ NA,
    .default = dose_with_separators
  )) |>
  # Extract and handle excess text in pharmacy order sentence
  mutate(
    pos_excess_text = str_extract(
      pharmacy_order_sentence,
      ",[^,]*\\.\\.\\.$"
    ),
    cleaned_pharmacy_order = if_else(
      is.na(pos_excess_text),
      pharmacy_order_sentence,
      str_remove(
        pharmacy_order_sentence,
        coll(pos_excess_text)
      )
    )
  )

# Remove text patterns from pharmacy order sentence
for (pattern in text_patterns_to_remove) {
  medications_with_clean_pos <- medications_with_clean_pos |>
    mutate(
      cleaned_pharmacy_order = str_remove(
        cleaned_pharmacy_order,
        pattern
      )
    )
}

# Remove date-time values from pharmacy order sentence
for (dt_col in datetime_cols_to_remove) {
  medications_with_clean_pos <- medications_with_clean_pos |>
    mutate(formatted_datetime = format(
      .data[[dt_col]],
      format = "%d/%b/%y %H:%M:%S %Z"
    )) |>
    mutate(cleaned_pharmacy_order = if_else(
      is.na(.data[[dt_col]]),
      cleaned_pharmacy_order,
      str_remove(
        cleaned_pharmacy_order,
        coll(formatted_datetime)
      )
    )) |>
    select(-formatted_datetime)
}

# Remove other column values from pharmacy order sentence
for (col in other_cols_to_remove) {
  medications_with_clean_pos <- medications_with_clean_pos |>
    mutate(cleaned_pharmacy_order = if_else(
      is.na(.data[[col]]),
      cleaned_pharmacy_order,
      str_remove(
        cleaned_pharmacy_order,
        coll(.data[[col]])
      )
    ))
}

# Final cleanup of pharmacy order sentence
medications_with_clean_pos <- medications_with_clean_pos |>
  mutate(
    cleaned_pharmacy_order = str_remove(
      str_squish(cleaned_pharmacy_order),
      "^ *(, *)*"
    ),
    cleaned_pharmacy_order = str_replace_all(
      cleaned_pharmacy_order,
      "(, *){2,}",
      ", "
    )
  ) |>
  mutate(pos_truncated = if_else(
    is.na(pos_excess_text), FALSE, TRUE
  )) |>
  select(-dose_with_separators) |>
  rename(pharmacy_order_clean = cleaned_pharmacy_order) |>
  rename(pos_character_limit_reached = pos_truncated)

# Create a comprehensive list of antimicrobial names
# have added some trade names
antimicrobial_names <- c(
  AMR::antimicrobials$name, AMR::antivirals$name, "valganciclovir", "Temocillin", "Posaconazole", "Oseltamivir", "CeFALEXin", 
  "AzithroMYCIN", "Aciclovir", "Co-trimoxazole", "Co-amoxiclav", "ValACICLOVIR", 
  "Aztreonam/avibactam", "Aztreonam-avibactam", "Aztreonam avibactam", "Misc Prescription", "Relebactam", "velpatasvir", 
  "Tromethamine", "Emblaveo", "Augmentin", "Flagyl", "Zavicefta", "Fidaxomicin", "Zerbaxa", "Dificid", "Fidaxomicin"
) |>
  as_tibble() |>
  arrange(value)

# Create pattern for antimicrobial filtering
(antimicrobial_pattern <- str_flatten(antimicrobial_names$value, collapse = "|"))

# Create pattern for antimicrobial filtering (excluding "Misc Prescription")
antimicrobial_pattern_no_misc <- antimicrobial_names |>
  filter(!str_detect(value, "Misc")) |>
  pull(value) |>
  str_flatten(collapse = "|")

# Filter for antimicrobial medications
antimicrobial_medications <- medications_with_clean_pos |>
  filter(str_detect(order_name, regex(antimicrobial_pattern, ignore_case = TRUE)) |
           str_detect(synonym_desc, regex(antimicrobial_pattern, ignore_case = TRUE)))

# Extract antimicrobial names from order_name
antimicrobials_with_names <- antimicrobial_medications |>
  mutate(extracted_names = str_extract_all(
    order_name,
    regex(antimicrobial_pattern_no_misc, ignore_case = TRUE)
  )) |>
  unnest_wider(extracted_names, names_sep = "_") |>
  unite(
    col = order_name_c,
    starts_with("extracted_names"),
    sep = " ",
    na.rm = TRUE
  ) |>
  mutate(order_name_c = str_squish(order_name_c)) |>
  mutate(order_name_c = str_to_sentence(order_name_c)) |>
  mutate(order_name_c = if_else(order_name_c == "", NA, order_name_c)) |>
  select(
    encntr_id,
    order_id,
    mrn,
    starts_with("order"),
    everything()
  )

# Extract antimicrobial names from synonym_desc
antimicrobials_with_synonyms <- antimicrobials_with_names |>
  mutate(extracted_names = str_extract_all(
    synonym_desc,
    regex(antimicrobial_pattern_no_misc, ignore_case = TRUE)
  )) |>
  unnest_wider(extracted_names, names_sep = "_") |>
  unite(
    col = synonym_antimicrobial,
    starts_with("extracted_names"),
    sep = " ",
    na.rm = TRUE
  ) |>
  mutate(synonym_antimicrobial = str_squish(synonym_antimicrobial)) |>
  mutate(synonym_antimicrobial = str_to_sentence(synonym_antimicrobial)) |>
  mutate(synonym_antimicrobial = if_else(synonym_antimicrobial == "", NA, synonym_antimicrobial)) |>
  select(
    encntr_id,
    order_id,
    mrn,
    starts_with("order"),
    starts_with("syn"),
    everything()
  )

# Extract antimicrobial names from pharmacy order sentence
antimicrobials_with_pos_names <- antimicrobials_with_synonyms |>
  mutate(pos_extracted_name = str_extract_all(
    pharmacy_order_clean,
    regex(antimicrobial_pattern_no_misc, ignore_case = TRUE)
  )) |>
  unnest_wider(pos_extracted_name, names_sep = "_") |>
  unite(
    col = pos_extracted_name,
    starts_with("pos_extracted_name"),
    sep = " ",
    na.rm = TRUE
  ) |>
  mutate(pos_extracted_name = str_squish(pos_extracted_name)) |>
  mutate(pos_extracted_name = str_to_sentence(pos_extracted_name)) |>
  mutate(pos_extracted_name = if_else(pos_extracted_name == "", NA, pos_extracted_name)) |>
  select(
    encntr_id,
    order_id,
    mrn,
    starts_with("order"),
    starts_with("syn"),
    everything()
  )

# #check aztreonam/avibactam -------------

drug_names <- "Embl.*|Azt.*|Aztreonam/avibactam|Aztreonum|Aztreonam-avibactam|Aztreonam avibactam"

antimicrobials_with_pos_names |>
  #select(order_name, synonym_desc, pharmacy_order_sentence) |>
  filter(str_detect(order_name, "Misc Prescription")) |>
  filter(str_detect(synonym_desc, regex(drug_names, ignore_case = TRUE)) | str_detect(pharmacy_order_sentence, regex(drug_names, ignore_case = TRUE))) |>
  glimpse()
# 
# # yay! synonym_antimicrobial now includes e.g. Aztreonam avibactam

#keep going ------------------

# Consolidate antimicrobial names from different sources
antimicrobials_consolidated <- antimicrobials_with_pos_names |>
  mutate(
    order_name_c = if_else(is.na(order_name_c) & !is.na(pos_extracted_name),
                           true = pos_extracted_name,
                           false = order_name_c
    ),
    order_name_c = if_else(is.na(order_name_c) & !is.na(synonym_antimicrobial),
                           true = synonym_antimicrobial,
                           false = order_name_c
    ),
    order_name_c = str_to_sentence(order_name_c)
  ) |>
  select(
    -synonym_antimicrobial,
    -pos_extracted_name
  )

# Convert character columns to factors for efficiency
antimicrobials_factored <- antimicrobials_consolidated |>
  # Special case for Fidaxomicin
  mutate(order_name_c = if_else(
    dose == "200" & indication == "C. diff" & order_name == "Misc Prescription",
    "Fidaxomicin",
    order_name_c
  )) |>
  # Convert character columns to factors (excluding specific columns)
  mutate(across(
    where(is.character) &
      !matches(c("mrn", "patient", "pharmacy_order_clean")),
    forcats::as_factor
  ))

# Final dataset with only identified antimicrobials
data_medications_antimicrobials <- antimicrobials_factored |>
  filter(!is.na(order_name_c))

data_medications_antimicrobials |> 
  str()

# Investigating data_medications_antimicrobials ---------------------------

data_medications_antimicrobials |> 
  count(order_name_c) |> 
  arrange(desc(n)) |> 
  print(n = 100)

# # check out orders data ---------------------------------------------------

# data_medications_antimicrobials |>
#   arrange(start_date_time) |>
#   DT::datatable(filter = "top")

# # why are some orders missing start_date_time?  ----------------------------
data_medications_antimicrobials |>
  filter(is.na(start_date_time))
# these drugs are given with frequency == ONCE (i.e. stat dose only)

# # what about drugs where dose is indeed listed? What else is missing from these ------------------------------------------
# dose NOT missing
data_medications_antimicrobials |>
  filter(!is.na(dose))

# dose NOT missing BUT dose_unit is
data_medications_antimicrobials |>
  filter(!is.na(dose)) |>
  filter(is.na(dose_unit)) |>
  slice_sample(n = 10)
# where dose_unit is missing, this is because dose == "See Instructions"

# frequency AND dose NOT missing
data_medications_antimicrobials |>
  filter(!is.na(dose)) |>
  filter(!is.na(frequency))

# drug form is not hugely important as it's better reflected in route_of_administration (which is a more complete dataset anyway)
data_medications_antimicrobials |>
  filter(!is.na(dose)) |>
  filter(!is.na(frequency)) |>
  filter(!is.na(route_of_administration))

# Optional: Visualise date distribution of antimicrobial orders
date_summary <- data_medications_antimicrobials |>
  mutate(date = format(start_date_time, "%Y-%m-%d")) |>
  mutate(date = ymd(date)) |>
  count(date) |>
  arrange(desc(n))

# # Plot date distribution
# ggplot(date_summary, aes(date, n)) +
#   geom_col() +
#   labs(title = "Distribution of Antimicrobial Orders by Date",
#        x = "Date",
#        y = "Number of Orders")
# #some orders are made for in the future, which seems logical enough

# # Check misc Rx data ------------------------------------------------------
# 
# # Check misc Rx in data_medications
# data_medications |>
#   filter(str_detect(order_name, regex("misc", ignore_case = TRUE))) |>
#   distinct(order_name, synonym_desc, pharmacy_order_sentence)
# 
# # Check misc Rx in antimicrobial_medications
# antimicrobial_medications |>
#   filter(str_detect(order_name, regex("misc", ignore_case = TRUE))) |>
#   distinct(order_name, synonym_desc)
# 
# # Check misc Rx in data_medications_antimicrobials
# data_medications_antimicrobials |>
#   filter(str_detect(order_name, regex("misc", ignore_case = TRUE))) |>
#   distinct(order_name_c, order_name, synonym_desc)
# 
# data_medications_antimicrobials |>
#   filter(str_detect(order_name, regex("misc", ignore_case = TRUE))) |>
#   distinct(order_name_c, order_name, synonym_desc) |>
#   flextable() |>
#   autofit()
# 
# data_medications_antimicrobials |>
#   filter(str_detect(order_name, regex("misc", ignore_case = TRUE))) |>
#   distinct(order_name_c, order_name, synonym_desc, pharmacy_order_sentence)
#   autofit()

# check aztreonam/avibactam

## looking for
# Misc Prescription	147483429	aztreonum with avibactar	        04/11/25	0	Completed	Pharmacy	2692511	aztreonum with avibactar, 2/0.67, intravenous infusion, ONCE, -1, 11/Apr/25 21:01:00 BST, Physician Stop, 11/Apr/25 21:01:00 BST, 1, 0	In patient
# Misc Prescription	147483429	Aztreonam/avibactam	        04/11/25	0	Discontinued	Pharmacy	2692511	Aztreonam/avibactam, 1.5/0.67, intravenous infusion, every SIX hours, -1, 11/Apr/25 22:00:00 BST, 11/Apr/25 22:00:00 BST, 1	In patient

drug_names <- "Embl.*|Azt.*|Aztreonam/avibactam|Aztreonum|Aztreonam-avibactam|Aztreonam avibactam"

data_medications |>
  select(order_name, synonym_desc, pharmacy_order_sentence) |>
  filter(str_detect(order_name, "Misc Prescription")) |>
  filter(str_detect(synonym_desc, regex(drug_names, ignore_case = TRUE)) | str_detect(pharmacy_order_sentence, regex(drug_names, ignore_case = TRUE)))

data_medications_antimicrobials |>
  select(order_name_c, order_name, synonym_desc, pharmacy_order_sentence) |>
  filter(str_detect(order_name, "Misc Prescription")) |>
  filter(str_detect(synonym_desc, regex(drug_names, ignore_case = TRUE)) | str_detect(pharmacy_order_sentence, regex(drug_names, ignore_case = TRUE)))

# yay this is now working!