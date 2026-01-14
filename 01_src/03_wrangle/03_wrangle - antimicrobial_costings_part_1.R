# pre-req -----------------------------------------------------------------

# source("./01_src/01_startup/01_initialise.R")
# 
# source("./01_src/functions.R")
# 
# source("./01_src/02_data_import/02_data_import - read database.R")
# 
# source("./01_src/03_wrangle/03_wrangle - merge_data_to_create_antimicrobial_pool.R")
# 
# source("./01_src/03_wrangle/03_wrangle - point_prevalence_survey.R")

# wrangle -----------------------------------------------------------------

# Specify the path to the Excel file
file_path_costings <- "02_data/Antimicrobial Costings - LATEST COPY.xlsx"

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path_costings)

# Read all sheets into a named list
all_sheets <- lapply(sheet_names, function(sheet) {
  read_excel(file_path_costings, sheet = sheet, col_names = TRUE)
})

names(all_sheets) <- sheet_names

# Helper function to make safe snake_case variable names
make_snake_case <- function(x) {
  name <- tolower(gsub("[^a-zA-Z0-9]", "_", x))
  name <- gsub("_+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  name
}

for (sheet in sheet_names) {
  # Read the full sheet as character to find the "BNF" row
  temp_sheet <- read_excel(file_path_costings, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  
  # Find the first row containing "BNF" in any cell
  bnf_row <- which(apply(temp_sheet, 1, function(x) any(grepl("^BNF", as.character(x), ignore.case = TRUE))))
  
  if (length(bnf_row) == 0) {
    warning(paste("No BNF header found in sheet:", sheet))
    next
  }
  
  # Read the sheet again, skipping rows before the "BNF" header
  df <- read_excel(file_path_costings, sheet = sheet, skip = bnf_row[1]-1, col_names = TRUE)
  
  safe_name <- paste0("data_costings_", make_snake_case(sheet))
  assign(safe_name, df, envir = .GlobalEnv)
}

print(paste("Imported sheets with BNF headers:", paste(sheet_names, collapse = ", ")))


# Get all object names that start with "data_costings"
costings_objects <- ls(pattern = "^data_costings")


# If none found, stop with a message
if (length(costings_objects) == 0) stop("No objects found with names starting with 'data_costings'.")

# Retrieve as list
costings_list <- mget(costings_objects, envir = .GlobalEnv)

# Find all unique column names
all_cols <- unique(unlist(lapply(costings_list, names)))


# New function; Coerce all columns to character---------------------------------------------

# Coerce all columns to character
costings_list_chr <- lapply(costings_list, function(df) {
  for (col in all_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    } else {
      df[[col]] <- NA_character_
    }
  }
  # Ensure column order is the same
  df <- df[all_cols]
  return(df)
})

# Now do the full join
data_full_costings <- bind_rows(costings_list_chr) |> 
  janitor::clean_names()

data_full_costings |> 
  glimpse()


# remove really expensive formulations of fluconazole ---------------------

# Q for Kerry Sproates 15/09/2025:
# Why does one IV formulation of fluconazole cost less than a coffee a day, whilst the other costs £160 per day?
# A: Only the cheaper version is actually dispensed

#check
data_full_costings |> 
  filter(str_detect(antibiotic, "Flucon")) |> 
  count(antibiotic, cost_per_7_days)

#remove
data_full_costings <- data_full_costings |> 
  filter(!antibiotic == "Fluconazole 50 mg injection")

#re-check
data_full_costings |> 
  filter(str_detect(antibiotic, "Flucon")) |> 
  count(antibiotic, cost_per_7_days)
# removed: 
# 6 Fluconazole 50 mg injection            1120                1
# 7 Fluconazole 50 mg injection            560                 1





# rationalise the data ----------------------------------------------------

data_full_costings |> 
  count(antibiotic, iv_po, usual_dose) 

# some antibiotic names have "+" in them. Where this happens, create a name name in a new column called "antibiotic_corrected", which takes into account all the different names of each antibiotic included in the preparation

data_full_costings |> 
  filter(str_detect(antibiotic, "\\+")) |> 
  select(antibiotic) 


# This regex will remove numbers, /, mg, g, %, mcg, units, mega units, mu, ml, ml, mcg, tabs, tablets, capsules, injection, infusion, suspension, cream, ointment, solution, oral gel, pessaries, powder, PWD, capsule, tablet, drops, nasal, eye, for, and other common pharmaceutical suffixes.
# It will also remove extra spaces.

data_full_costings <- data_full_costings %>%
  mutate(
    ab_name_costings = antibiotic %>%
      # Remove numbers, units, %, /, parentheses, and common dosage/form terms, but keep "+"
      str_remove_all("[0-9]+(\\.[0-9]+)?") %>%              # Remove all numbers (including decimals)
      str_remove_all("%") %>%                               # Remove percent symbol
      str_remove_all(",") %>%                               # Remove comma symbol
      str_remove_all("\\b(mg|g|mcg|ml|mu|units|Mega|mega units|microgram|mcg|mL)\\b") %>% # Remove units (case-insensitive)
      str_remove_all("\\([^\\)]*\\)") %>%                   # Remove all parenthetical content
      str_remove_all("\\/") %>%                             # Remove all slashes
      str_remove_all("\\b(tab(lets)?|caps(ules)?|inj(ection)?|Infusion|Injection|njection|infusion|suspension|Suspension|cream|ointment|solution|oral gel|tube|oral|Oral gel|Oral gel tube|pessaries|powder|PWD|drops|nasal|eye|for|sachet|disp|stat|vial|capsule|tablet|in |liposomal|lactobionate)\\b") %>%
      str_replace_all("\\s*\\+\\s*", " + ") %>%             # Standardize spacing around +
      str_replace_all("\\s+", " ") %>%                      # Collapse multiple spaces
      str_remove_all("\\sin\\s")    |>                      # Remove all slashes
      str_replace("Glecaprevirpibrentasvir", "Glecaprevir/pibrentasvir") |> 
      str_replace("Elbasvir and grazoprevir", "Elbasvir/grazoprevir") |> 
      str_replace("Sodium fusidate fusidic acid ", "Fusidic acid") |> 
      str_replace("Nitrofurantoin MR", "Nitrofurantoin") |> 
      str_replace("Colistimethate sodium", "Colistin") |> 
      str_replace("Meropenem vaborbactam", "Meropenem/vaborbactam") |> 
      str_trim()                                          # Trim leading/trailing spaces
  )

data_full_costings <- data_full_costings |> 
  mutate(ab_name_costings = ab_name_costings %>% 
           str_replace("Cilastatin \\+ Imipenem", "Imipenem") |> 
           str_replace("Imipenem \\+ Relebactam", "Imipenem/relebactam") |> 
           str_replace("Aztreonam \\+ avibactam", "Aztreonam/avibactam") |> 
           str_replace("Piperacillin \\& tazobactam ", "Piperacillin/tazobactam ") |> 
           str_replace("Ceftazidime \\+ avibactam", "Ceftazidime/avibactam"))

# check
data_full_costings |> 
  filter(str_detect(antibiotic, "\\+")) |> 
  select(antibiotic, ab_name_costings) |> 
  print(n = 30)


# Precompute your mapping
# If ab_name_costings only has a limited set of unique values, you can precompute the mapping from ab_name_costings to the new name once, then join that mapping.


# Step 1: Get unique names as character
unique_names <- as.character(unique(data_full_costings$ab_name_costings))

# Step 2: Precompute mapping, vectorizing the name lookup
is_av <- unique_names %in% AMR::antivirals$name

ab_name_costings_amr <- character(length(unique_names))
if (any(is_av)) {
  ab_name_costings_amr[is_av] <- AMR::av_name(unique_names[is_av])
}
if (any(!is_av)) {
  ab_name_costings_amr[!is_av] <- AMR::ab_name(unique_names[!is_av])
}

mapping <- tibble(
  ab_name_costings = unique_names,
  ab_name_costings_amr = ab_name_costings_amr
)

# Step 3: Join mapping back (ensure ab_name_costings is character)
data_full_costings <- data_full_costings %>%
  mutate(ab_name_costings = as.character(ab_name_costings)) %>%
  left_join(mapping, by = "ab_name_costings")

# Step 4: Join with AMR::antimicrobials (ensure key types match)
data_full_costings_amr <- data_full_costings %>%
  left_join(
    AMR::antimicrobials %>% mutate(name = as.character(name)),
    by = c("ab_name_costings_amr" = "name")
  )

#check output 

data_full_costings_amr |> 
  distinct(ab_name_costings, ab_name_costings_amr, ab) |> 
  print(n = 100)


# Show rows where the AMR-mapped name is missing, or does not closely resemble the input (case-insensitive, trimmed)
mismatches_costings <- data_full_costings_amr %>%
  distinct(ab_name_costings, ab_name_costings_amr, ab) %>%
  filter(
    is.na(ab_name_costings_amr) |
      is.na(ab_name_costings) |
      !str_detect(
        tolower(trimws(ab_name_costings_amr)),
        fixed(tolower(trimws(ab_name_costings)), ignore_case = TRUE)
      )
  )

#looks OK?
mismatches_costings 
#A: not for:
# A tibble: 5 × 3
# ab_name_costings      ab_name_costings_amr                            ab  
# <chr>                     <chr>                                        <ab>
# 2 Colistimethate sodium     Rifampicin/pyrazinamide/ethambutol/isoniazid RPEI

#looks OK?
data_full_costings_amr |> 
  filter(str_detect(antibiotic, "\\+")) |> 
  distinct(antibiotic, ab_name_costings, ab_name_costings_amr, ab, synonyms) |> 
  unnest(synonyms) 

data_full_costings_amr |> 
  filter(ab_name_costings_amr == "Temocillin")


# integrate iv_po and iv_po_2 ---------------------------------------------

data_full_costings_amr <- data_full_costings_amr |> 
  mutate(iv_po = if_else(
    is.na(iv_po),
    true = iv_po_2,
    false = iv_po
  ))

data_full_costings_amr |> 
  count(iv_po, iv_po_2)


# remove redundant columns ------------------------------------------------

#drop iv_po_2, dose_per_day, loinc
data_full_costings_amr <- data_full_costings_amr |> 
  select(
    -iv_po_2, 
    -dose_per_day,
    -loinc)

# rename iv_po to iv_po_costings ------------------------------------------

data_full_costings_amr <- data_full_costings_amr |> 
  rename(iv_po_costings = iv_po)



# # write csv ---------------------------------------------------------------
# 
# data_full_costings_amr |> 
#   arrange(bnf, iv_po_costings) |> 
#   write_csv("data_full_costings_amr.csv")


# 11 map drug_form against drug form-related information included with "antibiotic" in costings data --------

# data_antimicrobial_pool_iv_im_po |> 
#   distinct(drug_form)

data_full_costings_amr <- data_full_costings_amr |>
  mutate(
    drug_form_costings = case_when(
      str_detect(antibiotic, regex("\\binjection\\b|injection\\)|injection\\.|Injection", ignore_case = TRUE)) ~ "Injection",
      antibiotic == "Meropenem 1 g / vaborbactam  1 g" ~ "Injection",
      str_detect(antibiotic, regex("\\btablet\\b|tablets|disp tablet|Dispersible Tablet", ignore_case = TRUE)) ~ "Tablet",
      str_detect(antibiotic, regex("\\bcapsule\\b|capsules|MR capsule|Modified Release Capsule", ignore_case = TRUE)) ~ "Capsule",
      str_detect(antibiotic, regex("oral suspension|Suspension|suspension|Aciclovir\\s200\\smg\\/5ml\\s\\(125ml\\)", ignore_case = TRUE)) ~ "Oral Suspension",
      antibiotic == "Elbasvir and grazoprevir 100 mg/50 mg" ~ "Oral Suspension",
      antibiotic == "Glecaprevir/pibrentasvir 100 mg /40 mg" ~ "Oral Suspension",
      str_detect(antibiotic, regex("Modified Release Capsule|MR capsule", ignore_case = TRUE)) ~ "Modified Release Capsule",
      str_detect(antibiotic, regex("dispersible tablet|disp tablet", ignore_case = TRUE)) ~ "Dispersible Tablet",
      antibiotic == "Fosfomycin 3 g sachet" ~ "Sachet",
      str_detect(antibiotic, regex("solution for injection|infusion|in 2mL|in 4ml", ignore_case = TRUE)) ~ "solution for Injection",
      TRUE ~ NA_character_
    )
  )

data_full_costings_amr |> 
  distinct(drug_form_costings)


#where where is.na(drug_form_costings)... Should all be topical agents only now
data_full_costings_amr |>
  filter(is.na(drug_form_costings)) |> 
  distinct(antibiotic, ab_name_costings_amr, drug_form_costings) 
# ?Only topicals listed ... A: Yes, only topicals listed here


# change tedizolid Tablet from IV to PO -----------------------------------

ind <- which(data_full_costings_amr$ab_name_costings_amr == "Tedizolid" & data_full_costings_amr$drug_form_costings == "Tablet")

data_full_costings_amr$iv_po_costings[ind] <- "PO"

# extract dose information from "antibiotic" ------------------------------

data_full_costings_amr |> 
  select(antibiotic) |> 
  print(n = 188)

# Extract dose information from antibiotic names
data_full_costings_amr <- data_full_costings_amr |>
  mutate(
    # Extract various dose patterns
    dose_extracted = case_when(
      # Pattern 1: Standard mg/g doses (e.g., "250 mg", "1.2 g")
      str_detect(antibiotic, "\\b\\d+(\\.\\d+)?\\s*(mg|g)\\b") ~ 
        str_extract(antibiotic, "\\b\\d+(\\.\\d+)?\\s*(mg|g)\\b"),
      
      # Pattern 2: Combination doses (e.g., "250/125", "500/125")
      str_detect(antibiotic, "\\b\\d+/\\d+\\b") ~ 
        str_extract(antibiotic, "\\b\\d+/\\d+\\b"),
      
      # Pattern 3: Mega units (e.g., "2 Mega units", "1 Mega units")
      str_detect(antibiotic, "\\b\\d+\\s*Mega\\s*units?\\b") ~ 
        str_extract(antibiotic, "\\b\\d+\\s*Mega\\s*units?\\b"),
      
      # Pattern 4: Micrograms (e.g., "500 microgram")
      str_detect(antibiotic, "\\b\\d+\\s*microgram\\b") ~ 
        str_extract(antibiotic, "\\b\\d+\\s*microgram\\b"),
      
      # Pattern 5: Units (e.g., "100,000 units")
      str_detect(antibiotic, "\\b[\\d,]+\\s*units?\\b") ~ 
        str_extract(antibiotic, "\\b[\\d,]+\\s*units?\\b"),
      
      # Pattern 6: Percentage (e.g., "0.5%", "1%", "2%")
      str_detect(antibiotic, "\\b\\d+(\\.\\d+)?%\\b") ~ 
        str_extract(antibiotic, "\\b\\d+(\\.\\d+)?%\\b"),
      
      # Pattern 7: Complex combinations (e.g., "100 mg/50 mg", "1 g + avibactam 0.5 g")
      str_detect(antibiotic, "\\b\\d+\\s*mg/\\d+\\s*mg\\b") ~ 
        str_extract(antibiotic, "\\b\\d+\\s*mg/\\d+\\s*mg\\b"),
      
      TRUE ~ NA_character_
    ),
    
    # Clean up the extracted dose
    dose_clean = str_trim(dose_extracted),
    
    # Extract just the numeric value for analysis
    dose_numeric = case_when(
      str_detect(dose_clean, "^\\d+(\\.\\d+)?\\s*mg$") ~ 
        as.numeric(str_extract(dose_clean, "\\d+(\\.\\d+)?")),
      str_detect(dose_clean, "^\\d+(\\.\\d+)?\\s*g$") ~ 
        as.numeric(str_extract(dose_clean, "\\d+(\\.\\d+)?")) * 1000, # Convert to mg
      TRUE ~ NA_real_
    ),
    
    # Extract dose unit
    dose_unit = case_when(
      str_detect(dose_clean, "mg") ~ "mg",
      str_detect(dose_clean, "\\bg\\b") ~ "g",
      str_detect(dose_clean, "Mega\\s*units?") ~ "Mega units",
      str_detect(dose_clean, "microgram") ~ "microgram",
      str_detect(dose_clean, "units?") ~ "units",
      str_detect(dose_clean, "%") ~ "%",
      str_detect(dose_clean, "/") ~ "combination",
      TRUE ~ NA_character_
    )
  )

# Display the results
data_full_costings_amr |>
  select(antibiotic, dose_extracted, dose_unit) |>
  print(n = 200)

# Examples of each dose type
print("\nExamples of different dose patterns:")
data_full_costings_amr |>
  filter(!is.na(dose_extracted)) |>
  group_by(dose_unit) |>
  slice_head(n = 3) |>
  select(antibiotic, dose_extracted, dose_unit) |>
  arrange(dose_unit) |>
  print()

# Extract numerical values from dose_extracted and convert to numeric
data_full_costings_amr <- data_full_costings_amr |>
  mutate(
    # Extract numerical values from dose_extracted
    dose_numeric_value = case_when(
      # For standard doses (mg, g, microgram, %)
      str_detect(dose_extracted, "^\\d+(\\.\\d+)?\\s*(mg|g|microgram|%)$") ~ 
        as.numeric(str_extract(dose_extracted, "\\d+(\\.\\d+)?")),
      
      # For Mega units (extract the number before "Mega")
      str_detect(dose_extracted, "\\d+\\s*Mega\\s*units?") ~ 
        as.numeric(str_extract(dose_extracted, "\\d+")),
      
      # For regular units (extract number, handling commas)
      str_detect(dose_extracted, "^[\\d,]+\\s*units?$") ~ 
        as.numeric(str_remove_all(str_extract(dose_extracted, "[\\d,]+"), ",")),
      
      # For combination doses (extract first number)
      str_detect(dose_extracted, "^\\d+/\\d+$") ~ 
        as.numeric(str_extract(dose_extracted, "^\\d+")),
      
      # For complex mg combinations (extract first number)
      str_detect(dose_extracted, "\\d+\\s*mg/\\d+\\s*mg") ~ 
        as.numeric(str_extract(dose_extracted, "\\d+")),
      
      TRUE ~ NA_real_
    ),
    
    # Also extract the second number for combination doses
    dose_numeric_value_2 = case_when(
      # For combination doses (extract second number)
      str_detect(dose_extracted, "^\\d+/\\d+$") ~ 
        as.numeric(str_extract(dose_extracted, "(?<=/)\\d+")),
      
      # For complex mg combinations (extract second number)
      str_detect(dose_extracted, "\\d+\\s*mg/\\d+\\s*mg") ~ 
        as.numeric(str_extract(dose_extracted, "(?<=mg/)\\d+")),
      
      TRUE ~ NA_real_
    )
  )

# Display the results with numerical values
data_full_costings_amr |>
  select(antibiotic, dose_extracted, dose_unit, dose_numeric_value, dose_numeric_value_2) |>
  print(n = 188)

# Summary of numerical dose values
print("\nSummary of numerical dose values:")
data_full_costings_amr |>
  filter(!is.na(dose_numeric_value)) |>
  group_by(dose_unit) |>
  summarise(
    count = n(),
    min_dose = min(dose_numeric_value, na.rm = TRUE),
    max_dose = max(dose_numeric_value, na.rm = TRUE),
    median_dose = median(dose_numeric_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(count)) |>
  print()

# Show examples of combination doses with both values
print("\nCombination doses with both numerical values:")
data_full_costings_amr |>
  filter(!is.na(dose_numeric_value_2)) |>
  select(antibiotic, dose_extracted, dose_numeric_value, dose_numeric_value_2) |>
  print()

# rationalise data --------------------------------------------------------

data_full_costings_amr |> 
  colnames()

data_full_costings_amr |> 
  glimpse()


# keep only certain columns to create data_full_costings_amr_simple -----------------------------------------------

data_full_costings_amr_simple <- data_full_costings_amr |> 
  select(ab_name_costings_amr, dose_numeric_value, iv_po_costings, drug_form_costings, usual_dose, ab_name_costings_amr, starts_with("cost")) |> 
  select(-cost_per_ddd_from_rx_info) |> 
  arrange(ab_name_costings_amr, iv_po_costings)

data_full_costings_amr_simple |> 
  glimpse()

# Exclude suspension ------------------------------------------------------

data_full_costings_amr_simple <- data_full_costings_amr_simple |> 
  filter(!drug_form_costings == "Oral Suspension") 


# Check for duplicate rows to create data_full_costings_amr_simple_filtered ------------------------------------------------

data_full_costings_amr_simple |> 
  count(ab_name_costings_amr, ab_name_costings_amr, iv_po_costings) |> 
  arrange(desc(n))

# keep only one row per antibiotic ----------------------------------------

data_full_costings_amr_simple_filtered <- data_full_costings_amr_simple |> 
  group_by(ab_name_costings_amr, iv_po_costings) |> 
  slice_max(dose_numeric_value, n = 1) |> #max dose
  slice_max(cost_per_dose, n = 1) |> #max cost per dose
  slice_max(cost_per_day, n = 1) |> #max cost per day
  ungroup()

data_full_costings_amr_simple_filtered |> 
  count(ab_name_costings_amr, ab_name_costings_amr, iv_po_costings) |> 
  arrange(desc(n))
#should only be n == 1 for each pairing of ab_name_costings_amr and iv_po_costings     



