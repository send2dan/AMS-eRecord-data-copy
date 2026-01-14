# wrangle

# Read functions ----------------------------------------------------------

# source("./01_src/functions.R")

# prerequisite R scripts -----------------------------------------------------

# If .db files don't exist/new data needs to be added, run this:
# source('./01_src/02_data_import/02_data_import - 01_test eRecord data.R')

# Read in (or create) Allergies dataset -----------------------------------------------

# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Packages",
#                end_string = "END PACKAGES")
# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Allergies",
#                end_string = "END ALLERGIES")

# Skim Allergies ----------------------------------------------------------

# data_allergies |>
#   skimr::skim()

# START ALLERGIES WRANGLED
data_allergies_wrangled_01 <- data_allergies |>
  select(-onset_dt_tm) |> # because this field is all NA [UPDATE 27-06-24: There are actually some entries in this field, so may want to reconsider removal]
  select(-end_effective_dt_tm) # All rows should have the same value in this column, so does not provide any further useful info and can be safely removed.

data_allergies_wrangled_02 <- data_allergies_wrangled_01 |>
  mutate(across(where(is.character), \(x) na_if(x, "NULL"))) |>
  mutate(across(ends_with("_dt_tm"), \(x) ymd_hms(x, tz = "GB"))) |>
  mutate(diff_created_reviewed = reviewed_dt_tm - created_dt_tm) |>
  group_by(mrn) |>
  mutate(concurrent_allergies = n_distinct(substance)) |>
  ungroup()

data_allergies_wrangled_03 <- data_allergies_wrangled_02 |>
  mutate(reaction_ft_desc = if_else(str_squish(reaction_ft_desc) == "",
    NA,
    str_squish(reaction_ft_desc)
  )) |>
  rename(earliest_import_date = import_date) |>
  group_by(across(-c(reaction_ft_desc, allergy_comment))) |>
  summarise(
    reaction_ft_desc = str_c(reaction_ft_desc,
      collapse = ", "
    ),
    allergy_comment = str_c(allergy_comment,
      collapse = ", "
    )
  ) |>
  ungroup() # When all fields are the same except reaction_ft_desc and/or allergy_comment, this implies that there are duplicate entries for essentially the same allergy record.

data_allergies_wrangled <- data_allergies_wrangled_03 |>
  mutate(
    reaction_ft_desc_length = str_length(reaction_ft_desc),
    allergy_comment_length = str_length(allergy_comment)
  ) |>
  mutate(beta_lac_allergy = case_when(
    str_detect(substance, regex(
      list_of_beta_lacs,
      ignore_case = TRUE
    )) ~ TRUE,
    .default = FALSE
  ))

# END ALLERGIES WRANGLED
# # add AMR class of drug ---------------------------------------------------
#
# # too slow [UPDATE 27-06-24: addition of groups is eventually done in a later wrangle script, could consider moving it to here]
#
# data_allergies_wrangled <- data_allergies_wrangled  |>
#   mutate(ab_class = AMR::ab_group(substance))
#
# data_allergies_wrangled |>
#   count(ab_class)


# check correct removal of white space and "" to NA -----------------------

# check 1
data_allergies_wrangled |>
  filter(!is.na(allergy_comment) & !is.na(reaction_ft_desc)) |>
  select(substance, allergy_comment, reaction_ft_desc, reaction_ft_desc_length, allergy_comment_length) |>
  arrange(desc(reaction_ft_desc_length))

# check 2
data_allergies_wrangled |>
  filter(!is.na(allergy_comment) & !is.na(reaction_ft_desc)) |>
  glimpse()

data_allergies_wrangled |>
  arrange(mrn, encntr_id, substance, earliest_import_date) |>
  count(encntr_id, mrn, substance, sort = TRUE) |>
  glimpse()


# looking for non-active (i.e. cancelled) allergies -----------------------

# requires functions.R script to be run first for object: list_of_beta_lacs
# i.e.
# source("./01_src/functions.R")

# data_allergies |>
#   str()
# 
# data_allergies |> 
#   count()

data_allergies |>
  distinct(mrn, substance, reaction_status) |> 
  filter(str_detect(substance, regex("clav", ignore_case = TRUE)))

#look for all beta lactam allergies
data_allergies |>
  distinct(mrn, substance, reaction_status) |> 
  filter(str_detect(substance, regex(list_of_beta_lacs, ignore_case = TRUE))) |> 
  arrange(mrn, substance) |> 
  add_count(mrn) |> 
  print(n = 100) 

#look for all NON-ACTIVE beta lactam allergies (using reaction_status)
data_allergies |> 
  filter(!reaction_status == "Active",
         str_detect(substance, regex(list_of_beta_lacs, ignore_case = TRUE))) 

data_allergies |>
  distinct(mrn, substance, reaction_status) |> 
  filter(!reaction_status == "Active",
         str_detect(substance, regex(list_of_beta_lacs, ignore_case = TRUE))) |> 
  arrange(mrn, substance) |> 
  add_count(mrn) |> 
  print(n = 100) 
