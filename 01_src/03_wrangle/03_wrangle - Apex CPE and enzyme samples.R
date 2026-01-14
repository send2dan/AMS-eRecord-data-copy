
data_cpe |> 
  count(order_code)

# remove data e.g. test patients ------------------------------------------

data_cpe |> 
  distinct(patient_full_name, hospital_number, specimen_no) |> 
  count(patient_full_name, hospital_number) |> 
  arrange(desc(n)) 

# e.g. 
# 999999999999 hospital_number
# 999999 hospital_number
# UNREGISTERED,UNREGISTERED patient_full_name
# EQA,MICROBIOLOGY patient_full_name
# patient_full_name      hospital_number specimen_no
# <chr>                  <chr>           <chr>      
#   1 "HARVEYTEST,TEST"      FRA444444       MG212122Z  
# 2 "EDITESTPATIENT,EIGHT" 91473937        MG234567T  
# 3 "TESTO,WAINE MAX"      0564752U        MG391565F  
# 4 "ZZZEDBBVTEST,OIN"     FRA6286311      MS939346W  
# 5 "ZZZEDBBVTEST,DORI"    FRA6286308      MS939349F  
# 6 "ZZZEDBBVTEST,KILI"    FRA6286307      MS939350L  
# 7 "ZZZLABTEST,MALE"      91680737        MG353535C  
# 8 "TEST, "               666666          MP724038A 

data_cpe_with_initials <- data_cpe |> 
  filter(!str_detect(hospital_number, "999999999999|999999|FRA|666666"),
         !str_detect(patient_full_name, "UNREGISTERED|MICROBIOLOGY|EDITESTPATIENT|^ZZZ"))

data_cpe_with_initials |> 
  distinct(patient_full_name, hospital_number, specimen_no) |> 
  count(patient_full_name, hospital_number) |> 
  arrange(desc(n)) 

data_cpe_with_initials |> 
  distinct(patient_full_name, hospital_number, specimen_no) |> 
  filter(str_detect(patient_full_name, regex("Test", ignore_case = TRUE)))

# patient initials --------------------------------------------------------

data_cpe_with_initials <- data_cpe_with_initials |> 
  mutate(patient_full_name = str_replace(patient_full_name, ",", ", ")) |> #add space after comma
  mutate(initials = if_else(!is.na(patient_full_name), 
                            get_initials(patient_full_name),
                            NA_character_)) |> 
  relocate(initials, .after = patient_full_name)

# add age -----------------------------------------------------------------

data_cpe_with_initials <- data_cpe_with_initials |> 
  mutate(age = AMR::age(date_of_birth)) |> 
  relocate(age, .after = patient_full_name)

# Check original data -----------------------------------------------------

# data_cpe_with_initials |> 
#   View()

#CRIRES flagged specimens only (any type)
#faeces only
crires_spec <- data_cpe_with_initials |> 
  filter(order_code == "CRIRES") |> 
  pull(specimen_no)

## faeces with CRIRES only
# data_cpe_with_initials |> 
#   filter(specimen_no %in% crires_spec) |> 
#   arrange(desc(quantifier)) |> 
#   filter(specimen_type_code == "F") |> 
#   View

# FILTER to keep only rows of interest ----------------------------------------------

#search includes OXA23
#IMA = immunoassay
data_cpe_filtered_1 <- data_cpe_with_initials |> 
  filter(quantifier == "I" | quantifier == "DET" | quantifier == "POS" | quantifier == "OXA48" | quantifier == "OXA23" | quantifier == "NDM" | quantifier == "VIM" | quantifier == "KPC" | quantifier == "IMP1" | organism_code == "IMA" | result == "DET" | result == "POS") |> 
  arrange(quantifier)

# data_cpe_filtered_1 |>
#   View()

data_cpe_filtered_2 <- data_cpe_filtered_1 |> 
  filter(is.na(organism_code) | str_detect(organism_code, "CPE|CPO|OXA48|OXA23|NDM|VIM|KPC|IMP1|IMA"))

# step 1 remove test_code = CPESCR and result = R plus organism_code = IMA and quantifier = NEG 
data_cpe_filtered_3 <- data_cpe_filtered_2 |> 
  filter(test_code == "CPESCR" & result == "R" & organism_code == "IMA" & quantifier == "NEG" )

# step 2 remove test_code = CPESCR and result = R plus organism_code = IMA and quantifier = NEG 
data_cpe_filtered <- data_cpe_filtered_2 |> 
  anti_join(data_cpe_filtered_3)

# data_cpe_filtered |>
#   View()

# Check what's missing ----------------------------------------------------

#################
#this shouldn't include anything that looks interesting!
#################

# data_cpe |>
#   anti_join(data_cpe_filtered) |>
#   View()

#result_expansion (i.e. molecular tests)
data_cpe |>
  anti_join(data_cpe_filtered) |> 
  count(result, result_expansion)

#quantifier & organism_code (i.e. culture)
data_cpe |>
  anti_join(data_cpe_filtered) |> 
  count(organism_code, quantifier) |> 
  arrange(desc(n))

#quantifier & organism_code (i.e. culture) where !is.na(quantifier) & is.na(organism_code)
data_cpe |>
  anti_join(data_cpe_filtered) |> 
  filter(!is.na(quantifier) & is.na(organism_code)) |> 
  count(organism_code, quantifier) |> 
  arrange(desc(n))

#quantifier == "I"
data_cpe |>
  anti_join(data_cpe_filtered) |> 
  filter(quantifier == "I") |> 
  count(organism_code) |> 
  arrange(desc(n))

#specimen type
data_cpe |>
  anti_join(data_cpe_filtered) |> 
  count(specimen_type_code) |> 
  arrange(desc(n))

#faecs
data_cpe |>
  anti_join(data_cpe_filtered) |> 
  filter(specimen_type_code == "F") |> 
  count(test_code, specimen_type_code, order_code, result, quantifier, organism_code) |> 
  arrange(test_code, specimen_type_code, order_code, result, quantifier, organism_code) |> 
  print(n = 100)

#all
data_cpe |>
  anti_join(data_cpe_filtered) |> 
  #filter(specimen_type_code == "F") |> 
  count(test_code, specimen_type_code, order_code, result) |> 
  arrange(test_code, specimen_type_code, order_code, result) |> 
  print(n = 100)

#all
data_cpe |>
  anti_join(data_cpe_filtered) |> 
  #filter(specimen_type_code == "F") |> 
  count(test_code, result) |> 
  arrange(test_code, result) |> 
  print(n = 100)

# Check what's left -------------------------------------------------------

data_cpe |> 
  glimpse()

# Check final output ------------------------------------------------------

data_cpe_filtered |> 
  head()

data_cpe_filtered |> 
  glimpse()

# data_cpe_filtered |> 
#   kableExtra::kable()

# histogram age ----------

age_histogram <- data_cpe_filtered |> 
  distinct(hospital_number, .keep_all = TRUE) |>
  filter(!is.na(age)) |> 
  ggplot(aes(x = age)) +
  geom_histogram(bins = 10) +
  theme_minimal() +
  labs(
    title = "Age of patients with significant CPE-related results",
    subtitle = glue::glue("Data for the 24 weeks prior to {date}",
                          date = format(ymd(receive_date_end), "%A %d %B %Y")
    ),
    caption = "Significant CPE-related results including immunoassay, molecular tests and culture",
    x = "Age",
    y = "n"
  )

age_histogram

# location_code ----------

#add location_code_lump 
data_cpe_filtered <- data_cpe_filtered |> 
  mutate(location_code_lump = fct_lump(location_code, 10)) |> 
  relocate(location_code_lump, .after = location_code)

location_code_geom_col <- data_cpe_filtered |> 
  distinct(hospital_number, specimen_no, .keep_all = TRUE) |>
  filter(!is.na(location_code_lump)) |>
  count(location_code_lump) |> 
  ggplot(aes(fct_reorder(location_code_lump, -n), n)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Collection location of specimens leading to \nsignificant CPE-related results",
    subtitle = glue::glue("Data for the 24 weeks prior to {date}",
                          date = format(ymd(receive_date_end), "%A %d %B %Y")
    ),
    caption = "Significant CPE-related results including immunoassay, molecular tests and culture",
    x = NULL,
    y = "n"
  ) +
  coord_flip()

location_code_geom_col

# specimen_type_code ----------

## all data
# data_cpe|> 
#   count(specimen_type_code)

#filtered data
data_cpe_filtered |> 
  count(specimen_type_code) |> 
  arrange(desc(n))

#import LIMS codes RE: specimen_type
LIMS_All_APEX_codes <- read_excel("02_data/LIMS - All APEX codes.xlsx", 
                                  sheet = "Specimen Types") |> 
  clean_names() |> 
  select(specimen_type_code, specimen_type_expansion)

#join LIMS_All_APEX_codes onto data_cpe_filtered to get specimen_type_expansion
data_cpe_filtered <- data_cpe_filtered |> 
  left_join(LIMS_All_APEX_codes) |> 
  glimpse()

#create figure
specimen_type_code_geom_col <- data_cpe_filtered |> 
  distinct(hospital_number, specimen_no, .keep_all = TRUE) |>
  filter(!is.na(specimen_type_code)) |>
  count(specimen_type_code, specimen_type_expansion) |> 
  ggplot(aes(fct_reorder(specimen_type_expansion, -n), n)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Specimen type information for \n CPE-related results",
    subtitle = glue::glue("Data for the 24 weeks prior to {date}",
                          date = format(ymd(receive_date_end), "%A %d %B %Y")
    ),
    caption = "Significant CPE-related results including immunoassay, molecular tests and culture",
    x = NULL,
    y = "n"
  ) +
  coord_flip()

specimen_type_code_geom_col


# check out faeces ("F") results ------------------------------------------

data_cpe_filtered |> 
  filter(specimen_type_code == "F") |> 
  count(order_code, result_expansion, quantifier, organism_code)

# summarise results -------------------------------------------------------

# For each hospital_number, summarise:
#   - cpe PCR pos (where test_code == CPEPC and result == "DET" or "POS")
# -cpe mechanism (which is found in test_code, if you exlcude CPEPC and CPESCR, where result == "DET")
# - immunoassay result (which is found in the quantifier field where organism_code == "IMA")
# - whether CPE was isolated on culture as well (which is found when organism code == "CPE" and quantifier == "I")

data_cpe_filtered |> 
  count(test_code, order_code, result_expansion, quantifier, organism_code) |> 
  arrange(desc(n), test_code, order_code, result_expansion, quantifier, organism_code) |> 
  print(n = 50)

# data_cpe |> 
#   filter(test_code == "CPESCR") |> 
#   View()
# 
# data_cpe_filtered |> 
#   filter(test_code == "CPESCR") |> 
#   View()


data_summary <- data_cpe_filtered %>%
  group_by(location_code, initials, hospital_number, specimen_no, specimen_type_expansion, receive_date) %>%
  summarise(
    cpe_pcr_pos = any(test_code == "CPEPC" & str_detect(result, "POS|DET")),
    cpe_mechanism = paste(
      unique(test_code[test_code != "CPEPC" & test_code != "CPESCR" & result == "DET"]),
      collapse = ", "
    ),
    immunoassay_result = paste(
      unique(quantifier[organism_code == "IMA" & !is.na(quantifier)]),
      collapse = ", "
    ),
    cpe_isolated = any(quantifier == "I" & organism_code == "CPE") | any(!is.na(quantifier) & organism_code == "IMA"),
    cpo_isolated = any(quantifier == "I" & organism_code == "CPO"),
  ) |> 
  ungroup()

data_summary |> 
  filter(!is.na(cpe_mechanism)) |> 
  glimpse()

data_summary_2 <- data_summary |> 
  select(location_code, initials, hospital_number, specimen_no, receive_date, specimen_type_expansion, cpe_pcr_pos, cpe_isolated, cpe_mechanism, immunoassay_result, cpo_isolated) |> 
  arrange(cpe_pcr_pos, cpe_isolated, cpe_mechanism, immunoassay_result, cpo_isolated) 

# Clean up any leading/trailing whitespace in key string columns
data_summary_3 <- data_summary_2 %>%
  mutate(across(c(location_code, hospital_number, cpe_mechanism, immunoassay_result), ~trimws(.))) |> 
  mutate(cpo_cpe_isolated = if_else(condition = cpe_isolated == TRUE | cpo_isolated == TRUE,
                                    true = TRUE,
                                    false = FALSE,
                                    missing = FALSE),
  ) |> 
  select(-cpe_isolated, -cpo_isolated)

data_summary_3 |> 
  print(n = 50)

data_summary_3 |> 
  glimpse()


# specimen type analysis --------------------------------------------------

#create figure
specimen_type_code_geom_col_2 <- data_summary_3 |> 
  distinct(specimen_no, .keep_all = TRUE) |>
  filter(!is.na(specimen_type_expansion)) |>
  count(specimen_type_expansion, cpo_cpe_isolated) |> 
  ggplot(aes(fct_reorder(specimen_type_expansion, -n), n)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Specimen type information for \n CPE/CPO-related results",
    subtitle = glue::glue("Data for the 24 weeks prior to {date}",
                          date = format(ymd(receive_date_end), "%A %d %B %Y")
    ),
    caption = "Significant CPE/CPO-related results including molecular tests and culture/immunoassay",
    x = NULL,
    y = "n"
  ) +
  coord_flip()

specimen_type_code_geom_col_2


# Temporal trends ---------------------------------------------------------

# Temporal Trends:
#  time series analyses (e.g., cases per month/quarter/year) to identify outbreaks, surges, or seasonal variation.

# Make sure receive_date is Date type
# data_summary_3$receive_date <- as.Date(data_summary_3$receive_date) # Uncomment if needed

# 1. Count unique cases per month (by specimen NOT patient)
spec_per_month <- data_summary_3 |>
  mutate(month = floor_date(ymd(receive_date), "month")) |>
  distinct(specimen_no, month, .keep_all = TRUE) |> # one case per patient per month
  count(month, cpo_cpe_isolated) 

# 2. Plot temporal trends
cpo_cpe_patients_per_month <- ggplot(spec_per_month, aes(x = month, y = n, fill = cpo_cpe_isolated)) +
  geom_col() +
  labs(
    title = "Monthly CPE/CPO Detections",
    x = NULL,
    y = "Number of specimens (n)",
    caption = "Analysis of distinct specimens received for \nculture/immunoassay, or molecular diagnostic testing",
    fill = "Detection Method"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_fill_manual(
    values = c(
      "TRUE" = "#27ae60",   # greenish for CPE/CPO isolated on culture
      "FALSE" = "#e74c3c"   # reddish for CPE/CPO detected by other methods
    ),
    labels = c(
      "TRUE" = "Culture",
      "FALSE" = "Molecular test only"
    ),
    name = "Detection Method"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cpo_cpe_patients_per_month

# proportion of samples pos by any means that were pos by culture ---------

prop_samples_culture_pos <- data_summary_3 |> 
  distinct(specimen_no, .keep_all = TRUE) |> 
  count(cpo_cpe_isolated)

# Your processed data
# Processed data
tbl_prop_samples_culture_pos <- prop_samples_culture_pos |> 
  mutate(Total = sum(n)) |> 
  group_by(cpo_cpe_isolated) |> 
  mutate(`Perc. CPO/CPE isolated` = round(n / Total * 100, 1)) |> 
  ungroup() |> 
  mutate(`CPO/CPE isolated` = if_else(cpo_cpe_isolated, "Yes", "No")) |> 
  select(`CPO/CPE isolated`, n, `Perc. CPO/CPE isolated`) 

#Frequency and Percentage of Samples with CPO or CPE Isolated by Culture
perc_molecular <- tbl_prop_samples_culture_pos$`Perc. CPO/CPE isolated`[[1]]

# # Check names
# print(names(tbl_prop_samples_culture_pos))

# Create flextable
ft_prop_samples_culture_pos <- tbl_prop_samples_culture_pos |>
  flextable() |> 
  set_header_labels(
    `CPO/CPE isolated` = "CPO/CPE isolated on culture",
    n = "Number of linked cases",
    `Perc. CPO/CPE isolated` = "Percentage of samples (%)"
  ) |>
  colformat_num(j = "Perc. CPO/CPE isolated", digits = 1) |> # use string or backticks version
  autofit()

ft_prop_samples_culture_pos


# Linked CPE/CPO ----------------------------------------------------------

# I want to produce an analysis where I find patients (identified by hospital_number) that are linked in time (within 4 weeks before and 4 weeks after receive_date) and place (location_code). Write R code that identifies any links using these categories.
# For linked patients, also check whether there is any overlap in the cpe_mechanism OR immunoassay_result 

# Self join by location and date within 4 weeks
data_cpe_links <- data_summary_3 %>%
  mutate(row_id = row_number()) %>%
  inner_join(data_summary_3 %>% mutate(row_id2 = row_number()), by = "location_code") %>%
  filter(row_id < row_id2) %>%
  filter(abs(as.numeric(difftime(receive_date.x, receive_date.y, units = "days"))) <= 28) %>%
  filter(hospital_number.x != hospital_number.y) %>%
  filter(
    (!is.na(cpe_mechanism.x) & !is.na(cpe_mechanism.y) & cpe_mechanism.x != "" & cpe_mechanism.y != "" & cpe_mechanism.x == cpe_mechanism.y) |
      (!is.na(immunoassay_result.x) & !is.na(immunoassay_result.y) & immunoassay_result.x != "" & immunoassay_result.y != "" & immunoassay_result.x == immunoassay_result.y)
  ) %>%
  select(
    location_code,
    hospital_number.x, specimen_no.x, receive_date.x, cpe_mechanism.x, immunoassay_result.x,
    hospital_number.y, specimen_no.y, receive_date.y, cpe_mechanism.y, immunoassay_result.y
  )

data_cpe_links |>  
  glimpse()

#add id.x and id.y that includes specimen_no and hospital_number and location-code
data_cpe_links <- data_cpe_links |> 
  mutate(id.x = str_c(location_code, "\n", hospital_number.x, "\n ", specimen_no.x),
         id.y = str_c(location_code, "\n", hospital_number.y, "\n ", specimen_no.y))


# a column for the outbreak/cluster month and year by extracting the year and month from the receive_date.x (or receive_date.y if you prefer) column. 

# If you want the month name and year (e.g., "Feb 2025"), use:
  
data_cpe_links <- data_cpe_links %>%
  mutate(
    receive_date.x = ymd(receive_date.x),
    linked_case_date = format(receive_date.x, "%b %Y")
  )

# Location of linked CPE/CPO specimens ------------------------------------

data_cpe_links_loc <- data_cpe_links |> 
  count(location_code, linked_case_date, receive_date.x) |> 
  arrange(desc(receive_date.x)) |> 
  select(-receive_date.x)

ft_data_cpe_links_loc <- data_cpe_links_loc |>
  flextable() |> 
  set_header_labels(
    linked_case_date = "Date of linked cases",
    location_code = "Ward",
    n = "Number of samples"
  ) |>
  autofit()

ft_data_cpe_links_loc

# Visualisation of linked CPE/CPO -----------------------------------------

#  The most common way to visualise links between patients is as a network (graph) where each node represents a patient (hospital_number), and an edge connects them if they are linked by your criteria.

# library(igraph)
# library(ggraph)
# library(ggplot2)

# Each row in 'data_cpe_links' is a pair of linked patients

# Create an edge list: one edge per linked pair
# edges <- data_cpe_links %>%
#   select(from = hospital_number.x, to = hospital_number.y)
# 
# edges <- data_cpe_links %>%
#   select(from = specimen_no.x, to = specimen_no.y)
# 
# edges <- data_cpe_links %>%
#   select(from = cpe_mechanism.x, to = cpe_mechanism.y)

edges <- data_cpe_links %>%
  select(from = id.x, to = id.y)

# Make sure patients are unique nodes
nodes <- data.frame(name = unique(c(edges$from, edges$to)))

# Build the igraph object
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

# Plot using ggraph
links_plot <- ggraph(g, layout = 'fr') +  # 'fr' = Fruchterman-Reingold layout
  geom_edge_link(alpha = 0.7) +
  geom_node_point(size = 2.5, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  ggtitle("Network of Linked Patients")

links_plot


