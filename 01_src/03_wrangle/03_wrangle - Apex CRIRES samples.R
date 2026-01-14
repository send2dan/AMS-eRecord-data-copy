# ## initialise
# source("./01_src/01_startup/01_initialise.R")
# 
# ##functions
# source("./01_src/functions.R")
# 
# ##import data
# source("./01_src/02_data_import/02_data_import - Apex CRIRES samples.R")
# 

# dates -------------------------------------------------------------------

# data_crires_bc |> 
#   glimpse()

crires_results_dates <- data_crires_bc |> 
  arrange(desc(receive_date)) |> 
  summarise(most_rececent_receive_date = min(receive_date, na.rm = TRUE),
            earliest_receive_date = max(receive_date, na.rm = TRUE))

#earliest critical-flagged result
format(ymd(crires_results_dates$earliest_receive_date), '%a %B %d %Y')

#most recent critical-flagged result
format(ymd(crires_results_dates$most_rececent_receive_date), '%a %B %d %Y')

# remove unnecessary data -------------------------------------------------

data_crires_bc_wrangled <- data_crires_bc |> 
  filter(!str_detect(patient_full_name, "DUMMY"), #remove dummy patient(s)
         !quantifier == "STOP") #remove stop code data

# rename some variables to assist with join later on

data_crires_bc_wrangled <- data_crires_bc_wrangled |> 
  rename(mrn = hospital_number,
         nhs_number = new_nhs_number)

# clean up antibiotic codes -----------

# data_crires_bc_wrangled |> 
#   glimpse()

#clean up antibiotic codes so they can be passed through the AMR package functions
data_crires_bc_wrangled <- data_crires_bc_wrangled |> 
  #remove CEFB antibiotic code because I don't know what it means? It's not used very often
  filter(!antibiotic_code == "CEFB") |> 
  #remove CPOD antibiotic code 
  filter(!antibiotic_code == "CPOD") |>
  #remove OCARB antibiotic code 
  filter(!antibiotic_code == "OCARB") |>
  #remove STP antibiotic code 
  filter(!antibiotic_code == "STP") |>
  #remove OCARB antibiotic code 
  filter(!antibiotic_code == "KCARB") |>
  #remove 23CARB antibiotic code
  filter(!antibiotic_code == "23CARB") |>
  #remove VCARB antibiotic code 
  filter(!antibiotic_code == "VCARB") |>
  #remove ICARB antibiotic code 
  filter(!antibiotic_code == "ICARB") |>
  #remove PIPER antibiotic code 
  filter(!antibiotic_code == "PIPER") |>
  #remove NOVO antibiotic code 
  filter(!antibiotic_code == "NOVO") |>
  #replace AP with AMP 
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^AP$", replacement = "AMX")) |> 
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^CFX$", replacement = "LEX")) |> 
  #replace CD with CLI
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^CD$", replacement = "CLI")) |> 
  #replace CO with COL
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^CO$", replacement = "COL")) |> 
  #replace ERT with ETP 
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^ERT$", replacement = "ETP")) |>
  #replace ISA wit ISV 
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^ISA$", replacement = "ISV")) |>
  #replace MVA with MEV 
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^MVA$", replacement = "MEV")) |>
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^TET$", replacement = "TCY")) |>
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^CTT$", replacement = "CZT")) |>
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^LIN$", replacement = "LNZ")) |>
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^MZ$", replacement = "MTR")) |>
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^TM$", replacement = "TMP")) |>
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^RP$", replacement = "RIF")) |>
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "TED", replacement = "TZD")) |>
  mutate(antibiotic_code = str_replace(antibiotic_code, pattern = "^FOT$", replacement = "FOS")) |>
  #remove NCARB
  filter(!antibiotic_code == "NCARB")

# data_crires_bc_wrangled |> 
#   glimpse()

#convert (cleaned) APEX antibiotic codes to as_ab antibiotic codes using as.ab() ------------
data_crires_bc_wrangled <- data_crires_bc_wrangled |> 
  mutate(as_ab = as.ab(antibiotic_code))

# data_crires_bc_wrangled |> 
#   glimpse()

(
  antibiotic_code_vs_as_ab <- data_crires_bc_wrangled |> 
    distinct(antibiotic_code, as_ab) |> 
    arrange(as_ab) |> 
    filter(!as_ab == antibiotic_code) 
)

# pivot wider to get a dataset where the antibiotic susceptibility test results are wide (i.e. tidy) ----------

# To pivot wider, we need a set of columns that uniquely identifies each observation
data_crires_bc_wrangled_distinct <- data_crires_bc_wrangled |> 
  distinct(mrn, specimen_no, organism_code, as_ab, interpreted_sensitivity_result)

# Before you run pivot_wider(), make sure each combination of the non-pivoted columns and your names_from column is unique. You can do this with:

nrow(data_crires_bc_wrangled_distinct) == nrow(distinct(data_crires_bc_wrangled_distinct, across(-interpreted_sensitivity_result)))

# If you have duplicates and need to collapse them, set values_fn to an appropriate function (e.g., first, max, paste, etc.). For example, if you just want the first value:

data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_distinct |> 
  pivot_wider(
    id_cols = NULL,
    id_expand = FALSE,
    names_from = as_ab, #antibiotic codes to be pivot_wider
    names_prefix = "",
    names_sep = "_",
    names_glue = NULL,
    names_sort = FALSE,
    names_vary = "fastest",
    names_expand = FALSE,
    names_repair = "check_unique",
    values_from = interpreted_sensitivity_result,
    values_fill = NULL,
    values_fn = list(interpreted_sensitivity_result = ~ first(.x)), #if you just want the first value
    unused_fn = NULL
  )

data_crires_bc_wrangled_wide |> 
  glimpse()

# add column back to data_clean_sens_wide from data_clean_sens using left_join()
data_crires_bc_wrangled_distinct2 <- data_crires_bc_wrangled |> 
  distinct(mrn, specimen_no, organism_code, .keep_all = TRUE) |> 
  select(-interpreted_sensitivity_result) |> 
  #remove interpreted_sensitivity_result because it has been used to pivot_wide() and doesn't make any sens any more
  select(-as_ab) #remove as_ab because it was used to pivot_wider, so is meaningless thereafter

data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  left_join(data_crires_bc_wrangled_distinct2)

# The is_sir_eligible() can check which columns are probably columns with R/SI test results. Using mutate() and across(), we can apply the transformation to the formal <sir> class:

# is_sir_eligible(data_clean_sens_wide)

# #which column(s) contain sir data? (S/i/R)
antibiotics_in_dataset <- colnames(data_crires_bc_wrangled_wide)[is_sir_eligible(data_crires_bc_wrangled_wide)] 

# #test to see how specimens look that have org with "D" susceptibility
AMR::as.sir(c("S", "I", "R", "A", "B", "C"))
AMR::as.sir(c("S", "I", "R", "D", "U", "C"))

data_crires_bc_wrangled_wide |> 
  glimpse()


# remove antibiotic_code --------------------------------------------------

data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  select(-antibiotic_code) |> 
  distinct()

# Change D and U to I... --------------------------------------------------

#replace "D" with "R" to work with as.sir and is_sir_eligible
data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  mutate(across(where(AMR::is_sir_eligible), 
                ~ str_replace_all(.x,
                                  pattern = "D", 
                                  replacement = "R")))

data_crires_bc_wrangled_wide |> 
  glimpse()

# D gets interpreted as I... this isn't helpful... so prev step replaced "D" with "R" to work with as.sir and is_sir_eligible
data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  mutate(across(where(is_sir_eligible), as.sir))

# wrangle organisms data --------------------------------------------------

# Apex organisms list -----------------------------------------------------

apex_organisms_list <- read_excel(here("02_data", "LIMS - All APEX codes.xlsx"), 
                                  sheet = "Organisms", col_types = c("text", 
                                                                     "text", "skip", "skip", "skip", 
                                                                     "skip", "skip", "skip", "skip", 
                                                                     "skip"))

apex_organisms_list <- apex_organisms_list |> 
  clean_names() |> 
  rename(organism_code = code,
         organism_code_verbose = description)

# APEX code list ----------------------------------------------------------

# left_join data_clean with apex_organisms_list (derived from LIMS - Apex codes.xlsx to ), using the 'organism_code' column as the key to add the organism_code_verbose columns
data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  left_join(apex_organisms_list)

data_crires_bc_wrangled_wide |> 
  count(organism_code_verbose)

# clean up organism_code_verbose to allow passing through as.mo()
data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |>
  mutate(original_organism_code = organism_code,
         original_organism_code_verbose = organism_code_verbose,
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Kliyveromyces\\sMarxianus$", replacement = "Kluyveromyces marxianus"), #AMR calls Kluyveromyces marxianus "UNKNOWN". Note spelling error in APEX ID.
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Staph cohnii\\ssub\\sureolyticum$", replacement = "Coagulase negative Staphylocococcus"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Strep\\sanginosus\\sgroup\\sorganism$", replacement = "Streptococcus anginosus"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Strep.dysgalactiae\\s\\(Group\\sC/G\\)$", replacement = "Streptococcus dysgalactiae"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Strep.agalact.*$", replacement = "Streptococcus agalactiae"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Coliform\\s\\(K.E.S\\sGroup\\)$", replacement = "Enterobacterales"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Strep\\sbovis/equinus\\scomplex$", replacement = "Streptococcus equinus"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Salmonella\\sparatyphi\\sA$", replacement = "Streptococcus anginosus"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Salmonella\\sparatyphi\\sA$", replacement = "Salmonella enterica enterica"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Paenibacillus\\samyloltyicus$", replacement = "Paenibacillus"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Lactococcus\\sgarviae$", replacement = "Lactococcus"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Kytococcus\\sscroeteri$", replacement = "Kytococcus schroeteri"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Coag\\snegative\\sStaphylococcus$", replacement = "CoNS"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Strep.agalact.*$", replacement = "Streptococcus agalactiae"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Strep.dysgalacti.*$", replacement = "Streptococcus dysgalactiae"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Streptococcus\\spyoge.*$", replacement = "Streptococcus pyogenes"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Strep\\sanginos*.$", replacement = "Streptococcus anginosus"),
         organism_code_verbose = str_replace(organism_code_verbose, pattern = "^Coliform\\s\\(K.E.S\\sGroup\\)$", replacement = "Enterobacterales"),
         organism_code_verbose = case_when(
           organism_code == "STEMAG" ~ "Stenotrophomonas maltophilia group",
           organism_code == "ENTCLC" ~ "Enterobacter cloacae group",
           TRUE ~ organism_code_verbose  # Keep existing values for all other cases
         )
  )

# # remove organism codes etc. that can't pass through as.mo()
# data_clean_sens <- data_clean_sens %>% 
#   #remove species that lead to as_mo_shortname == "(unknown species)"
#   filter(!str_detect(organism_code, "^AN$|^DIPHTH$|^GNAB$|^GPAC$|^GPB$|^GPC$|^GVB$|^MIXCOL$")) |> 
#   filter(!str_detect(organism_code_verbose, "^Anaerobe$|^Diphtheroids$|^Gram\\svariable\\sbacillus$|^Mixed\\scoliforms$|^Gram\\snegative\\sBacillus")) 

data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  mutate(as_mo = AMR::as.mo(organism_code_verbose,
                            Becker = TRUE,
                            Lancefield = TRUE,
                            allow_uncertain = TRUE),
         as_mo_genus = mo_genus(as_mo,
                                Becker = TRUE,
                                Lancefield = TRUE,
                                allow_uncertain = FALSE),
         as_mo_species = mo_species(as_mo,
                                    Becker = TRUE,
                                    Lancefield = TRUE,
                                    allow_uncertain = FALSE),
         as_mo_fullname = mo_fullname(as_mo,
                                      Becker = TRUE,
                                      Lancefield = TRUE,
                                      allow_uncertain = TRUE),
         as_mo_shortname = mo_shortname(as_mo,
                                        Becker = TRUE,
                                        Lancefield = TRUE,
                                        allow_uncertain = TRUE),
         as_mo_gramstain = mo_gramstain(as_mo,
                                        Becker = TRUE,
                                        Lancefield = TRUE,
                                        allow_uncertain = TRUE),
         as_mo_yeast = mo_is_yeast(as_mo))

print(AMR::mo_uncertainties(), n = 5)


# compare as_mo with organism_code ----------------------------------------

data_crires_bc_wrangled_wide |> 
  count(as_mo_shortname, organism_code, organism_code_verbose) |> 
  print(n = 100)


# make distinct -----------------------------------------------------------

data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  distinct()


# remove quantifier -------------------------------------------------------

data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  select(-quantifier)


# change patient name to initials -----------------------------------------

data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  mutate(patient_full_name = str_replace(patient_full_name, ",", ", ")) |> #add space after comma
  mutate(initials = get_initials(patient_full_name))

data_crires_bc_wrangled_wide |> 
  select(initials, patient_full_name)

# add age -----------------------------------------------------------------

data_crires_bc_wrangled_wide <- data_crires_bc_wrangled_wide |> 
  mutate(age = AMR::age(date_of_birth)) 

# create table ------------------------------------------------------------

# colnames(data_crires_bc_wrangled_wide)
# 
# data_crires_bc_wrangled_wide_col_names <- data_crires_bc_wrangled_wide |>
#   colnames()
# 
# data_crires_bc_wrangled_wide_col_names <- str_c(data_crires_bc_wrangled_wide_col_names, ",")
# 
# kable(data_crires_bc_wrangled_wide_col_names, format = "simple")

# data_crires_bc_wrangled_wide |> 
#   count(  reason_for_req_line1,           
#           reason_for_req_line2,           
#           reason_for_req_line3)

# You can prevent errors from missing columns in dplyr::select() by using the any_of() helper instead of unquoted column names or backticked names. any_of() will silently ignore any column names that are not present in your data frame. 
data_crires_bc_wrangled_wide_for_table <- data_crires_bc_wrangled_wide |> 
  rename(`receive date` = receive_date,                   
         `spec. type` = specimen_type_code,
         organism = organism_code_verbose,    
         `current/intended antibiotic therapy` = order_comment, 
         `reason for request` = reason_for_req_line1,           
         `anatomical site sampled` = reason_for_req_line2,           
         `indwelling device nature (if applicable)` = reason_for_req_line3,
         genus = as_mo_genus,
         `short name` = as_mo_shortname,                
         `Gram stain` = as_mo_gramstain 
  ) |> 
  select(any_of(c("location_code",   
                  #patient_full_name, 
                  "initials",
                  #sex,
                  "mrn",
                  #consultant,
                  #age,
                  #date_of_birth,
                  "specimen_no",  
                  "receive date",                   
                  "spec. type",    
                  # order_code,                     
                  # test_code, 
                  #organism_code,    
                  "organism",          
                  #original_organism_code,         
                  #original_organism_code_verbose, 
                  #nhs_number,                     
                  "current/intended antibiotic therapy",
                  "reason for request",
                  "anatomical site sampled",
                  "indwelling device nature (if applicable)",
                  "CLI", "DOX", "ERY", "FLC", "GEN", "LNZ", "MFX", "PEN", "RIF", "TMP", "SXT", "AMX", "CHL", "OPT", "TEC", "VAN", "AMK", "ATM", "CAZ", "CIP", "AMC", "CXM", "CZA", "ETP", "FOS", "MEM", "TAZ", "TEM", "TGC", "MTR", "DAP", "TCY", "CRO", "FDC", "IMR", "TOB", "DAL",
                  #as_mo,                          
                  "genus",
                  "short name",
                  "Gram stain"
                  #as_mo_species,                  
                  #as_mo_fullname,                 
                  #as_mo_yeast
  ))) |> 
  distinct()

data_crires_bc_wrangled_wide_for_table

# quick analysis of final dataset -----------------------------------------

data_crires_bc_wrangled_wide |> 
  glimpse()

# skimr::skim(data_crires_bc_wrangled_wide)
# 
# data_crires_bc_wrangled_wide |> 
#   count(specimen_type_code, test_code, order_code)
# 
# data_crires_bc_wrangled_wide |> 
#   count(quantifier)


# histogram age ----------
data_crires_bc_wrangled_wide |>
  distinct(mrn, .keep_all = TRUE) |>
  filter(!is.na(age)) |>
  ggplot(aes(x = age)) +
  geom_histogram(bins = 10) +
  theme_minimal() +
  labs(
    title = "Age of patients with critical-flagged blood cultures",
    subtitle = glue::glue("Data for the 14 days prior to {date}",
                          date = format(ymd(crires_results_dates$most_rececent_receive_date), "%A %d %B %Y")
    ),
    x = "Age",
    y = "n"
  )

# location_code ----------
data_crires_bc_wrangled_wide %>%
  distinct(mrn, specimen_no, .keep_all = TRUE) |>
  filter(!is.na(location_code)) |>
  count(location_code) |> 
  ggplot(aes(fct_reorder(location_code, -n), n)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Specimen collection location for patients with critical-flagged blood cultures",
    subtitle = glue::glue("Data for the 14 days prior to {date}",
                          date = format(ymd(crires_results_dates$most_rececent_receive_date), "%A %d %B %Y")
    ),
    x = NULL,
    y = "n"
  ) +
  coord_flip()

