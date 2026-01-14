
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
# 
# source("./01_src/03_wrangle/03_wrangle - antimicrobial_costings_part_1.R")

data_full_costings_amr_simple_filtered

data_full_costings_amr_simple_filtered |> 
  filter(ab_name_costings_amr == "Temocillin")

# get antimicrobial prescription data -------------------------------------

data_antimicrobial_pool_iv_im_po_not_prophy |> 
  glimpse()

data_antimicrobial_pool_iv_im_po_not_prophy |> 
  filter(order_name_c == "Temocillin") 
  

#use AMR package to label antimicrobials ---------

## avoid antivirals as AMR::ab_name() and AMR::as.ab() don't play nicely with these
# AMR::antivirals$name

# Precompute your mapping
# If order_name_c only has a limited set of unique values, you can precompute the mapping from order_name_c to the new name once, then join that mapping.

# Get unique order names
unique_names <- unique(data_antimicrobial_pool_iv_im_po_not_prophy$order_name_c)

# Identify which are antivirals
is_av <- unique_names %in% AMR::antivirals$name

# Map names in a vectorized way
order_name_c_amr <- character(length(unique_names))
if (any(is_av)) {
  order_name_c_amr[is_av] <- AMR::av_name(unique_names[is_av])
}
if (any(!is_av)) {
  order_name_c_amr[!is_av] <- AMR::ab_name(unique_names[!is_av])
}

mapping <- tibble(
  order_name_c = unique_names,
  order_name_c_amr = order_name_c_amr
)

# Join mapping back to main data
data_antimicrobial_pool_iv_im_po_not_prophy <- data_antimicrobial_pool_iv_im_po_not_prophy %>%
  left_join(mapping, by = "order_name_c")

# The rest stays the same
data_antimicrobial_pool_iv_im_po_not_prophy |> 
  count(order_name_c, order_name_c_amr) |> 
  arrange(desc(n)) |> 
  print(n = 100)

# Check mismatches
mismatches_order_name_c_amr <- data_antimicrobial_pool_iv_im_po_not_prophy %>%
  distinct(order_name_c, order_name_c_amr) |> 
  filter(
    is.na(order_name_c) |
      is.na(order_name_c_amr) |
      !str_detect(
        tolower(trimws(order_name_c)),
        fixed(tolower(trimws(order_name_c_amr)), ignore_case = TRUE)
      )
  )

unique(mismatches_order_name_c_amr)

# check order_name_c vs order_name_c_amr ----------------------------------
# NB order_name_c_amr created using AMR::as.ab and AMR::as.av in other script

#check
#now works for antivirals! except Elbasvir grazoprevir
data_antimicrobial_pool_iv_im_po_not_prophy |> 
  count(order_name_c, order_name_c_amr) |> 
  filter(is.na(order_name_c_amr)) |> 
  arrange(n) 

# Check tibbles: data_antimicrobial_pool_iv_im_po_not_prophy-----------------------------------------------------------

# focus on prescriptions made on particular days (scope)?  ------------------------------------------------------
# A: no, keep all data, even though it causes overlapping prescriptions

#which dates are included in data?
data_antimicrobial_pool_iv_im_po_not_prophy |>
  reframe(max_date = max(start_date_time, na.rm = TRUE),
          min_date = min(start_date_time, na.rm = TRUE))

#frequency of distinct prescriptions per date
data_antimicrobial_pool_iv_im_po_not_prophy |>
  distinct(order_id, .keep_all = TRUE) |> 
  group_by(start_date_time) |> 
  count()

#which prescriptions don't have a start_date?
data_antimicrobial_pool_iv_im_po_not_prophy |>
  distinct(order_id, .keep_all = TRUE) |> 
  filter(is.na(start_date_time)) |> 
  count(order_name_c_amr)

#are these prescriptions ONCE/STAT doses?
data_antimicrobial_pool_iv_im_po_not_prophy |>
  distinct(order_id, .keep_all = TRUE) |> 
  filter(is.na(start_date_time)) |> 
  count(frequency, freq_per_day)
# A: Yes

#can the stop date be used for these prescriptions?
data_antimicrobial_pool_iv_im_po_not_prophy |>
  distinct(order_id, .keep_all = TRUE) |> 
  filter(is.na(start_date_time)) |> 
  count(frequency, freq_per_day, stop_date_time, pharmacy_order_sentence)
# A: Yes


# replace start_date_time and stop_date_time where is.na(start_date_time) --------

data_antimicrobial_pool_iv_im_po_not_prophy <- data_antimicrobial_pool_iv_im_po_not_prophy |> 
  mutate(
    start_date_time = 
      if_else(
        condition = is.na(start_date_time),
        true = stop_date_time,
        false = start_date_time
      )
  ) 
  
#NB: some prescriptions don't have either a start or stop date...  
data_antimicrobial_pool_iv_im_po_not_prophy |> 
  distinct(order_id, .keep_all = TRUE) |> 
  filter(is.na(start_date_time)) |> 
  count(mrn, order_id, pharmacy_order_sentence, frequency, order_name_c_amr, start_date_time, stop_date_time
        ) |> 
  glimpse()
# A: Not much I can do about these... deciding to keep these (rather than removing them)
# Looking at eRecord, prescriptions without a start_date_time and stop_date_time do get given

#i.e. the last Tue of the date of the script being run

reports_date <- lubridate::floor_date(
  lubridate::today(),
  unit = "week",
  week_start = 2
) |> 
  format("%d-%m-%Y") 

reports_date_interval_7_days <- lubridate::interval(
  start = lubridate::dmy(reports_date) - lubridate::days(7),
  end = lubridate::dmy(reports_date) 
)

#check
reports_date_interval_7_days

#keep prescriptions that fall within reports_date_interval_7_days (i.e. period from latest Tuesday till [latest Tuesday - 7 days] ) 
data_antimicrobial_pool_iv_im_po_not_prophy_scope <- data_antimicrobial_pool_iv_im_po_not_prophy |>
  filter(start_date_time %within% reports_date_interval_7_days | is.na(start_date_time)) 
#also keep orders with no start_date_time/stop_date_time

# 7 how many antimicrobials per order number? --------------------------------

data_antimicrobial_pool_iv_im_po_not_prophy_scope |> 
  distinct(mrn, order_id, order_name_c_amr) |> 
  group_by(mrn) |> 
  count(order_id, order_name_c_amr) |> 
  arrange(mrn, desc(n))
# A: just one, so that's good, because each order_id relates to one antimicrobial


# combine datasets ---------------------------------------------------------

data_full_costings_amr_simple_filtered |> 
  glimpse()

data_antimicrobial_pool_iv_im_po_not_prophy_scope |> 
  glimpse()

# intersect(colnames(data_full_costings_amr_simple_filtered), colnames(data_antimicrobial_pool_iv_im_po_not_prophy_scope))


# Combine datasets on the mapped columns (join on drug name & IV/PO)
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed <- data_antimicrobial_pool_iv_im_po_not_prophy_scope %>%
  left_join(
    data_full_costings_amr_simple_filtered,
    #relationship = "many-to-many",
    by = c(
      "order_name_c_amr" = "ab_name_costings_amr",
      #"drug_form" = "drug_form_costings", #joining on drug form breaks things, and is probably not needed
      "route_of_administration_recoded" = "iv_po_costings"
    )
  )

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed |> 
  glimpse()



# where frequency == "STAT" make freq_per_day == 1 --------------------

ind <- which(data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed$frequency == "STAT")

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed$freq_per_day[ind] <- 1

# where frequency == "ONCE" make freq_per_day == 1 --------------------

ind <- which(data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed$frequency == "ONCE")

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed$freq_per_day[ind] <- 1

# have all medications got costings? -------------
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed |> 
  count(order_name_c_amr, route_of_administration_recoded, drug_form, cost_per_day) |> 
  arrange(order_name_c_amr, route_of_administration_recoded, drug_form) |> 
  print(n = 1000)

# which drugs have NO costings? ----------------

# missing costings  
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_missing_costs <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed |> 
  filter(is.na(cost_per_day)) |> 
  distinct(order_name_c_amr, route_of_administration_recoded, drug_form)

print(data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_missing_costs)
# looks OK, e.g. SDD, some rare antivirals

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_missing_costs_2 <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_missing_costs |> 
  filter(!is.na(route_of_administration_recoded)) |> 
  filter(!is.na(order_name_c_amr))

# Create a single statement RE: missing drugs with proper grammar -------
drug_route_combinations <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_missing_costs_2 %>%
  mutate(
    drug_name = ifelse(is.na(order_name_c_amr), "Unknown drug", order_name_c_amr),
    route = ifelse(is.na(route_of_administration_recoded), "unknown", route_of_administration_recoded),
    drug_route = paste0(drug_name, " by ", route, " route")
  ) %>%
  pull(drug_route)

# Format with proper grammar
if(length(drug_route_combinations) > 1) {
  formatted_list <- paste0(paste(drug_route_combinations[-length(drug_route_combinations)], collapse = ", "), 
                           " and ", 
                           drug_route_combinations[length(drug_route_combinations)])
} else {
  formatted_list <- drug_route_combinations
}

single_statement <- paste0("There are no drug costings available for ", formatted_list)

cat(single_statement)

# look at CAZ-AVI ---------------------------------------------------------

#how is CAZ-AVI prescribed?
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed |> 
  filter(order_name_c_amr == "Ceftazidime/avibactam") |> 
  distinct(order_name_c_amr, route_of_administration_recoded, drug_form, frequency, freq_per_day, cost_per_day)

#which patients are Rx CAZ-AVI? (show every Rx)
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed |> 
  filter(order_name_c_amr == "Ceftazidime/avibactam") |> 
  #select
  select(mrn, order_id, order_name_c_amr, route_of_administration_recoded, drug_form, frequency, freq_per_day, cost_per_day)

#which patients are Rx CAZ-AVI? (show unique Rx)
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed |> 
  filter(order_name_c_amr == "Ceftazidime/avibactam") |> 
  #distinct incl order_id and start_date_time
  distinct(mrn, order_id, start_date_time, order_name_c_amr, route_of_administration_recoded, drug_form, frequency, freq_per_day, cost_per_day)

#which patients are Rx CAZ-AVI? (show unique Rx)
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed |> 
  filter(order_name_c_amr == "Ceftazidime/avibactam") |> 
  #distinct incl order_id only
  distinct(mrn, order_id, order_name_c_amr, route_of_administration_recoded, drug_form, frequency, freq_per_day, cost_per_day)

#which patients are Rx CAZ-AVI? (show unique Rx)
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed |> 
  filter(order_name_c_amr == "Ceftazidime/avibactam") |> 
  #distinct excl order_id and start_date_time
  distinct(mrn,  order_name_c_amr, route_of_administration_recoded, drug_form, frequency, freq_per_day, cost_per_day)

# de-duplicate the data  ------------

# RISK: this will lose multiple ONCE/STAT doses of the same medication... But the data on ONCE/STATd doses isn't that reliable... e.g. mrn 6159116A had MANY ONCE/STAT doses with unique order_id BUT actually was only given one dose of CAZ-AVI stat between 19-5 and 2-10...
# # A tibble: 83 × 8
# mrn      order_id   order_name_c_amr      route_of_administrati…¹ drug_form frequency freq_per_day
# <chr>    <chr>      <chr>                 <chr>                   <chr>     <chr>            <dbl>
#   1 6159116A 6863465797 Ceftazidime/avibactam IV                      Injection ONCE                 1
# 3 6159116A 6863473753 Ceftazidime/avibactam IV                      Injection STAT                 1
# 4 6159116A 6863473759 Ceftazidime/avibactam IV                      Injection STAT                 1
# 5 6159116A 6863473765 Ceftazidime/avibactam IV                      Injection STAT                 1
# 6 6159116A 6863473771 Ceftazidime/avibactam IV                      Injection STAT                 1
# 7 6159116A 6864482095 Ceftazidime/avibactam IV                      Injection STAT                 1
# 8 6159116A 6865432477 Ceftazidime/avibactam IV                      Injection STAT                 1
# 9 6159116A 6867595957 Ceftazidime/avibactam IV                      Injection STAT                 1
# 10 6159116A 6868476881 Ceftazidime/avibactam IV                      Injection STAT                 1

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_distinct <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed |> 
  distinct(mrn,  order_name_c_amr, route_of_administration_recoded, drug_form, frequency, freq_per_day, .keep_all = TRUE)

# calculate total_cost   ---------------------------------------------------

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_distinct |> 
  #keep only unique order_id
  distinct(order_id, .keep_all = TRUE) |> 
  mutate(cost_per_dose = as.numeric(cost_per_dose),
         cost_per_day = as.numeric(cost_per_day)) |> 
  group_by(order_id, order_name_c_amr, route_of_administration_recoded) |> 
  mutate(cost_per_day_average = mean(cost_per_day, na.rm = TRUE),
         cost_per_dose_average = mean(cost_per_dose, na.rm = TRUE)) |>
  mutate(total_cost = cost_per_dose_average * freq_per_day) |> 
  ungroup()

#check excl frequency == ONCE/STAT
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  filter(!str_detect(frequency, "ONCE|STAT")) |> 
  select(order_id, order_name_c_amr, freq_per_day, cost_per_dose, cost_per_day_average, cost_per_dose_average, total_cost)


# are any order_name_c_amr missing a total_cost? --------------------------

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  filter(is.na(total_cost)) |> 
  count(order_name_c_amr, route_of_administration_recoded)
#nothing major missing?


# viz ---------------------------------------------------------------------

# Load required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

#which are the most commonly prescribed drugs (incl prophylactic) in the given time period?
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  distinct(order_id, .keep_all = TRUE) |> 
  count(order_name_c_amr) |> 
  arrange(desc(n))

#which are the most commonly prescribed drugs (incl prophylactic) in the given time period, stratified by route of admin?
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  distinct(order_id, .keep_all = TRUE) |> 
  count(order_name_c_amr, route_of_administration_recoded) |> 
  arrange(desc(n))

# Which patients are Rx CAZ/AVI?
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  filter(order_name_c_amr == "Ceftazidime/avibactam") |> 
  distinct(order_id, .keep_all = TRUE) 

# how frequent are Rx for CAZ/AVI?
data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  filter(order_name_c_amr == "Ceftazidime/avibactam") |> 
  distinct(order_id, .keep_all = TRUE) |> 
  count(mrn, start_date_time, frequency)
