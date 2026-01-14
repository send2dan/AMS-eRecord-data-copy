# Calc no. pt on ward -----------------------------------------------

# No. patients on ward: might not be 100% accurate as it would only include patients on ward who have been admitted >24h ago + been prescribed any medication in the past 7 days; i.e. patients newly admitted to hospital and/or not prescribed any medication in the past 7 days would be excluded

pt_per_ward <- data_cohort_admission_wrangled |>
  distinct(mrn, current_ward) |>
  group_by(current_ward) |>
  count(name = "n_pt_on_ward")

pt_per_ward


# check temo data ---------------------------------------------------------

data_antimicrobial_pool |> 
  filter(order_name_c == "Temocillin") |> 
  glimpse()

# Keep only IV/IM/PO drug form or route of admin --------------------------

# object to search by on drug_form
drug_form_iv_im_po <- data_antimicrobial_pool %>%
  count(drug_form) 

drug_form_to_remove <- c("Cream", "Drops", "Spray", "Ointment", NA) |>
  as_tibble() |>
  rename(drug_form = value)

drug_form_iv_im_po <- drug_form_iv_im_po %>%
  anti_join(drug_form_to_remove, by = c("drug_form" = "drug_form")) 

# object to search by on route_of_administration
route_iv_im_po <- data_antimicrobial_pool %>%
  count(route_of_administration) 

route_to_remove <- c("both ears", "both eyes", "both nostrils", "inhaled", "left ear", "left eye", "nebulised", "right ear", "right eye", "topical", "vaginal", NA) |>
  as_tibble() |>
  rename(route_of_administration = value)

route_iv_im_po <- route_iv_im_po %>%
  anti_join(route_to_remove, by = c("route_of_administration" = "route_of_administration")) 

# only keep rows where BOTH route_of_administration == keep route_iv_im_po
# AND drug_form == drug_form_iv_im_po

route_iv_im_po_search <- route_iv_im_po |>
  pull(route_of_administration)

drug_form_iv_im_po_search <- drug_form_iv_im_po |>
  pull(drug_form)



# Join the route and drug form datasets --------

data_antimicrobial_pool_iv_im_po <- data_antimicrobial_pool |>
  filter(route_of_administration %in% route_iv_im_po_search & drug_form %in% drug_form_iv_im_po_search)

data_antimicrobial_pool_iv_im_po |> 
  filter(order_name_c == "Temocillin") |> 
  glimpse()

# make data_antimicrobial_pool_iv_im_po distinct -----------------------------------------------------------

data_antimicrobial_pool_iv_im_po <- data_antimicrobial_pool_iv_im_po |> 
  distinct()

data_antimicrobial_pool_iv_im_po |> 
  filter(order_name_c == "Temocillin") |> 
  glimpse()


# ensure route_of_administration in prescriptions data is coded similarly to iv_po in costings data ----------

data_antimicrobial_pool_iv_im_po |> 
  count(route_of_administration)

data_antimicrobial_pool_iv_im_po <- data_antimicrobial_pool_iv_im_po |> 
  mutate(
    route_of_administration_recoded = case_when(
      route_of_administration %in% c("intravenous", "intravenous infusion") ~ "IV",
      route_of_administration %in% c("oral", "NG", "NJ", "PEG", "enteral") ~ "PO",
      drug_form %in% c("Injection") ~ "IV", # drug form can be a clue to route of administration
      drug_form %in% c("Tablet") ~ "PO", # drug form can be a clue to route of administration
      is.na(route_of_administration) | route_of_administration == "" ~ "NA",
      TRUE ~ "NA" # catch-all for any other values
    )
  )

data_antimicrobial_pool_iv_im_po |> 
  filter(order_name_c == "Temocillin") |> 
  glimpse()


#check
data_antimicrobial_pool_iv_im_po |> 
  count(route_of_administration, route_of_administration_recoded)


# recode the frequency categories in "frequency" to numeric frequencies per day (to match with cost_per_day) -------------------------

# data_antimicrobial_pool_iv_im_po |> 
#   glimpse()

data_antimicrobial_pool_iv_im_po |>
  count(frequency)

data_medications_antimicrobials |>
  count(frequency)

data_antimicrobial_pool_iv_im_po <- data_antimicrobial_pool_iv_im_po %>%
  mutate(
    freq_per_day = case_when(
      # Common daily/once patterns
      frequency %in% c("DAILY", "DAILY after breakfast", "DAILY at 10am", "DAILY at 15:00", "DAILY before breakfast", "DAILY except Monday", "DAILY except Saturday", "DAILY except Sunday", "DAILY except Tuesday", "DAILY except Wednesday", "ONCE", "ONCE  A+E", "ONCE UNSCHEDULED", "ONCE a day", "ONCE a day (morning)") ~ 1,
      # Multiple times per day
      frequency %in% c("TWICE a day", "TWICE A DAY at 0600 and 1800", "TWICE a day after meals", "TWICE a day at 08:00 and 20:00", "TWICE a day at 10.00am and 10.00pm", "TWICE a day at 11am and 11pm", "TWICE a day at 12 noon and 12 midnight", "TWICE a day at 12noon and  6pm", "TWICE a day at 8.00am and 10.00pm", "TWICE a day at 8.00am and 12 noon", "TWICE a day before meals", "TWICE a day with meals", "TWICE a day, morning and afternoon", "TWICE a day, morning and evening") ~ 2,
      frequency %in% c("THREE times a day", "THREE times a day after meals", "THREE times a day at 0800, 1200 and 1800", "THREE times a day at 0800, 1400 and 2200", "THREE times a day before meals", "THREE times a day with main meals") ~ 3,
      frequency %in% c("FOUR times a day", "FOUR times a day (AFTER meals and NOCTE)", "FOUR times a day BEFORE meals and NOCTE", "FOUR times a day after meals", "FOUR times a day at 0600,1200,1800,2400", "FOUR times a day before meals") ~ 4,
      frequency %in% c("FIVE times a day") ~ 5,
      frequency %in% c("SIX times a day") ~ 6,
      frequency %in% c("EIGHT times a day while awake") ~ 8,
      # Alternate intervals
      frequency %in% c("ALTERNATE days", "ALTERNATE DAYS at 10am") ~ 0.5,
      frequency == "ALTERNATE weeks" ~ 0.5/7,
      frequency == "Fortnightly" ~ 1/14,
      frequency == "once a WEEK on the same day each week" ~ 1/7,
      frequency == "once a YEAR" ~ 1/365,
      frequency == "ONCE a DAY on Monday to Friday" ~ 5/7,
      frequency == "ONCE a DAY on Saturday and Sunday" ~ 2/7,
      frequency == "ONCE a day Monday, Wednesday and Friday" ~ 3/7,
      frequency == "ONCE a day Tuesday, Thursday & Saturday" ~ 3/7,
      frequency == "ONCE a day Tuesday, Thursday, Sat & Sun" ~ 4/7,
      frequency == "Three times per week, Mon,Wed & Friday" ~ 3/7,
      frequency == "TWICE a day Monday, Wednesday and Friday" ~ 3/7,
      frequency == "TWICE a day on Monday and Tuesday" ~ 2/7,
      frequency == "TWICE a day on a Saturday and Sunday" ~ 2/7,
      frequency %in% c("TWICE per week Tuesday & Friday at 22:00", "TWICE per week, Friday & Monday", "TWICE per week, Mondays & Thursdays", "TWICE per week, Saturday & Tuesday", "TWICE per week, Thursday & Sunday", "TWICE per week, Tuesday & Friday", "TWICE per week, Tuesday & Thursday", "TWICE per week, Wednesday & Saturday") ~ 2/7,
      # Monthly, quarterly, yearly, etc.
      frequency == "ONCE a month" ~ 1/30,
      frequency == "every FOUR weeks" ~ 1/28,
      frequency == "every THREE months" ~ 1/90,
      frequency == "every 28 days" ~ 1/28,
      frequency == "every EIGHT weeks" ~ 1/56,
      frequency == "every THREE weeks" ~ 1/21,
      frequency == "every WEEK" ~ 1/7,
      frequency == "every WEEK on a FRIDAY" ~ 1/7,
      frequency == "every WEEK on a MONDAY" ~ 1/7,
      frequency == "every WEEK on a SATURDAY" ~ 1/7,
      frequency == "every WEEK on a SUNDAY" ~ 1/7,
      frequency == "every WEEK on a THURSDAY" ~ 1/7,
      frequency == "every WEEK on a TUESDAY" ~ 1/7,
      frequency == "every WEEK on a WEDNESDAY" ~ 1/7,
      frequency == "every SEVEN days" ~ 1/7,
      frequency == "every 10 days" ~ 0.1,
      # Hourly, etc
      frequency == "every HOUR" ~ 24,
      frequency == "every TWO hours" ~ 12,
      frequency == "every THREE hours" ~ 8,
      frequency == "every FOUR hours" ~ 6,
      frequency == "every SIX hours" ~ 4,
      frequency == "every EIGHT hours" ~ 3,
      frequency == "every TEN hours" ~ 2.4,
      frequency == "every TWELVE hours" ~ 2,
      frequency == "every 24 hours" ~ 1,
      frequency == "every 36 hours" ~ 1/1.5,
      frequency == "every 48 hours" ~ 0.5,
      frequency == "every 72 hours" ~ 1/3,
      # Meal-based
      frequency %in% c("AFTER breakfast", "AFTER lunch", "AFTER evening meal", "AFTER main meals", "BEFORE breakfast", "BEFORE lunch", "BEFORE evening meal", "BEFORE main meals") ~ 1,
      frequency %in% c("AFTER breakfast and evening meal", "BEFORE breakfast and lunch") ~ 2,
      frequency == "AFTER breakfast and lunch" ~ 2,
      frequency == "every MORNING" ~ 1,
      frequency == "every MORNING and NIGHT" ~ 2,
      frequency == "With Snacks" ~ 1,
      frequency == "NOCTE" ~ 1,
      frequency == "NOON" ~ 1,
      frequency == "at NIGHT" ~ 1,
      frequency == "in the EVENING" ~ 1,
      frequency == "in the EVENING at 8pm" ~ 1,
      frequency == "in the EVENING on Tues, Thur, Sat + Sun" ~ 4/7,
      frequency == "in the Evening on Mon, Wed and Friday" ~ 3/7,
      # Complex meal patterns
      frequency == "ONCE before meals" ~ 3,
      frequency == "ONCE after meals" ~ 3,
      frequency == "THREE times a day with main meals" ~ 3,
      frequency == "FOUR times a day after meals" ~ 4,
      frequency == "FOUR times a day before meals" ~ 4,
      frequency == "TWICE a day before meals" ~ 2,
      frequency == "TWICE a day after meals" ~ 2,
      frequency == "THREE times per week, Mon,Wed & Friday" ~ 0.5,
      # Continuous, symptom relief, as required, or indicated
      frequency %in% c("Continuous", "As Indicated", "As required", "Symptom Relief") ~ NA_real_,
      # Yearly
      frequency == "once a YEAR" ~ 1/365,
      # Unscheduled/unknown
      is.na(frequency) ~ NA, # i.e. can we assume where is.na(frequency) it's a one-off stat dose? A: Yes
      TRUE ~ NA # i.e. can we assume where is.na(frequency) it's a one-off stat dose?
    )
  )

data_antimicrobial_pool_iv_im_po |> 
  count(freq_per_day)

data_antimicrobial_pool_iv_im_po |> 
  filter(order_name_c == "Temocillin") |> 
  glimpse()


#check to see if where is.na(freq_per_day), is the total duration just one day?
data_antimicrobial_pool_iv_im_po |> 
  filter(is.na(freq_per_day)) |> 
  reframe(total_duration = interval(start_date_time, stop_date_time)) |> 
  count(total_duration)
#A: Yes, all stop dates == start dates

# where freq_per_day == NA and frequency == NA, change frequency to STAT -----------------------------

data_antimicrobial_pool_iv_im_po <- data_antimicrobial_pool_iv_im_po %>%
  mutate(frequency = if_else(
    condition = is.na(freq_per_day) & is.na(frequency),
    true = "STAT",
    false = frequency)
  )

data_antimicrobial_pool_iv_im_po |> 
  filter(order_name_c == "Temocillin") |> 
  glimpse()


#check 
data_antimicrobial_pool_iv_im_po |> 
  count(freq_per_day, frequency) |> 
  print(n = 100)
#A: That worked: 
# freq_per_day frequency                                  n
# <dbl> <chr>                                  <int>
# 18       NA     STAT                                     963

# Q: Can we assume where is.na(frequency) it's a one-off stat dose? 
# A: Yes, we can as total_duration where is.na(frequency) == start and stop dates are almost all on the same day. If not on same day, then stop date == NA

#check where is.na(freq_per_day)
data_antimicrobial_pool_iv_im_po |> 
  filter(is.na(freq_per_day)) |> 
  distinct(order_name_c, pharmacy_order_sentence) 
#these prescriptions don't include a frequency THAT MATCHES THE OPTIONS ABOVE... so can it be safely assumed to be only a single dose?

# data_antimicrobial_pool_iv_im_po |> 
#   filter(is.na(freq_per_day)) |> 
#   distinct(order_name_c, pharmacy_order_sentence, frequency, freq_per_day) |> 
#   slice_sample(n = 10) |> 
#   flextable()

# data_antimicrobial_pool_iv_im_po |> 
#   filter(is.na(freq_per_day)) |> 
#   count(order_name_c, frequency, freq_per_day)

# Remove prophylactic abx/antivirals -------

indication_analysis <- data_antimicrobial_pool_iv_im_po |>
  count(indication) |>
  arrange(desc(n))

indication_prophylaxis <- indication_analysis |>
  filter(str_detect(indication, regex(ignore_case = TRUE, "Prophy.*|\\sProphy.*")))

# remove indications mentioning prophylaxis --------
data_antimicrobial_pool_iv_im_po_for_anti_join <- data_antimicrobial_pool_iv_im_po |>
  filter(str_detect(indication, regex(ignore_case = TRUE, "Prophy.*|\\sProphy.*"))) 

data_antimicrobial_pool_iv_im_po_not_prophy <- data_antimicrobial_pool_iv_im_po |> 
  anti_join(data_antimicrobial_pool_iv_im_po_for_anti_join)

#check Temo still present (this was being accidentally removed when filtering using "|>
# filter(str_detect(indication, regex(ignore_case = TRUE, "Prophy.*|\\sProphy.*")))" because where is.na(indication) was being removed 
# data_antimicrobial_pool_iv_im_po |> 
#   filter(order_name_c == "Temocillin") |> 
#   glimpse()

data_antimicrobial_pool_iv_im_po_not_prophy |> 
  filter(order_name_c == "Temocillin") |> 
  glimpse()


# remove PO nystatin / colistin / tobramycin (SDD) -------------------------------------

## which routes of admin are there?
# data_antimicrobial_pool_iv_im_po_not_prophy |> 
#   count(route_of_administration)
# 
## which routes of admin are there for Colistin|Nystatin|Tobramycin
# data_antimicrobial_pool_iv_im_po_not_prophy |>  
#   count(order_name_c, route_of_administration) |> 
#   filter(str_detect(order_name_c, regex("Colistin|Nystatin|Tobramycin", ignore_case = TRUE))) |> 
#   arrange(desc(n)) |> 
#   print(n = 100)

#create object to anti_join against
data_antimicrobial_pool_sdd_po <-  data_antimicrobial_pool_iv_im_po_not_prophy |> 
  filter(
    str_detect(route_of_administration, regex("oral|enteral|PEJ|NG|NJ", ignore_case = TRUE)) & str_detect(order_name_c, regex("Colistin|Nystatin|Tobramycin", ignore_case = TRUE))
  ) 

#perform antijoin to remove SDD from data_antimicrobial_pool_iv_im_po_not_prophy
data_antimicrobial_pool_iv_im_po_not_prophy <- data_antimicrobial_pool_iv_im_po_not_prophy |> 
  anti_join(data_antimicrobial_pool_sdd_po) 

data_antimicrobial_pool_iv_im_po_not_prophy |> 
  filter(order_name_c == "Temocillin") |> 
  glimpse()

# how many patients are included in data_antimicrobial_pool_iv_im_po_not_prophy?

data_antimicrobial_pool_iv_im_po_not_prophy_mrn <- data_antimicrobial_pool_iv_im_po_not_prophy |> 
  pull(mrn) |> 
  unique() |> 
  length ()

pt_on_rx_per_ward <- data_antimicrobial_pool_iv_im_po_not_prophy |>
  distinct(mrn, current_ward) |>
  group_by(current_ward) |>
  count(name = "n_pt_on_rx")

pt_on_rx_per_ward

# check data_antimicrobial_pool_iv_im_po_not_prophy
data_antimicrobial_pool_iv_im_po_not_prophy |>
  select(order_name_c, route_of_administration, drug_form) |>
  distinct() |>
  arrange(order_name_c) |>
  print(n = 200)

# Join datasets to calc % patients on Rx ----------------------------------

perc_pt_on_rx_per_ward <- pt_per_ward |>
  left_join(pt_on_rx_per_ward, by = "current_ward") |>
  ungroup() |>
  mutate(
    perc_pt_on_rx_per_ward = ifelse(is.na(n_pt_on_rx), 0, n_pt_on_rx) / n_pt_on_ward * 100,
    median_perc_pt_on_rx_per_ward = median(perc_pt_on_rx_per_ward, na.rm = TRUE)
  )


# Create table to share ---------------------------------------------------

perc_pt_on_rx_per_ward |>
  glimpse()

# Restrict wards to those where more than the median percentage of patients are Rx antimicrobials  --------
# and >4 patients Rx IV/IM/PO antimicrobials

median_perc_patients_rx_per_ward <- perc_pt_on_rx_per_ward |>
  distinct(median_perc_pt_on_rx_per_ward) |>
  pull()

perc_pt_on_rx_per_ward_restricted <- perc_pt_on_rx_per_ward |>
  filter(perc_pt_on_rx_per_ward > median_perc_patients_rx_per_ward & n_pt_on_rx > 4)

# Calc perc pt with antimicrobial rx per ward -----------------------------
## excl prophylactic antimicrobials
## only including IV/IM/PO antimicrobials
## only incl wards with more than the median percentage of patients Rx IV/IM/PO antimicrobials and >4 patients Rx IV/IM/PO antimicrobials
# 
# perc_pt_on_rx_per_ward_restricted |>
#   filter(!is.na(perc_pt_on_rx_per_ward)) |>
#   ggplot() +
#   geom_col(aes(fct_reorder(current_ward, perc_pt_on_rx_per_ward, .na_rm = FALSE), perc_pt_on_rx_per_ward)) +
#   geom_hline(
#     yintercept = unique(perc_pt_on_rx_per_ward$median_perc_pt_on_rx_per_ward),
#     color = "red",
#     linetype = 2
#   ) +
#   coord_flip() +
#   # theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
#   # theme(aspect.ratio = 0.5) + # Adjust the aspect ratio as needed to make figure taller
#   # theme(axis.text.y = element_text(size = 6)) +  # Reduce X-axis text size to 8
#   labs(
#     x = "Ward",
#     y = "Patients prescribed antimicrobials (%)\nIncluding median percentage patients prescribed antimicrobials across all wards (red dotted line)",
#     title = "Percentage of Patients prescribed Antimicrobials per Ward",
#     subtitle = glue::glue('Data for inpatients prescribed IV/IM/PO antimicrobials, in hospital, \nin the 7 days prior to {date},\nexcluding antimicrobial prescriptions with indication mentioning "Prophylaxis"',
#       date = format(ymd(import_date_report), "%A %d %B %Y")
#     )
#   )
