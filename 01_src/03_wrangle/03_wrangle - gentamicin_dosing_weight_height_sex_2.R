# Requirements ------------------------------------------------------------

### REQUIRES ###
# source("./01_src/03_wrangle/03_wrangle - data_medications_antimicrobials.R")

data_medications_antimicrobials |> 
  glimpse()

# Data filtering ----------------------------------------------------------
data_antimicrobial_pool_gentamicin <- data_medications_antimicrobials |>
  filter(
    str_detect(order_name_c, regex("gent.*", ignore_case = TRUE)),
    str_detect(route_of_administration, regex("intraven.*|intram.*", ignore_case = TRUE))
  )

data_antimicrobial_pool_gentamicin_clin_rec <- data_antimicrobial_pool_gentamicin |>
  left_join(data_clinical_records_wide, relationship = "many-to-many") |>
  glimpse()

# # Add NHS number ----------------------------------------------------------
# # First check that the relationships between mrn, fin and nhs_number are one-to-one
# if (
#   all(
#     data_cohort_admission_wrangled |> 
#     select(mrn, fin, nhs_number) |> 
#     count(mrn, sort = TRUE) |>
#     filter(!is.na(mrn)) |>
#     pull(n) == 1
#   ) &
#   all(
#     data_cohort_admission_wrangled |> 
#     select(mrn, fin, nhs_number) |> 
#     count(nhs_number, sort = TRUE) |> 
#     filter(!is.na(nhs_number)) |> 
#     pull(n) == 1
#   )) {
#   print("The relationships between mrn, fin and (non-missing) nhs_number are one-to-one as expected")
# } else {
#   stop("Error: mrn, fin and (non-missing) nhs_number do not have one-to-one relationships. Please check processing.")
# }

#if error message, this may be because one NHS number has been assigned two MRN numbers... as in week of 4/11/25. In this case, one patient has two MRN numbers...:
# > data_cohort_admission_wrangled |> 
#   +   select(mrn, fin, nhs_number, patient) |> 
#   +   filter(nhs_number == "4044437955")
# # A tibble: 2 × 4
# mrn      fin      nhs_number patient  
# <chr>    <chr>    <chr>      <chr>    
#   1 1296554R 16369756 4044437955 BROPHY, E
# 2 91995378 16369725 4044437955 BROPHY, E

# #Check problematic NHS number/MRN
# check_multiple_mrn <- data_cohort_admission_wrangled |> 
#   #select(mrn, fin, nhs_number, patient) |> 
#   filter(nhs_number == "4044437955") |> 
#   View()

# #In this case, remove 91995378 as this links to 1296554R
# data_cohort_admission_wrangled <- data_cohort_admission_wrangled |>
#   filter(!mrn == "91995378")

data_antimicrobial_pool_gentamicin_clin_rec <- data_antimicrobial_pool_gentamicin_clin_rec |> 
  left_join(data_cohort_admission_wrangled, relationship = "many-to-one") |> 
  relocate(nhs_number, .after = fin) |>
  distinct()

# dedup -------------------------------------------------------------------

data_antimicrobial_pool_gentamicin_clin_rec <- data_antimicrobial_pool_gentamicin_clin_rec |> 
  distinct(order_id, order_name_c, dose, .keep_all = TRUE) |> 
  filter(!is.na(frequency)) |> 
  arrange(mrn, order_name_c)

# data_antimicrobial_pool_gentamicin_clin_rec |>
#   View()

# Add rounded gentamicin dose ---------------------------------------------
data_antimicrobial_pool_gentamicin_clin_rec <- data_antimicrobial_pool_gentamicin_clin_rec |>
  mutate(
    mathematically_corrected_ibw = case_when(
      weight_max < ibw ~ weight_max * 1.13,
      weight_max >= ibw ~ 0.43 * (weight_max - ibw) + ibw,
      .default = NA_real_
    ),
    gentamicin_dose_uncorrected = 7 * mathematically_corrected_ibw,
    gentamicin_dose_rounded = round_to_nearest_40(gentamicin_dose_uncorrected),
    gent_diff_in_dose = as.numeric(dose) - gentamicin_dose_rounded
  )

# Rename columns for readability ------------------------------------------
data_antimicrobial_pool_gentamicin_clin_rec <- data_antimicrobial_pool_gentamicin_clin_rec |>
  rename(
    `nhs number` = nhs_number,
    creat = creat_max,
    weight = weight_max,
    height = height_max,
    `ibw (corrected)` = mathematically_corrected_ibw,
    `gentamicin dose recommended (uncorrected)` = gentamicin_dose_uncorrected,
    `gentamicin dose recommended (rounded)` = gentamicin_dose_rounded,
    `gentamicin dose difference` = gent_diff_in_dose
  )

# Display data structure
data_antimicrobial_pool_gentamicin_clin_rec |> glimpse()

data_antimicrobial_pool_gentamicin_clin_rec |> str()

# number of patients Rx gent ----------------------------------------------

length(data_antimicrobial_pool_gentamicin_clin_rec |> 
  distinct(mrn) |> 
    pull(mrn))

# column names
colnames(data_antimicrobial_pool_gentamicin_clin_rec)

data_antimicrobial_pool_gentamicin_clin_rec_col_names <- data_antimicrobial_pool_gentamicin_clin_rec |>
  colnames()

data_antimicrobial_pool_gentamicin_clin_rec_col_names <- str_c(data_antimicrobial_pool_gentamicin_clin_rec_col_names, ",")

kable(data_antimicrobial_pool_gentamicin_clin_rec_col_names, format = "simple")

# reformat for table ------------------------------------------------------

#name patient name = initials
#round large numbers
data_antimicrobial_pool_gentamicin_clin_rec <- data_antimicrobial_pool_gentamicin_clin_rec |> 
  mutate(initials = get_initials(patient),
         bmi = round(bmi, 1),
         `ibw (corrected)` = round(`ibw (corrected)`, 1),
         `gentamicin dose recommended (uncorrected)` = round(`gentamicin dose recommended (uncorrected)`, 1),
         `gentamicin dose recommended (rounded)` = round(`gentamicin dose recommended (rounded)`, 1),
         `gentamicin dose difference` = round(`gentamicin dose difference`, 1),
         height = round(height, 1)
         )

data_antimicrobial_pool_gentamicin_clin_rec_table <- data_antimicrobial_pool_gentamicin_clin_rec |> 
  select( ward = current_ward,   
          fin,
          `nhs number`, 
          initials,
          mrn,        
          sex,                                       
          age = age_on_admission,                          
          #ibw,                                       
          bmi,  
          `ibw (corrected)`,                           
          #encntr_id,                                 
          #order_id,                                  
          #order_name,                                
          #drug = order_name_c,                              
          #synonym_desc,                              
          `gentamicin dose prescribed` = dose,                                               `gentamicin dose unit prescribed` = dose_unit,    
          `gentamicin dose recommended (uncorrected)`, 
          `gentamicin dose recommended (rounded)`,
          `gentamicin dose difference`,
          `drug form` = drug_form,                                 
          route = route_of_administration,                   
          frequency,                                 
          indication,                                
          #diagnostic_confidence,                     
          `start date/time` = start_date_time,                           
          #next_dose_date_time,                       
          duration = intended_duration,                         
          `duration unit` = duration_unit,                             
          #stop_type,                                 
          `stop date/time` = stop_date_time,                            
          #review_date,                               
          #special_instructions,                      
          #pharmacy_order_sentence,                   
          #na,                                        
          #earliest_import_date,                      
          #medication_beta_lac,                       
          #beta_lac_name,                             
          #record_num,                                
          #c_reactive_protein_performed_dt_tm,        
          #c_reactive_protein_result_val,             
          #c_reactive_protein_result_units,           
          #temperature_performed_dt_tm,               
          #temperature_result_val,                    
          #temperature_result_units,                  
          #height_length_measured_performed_dt_tm,    
          #height_length_measured_result_val,         
          #height_length_measured_result_units,       
          #weight_measured_performed_dt_tm,           
          #weight_measured_result_val,                
          #weight_measured_result_units,              
          #creatinine_performed_dt_tm,                
          #creatinine_result_val,                     
          #creatinine_result_units,                   
          `CRP (max)` = crp_max,                                   
          creat,                                     
          weight,                                    
          height,                                    
          `Temp (max)` = temp_max_clin_rec_wide                    
          #encntr_id_cohort_admission,                
          #date_of_birth,                             
          #admission_dt,                              
          #admission_source,                          
          #current_bay,                               
          #current_bed,                               
          #history_number,                            
          #age_group,                                 
          #paeds,                                   
          )

data_antimicrobial_pool_gentamicin_clin_rec_table

# View(data_antimicrobial_pool_gentamicin_clin_rec_table)

# Analysis and Visualization ----------------------------------------------

# Prepare dose comparison data
dose_comparison <- data_antimicrobial_pool_gentamicin_clin_rec |>
  filter(!is.na(dose)) |>
  mutate(
    dose_numeric = as.numeric(dose),
    dose_difference_calc = dose_numeric - `gentamicin dose recommended (rounded)`,
    dose_category = case_when(
      dose_difference_calc < -50 ~ "Significantly Under-dosed (>50mg)",
      dose_difference_calc >= -50 & dose_difference_calc < 0 ~ "Under-dosed (≤50mg)",
      dose_difference_calc == 0 ~ "Correct dose",
      dose_difference_calc > 0 & dose_difference_calc <= 50 ~ "Over-dosed (≤50mg)",
      dose_difference_calc > 50 ~ "Significantly Over-dosed (>50mg)",
      .default = "Unknown"
    )
  )

# Create plots ------------------------------------------------------------

# Plot 1: Dose difference histogram
plot_dose_comparison_hist <- ggplot(dose_comparison, aes(x = dose_difference_calc)) +
  geom_histogram(binwidth = 20, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Comparison of prescribed vs. recommended dose",
    subtitle = "Visualisation of [prescribed - recommended] dose (mg)",
    x = "Dose Difference (mg)",
    y = "Number of Prescriptions"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal()

plot_dose_comparison_hist

# Plot 2: Dose comparison scatter plot
plot_dose_comparison_scatter <- ggplot(dose_comparison, aes(x = `gentamicin dose recommended (rounded)`, y = dose_numeric)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(
    title = "Comparison of prescribed vs. recommended dose",
    subtitle = "Visualisation of prescribed vs. recommended dose (mg)",
    x = "Recommended Dose (mg)",
    y = "Prescribed Dose (mg)"
  ) +
  theme_minimal()

plot_dose_comparison_scatter

# Plot 3: Dose category breakdown
plot_dose_comparison_breakdown <- dose_comparison |>
  count(dose_category) |>
  mutate(percentage = n / sum(n) * 100) |>
  ggplot(aes(x = reorder(dose_category, n), y = n)) +
  geom_col(fill = "coral", alpha = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", round(percentage, 1), "%)")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Comparison of prescribed vs. recommended dose",
    subtitle = "Categorical subanalysis of gentamicin dosing",
    x = NULL,
    y = "Number of Prescriptions"
  ) +
  theme_minimal()

plot_dose_comparison_breakdown

# Plot 9: Relationship between weight and dose difference
plot_weight_dose_diff <- dose_comparison |>
  filter(!is.na(weight)) |>
  ggplot(aes(x = weight, y = dose_difference_calc)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Comparison of dose difference vs. patient weight",
    subtitle = "Visualisation of dose difference vs. patient weight",
    x = "Weight (kg)",
    y = "Dose Difference (mg)",
    caption = "Dose difference is defined as the prescribed dose \nminus the recommended dose, and reported in milligrams (mg)"
  ) +
  theme_minimal()

plot_weight_dose_diff

# Combine plots
combined_plot1 <- plot_dose_comparison_scatter + plot_weight_dose_diff

combined_plot1

# Multiple doses analysis -------------------------------------------------
data_antimicrobial_pool_gentamicin_clin_rec |> 
  glimpse()

multiple_doses <- data_antimicrobial_pool_gentamicin_clin_rec |>
  group_by(mrn) |>
  summarise(
    dose_count = n(),
    ward = first(current_ward),
    .groups = "drop"
  ) |>
  mutate(multiple_doses = dose_count > 1)

multiple_doses |> 
  arrange(desc(dose_count))

# Plot 4: Multiple doses distribution
plot_multiple_doses <- multiple_doses |>
  ggplot(aes(x = as_factor(dose_count))) +
  geom_bar(fill = "lightgreen", alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, hjust = -0.5) +
  labs(
    title = "Gentamicin Doses per Patient",
    x = "Number of Doses",
    y = "Number of Patients"
  ) +
  #coord_flip()+
  theme_minimal()

plot_multiple_doses

# Time between doses analysis ---------------------------------------------
time_between_doses <- data_antimicrobial_pool_gentamicin_clin_rec |>
  filter(!is.na(dose)) |>
  group_by(mrn) |>
  filter(n() > 1) |>
  arrange(start_date_time) |>
  mutate(
    time_diff_hours = as.numeric(difftime(lead(start_date_time), start_date_time, units = "hours"))
  ) |>
  filter(!is.na(time_diff_hours)) |>
  ungroup()

time_between_doses

time_between_doses_table <- time_between_doses |> 
  select( ward = current_ward,   
          mrn,        
          `nhs number`, 
          fin,
          initials,
          #sex,                          
          #age = age_on_admission,                          
          #ibw,                                       
          #bmi,  
          #`ibw (corrected)`,                           
          #encntr_id,                                 
          #order_id,                                  
          #order_name,                                
          #drug = order_name_c,                              
          #synonym_desc,                              
          `gentamicin dose prescribed` = dose,     
          `gentamicin dose unit prescribed` = dose_unit,    
          `time between doses (h)` = time_diff_hours,
          `start date/time` = start_date_time,
          #`gentamicin dose recommended (uncorrected)`, 
          #`gentamicin dose recommended (rounded)`,
          #`gentamicin dose difference`,
          #`drug form` = drug_form,                                 
          #route = route_of_administration,                   
          # frequency,                                 
          indication                                
          #diagnostic_confidence,                  
          #next_dose_date_time,                       
          #duration = intended_duration,                         
          #`duration unit` = duration_unit,                             
          #stop_type,                                 
          #`stop date/time` = stop_date_time,  
          #review_date,                               
          #special_instructions,                      
          #pharmacy_order_sentence,                   
          #na,                                        
          #earliest_import_date,                      
          #medication_beta_lac,                       
          #beta_lac_name,                             
          #record_num,                                
          #c_reactive_protein_performed_dt_tm,        
          #c_reactive_protein_result_val,             
          #c_reactive_protein_result_units,           
          #temperature_performed_dt_tm,               
          #temperature_result_val,                    
          #temperature_result_units,                  
          #height_length_measured_performed_dt_tm,    
          #height_length_measured_result_val,         
          #height_length_measured_result_units,       
          #weight_measured_performed_dt_tm,           
          #weight_measured_result_val,                
          #weight_measured_result_units,              
          #creatinine_performed_dt_tm,                
          #creatinine_result_val,                     
          #creatinine_result_units,                   
          #`CRP (max)` = crp_max,                                   
          #creat,                                     
          #weight,                                    
          #height,                                    
          #`Temp (max)` = temp_max_clin_rec_wide,                    
          #encntr_id_cohort_admission,                
          #date_of_birth,                             
          #admission_dt,                              
          #admission_source,                          
          #current_bay,                               
          #current_bed,                               
          #history_number,                            
          #age_group,                                 
          #paeds,                                   
  )

time_between_doses_table

# View(time_between_doses_table)

#<24h between doses

ind <- which(time_between_doses$time_diff_hours <24)
mrn_dosed_less_than_24h <- time_between_doses$mrn[ind]

time_between_doses_table_less_than_24h <- time_between_doses_table |> 
  filter(mrn %in% mrn_dosed_less_than_24h) |> 
  mutate(`time between doses (h)` = round(`time between doses (h)`, 1)) |> 
  arrange(ward, mrn, `start date/time`)

time_between_doses_table_less_than_24h

# View(time_between_doses_table_less_than_24h)

# Plot 5: Time between doses
if(nrow(time_between_doses) > 0) {
  plot_time_between_doses <- ggplot(time_between_doses, aes(x = time_diff_hours)) +
    geom_histogram(binwidth = 2, fill = "purple", alpha = 0.7, color = "white") +
    geom_vline(xintercept = 24, color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = "Time Between Consecutive Gentamicin Doses",
      subtitle = "For patients with multiple doses (red line = 24 hours)",
      x = "Hours Between Doses",
      y = "Number of Dose Intervals"
    ) +
    theme_minimal()
} else {
  plot_time_between_doses <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "No patients with multiple doses\nwhere dose data is available") +
    theme_void()
}

plot_time_between_doses

# # Plot 6: Dose by indication
# plot_dose_by_indication <- dose_comparison |>
#   filter(!is.na(indication)) |>
#   ggplot(aes(x = reorder(indication, dose_numeric, median), y = dose_numeric)) +
#   geom_boxplot(fill = "lightblue", alpha = 0.7) +
#   coord_flip() +
#   labs(
#     title = "Gentamicin Doses by Indication",
#     x = "Indication",
#     y = "Prescribed Dose (mg)"
#   ) +
#   theme_minimal()
# 
# plot_dose_by_indication

# Summary statistics ------------------------------------------------------

dose_summary <- dose_comparison |>
  summarise(
    total_prescriptions = n(),
    correct_doses = sum(dose_difference_calc == 0, na.rm = TRUE),
    under_dosed = sum(dose_difference_calc < 0, na.rm = TRUE),
    over_dosed = sum(dose_difference_calc > 0, na.rm = TRUE),
    mean_difference = round(mean(dose_difference_calc, na.rm = TRUE), 1),
    median_difference = round(median(dose_difference_calc, na.rm = TRUE), 1)
  )

dose_summary

cat(sprintf("Total prescriptions with dose data: %d\n", dose_summary$total_prescriptions))

cat(sprintf("Under-dosed: %d (%.1f%%)\n", 
            dose_summary$under_dosed, 
            dose_summary$under_dosed/dose_summary$total_prescriptions*100))

cat(sprintf("Over-dosed: %d (%.1f%%)\n", 
            dose_summary$over_dosed, 
            dose_summary$over_dosed/dose_summary$total_prescriptions*100))

cat(sprintf("Mean dose difference: %.1f mg\n", dose_summary$mean_difference))

cat(sprintf("Median dose difference: %.1f mg\n\n", dose_summary$median_difference))

multiple_summary <- multiple_doses |>
  summarise(
    total_patients = n(),
    patients_multiple_doses = sum(multiple_doses),
    max_doses = max(dose_count)
  )

multiple_summary

cat(sprintf("Total patients: %d\n", multiple_summary$total_patients))

cat(sprintf("Distinct patients prescribed multiple doses: %d (%.1f%%)\n", 
            multiple_summary$patients_multiple_doses,
            multiple_summary$patients_multiple_doses/multiple_summary$total_patients*100))

cat(sprintf("Maximum doses per patient: %d\n\n", multiple_summary$max_doses))

if(nrow(time_between_doses) > 0) {
  cat("3. TIME BETWEEN DOSES ANALYSIS\n")
  cat("------------------------------\n")
  time_summary <- time_between_doses |>
    summarise(
      total_intervals = n(),
      mean_hours = round(mean(time_diff_hours, na.rm = TRUE), 1),
      median_hours = round(median(time_diff_hours, na.rm = TRUE), 1),
      min_hours = round(min(time_diff_hours, na.rm = TRUE), 1),
      max_hours = round(max(time_diff_hours, na.rm = TRUE), 1)
    )
  
  cat(sprintf("Total dose intervals analyzed: %d\n", time_summary$total_intervals))
  cat(sprintf("Mean time between doses: %.1f hours\n", time_summary$mean_hours))
  cat(sprintf("Median time between doses: %.1f hours\n", time_summary$median_hours))
  cat(sprintf("Range: %.1f - %.1f hours\n", time_summary$min_hours, time_summary$max_hours))
}

# Filter to include only patients with multiple doses where "dose" is available
data_multiple_doses_only <- data_antimicrobial_pool_gentamicin_clin_rec |>
  filter(!is.na(dose)) |>
  group_by(mrn) |>
  filter(n() > 1) |>
  arrange(mrn, fin, start_date_time) |>
  ungroup()

data_multiple_doses_only <- data_multiple_doses_only |> 
  select( ward = current_ward,   
          fin,
          `nhs number`, 
          initials,
          mrn,        
          sex,                                       
          age = age_on_admission,                          
          #ibw,                                       
          bmi,  
          `ibw (corrected)`,                           
          #encntr_id,                                 
          #order_id,                                  
          #order_name,                                
          #drug = order_name_c,                              
          #synonym_desc,                              
          `gentamicin dose prescribed` = dose,                                               `gentamicin dose unit prescribed` = dose_unit,    
          `gentamicin dose recommended (uncorrected)`, 
          `gentamicin dose recommended (rounded)`,
          `gentamicin dose difference`,
          `drug form` = drug_form,                                 
          route = route_of_administration,                   
          frequency,                                 
          indication,                                
          #diagnostic_confidence,                     
          `start date/time` = start_date_time,                           
          #next_dose_date_time,                       
          duration = intended_duration,                         
          `duration unit` = duration_unit,                             
          #stop_type,                                 
          `stop date/time` = stop_date_time,                            
          #review_date,                               
          #special_instructions,                      
          #pharmacy_order_sentence,                   
          #na,                                        
          #earliest_import_date,                      
          #medication_beta_lac,                       
          #beta_lac_name,                             
          #record_num,                                
          #c_reactive_protein_performed_dt_tm,        
          #c_reactive_protein_result_val,             
          #c_reactive_protein_result_units,           
          #temperature_performed_dt_tm,               
          #temperature_result_val,                    
          #temperature_result_units,                  
          #height_length_measured_performed_dt_tm,    
          #height_length_measured_result_val,         
          #height_length_measured_result_units,       
          #weight_measured_performed_dt_tm,           
          #weight_measured_result_val,                
          #weight_measured_result_units,              
          #creatinine_performed_dt_tm,                
          #creatinine_result_val,                     
          #creatinine_result_units,                   
          `CRP (max)` = crp_max,                                   
          creat,                                     
          weight,                                    
          height,                                    
          `Temp (max)` = temp_max_clin_rec_wide                    
          #encntr_id_cohort_admission,                
          #date_of_birth,                             
          #admission_dt,                              
          #admission_source,                          
          #current_bay,                               
          #current_bed,                               
          #history_number,                            
          #age_group,                                 
          #paeds,                                   
  )

patient_summary_multiple_gent_doses <- data_multiple_doses_only %>%
  group_by(mrn, `nhs number`, fin) %>%
  reframe(
    ward,
    initials,
    dose_count = n(),
    first_dose_date = min(`start date/time`),
    last_dose_date = max(`start date/time`)
  ) %>%
  ungroup() |> 
  relocate(ward, .before = "mrn") |> 
  arrange(desc(dose_count), first_dose_date) |> 
  distinct()

patient_summary_multiple_gent_doses |> 
  glimpse()

# Number of doses given to unique patients given multiple doses
dose_distribution <- data_multiple_doses_only %>%
  group_by(mrn) %>%
  summarise(dose_count = n(), .groups = "drop") %>%
  count(dose_count, name = "patient_count")

dose_distribution

