# wrangle

# Read functions ----------------------------------------------------------

# source("./01_src/functions.R")

# prerequisite R scripts -----------------------------------------------------

# If .db files don't exist/new data needs to be added, run this:
# source('./01_src/02_data_import/02_data_import - 01_test eRecord data.R')

# Read in (or create) Obs dataset -----------------------------------------------

# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Packages",
#                end_string = "END PACKAGES")
# source_section('./01_src/02_data_import/02_data_import - 03_read pre-wrangle database.R',
#                start_string = "Obs",
#                end_string = "END OBS")

# data_obs_news2 ------------------------------------------

data_obs_news2_01 <- data_obs |>
  select(-obs_number) |>
  rename(earliest_import_date = import_date) |>
  add_observations_history_number(
    sort_order_cols = c(
      "encntr_id",
      "mrn",
      "obs_date_time",
      "earliest_import_date"
    ),
    observation_group_cols = c("encntr_id", "mrn"),
    history_col_name = "obs_number"
  )

data_obs_news2_02 <- data_obs_news2_01 |>
  mutate(
    respiratory_rate = as.numeric(respiratory_rate),
    oxygen_saturation = as.numeric(oxygen_saturation),
    systolic_blood_pressure = as.numeric(systolic_blood_pressure),
    heart_rate_monitored = as.numeric(heart_rate_monitored),
    temperature = as.numeric(temperature),
    obs_date_time = as_datetime(obs_date_time, tz = "GB")
  )

data_obs_news2_03 <- data_obs_news2_02 |>
  mutate(NEWS2 = 0) |>
  mutate(
    systolic_blood_pressure = case_when(
      systolic_blood_pressure < 30 | systolic_blood_pressure > 250 ~ NA,
      .default = systolic_blood_pressure
    ),
    heart_rate_monitored = case_when(
      heart_rate_monitored < 30 | heart_rate_monitored > 250 ~ NA,
      .default = heart_rate_monitored
    ),
    oxygen_saturation = case_when(
      oxygen_saturation < 70 | oxygen_saturation > 100 ~ NA,
      .default = oxygen_saturation
    ),
    respiratory_rate = case_when(
      respiratory_rate < 5 | respiratory_rate > 30 ~ NA,
      .default = respiratory_rate
    ),
    temperature = case_when(
      temperature < 30 | temperature > 45 ~ NA,
      .default = temperature
    )
  )

data_obs_news2_04 <- data_obs_news2_03 |>
  mutate(NEWS2 = case_when(
    respiratory_rate <= 8 ~ NEWS2 + 3,
    respiratory_rate <= 11 ~ NEWS2 + 1,
    respiratory_rate <= 20 ~ NEWS2 + 0,
    respiratory_rate <= 24 ~ NEWS2 + 2,
    respiratory_rate >= 25 ~ NEWS2 + 3,
    .default = NEWS2
  ))

data_obs_news2_05 <- data_obs_news2_04 |>
  mutate(NEWS2 = case_when(
    oxygen_saturation <= 91 ~ NEWS2 + 3,
    oxygen_saturation <= 93 ~ NEWS2 + 2,
    oxygen_saturation <= 95 ~ NEWS2 + 1,
    oxygen_saturation >= 96 ~ NEWS2 + 0,
    .default = NEWS2
  ))

data_obs_news2_06 <- data_obs_news2_05 |>
  mutate(
    oxygen_l = case_match(
      oxygen_l,
      c(" ", "NULL", "0") ~ NA,
      .default = oxygen_l
    ),
    NEWS2 = case_when(
      is.na(oxygen_l) ~ NEWS2,
      .default = NEWS2 + 2
    )
  )

data_obs_news2_07 <- data_obs_news2_06 |>
  mutate(NEWS2 = case_when(
    systolic_blood_pressure <= 90 ~ NEWS2 + 3,
    systolic_blood_pressure <= 100 ~ NEWS2 + 2,
    systolic_blood_pressure <= 110 ~ NEWS2 + 1,
    systolic_blood_pressure <= 219 ~ NEWS2 + 0,
    systolic_blood_pressure >= 220 ~ NEWS2 + 3,
    .default = NEWS2
  ))

data_obs_news2_08 <- data_obs_news2_07 |>
  mutate(NEWS2 = case_when(
    heart_rate_monitored <= 40 ~ NEWS2 + 3,
    heart_rate_monitored <= 50 ~ NEWS2 + 1,
    heart_rate_monitored <= 90 ~ NEWS2 + 0,
    heart_rate_monitored <= 110 ~ NEWS2 + 1,
    heart_rate_monitored <= 130 ~ NEWS2 + 2,
    heart_rate_monitored >= 131 ~ NEWS2 + 3,
    .default = NEWS2
  ))

data_obs_news2_09 <- data_obs_news2_08 |>
  mutate(NEWS2 = case_match(
    avpu_conscious_level,
    c("A", "A - Alert", "NULL") ~ NEWS2,
    .default = NEWS2 + 3
  ))

data_obs_news2_10 <- data_obs_news2_09 |>
  mutate(NEWS2 = case_when(
    temperature <= 35.0 ~ NEWS2 + 3,
    temperature <= 36.0 ~ NEWS2 + 1,
    temperature <= 38.0 ~ NEWS2 + 0,
    temperature <= 39.0 ~ NEWS2 + 1,
    temperature >= 39.1 ~ NEWS2 + 2,
    .default = NEWS2
  ))

data_obs_news2_11 <- data_obs_news2_10 |>
  mutate(NEWS_Risk = case_when(
    NEWS2 >= 7 ~ "High",
    NEWS2 >= 5 ~ "Medium", # any individual item triggering a NEWS increment of 3 = Low-medium unless already captured above
    respiratory_rate <= 8 ~ "Low-medium",
    respiratory_rate >= 25 ~ "Low-medium",
    oxygen_saturation <= 91 ~ "Low-medium",
    systolic_blood_pressure <= 90 ~ "Low-medium",
    systolic_blood_pressure >= 220 ~ "Low-medium",
    heart_rate_monitored <= 40 ~ "Low-medium",
    heart_rate_monitored >= 131 ~ "Low-medium",
    avpu_conscious_level %in% c("A", "A - Alert", "NULL") ~ "Low-medium",
    temperature <= 35.0 ~ "Low-medium", # score of 0-4 and no individual items triggering an increment of 3 = Low
    NEWS2 <= 4 ~ "Low",
    .default = "Low"
  ))

data_obs_news2 <- data_obs_news2_11 |>
  group_by(mrn) |> # possibly could add encntr_id later.
  mutate(
    systolic_blood_min = ifelse(any(!is.na(systolic_blood_pressure)), min(systolic_blood_pressure, na.rm = TRUE), NA),
    oxygen_saturation_min = ifelse(any(!is.na(oxygen_saturation)), min(oxygen_saturation, na.rm = TRUE), NA),
    heart_rate_max = ifelse(any(!is.na(heart_rate_monitored)), max(heart_rate_monitored, na.rm = TRUE), NA),
    respiratory_rate_max = ifelse(any(!is.na(respiratory_rate)), max(respiratory_rate, na.rm = TRUE), NA),
    news2_max = ifelse(any(!is.na(NEWS2)), max(NEWS2, na.rm = TRUE), NA),
    temp_max = ifelse(any(!is.na(temperature)), max(temperature, na.rm = TRUE), NA)
  ) |>
  ungroup()


# change oxygen_l into more natural language ------------------------------

data_obs_news2 <- data_obs_news2 |>
  mutate(oxygen_status = case_when(
    is.na(oxygen_l) ~ "room air", # Missing values in oxygen_l are "room air"
    TRUE ~ "oxygen prescribed" # Any other value (including non-missing) is "oxygen prescribed"
  ))

# create frequency poly charts with patchwork using plot_freqpoly from functions.R -----------------------------
frequency_poly_plot_data_news2 <- data_obs_news2 |>
  select(-encntr_id, -obs_number)

# Use map to iterate over numeric columns and create plots
plots_news2 <- lapply(names(frequency_poly_plot_data_news2)[sapply(frequency_poly_plot_data_news2, is.numeric)], plot_freqpoly, data = frequency_poly_plot_data_news2)

combined_plot_news2 <- patchwork::wrap_plots(plots_news2, nrow = nrow(plots_news2))

combined_plot_news2


# check for patients who have stable obs in last 24h  -----------------------

# data_obs_news2 |>
#   skimr::skim_without_charts()

stable_obs_24h <- glue::glue(
  "temperature", " <37.9",
  " & ",
  "heart_rate_monitored", " <101",
  " & ",
  "respiratory_rate", " <25",
  " & ",
  "oxygen_saturation", " >89",
  " & ",
  "systolic_blood_pressure", " >89"
)

# #check stable_obs_24h
# stable_obs_24h
#
# #check filtering by stable_obs_24h works
# data_obs_news2 |>
#   filter(base::eval(base::parse(text = stable_obs_24h)))

# check for date/time of last measured obs
date_time_obs_end <- max(data_obs_news2$obs_date_time, na.rm = FALSE)

# Calculate the start time 24 hours prior to the last observation
date_time_obs_start <- date_time_obs_end - lubridate::hours(24)

# Create a period object representing the time interval
date_time_obs_period_24h <- lubridate::interval(date_time_obs_start, date_time_obs_end)

# Create new column to denote that obs were done in last 24h of data collection period
# modify the mutate function to apply the calculations only when obs_done_in_last_24h == TRUE
data_obs_news2 <- data_obs_news2 %>%
  group_by(mrn) |> # Group by patient ID
  mutate(
    obs_done_in_last_24h = obs_date_time %within% date_time_obs_period_24h,
    systolic_blood_min_last_24h = case_when(
      obs_done_in_last_24h == TRUE & any(!is.na(systolic_blood_pressure)) ~ min(systolic_blood_pressure, na.rm = TRUE),
      TRUE ~ NA
    ),
    oxygen_saturation_min_last_24h = case_when(
      obs_done_in_last_24h == TRUE & any(!is.na(oxygen_saturation)) ~ min(oxygen_saturation, na.rm = TRUE),
      TRUE ~ NA
    ),
    heart_rate_max_last_24h = case_when(
      obs_done_in_last_24h == TRUE & any(!is.na(heart_rate_monitored)) ~ max(heart_rate_monitored, na.rm = TRUE),
      TRUE ~ NA
    ),
    respiratory_rate_max_last_24h = case_when(
      obs_done_in_last_24h == TRUE & any(!is.na(respiratory_rate)) ~ {
        if (any(!is.na(respiratory_rate))) {
          max(respiratory_rate, na.rm = TRUE)
        } else {
          NA
        }
      },
      TRUE ~ NA
    ),
    news2_max_last_24h = case_when(
      obs_done_in_last_24h == TRUE & any(!is.na(NEWS2)) ~ max(NEWS2, na.rm = TRUE),
      TRUE ~ NA
    ),
    temp_max_last_24h = case_when(
      obs_done_in_last_24h == TRUE & any(!is.na(temperature)) ~ max(temperature, na.rm = TRUE),
      TRUE ~ NA
    )
  ) |>
  ungroup()

# Check output e.g. for systolic_blood_pressure vs. systolic_blood_min_last_24h ... The systolic_blood_min_last_24h value should be the last value in systolic_blood_pressure

# data_obs_news2 |>
#   select(obs_date_time, systolic_blood_pressure, ends_with("last_24h"))
# #this is working (21/10/24)

# # Check colnames in new tibble
# data_obs_news2 |>
#   glimpse() |>
#   colnames()

# mimic limits set in stable_obs_24h to create stable_obs_last_24h
stable_obs_last_24h <- glue::glue(
  "temp_max_last_24h", " <37.9",
  " & ",
  "heart_rate_max_last_24h", " <101",
  " & ",
  "respiratory_rate_max_last_24h", " <25",
  " & ",
  "oxygen_saturation_min_last_24h", " >89",
  " & ",
  "systolic_blood_min_last_24h", " >89"
)

# Check if the new parameters created in data_obs_news2_last_24h fall within the limits set in stable_obs_24h
data_obs_news2 <- data_obs_news2 |>
  mutate(
    obs_stable_in_last_24h = eval(parse(text = stable_obs_last_24h))
  )

# # check output... does obs_stable_in_last_24h include both TRUE and FALSE?
# data_obs_news2 |>
#   select(obs_stable_in_last_24h, everything()) |>
#   count(obs_stable_in_last_24h)
#
# # check where obs_stable_in_last_24h == FALSE... are some values outside limits defined in data_obs_news2_last_24h_stable_query?
# data_obs_news2 |>
#   filter(obs_stable_in_last_24h == FALSE) |>
#   glimpse()
#
# # # check where obs_stable_in_last_24h == TRUE are all values within limits defined in stable_obs_last_24h?
# # stable_obs_last_24h
# data_obs_news2 |>
#   filter(obs_stable_in_last_24h == TRUE) |>
#   glimpse()
#
# # # check where obs_stable_in_last_24h == NA (these are patients where obs_done_in_last_24h == FALSE)
# # stable_obs_last_24h
# data_obs_news2 |>
#   filter(is.na(obs_stable_in_last_24h)) |>
#   glimpse()
#
# # # Check e.g. comparing NEWS2 with new2_max with news2_max_last_24h
# data_obs_news2 |>
#   group_by(obs_done_in_last_24h, obs_stable_in_last_24h) |>
#   select(mrn, obs_date_time, obs_done_in_last_24h, obs_stable_in_last_24h, NEWS2, news2_max, news2_max_last_24h) |>
#   skimr::skim_without_charts()
# # looks OK!

# arrange colnames in alpha order -----------------------------------------

data_obs_news2 <- data_obs_news2 %>%
  select(mrn, starts_with("obs"), order(colnames(.)))

# data_obs_news2 |>
#   glimpse()
#
# data_obs_news2 |>
#   filter(obs_done_in_last_24h == TRUE) |>
#   glimpse()
