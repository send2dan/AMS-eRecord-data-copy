
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
# 
# source("./01_src/03_wrangle/03_wrangle - antimicrobial_costings_part_2.R")

## original data from 03_wrangle - antimicrobial_costings_part_2.R
# data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  filter(order_name_c == "Temocillin") |> 
  glimpse()

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  count(order_name_c_amr) |> 
  print(n = 100)

# Calculate drug expenditure summary by order_name_c_amr
drug_expenditure_summary <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total %>%
  group_by(order_name_c_amr) %>%
  summarise(
    total_orders = n(),
    mean_cost_per_dose = mean(cost_per_dose_average, na.rm = TRUE),
    mean_cost_per_day = mean(cost_per_day_average, na.rm = TRUE),
    total_expenditure = sum(total_cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_expenditure)) |> 
  filter(!total_expenditure == 0) 

# drug_expenditure_summary |>
#   glimpse()

# Create flextable with improved column names ---
drug_expenditure_table <- drug_expenditure_summary |>
  flextable() |>
  set_header_labels(
    order_name_c_amr = "Drug Name",
    total_orders = "Total Orders",
    mean_cost_per_dose = "Cost per Dose (£)",
    mean_cost_per_day = "Cost per Day (£)",
    total_expenditure = "Total Expenditure (£)"
  ) |>
  # Format numeric columns for better readability
  colformat_double(
    j = c("total_expenditure", "mean_cost_per_dose", "mean_cost_per_day"),
    digits = 2,
    big.mark = ","
  ) |>
  colformat_int(
    j = "total_orders",
    big.mark = ","
  ) |>
  # Add styling
  theme_vanilla() |>
  autofit() |>
  # Make header bold
  bold(part = "header") |>
  # Align numeric columns to the right
  align(j = c("mean_cost_per_dose", "mean_cost_per_day", "total_orders", "total_expenditure"), 
        align = "right", part = "all")

# # Display the table
# drug_expenditure_table

# Print summary table
print("Drug Expenditure Summary:")
print(drug_expenditure_summary, n = 100)

# 1. Total Expenditure by Drug (Bar Chart)
p1 <- drug_expenditure_summary %>%
  mutate(order_name_c_amr = fct_reorder(order_name_c_amr, total_expenditure)) %>%
  ggplot(aes(x = order_name_c_amr, y = total_expenditure)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  labs(
    title = "Total Drug Expenditure by Antimicrobial",
    subtitle = "Ordered by total expenditure (highest to lowest)",
    x = NULL,
    y = "Total Expenditure (£)",
    caption = "Expenditure data for antimicrobials where cost-per-day is available"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

# 2. Cost per Dose vs Number of Orders (Scatter Plot)
p2 <- drug_expenditure_summary %>%
  ggplot(aes(x = total_orders, y = mean_cost_per_dose)) +
  geom_point(aes(size = total_expenditure), alpha = 0.7, color = "darkred") +
  geom_text(aes(label = ifelse(mean_cost_per_dose > 1 | total_orders > 500, 
                               order_name_c_amr, "")), 
            vjust = -0.5, hjust = 0.5, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  scale_size_continuous(name = "Total\nExpenditure (£)", 
                        labels = dollar_format(prefix = "£")) +
  labs(
    title = "Cost per Dose vs Number of Orders",
    subtitle = "Point size represents total expenditure",
    x = "Total Number of Orders",
    y = "Cost per Dose (£)",
    caption = "Labels shown for high-cost or high-volume drugs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

# 3. Top 10 Drugs by Total Expenditure (Horizontal Bar Chart)
p3 <- drug_expenditure_summary %>%
  slice_head(n = 10) %>%
  mutate(order_name_c_amr = fct_reorder(order_name_c_amr, total_expenditure)) %>%
  ggplot(aes(x = order_name_c_amr, y = total_expenditure)) +
  geom_col(fill = "forestgreen", alpha = 0.8) +
  geom_text(aes(label = paste0("£", round(total_expenditure, 2))), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£"), 
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 10 Antimicrobials by Total Expenditure",
    x = NULL,
    y = "Total Expenditure (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Display all plots
print(p1)
print(p2)
print(p3)

# Additional summary statistics
cat("\n=== KEY INSIGHTS ===\n")
cat("Total number of distinct antimicrobials prescribed:", nrow(drug_expenditure_summary), "\n")
cat("Total expenditure across all drugs: £", round(sum(drug_expenditure_summary$total_expenditure), 2), "\n")

total_expenditure <- round(sum(drug_expenditure_summary$total_expenditure), 2)

cat("Most expensive drug (total expenditure):", drug_expenditure_summary$order_name_c_amr[1], 
    "- £", round(drug_expenditure_summary$total_expenditure[1], 2), "\n")
cat("Highest cost per dose:", 
    drug_expenditure_summary$order_name_c_amr[which.max(drug_expenditure_summary$mean_cost_per_dose)],
    "- £", round(max(drug_expenditure_summary$mean_cost_per_dose, na.rm = TRUE), 2), "\n")
cat("Most frequently prescribed:", 
    drug_expenditure_summary$order_name_c_amr[which.max(drug_expenditure_summary$total_orders)],
    "- ", max(drug_expenditure_summary$total_orders), " orders\n")


# add drug class using AMR::ab_group --------------------------------------
drug_expenditure_summary_class <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  mutate(
    class = AMR::ab_group(order_name_c_amr)
  ) |> 
  relocate(class, .after = order_name_c_amr) |> 
  group_by(class) |> 
  summarise(
    class_total_orders = n(),
    class_mean_cost_per_dose = mean(cost_per_dose_average, na.rm = TRUE),
    class_mean_cost_per_day = mean(cost_per_day_average, na.rm = TRUE),
    class_total_expenditure = sum(total_cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(class_total_expenditure)) |> 
  filter(!class_total_expenditure == 0) |> 
  filter(!is.na(class))

# drug_expenditure_summary_class

drug_expenditure_table_class <- drug_expenditure_summary_class |>
  flextable() |>
  set_header_labels(
    order_name_c_amr = "Drug Name",
    class_total_orders = "Total Orders",
    class_mean_cost_per_dose = "Mean Cost per Dose (£)",
    class_mean_cost_per_day = "Mean Cost per Day (£)",
    class_total_expenditure = "Total Expenditure (£)"
  ) |>
  # Format numeric columns for better readability
  colformat_double(
    j = c("class_total_expenditure", "class_mean_cost_per_dose", "class_mean_cost_per_day"),
    digits = 2,
    big.mark = ","
  ) |>
  colformat_int(
    j = "class_total_orders",
    big.mark = ","
  ) |>
  # Add styling
  theme_vanilla() |>
  autofit() |>
  # Make header bold
  bold(part = "header") |>
  # Align numeric columns to the right
  align(j = c("class_mean_cost_per_dose", "class_mean_cost_per_day", "class_total_orders", "class_total_expenditure"), 
        align = "right", part = "all")

# drug_expenditure_table_class

# # Display the table
# drug_expenditure_table

# Print summary table
print("Drug Expenditure Summary, by class:")
print(drug_expenditure_summary_class, n = 100)

# 1. Total Expenditure by Drug (Bar Chart)
p1_class <- drug_expenditure_summary_class %>%
  mutate(class = fct_reorder(class, class_total_expenditure)) %>%
  ggplot(aes(x = class, y = class_total_expenditure)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  labs(
    title = "Total Drug Expenditure by Antimicrobial Class",
    subtitle = "Ordered by total expenditure (highest to lowest)",
    x = NULL,
    y = "Total Expenditure (£)",
    caption = "Expenditure data for antimicrobials where cost-per-day is available"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

# 2. Mean Cost per Dose vs Number of Orders (Scatter Plot)
p2_class <- drug_expenditure_summary_class %>%
  ggplot(aes(x = class_total_orders, y = class_mean_cost_per_dose)) +
  geom_point(aes(size = class_total_expenditure), alpha = 0.7, color = "darkred") +
  geom_text(aes(label = ifelse(class_mean_cost_per_dose > 1 | class_total_orders > 500, 
                               class, "")), 
            vjust = -0.5, hjust = 0.5, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  scale_size_continuous(name = "Total\nExpenditure (£)", 
                        labels = dollar_format(prefix = "£")) +
  labs(
    title = "Mean Cost per Dose vs Number of Orders",
    subtitle = "Point size represents total expenditure per class",
    x = "Total Number of Orders",
    y = "Mean Cost per Dose (£)",
    caption = "Labels shown for high-cost or high-volume drugs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

# 3. Top 10 Drugs by Total Expenditure (Horizontal Bar Chart)
p3_class <- drug_expenditure_summary_class %>%
  slice_head(n = 10) %>%
  mutate(class = fct_reorder(class, class_total_expenditure)) %>%
  ggplot(aes(x = class, y = class_total_expenditure)) +
  geom_col(fill = "forestgreen", alpha = 0.8) +
  geom_text(aes(label = paste0("£", round(class_total_expenditure, 2))), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£"), 
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 10 Antimicrobial Classes by Total Expenditure",
    x = NULL,
    y = "Total Expenditure (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Display all plots
print(p1_class)
print(p2_class)
print(p3_class)

# Additional summary statistics
cat("\n=== KEY INSIGHTS ===\n")
cat("Total number of distinct antimicrobial classes prescribed:", nrow(drug_expenditure_summary_class), "\n")
cat("Total expenditure across all drug classes: £", round(sum(drug_expenditure_summary_class$class_total_expenditure), 2), "\n")
cat("Most expensive drug (total expenditure):", drug_expenditure_summary_class$class[1], 
    "- £", round(drug_expenditure_summary_class$class_total_expenditure[1], 2), "\n")
cat("Highest cost per dose:", 
    drug_expenditure_summary_class$class[which.max(drug_expenditure_summary_class$class_mean_cost_per_dose)],
    "- £", round(max(drug_expenditure_summary_class$class_mean_cost_per_dose, na.rm = TRUE), 2), "\n")
cat("Most frequently prescribed:", 
    drug_expenditure_summary_class$class[which.max(drug_expenditure_summary_class$class_total_orders)],
    "- ", max(drug_expenditure_summary_class$class_total_orders), " orders\n")

# beta-lactam specific analysis  --------------------------------------
drug_expenditure_summary_beta_lac <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total|> 
  mutate(medication_beta_lac = if_else(
    medication_beta_lac == TRUE,
    true = "Beta-lactam antimicrobial",
    false = "Other antimicrobial"
  )) |> 
  group_by(medication_beta_lac) |> 
  summarise(
    beta_lac_total_orders = n(),
    beta_lac_mean_cost_per_dose = mean(cost_per_dose_average, na.rm = TRUE),
    beta_lac_mean_cost_per_day = mean(cost_per_day_average, na.rm = TRUE),
    beta_lac_total_expenditure = sum(total_cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(beta_lac_total_expenditure)) |> 
  filter(!beta_lac_total_expenditure == 0) 

drug_expenditure_summary_beta_lac


# Print summary table
print("Drug Expenditure Summary, by category:")
print(drug_expenditure_summary_beta_lac, n = 100)

# 1. Total Expenditure by Drug (Bar Chart)
p1_beta_lac <- drug_expenditure_summary_beta_lac %>%
  ggplot(aes(x = medication_beta_lac, y = beta_lac_total_expenditure)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  labs(
    title = "Total Drug Expenditure by Category",
    subtitle = "Ordered by total expenditure (highest to lowest)",
    x = NULL,
    y = "Total Expenditure (£)",
    caption = "Expenditure data for antimicrobials where cost-per-day is available"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

# 2. Mean Cost per Dose vs Number of Orders (Scatter Plot)
p2_beta_lac <- drug_expenditure_summary_beta_lac %>%
  ggplot(aes(x = beta_lac_total_orders, y = beta_lac_mean_cost_per_dose)) +
  geom_point(aes(size = beta_lac_total_expenditure), alpha = 0.7, color = "darkred") +
  geom_text(aes(label = ifelse(beta_lac_mean_cost_per_dose > 1 | beta_lac_total_orders > 500, 
                               medication_beta_lac, "")), 
            vjust = -0.5, hjust = 0.5, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  scale_size_continuous(name = "Total\nExpenditure (£)", 
                        labels = dollar_format(prefix = "£")) +
  labs(
    title = "Mean Cost per Dose vs Number of Orders",
    subtitle = "Point size represents total expenditure per category",
    x = "Total Number of Orders",
    y = "Mean Cost per Dose (£)",
    caption = "Labels shown for high-cost or high-volume drugs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

# 3. Top 10 Drugs by Total Expenditure (Horizontal Bar Chart)
p3_beta_lac <- drug_expenditure_summary_beta_lac %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = medication_beta_lac, y = beta_lac_total_expenditure)) +
  geom_col(fill = "forestgreen", alpha = 0.8) +
  geom_text(aes(label = paste0("£", round(beta_lac_total_expenditure, 2))), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£"), 
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Total Expenditure by Category",
    x = NULL,
    y = "Total Expenditure (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Display all plots
print(p1_beta_lac)
print(p2_beta_lac)
print(p3_beta_lac)

# Additional summary statistics
cat("\n=== KEY INSIGHTS ===\n")
cat("Total number of distinct categories prescribed:", nrow(drug_expenditure_summary_beta_lac), "\n")
cat("Total expenditure across all categories: £", round(sum(drug_expenditure_summary_beta_lac$beta_lac_total_expenditure), 2), "\n")
cat("Most expensive category (total expenditure):", drug_expenditure_summary_beta_lac$medication_beta_lac[1], 
    "- £", round(drug_expenditure_summary_beta_lac$beta_lac_total_expenditure[1], 2), "\n")
cat("Highest cost per dose:", 
    drug_expenditure_summary_beta_lac$medication_beta_lac[which.max(drug_expenditure_summary_beta_lac$beta_lac_mean_cost_per_dose)],
    "- £", round(max(drug_expenditure_summary_beta_lac$beta_lac_mean_cost_per_dose, na.rm = TRUE), 2), "\n")
cat("Most frequently prescribed:", 
    drug_expenditure_summary_beta_lac$medication_beta_lac[which.max(drug_expenditure_summary_beta_lac$beta_lac_total_orders)],
    "- ", max(drug_expenditure_summary_beta_lac$beta_lac_total_orders), " orders\n")

# route specific analysis  --------------------------------------
drug_expenditure_summary_route_recoded <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total|> 
  group_by(route_of_administration_recoded) |> 
  summarise(
    route_recoded_total_orders = n(),
    route_recoded_mean_cost_per_dose = mean(cost_per_dose_average, na.rm = TRUE),
    route_recoded_mean_cost_per_day = mean(cost_per_day_average, na.rm = TRUE),
    route_recoded_total_expenditure = sum(total_cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(route_recoded_total_expenditure)) |> 
  filter(!route_recoded_total_expenditure == 0) 

drug_expenditure_summary_route_recoded


# Print summary table
print("Drug Expenditure Summary, by category:")
print(drug_expenditure_summary_route_recoded, n = 100)

# 1. Total Expenditure by Drug (Bar Chart)
p1_route_recoded <- drug_expenditure_summary_route_recoded %>%
  ggplot(aes(x = route_of_administration_recoded, y = route_recoded_total_expenditure)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  labs(
    title = "Total Drug Expenditure by Category",
    subtitle = "Ordered by total expenditure (highest to lowest)",
    x = NULL,
    y = "Total Expenditure (£)",
    caption = "Expenditure data for antimicrobials where cost-per-day is available"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

# 2. Mean Cost per Dose vs Number of Orders (Scatter Plot)
p2_route_recoded <- drug_expenditure_summary_route_recoded %>%
  ggplot(aes(x = route_recoded_total_orders, y = route_recoded_mean_cost_per_dose)) +
  geom_point(aes(size = route_recoded_total_expenditure), alpha = 0.7, color = "darkred") +
  geom_text(aes(label = ifelse(route_recoded_mean_cost_per_dose > 1 | route_recoded_total_orders > 500, 
                               route_of_administration_recoded, "")), 
            vjust = -0.5, hjust = 0.5, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  scale_size_continuous(name = "Total\nExpenditure (£)", 
                        labels = dollar_format(prefix = "£")) +
  labs(
    title = "Mean Cost per Dose vs Number of Orders",
    subtitle = "Point size represents total expenditure per category",
    x = "Total Number of Orders",
    y = "Mean Cost per Dose (£)",
    caption = "Labels shown for high-cost or high-volume drugs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

# 3. Top 10 Drugs by Total Expenditure (Horizontal Bar Chart)
p3_route_recoded <- drug_expenditure_summary_route_recoded %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = route_of_administration_recoded, y = route_recoded_total_expenditure)) +
  geom_col(fill = "forestgreen", alpha = 0.8) +
  geom_text(aes(label = paste0("£", round(route_recoded_total_expenditure, 2))), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£"), 
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Total Expenditure by Category",
    x = NULL,
    y = "Total Expenditure (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Display all plots
print(p1_route_recoded)
print(p2_route_recoded)
print(p3_route_recoded)

# Additional summary statistics
cat("\n=== KEY INSIGHTS ===\n")
cat("Total number of distinct categories prescribed:", nrow(drug_expenditure_summary_route_recoded), "\n")
cat("Total expenditure across all categories: £", round(sum(drug_expenditure_summary_route_recoded$route_recoded_total_expenditure), 2), "\n")
cat("Most expensive category (total expenditure):", drug_expenditure_summary_route_recoded$route_of_administration_recoded[1], 
    "- £", round(drug_expenditure_summary_route_recoded$route_recoded_total_expenditure[1], 2), "\n")

total_expenditure_print <- as.character(total_expenditure)

total_expenditure_iv <- round(drug_expenditure_summary_route_recoded$route_recoded_total_expenditure[1], 2)

total_expenditure_iv_print <- as.character(total_expenditure_iv)

total_expenditure_iv_perc <- round(total_expenditure_iv / total_expenditure * 100, 1)

total_expenditure_iv_perc

total_expenditure_iv_perc_print <- as.character(total_expenditure_iv_perc)

cat("Highest cost per dose:", 
    drug_expenditure_summary_route_recoded$route_of_administration_recoded[which.max(drug_expenditure_summary_route_recoded$route_recoded_mean_cost_per_dose)],
    "- £", round(max(drug_expenditure_summary_route_recoded$route_recoded_mean_cost_per_dose, na.rm = TRUE), 2), "\n")
cat("Most frequently prescribed:", 
    drug_expenditure_summary_route_recoded$route_of_administration_recoded[which.max(drug_expenditure_summary_route_recoded$route_recoded_total_orders)],
    "- ", max(drug_expenditure_summary_route_recoded$route_recoded_total_orders), " orders\n")

# expenditure by ward --------------------------------------
drug_expenditure_summary_ward <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total|> 
  group_by(current_ward) |> 
  summarise(
    ward_total_orders = n(),
    ward_mean_cost_per_dose = mean(cost_per_dose_average, na.rm = TRUE),
    ward_mean_cost_per_day = mean(cost_per_day_average, na.rm = TRUE),
    ward_total_expenditure = sum(total_cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(ward_total_expenditure)) |> 
  filter(!ward_total_expenditure == 0) 

drug_expenditure_summary_ward

drug_expenditure_table_ward <- drug_expenditure_summary_ward |>
  flextable() |>
  set_header_labels(
    order_name_c_amr = "Drug Name",
    ward_total_orders = "Total Orders",
    ward_mean_cost_per_dose = "Mean Cost per Dose (£)",
    ward_mean_cost_per_day = "Mean Cost per Day (£)",
    ward_total_expenditure = "Total Expenditure (£)"
  ) |>
  # Format numeric columns for better readability
  colformat_double(
    j = c("ward_total_expenditure", "ward_mean_cost_per_dose", "ward_mean_cost_per_day"),
    digits = 2,
    big.mark = ","
  ) |>
  colformat_int(
    j = "ward_total_orders",
    big.mark = ","
  ) |>
  # Add styling
  theme_vanilla() |>
  autofit() |>
  # Make header bold
  bold(part = "header") |>
  # Align numeric columns to the right
  align(j = c("ward_mean_cost_per_dose", "ward_mean_cost_per_day", "ward_total_orders", "ward_total_expenditure"), 
        align = "right", part = "all")

# drug_expenditure_table_ward

# Print summary table
print("Drug Expenditure Summary, by ward:")
print(drug_expenditure_summary_ward, n = 100)

# 1. Total Expenditure by Drug (Bar Chart)
p1_ward <- drug_expenditure_summary_ward %>%
  mutate(current_ward = fct_reorder(current_ward, ward_total_expenditure)) %>%
  ggplot(aes(x = current_ward, y = ward_total_expenditure)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  labs(
    title = "Total Drug Expenditure by Ward",
    subtitle = "Ordered by total expenditure (highest to lowest)",
    x = NULL,
    y = "Total Expenditure (£)",
    caption = "Expenditure data for antimicrobials where cost-per-day is available"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

# 2. Mean Cost per Dose vs Number of Orders (Scatter Plot)
p2_ward <- drug_expenditure_summary_ward %>%
  ggplot(aes(x = ward_total_orders, y = ward_mean_cost_per_dose)) +
  geom_point(aes(size = ward_total_expenditure), alpha = 0.7, color = "darkred") +
  geom_text(aes(label = ifelse(ward_mean_cost_per_dose > 1 | ward_total_orders > 500, 
                               current_ward, "")), 
            vjust = -0.5, hjust = 0.5, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  scale_size_continuous(name = "Total\nExpenditure (£)", 
                        labels = dollar_format(prefix = "£")) +
  labs(
    title = "Mean Cost per Dose vs Number of Orders",
    subtitle = "Point size represents total expenditure per ward",
    x = "Total Number of Orders",
    y = "Mean Cost per Dose (£)",
    caption = "Labels shown for high-cost or high-volume wards"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

# 3. Top 10 Drugs by Total Expenditure (Horizontal Bar Chart)
p3_ward <- drug_expenditure_summary_ward %>%
  slice_head(n = 10) %>%
  mutate(current_ward = fct_reorder(current_ward, ward_total_expenditure)) %>%
  ggplot(aes(x = current_ward, y = ward_total_expenditure)) +
  geom_col(fill = "forestgreen", alpha = 0.8) +
  geom_text(aes(label = paste0("£", round(ward_total_expenditure, 2))), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£"), 
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 10 Wards by Total Expenditure",
    x = NULL,
    y = "Total Expenditure (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Display all plots
print(p1_ward)
print(p2_ward)
print(p3_ward)

# Additional summary statistics
cat("\n=== KEY INSIGHTS ===\n")
cat("Total number of distinct wards:", nrow(drug_expenditure_summary_ward), "\n")
cat("Total expenditure across all wards: £", round(sum(drug_expenditure_summary_ward$ward_total_expenditure), 2), "\n")
cat("Most expensive ward (total expenditure):", drug_expenditure_summary_ward$current_ward[1], 
    "- £", round(drug_expenditure_summary_ward$ward_total_expenditure[1], 2), "\n")
cat("Highest cost per dose:", 
    drug_expenditure_summary_ward$current_ward[which.max(drug_expenditure_summary_ward$ward_mean_cost_per_dose)],
    "- £", round(max(drug_expenditure_summary_ward$ward_mean_cost_per_dose, na.rm = TRUE), 2), "\n")
cat("Most frequently prescribed:", 
    drug_expenditure_summary_ward$current_ward[which.max(drug_expenditure_summary_ward$ward_total_orders)],
    "- ", max(drug_expenditure_summary_ward$ward_total_orders), " orders\n")



# 8 Basic analysis ----------------------------------------------------------

#date range
data_antimicrobial_pool_iv_im_po_not_prophy_scope |> 
  reframe(date_range  = range(start_date_time, na.rm = TRUE))

#number of orders
nrow(data_antimicrobial_pool_iv_im_po_not_prophy_scope |> 
       distinct(order_id))

#number of patients
nrow(data_antimicrobial_pool_iv_im_po_not_prophy_scope |> 
       distinct(mrn))

#range of number of prescriptions per patient --------
n_prescriptions_per_patient <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  distinct(mrn, order_id, .keep_all = TRUE) |> 
  group_by(mrn) |> 
  count() |> 
  arrange(desc(n)) |> 
  ungroup()

#print 
n_prescriptions_per_patient

#work our range min/max 
#max
n_prescriptions_per_patient |> 
  reframe(range = range(n)) |> 
  pluck(1,2)
#min
n_prescriptions_per_patient |> 
  reframe(range = range(n)) |> 
  pluck(1,1)

ton_n_prescriptions_per_patient <- n_prescriptions_per_patient |> 
  pluck(1,1)

data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total |> 
  distinct(order_id, .keep_all = TRUE) |> 
  filter(mrn == ton_n_prescriptions_per_patient) |> 
  select(mrn, order_id, order_name_c_amr, frequency, route_of_administration_recoded, start_date_time) |> 
  print(n = 100)


# Calculate drug expenditure summary by order_name_c_amr, stratified by route
drug_expenditure_summary_route <- data_antimicrobial_pool_iv_im_po_not_prophy_scope_costed_total %>%
  group_by(order_name_c_amr, route_of_administration_recoded) %>%
  summarise(
    total_orders = n(),
    mean_cost_per_dose = mean(cost_per_dose_average, na.rm = TRUE),
    mean_cost_per_day = mean(cost_per_day_average, na.rm = TRUE),
    total_expenditure = sum(total_cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_expenditure)) |> 
  filter(!total_expenditure == 0) 

# drug_expenditure_summary_route |>
#   glimpse()

# Create flextable with improved column names ---
drug_expenditure_table_route <- drug_expenditure_summary_route |>
  flextable() |>
  set_header_labels(
    order_name_c_amr = "Drug Name",
    route_of_administration_recoded = "IV/PO",
    total_orders = "Total Orders",
    mean_cost_per_dose = "Cost per Dose (£)",
    mean_cost_per_day = "Cost per Day (£)",
    total_expenditure = "Total Expenditure (£)"
  ) |>
  # Format numeric columns for better readability
  colformat_double(
    j = c("total_expenditure", "mean_cost_per_dose", "mean_cost_per_day"),
    digits = 2,
    big.mark = ","
  ) |>
  colformat_int(
    j = "total_orders",
    big.mark = ","
  ) |>
  # Add styling
  theme_vanilla() |>
  autofit() |>
  # Make header bold
  bold(part = "header") |>
  # Align numeric columns to the right
  align(j = c("mean_cost_per_dose", "mean_cost_per_day", "total_orders", "total_expenditure"), 
        align = "right", part = "all")

# Display the table
drug_expenditure_table_route

# Print summary table
print("Drug Expenditure Summary, by Route:")
print(drug_expenditure_summary_route, n = 100)

# 1. Total Expenditure by Drug (Bar Chart)
p1_route <- drug_expenditure_summary_route %>%
  mutate(order_name_c_amr = fct_reorder(order_name_c_amr, total_expenditure)) %>%
  ggplot(aes(x = order_name_c_amr, y = total_expenditure, fill = route_of_administration_recoded)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£")) +
  labs(
    title = "Total Drug Expenditure by Antimicrobial",
    subtitle = "Ordered by total expenditure (highest to lowest)",
    x = NULL,
    y = "Total Expenditure (£)",
    caption = "Expenditure data for antimicrobials where cost-per-day is available"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

# 3. Top 10 Drugs by Total Expenditure (Horizontal Bar Chart)
p3_route <- drug_expenditure_summary_route %>%
  slice_head(n = 20) %>%
  mutate(order_name_c_amr = fct_reorder(order_name_c_amr, total_expenditure)) %>%
  ggplot(aes(x = order_name_c_amr, y = total_expenditure, fill = route_of_administration_recoded)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0("£", round(total_expenditure, 2))), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "£"), 
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 20 Antimicrobials by Total Expenditure",
    x = NULL,
    y = "Total Expenditure (£)"
  ) +
  guides(fill = guide_legend("Route")) + # Set fill guide to FALSE to remove legend
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Display all plots
print(p1_route)
print(p3_route)

