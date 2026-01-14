# 0 Load required packages ------------------------------------------------

library(Microsoft365R)
library(blastula)
library(dotenv)
library(here)

# 1 Import functions and mailing list data --------------------------------

load_dot_env()

source(here("01_src", "functions.R"))

# Load three password-protected Excel sheets
data_mailing_list <- open_excel_password_prot(
  data = "report_mailing_lists.xlsx",
  data_folder_name = "",
  sheet = "report_mailing_list",
  password = "MAILING_LISTS_KEY"
)

data_senders <- open_excel_password_prot(
  data = "report_mailing_lists.xlsx",
  data_folder_name = "",
  sheet = "senders",
  password = "MAILING_LISTS_KEY"
)

data_report_description_text <- open_excel_password_prot(
  data = "report_mailing_lists.xlsx",
  data_folder_name = "",
  sheet = "report_desc_text",
  password = "MAILING_LISTS_KEY"
)

# 1.1 Clean and validate data -----------------------------------------------

# Strategy: Group recipients who need the same reports together. Create one email 
# per group with only their required reports attached. This ensures each person 
# receives just one email.

# Mailing list validation rules:
# - Remove rows with missing email addresses
# - Remove rows with missing values in any report column
# - Remove rows where no reports are selected

data_valid_mailing_list <- data_mailing_list |> 
  filter(!is.na(email_address)) |>
  pivot_longer(
    cols = starts_with("report"),
    names_to = "report",
    names_prefix = "report_",
    values_to = "include"
  ) |> 
  group_by(surname, forename, email_address) |> 
  mutate(count_included_reports = sum(include)) |> 
  ungroup() |> 
  filter(!is.na(count_included_reports) & count_included_reports != 0) |>
  pivot_wider(
    names_from = "report",
    names_prefix = "report_",
    values_from = "include"
  ) 

# Senders validation rules:
# - Remove rows with missing email addresses
# - Remove rows with missing values in any report column
# - Remove rows where no reports are selected
# - Keep senders with missing names but valid emails (they won't be included in sign-off)

data_valid_senders <- data_senders |> 
  filter(!is.na(email_address)) |>
  pivot_longer(
    cols = starts_with("report"),
    names_to = "report",
    names_prefix = "report_",
    values_to = "include"
  ) |> 
  group_by(surname, forename, email_address) |> 
  mutate(count_included_reports = sum(include)) |> 
  ungroup() |> 
  filter(!is.na(count_included_reports) & count_included_reports != 0) |>
  pivot_wider(
    names_from = "report",
    names_prefix = "report_",
    values_from = "include"
  )

# Report description validation rules:
# - Only include reports that have a Quarto markdown file recorded
# - Reports without a description can still be attached (but won't have intro text)

data_valid_report_desc_text <- data_report_description_text |> 
  filter(!is.na(project_file_name_qmd))

# 2 Organize recipients into mailing groups --------------------------------

# Assign each unique combination of selected reports to a group number
data_valid_mailing_list_grouped <- data_valid_mailing_list |> 
  group_by(across(starts_with("report_"))) |> 
  mutate(mail_group_number = cur_group_id()) |> 
  ungroup()

# Find the unique report combinations per group
data_valid_reports_to_attach_per_group <- data_valid_mailing_list_grouped |> 
  distinct(across(starts_with("report")), mail_group_number)

# Build list of reports and descriptions for each mailing group
data_bullet_list_specification <-
  data_valid_reports_to_attach_per_group |> 
  glimpse() |> 
  pivot_longer(
    cols = starts_with("report_"),
    names_to = "report",
    names_prefix = "report_",
    values_to = "include"
  ) |> 
  filter(include) |> 
  inner_join(data_valid_report_desc_text, join_by(report)) |> 
  arrange(mail_group_number, report)

# Count total unique emails needed
total_unique_emails_required <- data_bullet_list_specification |> 
  distinct(mail_group_number) |> 
  nrow()

# 3 Build email content -------------------------------------------------------

# 3.1 Create bullet list with report links and descriptions ------------------

data_full_email_bullet_text_and_attachments <- 
  data_bullet_list_specification |> 
  mutate(
    # Format each report as a markdown bullet point with link and description
    email_bullet_text_md = str_c(
      "-\t**[",
      email_body_link_label,
      "](",
      project_file_name_qmd,
      ")** : ",
      email_body_description_md,
      "\n\n"
    ),
    # Generate filename with current week date
    full_report_file_name = str_c(
      project_file_name_qmd,
      " ",
      lubridate::floor_date(
        lubridate::today(),
        unit = "week",
        week_start = 2
      ) |> 
        format("%d-%m-%Y"),
      ".html"
    )
  ) |> 
  group_by(mail_group_number) |> 
  summarise(
    full_email_bullet_text_md = str_flatten(email_bullet_text_md),
    full_report_file_name_list = list(full_report_file_name)
  ) |> 
  ungroup()

# 3.2 Determine who signs off each email -----------------------------------

data_senders_per_group <- 
  data_valid_reports_to_attach_per_group |>
  pivot_longer(
    cols = starts_with("report_"),
    names_to = "report",
    names_prefix = "report_",
    values_to = "include"
  ) |>
  filter(include) |> 
  left_join(
    {
      data_valid_senders |>
        pivot_longer(
          cols = starts_with("report_"),
          names_to = "report",
          names_prefix = "report_",
          values_to = "include"
        ) |>
        filter(include)
    },
    join_by(report, include),
    relationship = "many-to-many"
  ) |> 
  distinct(mail_group_number, surname, forename, email_address) |> 
  arrange(mail_group_number, forename, surname)

# Format sender names and create sign-off text
data_cc_and_sign_off_names <- 
  data_senders_per_group |> 
  mutate(
    surname = str_to_title(surname),
    full_name = str_c(forename, " ", surname)
  ) |> 
  select(-surname, -forename) |> 
  group_by(mail_group_number) |> 
  summarise(
    carbon_copy_list = list(email_address),
    sign_off_names_list_md = str_flatten(full_name, collapse = "\\; ")
  ) |> 
  ungroup()

# 3.3 Combine all email data for each group --------------------------------

data_inputs_per_mail_group <- 
  data_full_email_bullet_text_and_attachments |> 
  left_join(data_cc_and_sign_off_names, join_by(mail_group_number)) |> 
  left_join(
    {
      data_valid_mailing_list_grouped |> 
        arrange(mail_group_number, forename, surname) |> 
        select(mail_group_number, email_address) |> 
        group_by(mail_group_number) |> 
        summarise(to_list = list(email_address)) |> 
        ungroup()
    },
    join_by(mail_group_number)
  )

# Verify each mailing group has exactly one row of email inputs
if(nrow(filter(count(data_inputs_per_mail_group, mail_group_number), n != 1)) != 0) {
  stop("Error: Each mailing group should have exactly one email setup. Please review the data.")
}

# 3.4 Add standard email text sections ------------------------------------

data_inputs_per_mail_group_final <- 
  data_inputs_per_mail_group |> 
  mutate(
    # Opening message
    email_body_intro_md = "Dear all,\n\nThe latest reports are now available on \n\n1) [SharePoint](https://nhs.sharepoint.com/sites/msteams_c1e724/NUTH%20Data%20Science%20Intranet%20Site/) \n\nIf you don't already have access, click on 'Request access'\n\n",
    
    # Main content with access instructions
    # email_body_text_md = "<b>If upon following the link you see a Windows Security prompt, it means you do not yet have access to the site.</b> Access is organised through Active Directory (AD) groups configured by IT. If a colleague requires access, please have them submit a request through the IT Services Portal [here](https://nhsnewcastle.service-now.com/sp) to be added to an appropriate AD group e.g. \"Power BI - Labs AD group\" for anyone working in the labs.\n\nBelow are links to a few key reports for quick access:\n\n",
    
    # Closing and contact information
    email_body_conclusion_md = "If you have any questions concerning any of the reports, or wish to give general feedback, please do so via [Microsoft Forms](https://forms.cloud.microsoft/e/gNygyENWDd).\n\n",
    
    # Sign-off line
    email_body_sign_off_md = "Kind regards,\n\n"
  ) |> 
  unite(
    email_body_section_01_md,
    email_body_intro_md,
    sep = ""
  ) |> 
  unite(
    email_body_section_02_md,
    #email_body_text_md,
    full_email_bullet_text_md,
    email_body_conclusion_md,
    email_body_sign_off_md,
    sign_off_names_list_md,
    sep = ""
  ) |> 
  arrange(mail_group_number)

# 4 Compose email messages -----------------------------------------------

# Note: tibble columns cannot store email objects, so we build emails in a separate list

composed_email_list <- map2(
  data_inputs_per_mail_group_final |> pull(email_body_section_01_md),
  data_inputs_per_mail_group_final |> pull(email_body_section_02_md),
  \(x, y) compose_email(
    header = blocks(
      block_articles(
        block_title(title = "NUTH Data Reports"),
        article(image = here("logo", "full-colour-logo-2.jpg"))
      )
    ),
    body = blocks(
      block_text(md(x)),
      #article(image = here("email_images", "website-image-for-email.jpg")),
      article(
        content = md("***\n\n> ## <span style =\"color:#F46A25\">**Important notice**\n\n> The reports accessible from this site contain **sensitive clinical and operational data**. Please ensure you adhere to Trust policies and information governance requirements when viewing or discussing these reports. Do not distribute content outside approved channels or to unauthorised individuals.</span>\n\n***")
      ),
      block_text(md(y))
    ),
    footer = "If you no longer require access to the NUTH data science website for any reason e.g. switching roles, please contact us at the earliest opportunity."
  )
)

# 5 Connect to Outlook and add recipients/senders ---------------------------

# NOTE: Requires Azure folder at C:\Users\[username]\AppData\Local\AzureR

outlook_connect <- get_business_outlook()

# Combine emails with their recipient lists
composed_email_with_recipients_and_senders_list <- 
  Map(
    list,
    composed_email_list, 
    data_inputs_per_mail_group_final |> pull(to_list),
    data_inputs_per_mail_group_final |> pull(carbon_copy_list)
  )

# Create draft emails in Outlook with recipients and subject line
created_email_list <- lapply(
  composed_email_with_recipients_and_senders_list,
  \(x) outlook_connect$create_email()$
    set_body(x[[1]]$html_html, content_type = "html")$
    set_subject(
      str_c(
        "NUTH Data Reports ",
        lubridate::floor_date(
          lubridate::today(),
          unit = "week",
          week_start = 2
        ) |> format("%d/%m/%Y")
      )
    )$
    set_recipients(to = x[[2]], cc = x[[3]])
)

# Emails are now available as drafts in Outlook Desktop app

# 6 Send emails ---------------------------------------------------------------

message("IMPORTANT: Check your Drafts folder to review the generated emails before sending.")
