
# 0 generate reports_date -----------

#i.e. the last Tue of the date of the script being run

reports_date <- lubridate::floor_date(
  lubridate::today(),
  unit = "week",
  week_start = 2
) |> 
  format("%d-%m-%Y") 

# 1 Render reports ----

#ensure that data imported from ICNET first before running ams-mrsa-pos-patients-prescribed-antimicrobials-report.qmd

reports_to_render <- c(
  #"ams-antimicrobial-usage-report.qmd",
  #"ams-gentamicin-prescriptions-report.qmd",
  #"ams-penicillin-allergy-delabelling-report.qmd",
  #"ams-critical-results-report.qmd",
  #"ams-critical-results-and-antimicrobial-therapy-report.qmd",
  #"ams-mrsa-screening-compliance-report.qmd", #24 weeks' MRSA data now imported from DWH
  #"ams-mrsa-pos-patients-prescribed-antimicrobials-report.qmd", #24 weeks' MRSA data now imported from DWH
  #"ams-cpe-report.qmd", #this report currently takes a while to run as it imports data from APEX directly; may speed up when data imported from DWH
  "ams-antimicrobial-expenditure-tracking.qmd" #This report aims to analyse antimicrobial expenditure across NUTH
)

lapply(
  reports_to_render, 
  \(x) quarto::quarto_render(
    x, 
    output_format = "html",
    output_file = str_c(
      str_sub(x, end = str_length(x) - 4), 
      " ", 
      reports_date,
      ".html"
    )))


