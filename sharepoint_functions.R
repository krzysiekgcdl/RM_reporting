
write_sharepoint_file <- function(data, file_name) {
  temp_file <- tempfile(fileext = ".xlsx")
  write_xlsx(data, temp_file)
  drv$upload_file(temp_file, paste0(STUDY, file_name))
}


read_sharepoint_file <- function(folder, variables_data_name) {
  temp_file <- tempfile(fileext = ".xlsx")
  drv$download_file(paste0(STUDY, folder, "/", variables_data_name), dest = temp_file)
  report_description <- read_excel(temp_file, sheet = "RaportDescription")
  sheets <- as.character(report_description$items_names)
  
  variables_list <- lapply(sheets, function(sheet) {
    read_excel(temp_file, sheet = sheet) %>%
      clean_dataframe()  # CLEANING APPLIED HERE, unsure if applicable to both reads
  })
  
  names(variables_list) <- sheets
  return(variables_list)
}

# read_sharepoint_file_DA <- function(template_name) {
#   temp_file <- tempfile(fileext = ".xlsx")
#   drv$download_file(paste0(STUDY_DA_PATH, template_name), dest = temp_file)
#   
#   # Read Report Description
#   report_description <- read_excel(temp_file, sheet = "RaportDescription")
#   sheets <- as.character(report_description$items_names)
#   
#   # Load all specified sheets into a list and clean each one
#   variables_data <- lapply(sheets, function(sheet) {
#     read_excel(temp_file, sheet = sheet) %>%
#       clean_dataframe()  # CLEANING APPLIED HERE
#   })
#   
#   # Assign sheet names to list elements
#   names(variables_data) <- sheets
#   return(variables_data)
# }