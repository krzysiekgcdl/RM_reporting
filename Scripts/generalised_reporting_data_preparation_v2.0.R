
### Preparing the variables data

################### PREPARE VARIABLES FROM EXCEL FILE ####################

prepare_variables <- function(variables_data) {
  # Read all sheets into a named list
  sheets <- c("Recruitment", "Eligibility", "EOS", "AE", "SAE", "Descriptions")
  variables_list <- lapply(sheets, function(sheet) {
    read_excel(variables_data, sheet = sheet)
  })
  
  # Name the elements of the list based on sheet names
  names(variables_list) <- sheets
  
  return(variables_list)
}

variables <- prepare_variables(variables_data)


# Study Recruitment status (descriptive statistics)
# Subject’s Eligibility status (descriptive statistics + listings)
# End of study status (descriptive statistics + listing)
# Visit dates checking per protocol (descriptive statistics + listing)
# Data aging (descriptive statistics)
# Query Report (descriptive statistics)
# Safety Report (descriptive statistics + listing)

