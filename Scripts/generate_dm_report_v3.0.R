#functions

generate_dm_report <- function(session, input, output, all_data, progress = NULL) {
  
  if (!is.null(progress)) progress$set(value = 0, detail = "Initializing report...")
  
  descriptions_variables <- variables[["Descriptions"]]
  tab_descr_list <- list()
  for (i in 1:nrow(descriptions_variables)) {
    tab_descr_list[[paste0("tab", i, "_desc")]] <- descriptions_variables %>%
      filter(TableNo == i) %>%
      pull(TableDescription)
  }
  print("variables read")
  use_table <- descriptions_variables %>%
    filter(use == "Yes") %>%
    pull(TableNo)
  
  use_standard_code <- descriptions_variables %>%
    filter(use_general_code == "Yes") %>%
    pull(TableNo)
  
  ###
  
  my_doc <- read_docx(path = "CDL_ roboczy papier firmowy.docx")
  
  
  my_doc <- body_add_par(my_doc, "Data Management Report" , style = "Title")
  my_doc <- body_add_par(my_doc, " ")
  my_doc <- body_add_par(my_doc, paste("Study name:", study_name_dm) , style = "Subtitle")
  # my_doc <- body_add_par(my_doc, paste("Data export date:", export_date) , style = "HEADER_2")
  my_doc <- body_add_par(my_doc, paste("Report creation date:", Sys.Date()) , style = "Subtitle")
  my_doc <- body_add_par(my_doc, " ")
  my_doc <- body_add_toc(my_doc, level = 2, pos = "after", separator = ";")
  my_doc <- body_add_break(my_doc)
  
  print("mydoc gotowy")
  
  ###
  
  scripts_dir <- "445_SHINY_REPOS/DM_Reports/"
  
  for (row in 1:nrow(descriptions_variables)) {
    current_table_options <- descriptions_variables[row, ]
    print(current_table_options)
      if (current_table_options$use_general_code == "Yes") {
        print("wybrano domyślny kod")
        temp_file <- tempfile(fileext = ".R")
        # Get the script name for the current row
        script_name <- script_versions$ScriptVersion[script_versions$TableNo == row]
        
        # Add ".R" extension
        script_file <- paste0("SCRIPTS/", script_name, ".R")
        
        # Download and source
        drv$download_file(paste0(scripts_dir, script_file), dest = temp_file)
        source(temp_file)
      } else {                                                
        print(paste("wybrano kod", descriptions_variables$script_path[row]))
        temp_file <- tempfile(fileext = ".R")
        drv$download_file(paste0(scripts_dir, descriptions_variables$script_path[row]), dest = temp_file)
        source(temp_file)
      }
  }
  
  print("skrypty wczytane")
  
  # Step 2: Generate Tables
  
  # Check if each function exists and create the list
  

  
  
  # for (i in use_table) {
  #   print(paste("przygotowanie tabeli", i))
  #   current_function <- get(paste0("tab", i, "_fun"))
  #   my_doc <- current_function(variables, all_data, my_doc, descriptions_variables[i, "TableDescription"])
  # }
  
  num_tables <- length(use_table)  # Total number of tables
  
  for (idx in seq_along(use_table)) {
    i <- use_table[idx]
    
    print(paste("Generating table", i))
    
    # Call the table function dynamically
    current_function <- get(paste0("tab", i, "_fun"))
    my_doc <- current_function(variables, all_data, my_doc, descriptions_variables[i, "TableDescription"])
    
    # Update progress bar
    if (!is.null(progress)) {
      progress_value <- 0.1 + (0.8 * idx / num_tables)  # Scale progress from 10% to 90%
      progress$set(value = progress_value, detail = paste("Table", i, "generated"))
    }
  }
  
  Sys.sleep(1)  # Simulating finalizing report
  if (!is.null(progress)) progress$set(value = 1, detail = "Report complete!")
  
  
  
  print("wydrukowany raport")
  
  return(my_doc)  # Path to the generated report
}

# variables_data <- "TEMPLATES/SUPRASORB.xlsx"
# all_data <- read_excel("Data/export-20240115-114559_ALL_DATA_SUPRASORB.xlsx")
# study_name <- "SUPRASORB"
# study_name_dm <- "SUPRASORB"
# 
# dm_report <- generate_dm_report(variables_data, all_data)
# 
# print(dm_report, "report_test.docx")
# 
# print(my_doc, "report_test.docx")
