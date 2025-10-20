tab1_fun <- function(variables, all_data, my_doc, section_header_name) {

  # Filter Recruitment table variables
  dt1_variables <- variables[["Recruitment"]] %>%
    filter(TableNo == "1")
  
  # Extract required columns
  columns_needed <- dt1_variables$ExportTag
  
  # Select relevant data
  data_tab1 <- all_data[is.na(all_data$Reference), c("PatientNr", "Site", columns_needed)]
  
  # Process "Is not empty" columns
  isnotempty_columns <- dt1_variables %>%
    filter(ValueIncluded == "Is not empty") %>%
    pull(ExportTag)
  
  names(data_tab1) <- make.unique(names(data_tab1))
  
  data_tab1 <- data_tab1 %>%
    mutate(across(all_of(isnotempty_columns), ~ ifelse(!is.na(.), "Is not empty", NA)))
  
  # Convert into long format
  results_list <- lapply(seq_along(columns_needed), function(i) {
    temp_df <- data_tab1 %>%
      mutate(Status = dt1_variables$ColumnName[i]) %>%
      select(PatientNr, Site, dt1_variables$ExportTag[i], Status) %>%
      filter(.[[3]] == dt1_variables$ValueIncluded[i]) %>%
      select(1:2, 4)
    
    return(temp_df)
  })
  
  # Combine results into one dataframe
  data_tab1 <- bind_rows(results_list)
  
  # Convert Status column into a factor
  factors <- dt1_variables$ColumnName
  data_tab1$Status <- factor(data_tab1$Status, levels = factors, labels = factors)
  
  # Create All_Site column
  data_tab1$All_Site <- ifelse(is.na(data_tab1$Site), 999, 1)
  data_tab1$All_Site <- "N (%)"
  var_label(data_tab1$All_Site) <- "Overall"
  
  # Generate Summary Tables
  tab_1 <- tbl_summary(
    data = data_tab1,
    include = c('Site'),
    by = 'Status',
    missing = "no"
  )
  
  tab_1_1 <- tbl_summary(
    data = data_tab1,
    include = c('All_Site'),
    by = 'Status',
    missing = "no"
  )
  
  # Stack tables
  tab_1 <- tbl_stack(list(tab_1, tab_1_1))
  
  # Format table output
  tab_1 <- tab_1 %>%
    modify_header(label = '', all_stat_cols() ~ "**{level}**") %>%
    bold_labels() %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    as_flex_table() %>%
    bold(bold = TRUE, part = "header") %>%
    fontsize(size = 9, part = "body") %>%
    fontsize(size = 10, part = "header") %>%
    width(j = c(1:(length(factors) + 1)), width = c(1, rep(1, length(factors))))
  
  tab_1 <- flextable::font(tab_1, fontname = "Open Sans", part = "all")
  tab_1
  
  my_doc <- body_add_par(my_doc, section_header_name, style = "heading 1")
  my_doc <- body_add_par(my_doc, "Number of subjects with initiated visits", style = "heading 2")
  my_doc <- body_add_flextable(my_doc, tab_1)
  my_doc <- body_add_par(my_doc, " ")
  my_doc <- body_add_break(my_doc, pos = "after")
  
  my_doc <- body_end_section_continuous(my_doc)
  
  return(my_doc)
}
