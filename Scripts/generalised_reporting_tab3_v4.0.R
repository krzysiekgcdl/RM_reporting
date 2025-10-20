tab3_fun <- function(variables, all_data, my_doc, section_header_name) {
  # Extract relevant variables
  # === Step 1: Get EOS variables for Table 3 ===
  dt3_variables <- variables[["EOS"]] %>% 
    filter(TableNo == 3)
  
  # === Step 2: Handle duplicated ExportTags ===
  duplicated_vars <- dt3_variables %>%
    group_by(ExportTag) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Duplicate and rename in all_data and update dt3_variables
  for (i in seq_len(nrow(duplicated_vars))) {
    original <- duplicated_vars$ExportTag[i]
    new_name <- make.names(duplicated_vars$ColumnName[i])  # Safe name
    
    if (!new_name %in% names(all_data)) {
      all_data[[new_name]] <- all_data[[original]]
      
      # Update ExportTag in dt3_variables
      dt3_variables$ExportTag[dt3_variables$ColumnName == duplicated_vars$ColumnName[i]] <- new_name
    }
  }
  
  # === Step 3: Rebuild variable lists AFTER renaming ===
  summary_vars_binary <- dt3_variables %>%
    filter(part_of_summary == "Yes", ValueIncluded == "Yes") %>%
    pull(ExportTag) %>%
    unique()
  
  summary_vars_multi <- dt3_variables %>%
    filter(part_of_summary == "Yes", ValueIncluded == "Single Answer") %>%
    pull(ExportTag) %>%
    unique()
  
  summary_vars <- c(summary_vars_binary, summary_vars_multi)
  
  listing_vars <- dt3_variables %>%
    filter(part_of_listing == "Yes") %>%
    pull(ExportTag) %>%
    unique()
  
  columns_needed <- unique(c(summary_vars, listing_vars))
  
  # === Step 4: Subset data from all_data ===
  data_tab3 <- all_data[is.na(all_data$Reference), c("PatientNr", "Site", columns_needed)]
  
  # === Step 5: Prepare Summary Data ===
  data_tab3_summary <- data_tab3 %>%
    select(PatientNr, Site, all_of(summary_vars)) %>%
    mutate(across(all_of(summary_vars_binary), ~ factor(., levels = c("Yes", "No")))) %>%
    mutate(across(all_of(summary_vars_multi), as.factor))
  
  # === Step 6: Apply variable labels ===
  for (var in summary_vars) {
    label <- dt3_variables %>%
      filter(ExportTag == var) %>%
      pull(ColumnName) %>%
      unique() %>%
      .[1]
    
    var_label(data_tab3_summary[[var]]) <- label
  }
  
  # === Step 7: Generate Summary Table ===
  if (all(is.na(data_tab3_summary[, summary_vars]))) {
    my_doc <- body_add_par(my_doc, section_header_name, style = "heading 1")
    my_doc <- body_add_par(my_doc, "EOS", style = "heading 2")
    my_doc <- body_add_par(my_doc, "No end of study data")
    my_doc <- body_add_break(my_doc, pos = "after")
  } else {
    tab_3 <- tbl_summary(
      data = data_tab3_summary,
      by = 'Site',
      include = all_of(summary_vars),
      missing = "no"
    ) %>%
      add_overall(col_label = "**Overall**") %>%
      bold_labels() %>%
      modify_footnote(all_stat_cols() ~ NA) %>%
      as_flex_table() %>%
      bold(part = "header") %>%
      add_header_row(values = "End of Study", colwidths = length(unique(data_tab3_summary$Site)) + 2) %>%
      fontsize(size = 9, part = "body") %>%
      fontsize(size = 10, part = "header")
    
    # === Step 8: Prepare Listing Table ===
    data_tab3_listing <- data_tab3 %>%
      filter(!if_all(all_of(listing_vars), is.na)) %>%
      select(PatientNr, Site, all_of(listing_vars))
    
    listing_column_names <- dt3_variables %>%
      filter(ExportTag %in% listing_vars) %>%
      arrange(match(ExportTag, listing_vars)) %>%
      pull(ColumnName)
    
    colnames(data_tab3_listing) <- c("Subject No", "Site", listing_column_names)
    
    listing_3 <- flextable(data_tab3_listing) %>%
      fontsize(i = NULL, j = ~., size = 8, part = "body") %>%
      fontsize(i = NULL, j = ~., size = 9, part = "header") %>%
      theme_box()
    
    # === Step 9: Add Everything to Word Doc ===
    my_doc <- body_add_par(my_doc, section_header_name, style = "heading 1")
    my_doc <- body_add_par(my_doc, "EOS", style = "heading 2")
    my_doc <- body_add_flextable(my_doc, tab_3)
    my_doc <- body_add_par(my_doc, " ")
    my_doc <- body_add_par(my_doc, "EOS listing", style = "heading 2")
    my_doc <- body_add_flextable(my_doc, listing_3)
    my_doc <- body_add_break(my_doc, pos = "after")
  }
  
  # === Final output ===
  return(my_doc)
  
  
}