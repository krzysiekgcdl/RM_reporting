tab3_fun <- function(variables, all_data, my_doc, section_header_name) {
  # Extract relevant variables for Table 3
  dt3_variables <- variables[["EOS"]] %>% 
    filter(TableNo == "3")
  
  # Identify columns needed
  columns_needed <- dt3_variables$ExportTag
  
  # Identify key variables
  subject_completed_entire_course <- dt3_variables %>%
    filter(subject_completed_entire_course == "Yes") %>%
    pull(ExportTag)
  
  reason_for_premature_termination <- dt3_variables %>%
    filter(reason_for_premature_termination == "Yes") %>%
    pull(ExportTag)
  
  # Subset relevant data
  data_tab3 <- all_data[is.na(all_data$Reference), c("PatientNr", "Site", columns_needed)]
  
  # Rename selected columns
  data_tab3 <- data_tab3 %>%
    rename("subject_completed_entire_course" = all_of(subject_completed_entire_course),
           "reason_for_premature_termination" = all_of(reason_for_premature_termination))
  
  # Create Listing 3 (Filtered data)
  data_listing3 <- data_tab3 %>% filter(!is.na(subject_completed_entire_course))
  
  # Adjust data types
  data_listing3 <- data_listing3 %>%
    mutate(across(all_of(dt3_variables$ExportTag[dt3_variables$ValueIncluded == "Date"]), as.character)) %>%
    mutate(across(all_of(dt3_variables$ExportTag[dt3_variables$ValueIncluded == "Time"]), ~ substr(as.character(.), 12, 16)))
  
  # Keep only necessary columns for summary table
  data_tab3 <- data_tab3[, c("PatientNr", "Site", "subject_completed_entire_course", "reason_for_premature_termination")]
  
  # Assign variable labels
  var_label(data_tab3$subject_completed_entire_course) <- dt3_variables %>% 
    filter(subject_completed_entire_course == "Yes") %>% 
    pull(ColumnName)
  
  var_label(data_tab3$reason_for_premature_termination) <- dt3_variables %>% 
    filter(reason_for_premature_termination == "Yes") %>% 
    pull(ColumnName)
  
  ####### Table 3 Summary
  tab_3 <- tbl_summary(data = data_tab3,
                       include = c("subject_completed_entire_course", "reason_for_premature_termination"),
                       by = 'Site',
                       type = all_continuous() ~ "continuous2",
                       statistic = all_continuous() ~ c("{N_nonmiss}"),
                       missing = "no")
  
  # Formatting Table 3
  tab_3 <- add_overall(tab_3, col_label = "**Overall**")
  tab_3 <- bold_labels(tab_3)
  tab_3 <- modify_footnote(tab_3, all_stat_cols() ~ NA)
  tab_3 <- as_flex_table(tab_3)
  tab_3  <- bold(tab_3, bold = TRUE, part = "header")
  tab_3 <- add_header_row(tab_3, values = "End of Study", colwidths = length(unique(data_tab3$Site)) + 2)
  tab_3 <- fontsize(tab_3, size = 9, part = "body")
  tab_3 <- fontsize(tab_3, size = 10, part = "header")
  
  ####### Listing 3 (Detailed Table)
  colnames(data_listing3) <- c("Subject No", "Site", dt3_variables$ColumnName)
  
  listing_3 <- flextable(data_listing3)
  listing_3 <- fontsize(listing_3, i = NULL, j = 1:(length(dt3_variables$ColumnName) + 2), size = 8, part = "body")
  listing_3 <- fontsize(listing_3, i = NULL, j = 1:(length(dt3_variables$ColumnName) + 2), size = 9, part = "header")
  listing_3 <- theme_box(listing_3)
  listing_3 <- width(listing_3, j = c(1:(length(dt3_variables$ColumnName) + 2)), width = c(1, 1, rep(1, 8)))
  
  
  my_doc <- body_add_par(my_doc, section_header_name, style = "heading 1")
  my_doc <- body_add_par(my_doc, "EOS", style = "heading 2")
  my_doc <- body_add_flextable(my_doc, tab_3)
  my_doc <- body_add_par(my_doc, " ")
  my_doc <- body_add_par(my_doc, "EOS listing", style = "heading 2")
  my_doc <- body_add_flextable(my_doc, listing_3)
  my_doc <- body_add_break(my_doc, pos = "after")
  
  # Return both tables
  # return(list(tab3 = tab_3, listing3 = listing_3))
  return(my_doc)
}


