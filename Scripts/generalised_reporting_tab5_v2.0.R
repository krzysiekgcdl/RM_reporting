tab5_fun <- function(variables, all_data, my_doc, section_header_name) {
  # Extract relevant variables
  variables_list <- variables[["SAE"]]
  dt_variables <- variables_list %>% 
    filter(TableNo == "5")

  columns_needed <- dt_variables$ExportTag
  
  any_SAE <- dt_variables %>%
    filter(any_SAE == "Yes") %>%
    pull(ExportTag)
  
  part_of_summary <- dt_variables %>%
    filter(part_of_summary == "Yes") %>%
    pull(ExportTag)
  
  part_of_listing <- dt_variables %>%
    filter(part_of_listing == "Yes") %>%
    pull(ExportTag)
  
  
  data_tab5 <- all_data[!is.na(all_data$Reference), c("PatientNr", "Site", columns_needed)]
  
  
  data_tab5 <- data_tab5 %>%
    mutate(any_SAE = ifelse(!is.na(.data[[any_SAE]]), "Yes", "No"))
  
  
  data_listing5 <- data_tab5 %>%
    filter(any_SAE == "Yes") %>%
    select(PatientNr, Site, all_of(part_of_listing))
  
  data_listing5 <- data_listing5 %>%
    mutate(across(all_of(dt_variables$ExportTag[dt_variables$ValueIncluded == "Date"]), as.character)) %>%
    mutate(across(all_of(dt_variables$ExportTag[dt_variables$ValueIncluded == "Time"]), ~ substr(as.character(.), 12, 16)))
  
  
  data_tab5 <- data_tab5[,c("PatientNr","Site","any_SAE", part_of_summary)]
  
  data_tab5 <- data_tab5 %>%
    filter(any_SAE == "Yes") %>%
    select(-!!sym(any_SAE))
  
  
  # Create a named vector for renaming
  rename_vector <- setNames(dt_variables$ColumnName, dt_variables$ExportTag)
  
  data_tab5 <- data_tab5 %>%
    rename_with(~ rename_vector[.x], .cols = all_of(names(rename_vector)[names(rename_vector) %in% names(data_tab5)]))
  
  for (col in names(data_tab5)) {
    if (col %in% dt_variables$ExportTag) {
      var_label(data_tab5[[col]]) <- dt_variables$ColumnName[dt_variables$ExportTag == col]
    }
  }
  
  var_label(data_tab5$any_SAE) <- "SAE"
  part_of_summary <- colnames(data_tab5)[3:length(colnames(data_tab5))]
  
  
  
  ####### tab
  
  tab_5 <- tbl_summary(data = data_tab5,
                       include = c("any_SAE", part_of_summary),
                       by = 'Site',
                       type = all_continuous() ~ "continuous2",
                       statistic = all_continuous() ~ c("{N_nonmiss}"),
                       missing = "no")
  tab_5 <- add_overall(tab_5, col_label = "**Overall**")
  tab_5 <- bold_labels(tab_5)
  tab_5 <- modify_footnote(tab_5, all_stat_cols() ~ NA)
  tab_5 <- as_flex_table(tab_5)
  tab_5  <- bold(tab_5 , bold = TRUE, part = "header")
  # tab_5 <- add_header_row(tab_5, values = "SAE Summary")
  tab_5 <- fontsize(tab_5, size = 9, part = "body")
  tab_5 <- fontsize(tab_5, size = 10, part = "header")
  
  ### listing
  
  part_of_listing_names <- dt_variables %>%
    filter(part_of_listing == "Yes") %>%
    pull(ColumnName)
  
  colnames(data_listing5) <- c("Subject No","Site", part_of_listing_names)
  
  listing_5 <- flextable(data_listing5)
  listing_5 <- fontsize(listing_5, i = NULL, j = 1:(length(part_of_listing_names)+2), size = 8, part = "body")
  listing_5 <- fontsize(listing_5, i = NULL, j = 1:(length(part_of_listing_names)+2), size = 9, part = "header")
  listing_5 <- theme_box(listing_5)
  # listing_5 <- width(listing_5, j=c(1:(length(part_of_listing_names)+2)), width = c(1,1,rep(1,8)))
  
  my_doc <- body_add_par(my_doc, section_header_name, style = "HEADER_2")
  
  if(!is.null(tab_5)) {
    my_doc <- body_add_par(my_doc, "SAE summary", style = "heading 2")
    my_doc <- body_add_flextable(my_doc, tab_5)
    my_doc <- body_add_par(my_doc, " ")
    my_doc <- body_add_par(my_doc, "SAE listing", style = "heading 2")
    my_doc <- body_add_flextable(my_doc, listing_5)
  }
  if(is.null(tab_5)) {
    my_doc <- body_add_par(my_doc, "None reported SAE")
  }
  
  my_doc <- body_end_section_landscape(my_doc)
  
  # return(list(tab5 = tab_5, listing5 = listing_5))
  return(my_doc)
}