tab4_fun <- function(variables, all_data, my_doc, section_header_name) {
  # Extract relevant variables
  variables_list <- variables[["AE"]]
  dt_variables <- variables_list %>% 
    filter(TableNo == "4")

  columns_needed <- dt_variables$ExportTag
  
  any_AE <- dt_variables %>%
    filter(any_AE == "Yes") %>%
    pull(ExportTag)
  
  any_AE_type <- dt_variables %>%
    filter(any_AE == "Yes") %>%
    pull(ValueIncluded)
  
  part_of_summary <- dt_variables %>%
    filter(part_of_summary == "Yes") %>%
    pull(ExportTag)
  
  part_of_listing <- dt_variables %>%
    filter(part_of_listing == "Yes") %>%
    pull(ExportTag)
  
  
  data_tab4 <- all_data[!is.na(all_data$Reference), c("PatientNr", "Site", columns_needed)]
  
  data_tab4 <- data_tab4 %>%
    mutate(any_AE = ifelse(!is.na(.data[[any_AE]]), "Yes", "No")) %>%
    filter(any_AE == "Yes")


  
  data_listing4 <- data_tab4 %>%
    select(PatientNr, Site, all_of(part_of_listing))

  data_listing4 <- data_listing4 %>%
    mutate(across(all_of(dt_variables$ExportTag[dt_variables$ValueIncluded == "Date"]), as.character)) %>%
    mutate(across(all_of(dt_variables$ExportTag[dt_variables$ValueIncluded == "Time"]), ~ substr(as.character(.), 12, 16)))
  
  data_tab4 <- data_tab4[,c("PatientNr","Site","any_AE", part_of_summary)]
  
  data_tab4 <- data_tab4 %>%
    select(-!!sym(any_AE))
  
  if(any_AE_type != "Yes" & any_AE_type != "Single Answer") {
    data_tab4 <- data_tab4 %>%
      mutate(any_AE = ifelse(!is.na(any_AE), "Yes", NA))
  }
  
  # Create a named vector for renaming
  rename_vector <- setNames(dt_variables$ColumnName, dt_variables$ExportTag)
  
  data_tab4 <- data_tab4 %>%
    rename_with(~ rename_vector[.x], .cols = all_of(names(rename_vector)[names(rename_vector) %in% names(data_tab4)]))
  
  for (col in names(data_tab4)) {
    if (col %in% dt_variables$ExportTag) {
      var_label(data_tab4[[col]]) <- dt_variables$ColumnName[dt_variables$ExportTag == col]
    }
  }
  
  var_label(data_tab4$any_AE) <- "AE"
  part_of_summary <- colnames(data_tab4)[3:length(colnames(data_tab4))]
  
  
  ####### tab
  tab_4 <- tbl_summary(data = data_tab4,
                       include = c("any_AE", part_of_summary),
                       by = 'Site',
                       type = all_continuous() ~ "continuous2",
                       statistic = all_continuous() ~ c("{N_nonmiss}"),
                       missing = "no")
  tab_4 <- add_overall(tab_4, col_label = "**Overall**")
  tab_4 <- bold_labels(tab_4)
  tab_4 <- modify_footnote(tab_4, all_stat_cols() ~ NA)
  tab_4 <- as_flex_table(tab_4)
  tab_4  <- bold(tab_4 , bold = TRUE, part = "header")
  # tab_4 <- add_header_row(tab_4, values = "SAE Summary")
  tab_4 <- fontsize(tab_4, size = 9, part = "body")
  tab_4 <- fontsize(tab_4, size = 10, part = "header")
  
  ### listing
  
  part_of_listing_names <- dt_variables %>%
    filter(part_of_listing == "Yes") %>%
    pull(ColumnName)
  
  colnames(data_listing4) <- c("Subject No","Site", part_of_listing_names)
  
  listing_4 <- flextable(data_listing4)
  listing_4 <- fontsize(listing_4, i = NULL, j = 1:(length(part_of_listing_names)+2), size = 8, part = "body")
  listing_4 <- fontsize(listing_4, i = NULL, j = 1:(length(part_of_listing_names)+2), size = 9, part = "header")
  listing_4 <- theme_box(listing_4)
  # listing_4 <- width(listing_4, j=c(1:(length(part_of_listing_names)+2)), width = c(1,1,rep(1,8)))
  
  my_doc <- body_add_par(my_doc, section_header_name, style = "heading 1")
  my_doc <- body_add_par(my_doc, " ")
  
  if(!is.null(tab_4)) {
    my_doc <- body_add_par(my_doc, "AE summary", style = "heading 2")
    my_doc <- body_add_flextable(my_doc, tab_4)
    my_doc <- body_add_par(my_doc, " ")
    my_doc <- body_add_par(my_doc, "AE listing", style = "heading 2")
    my_doc <- body_add_flextable(my_doc, listing_4)
    my_doc <- body_add_par(my_doc, " ")
  }
  if(is.null(tab_4)) {
    my_doc <- body_add_par(my_doc, "None reported AE")  
  }
  my_doc <- body_add_break(my_doc, pos = "after")
  
  # return(list(tab4 = tab_4, listing4 = listing_4))
  return(my_doc)
}
