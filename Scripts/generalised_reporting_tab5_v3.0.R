tab5_fun <- function(variables, all_data, my_doc, section_header_name) {
  # Extract relevant variables
  variables_list <- variables[["SAE"]]
  dt_variables <- variables_list %>% filter(TableNo == "5")
  
  dt_variables <- dt_variables %>%
    mutate(ColumnName = str_squish(as.character(ColumnName)))
  
  columns_needed <- dt_variables$ExportTag
  
  # Define what we need from dt_variables
  any_SAE <- dt_variables %>%
    filter(any_SAE == "Yes") %>%
    pull(ExportTag)
  
  part_of_summary <- dt_variables %>%
    filter(part_of_summary == "Yes") %>%
    pull(ExportTag)
  
  part_of_listing <- dt_variables %>%
    filter(part_of_listing == "Yes") %>%
    pull(ExportTag)
  
  rename_vector <- setNames(dt_variables$ColumnName, dt_variables$ExportTag)
  
  part_of_checkbox <- dt_variables %>%
    filter(!is.na(part_of_checkbox)) %>%
    pull(ExportTag)

  # Prepare base data
  data_tab5 <- all_data[!is.na(all_data$Reference), c("PatientNr", "Site", columns_needed)]
  data_tab5 <- data_tab5 %>%
    mutate(any_SAE = ifelse(!is.na(.data[[any_SAE]]), "Yes", "No"))
  
    # Filter for rows with SAE
    data_listing5 <- data_tab5 %>%
      filter(any_SAE == "Yes") %>%
      select(PatientNr, Site, all_of(part_of_listing))

    # Handle dates and times
    data_listing5 <- data_listing5 %>%
      mutate(across(all_of(dt_variables$ExportTag[dt_variables$ValueIncluded == "Date"]), as.character)) %>%
      mutate(across(all_of(dt_variables$ExportTag[dt_variables$ValueIncluded == "Time"]),
                    ~ substr(as.character(.), 12, 16)))

    # Prepare summary table
    data_tab5 <- data_tab5[, c("PatientNr", "Site", "any_SAE", part_of_summary)] %>%
      filter(any_SAE == "Yes")

    # Rename for summary
    data_tab5 <- data_tab5 %>%
      rename_with(~ rename_vector[.x],
                  .cols = all_of(names(rename_vector)[names(rename_vector) %in% names(data_tab5)]))

    for (col in names(data_tab5)) {
      if (col %in% dt_variables$ExportTag) {
        var_label(data_tab5[[col]]) <- dt_variables$ColumnName[dt_variables$ExportTag == col]
      }
    }

    var_label(data_tab5$any_SAE) <- "SAE"
    part_of_summary <- colnames(data_tab5)[3:length(colnames(data_tab5))]
    
    tab_5 <- NULL
    
    if (nrow(data_tab5)>0) {

    # Build the summary table
    tab_5 <- tbl_summary(data = data_tab5,
                         include = c("any_SAE", part_of_summary),
                         by = 'Site',
                         type = all_continuous() ~ "continuous2",
                         statistic = all_continuous() ~ c("{N_nonmiss}"),
                         missing = "no") %>%
      add_overall(col_label = "**Overall**") %>%
      bold_labels() %>%
      modify_footnote(all_stat_cols() ~ NA) %>%
      as_flex_table()
    
    tab_5 <- flextable::font(tab_5, fontname = "Open Sans", part = "all")

    # Prepare checkbox columns
    print(part_of_checkbox)

    part_of_checkbox <- part_of_checkbox[part_of_checkbox %in% names(data_listing5) & colSums(!is.na(data_listing5[part_of_checkbox])) > 0]

    unique_checkbox <- dt_variables %>%
      filter(!is.na(part_of_checkbox))

    checkbox_text <- data_listing5 %>%
      mutate(across(all_of(part_of_checkbox), ~ if_else(. == "Yes",
                                                        dt_variables$ColumnName[which(dt_variables$ExportTag == cur_column())],
                                                        if_else(dt_variables$ValueIncluded[which(dt_variables$ExportTag == cur_column())] == "Text",
                                                                paste(.),
                                                                NA))))

    # Unite grouped checkboxes
    data_listing5 <- unique_checkbox %>%
      distinct(part_of_checkbox) %>%
      pull(part_of_checkbox) %>%
      reduce(function(df, group) {
        checkbox_grouped <- unique_checkbox$ExportTag[unique_checkbox$part_of_checkbox == group]
        df %>%
          unite(col = !!group,
                all_of(checkbox_grouped),
                sep = "; ",
                na.rm = TRUE)
      }, .init = checkbox_text)

    # Rename listing columns
    data_listing5 <- data_listing5 %>%
      rename_with(~ dt_variables$ColumnName[match(.x, dt_variables$ExportTag)],
                  .cols = intersect(colnames(data_listing5), dt_variables$ExportTag))

    # Create flextable listing
    listing_5 <- flextable(data_listing5)
    listing_5 <- fontsize(listing_5, i = NULL, j = ~., size = 8, part = "body")
    listing_5 <- fontsize(listing_5, i = NULL, j = ~., size = 9, part = "header")
    listing_5 <- theme_box(listing_5)
      } else {
      print("brak danych")
      }
  

  
  # Add to Word doc
  my_doc <- body_add_par(my_doc, section_header_name, style = "HEADER_2")
  
  if (!is.null(tab_5)) {
    my_doc <- body_add_par(my_doc, "SAE summary", style = "heading 2")
    my_doc <- body_add_flextable(my_doc, tab_5)
    my_doc <- body_add_par(my_doc, " ")
    my_doc <- body_add_par(my_doc, "SAE listing", style = "heading 2")
    my_doc <- body_add_flextable(my_doc, listing_5)
  } else {
    print("brak danych")
    my_doc <- body_add_par(my_doc, "No SAE reported")
  }
  
  my_doc <- body_end_section_landscape(my_doc)
  
  return(my_doc)
}
