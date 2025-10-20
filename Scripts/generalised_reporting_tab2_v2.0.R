
tab2_fun <- function(variables, all_data, my_doc, section_header_name) {
  # Extract relevant variables
  variables_list <- variables[["Eligibility"]]
  dt2_variables <- variables_list %>% 
    filter(TableNo == "2")
  
  eligibility_vars <- variables[["Eligibility"]] %>%
    filter(TableNo == 2) %>%
    select(ExportTag, ValueIncluded)
  
  # Required columns
  columns_needed <- dt2_variables$ExportTag
  dependat_variable <- unique(dt2_variables$DependantVariable_NotRequired)
  dependat_variable <- dependat_variable[!is.na(dependat_variable)]
  
  if (length(dependat_variable) == 0) {
    dependat_variable <- NULL
  }
  
  data_tab2 <- all_data[is.na(all_data$Reference), c("PatientNr", "Site", columns_needed, dependat_variable)]
  
  # Identify "Not Required" variables
  not_required <- variables_list %>% filter(!is.na(ValueIf_NotRequired))
  columns_not_required <- not_required$ExportTag
  
  # Inclusion and Exclusion Variables
  inclusion_variables <- variables_list %>% filter(TableNo == "2" & InclusionExclusion == "Inclusion")
  exclusion_variables <- variables_list %>% filter(TableNo == "2" & InclusionExclusion == "Exclusion")
  
  columns_needed_inclusion <- inclusion_variables$ExportTag
  columns_needed_exclusion <- exclusion_variables$ExportTag
  
  # Remove patients where all inclusion AND all exclusion criteria are NA
  all_data <- all_data %>%
    filter(!(if_all(all_of(c(columns_needed_inclusion, columns_needed_exclusion)), is.na)))
  
  data_tab2_inc <- all_data[is.na(all_data$Reference), c("PatientNr", "Site", columns_needed_inclusion, dependat_variable)]
  data_tab2_exc <- all_data[is.na(all_data$Reference), c("PatientNr", "Site", columns_needed_exclusion, dependat_variable)]
  
  #remove patients with completely unfilled inclusion/exclusion criteria

  
  # Inclusion Criteria Met
  inclusion_criteria_met <- data_tab2_inc %>%
    pivot_longer(cols = -c(PatientNr, Site, all_of(dependat_variable)), names_to = "ExportTag", values_to = "Value") %>%
    {
      if (!is.null(dependat_variable)) rename(., dependantVariable = all_of(dependat_variable)) else .
    } %>%
    left_join(not_required[, c("ExportTag", "ValueIf_NotRequired", "DependantVariable_Value")], by = "ExportTag") %>%
    left_join(eligibility_vars, by = "ExportTag") %>%
    mutate(Value = ifelse(is.na(Value), "Missing", Value)) %>%
    mutate(inc_criteria_met = case_when(
      ValueIncluded == "No" ~ ifelse(Value == "Yes", "No",
                                     ifelse(!is.na(ValueIf_NotRequired),
                                            ifelse((dependantVariable == DependantVariable_Value) & (Value == ValueIf_NotRequired), "No", "Yes"), "Yes")),
      TRUE ~ ifelse(Value == "Yes", "Yes",
                    ifelse(!is.na(ValueIf_NotRequired),
                           ifelse((dependantVariable == DependantVariable_Value) & (Value == ValueIf_NotRequired), "Yes", "No"), "No"))
    )) %>%
    group_by(PatientNr, Site) %>%
    mutate(all_inc_criteria_met = ifelse(all(inc_criteria_met == "Yes"), "Yes", "No"))
  
  
  # Exclusion Criteria Met
  exclusion_criteria_met <- data_tab2_exc %>%
    pivot_longer(-c(PatientNr, Site, all_of(dependat_variable)), names_to = "ExportTag", values_to = "Value") %>%
    {
      if (!is.null(dependat_variable)) rename(., dependantVariable = all_of(dependat_variable)) else .
    } %>%
    left_join(not_required[, c("ExportTag", "ValueIf_NotRequired", "DependantVariable_Value")], by = "ExportTag") %>%
    left_join(eligibility_vars, by = "ExportTag") %>%
    mutate(Value = ifelse(is.na(Value), "Missing", Value)) %>%
    mutate(exc_criteria_met = case_when(
      ValueIncluded == "No" ~ ifelse(Value == "Yes", "No",
                                     ifelse(!is.na(ValueIf_NotRequired),
                                            ifelse((dependantVariable == DependantVariable_Value) & (Value == ValueIf_NotRequired), "No", "Yes"), "Yes")),
      TRUE ~ ifelse(Value == "Yes", "Yes",
                    ifelse(!is.na(ValueIf_NotRequired),
                           ifelse((dependantVariable == DependantVariable_Value) & (Value == ValueIf_NotRequired), "Yes", "No"), "No"))
    )) %>%
    group_by(PatientNr, Site) %>%
    mutate(all_exc_criteria_met = ifelse(all(exc_criteria_met == "No"), "Yes", "No"))
  
  # Combine Inclusion and Exclusion Criteria
  all_inclusion_criteria_met <- inclusion_criteria_met %>%
    distinct(PatientNr, .keep_all = TRUE) %>%
    select(PatientNr, Site, all_inc_criteria_met)
  
  all_exclusion_criteria_met <- exclusion_criteria_met %>%
    distinct(PatientNr, .keep_all = TRUE) %>%
    select(PatientNr, Site, all_exc_criteria_met)
  
  all_criteria <- merge(all_inclusion_criteria_met, all_exclusion_criteria_met, by = c("PatientNr", "Site"))
  all_criteria$crit_full <- ifelse(all_criteria$all_inc_criteria_met == "Yes" & all_criteria$all_exc_criteria_met == "Yes", "Yes", "No")
  all_criteria <- all_criteria[, c("PatientNr", "Site", "crit_full")]
  
  # Formatting and Summary Table
  all_criteria$crit_full <- factor(all_criteria$crit_full, levels = c("Yes", "No"), labels = c("Yes", "No"))
  all_criteria$All_Site <- ifelse(is.na(all_criteria$Site), 999, 1)
  all_criteria$All_Site <- "N (%)"
  var_label(all_criteria$All_Site) <- "Overall"
  
  ### listing
  inc_not_met <- inclusion_criteria_met %>%
    filter(inc_criteria_met == "No" | is.na(inc_criteria_met)) %>%
    left_join(., dt2_variables[,c("ColumnName", "ExportTag")], by = c("ExportTag")) %>%
    select(PatientNr, Site, ColumnName, Value)
  
  exc_met <- exclusion_criteria_met %>%
    filter(exc_criteria_met == "Yes" | is.na(exc_criteria_met)) %>%
    left_join(., dt2_variables[,c("ColumnName", "ExportTag")], by = c("ExportTag")) %>%
    select(PatientNr, Site, ColumnName, Value)
  
  data_listing2 <- rbind(inc_not_met, exc_met)
  data_listing2 <-  data_listing2[order(data_listing2$PatientNr),]
    
  
  
  tab_2 <- tbl_summary(
    data = all_criteria,
    include = c('Site'),
    by = 'crit_full',
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{N_nonmiss}"),
    missing = "no"
  )
  
  tab_2_1 <- tbl_summary(
    data = all_criteria,
    include = c('All_Site'),
    by = 'crit_full',
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{N_nonmiss}"),
    missing = "no"
  )
  
  # Format Table Output
  tab_2 <- add_overall(tab_2, col_label = "**Overall**")
  tab_2_1 <- add_overall(tab_2_1, col_label = "**Overall**")
  tab_2 <- tbl_stack(list(tab_2, tab_2_1))
  tab_2 <- modify_header(tab_2, label = "****", stat_1 = "**Fulfilled**", stat_2 = "**Not fulfilled**", stat_3 = "**Form not completed**")
  tab_2 <- bold_labels(tab_2)
  tab_2 <- modify_footnote(tab_2, all_stat_cols() ~ NA)
  tab_2 <- as_flex_table(tab_2)
  tab_2 <- flextable::font(tab_2, fontname = "Calibri", part = "all")
  tab_2  <- bold(tab_2, bold = TRUE, part = "header")
  tab_2 <- add_header_row(tab_2, values = "Inclusion/exclusion criteria fulfilled", colwidths = 4)
  tab_2 <- fontsize(tab_2, size = 9, part = "body")
  tab_2 <- fontsize(tab_2, size = 10, part = "header")
  
  colnames(data_listing2) <- c("Subject No", "Site", "Ex/In Criterion", "Value")
  
  listing_2 <- flextable(data_listing2)
  # listing_2 <- add_header_row(listing_2, values = "Eligibility Criteria - not met or empty", colwidths = 4)
  listing_2 <- fontsize(listing_2, i = NULL, j = 1:4, size = 8, part = "body")
  listing_2 <- fontsize(listing_2, i = NULL, j = 1:4, size = 9, part = "header")
  listing_2 <- theme_box(listing_2)
  listing_2 <- set_table_properties(listing_2, layout = "autofit")
  
  my_doc <- body_add_par(my_doc, section_header_name, style = "heading 1")
  my_doc <- body_add_par(my_doc, "Number of subjects:", style = "heading 2")
  my_doc <- body_add_flextable(my_doc, tab_2)
  my_doc <- body_add_par(my_doc, " ")
  my_doc <- body_add_par(my_doc, "Eligibility Criteria - not met or empty:", style = "heading 2")
  my_doc <- body_add_flextable(my_doc, listing_2)
  my_doc <- body_add_break(my_doc, pos = "after")
  
  # Return both tables
  # return(list(tab2 = tab_2, listing2 = listing_2))
  return(my_doc)
  
}

