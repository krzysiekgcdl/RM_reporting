
# temp_file <- "C:/CDL/OneDrive - CleanDataLabs/Dyski/445_SHINY_REPOS/DM_Reports/DataAging_Templates/DEBRISOFT.xlsx"
# 
# log.dat <- read_excel("C:/CDL/OneDrive - CleanDataLabs/Dyski/036_LOHMANN/DEBRISOFT/ZAPISY/RAPORTY/Raw data/Research Manager 2025-05-29 14_30_09.xlsx")
# com.dat <- read_excel("C:/CDL/OneDrive - CleanDataLabs/Dyski/036_LOHMANN/DEBRISOFT/ZAPISY/RAPORTY/Raw data/CRFOverview_20250529_1429_sub.xlsx")
# all_data <- read_excel("C:/CDL/OneDrive - CleanDataLabs/Dyski/036_LOHMANN/DEBRISOFT/ZAPISY/RAPORTY/Raw data/export-20250529-142946_ALL_DATA_20250529.xlsx")
# # variables <- read_excel("C:/CDL/OneDrive - CleanDataLabs/Dyski/445_SHINY_REPOS/DM_Reports/DataAging_Templates/DEBRISOFT.xlsx")
# com.dat_ch <- read_excel("C:/CDL/OneDrive - CleanDataLabs/Dyski/445_SHINY_REPOS/DM_Reports/DataAging_Chapters/DEBRISOFT.xlsx")

######## load template
# prepare_variables <- function(variables_data) {
#   # Read all sheets into a named list
#   report_description <- read_excel(temp_file, sheet = "RaportDescription")
#   sheets <- as.character(report_description$items_names)
#   variables_list <- lapply(sheets, function(sheet) {
#     read_excel(temp_file, sheet = sheet)
#     # filter(!is.na(ExportTag))  # Filter out rows with missing ExportTag
#   })
#   # Name the elements of the list based on sheet names
#   names(variables_list) <- sheets
#   print(variables_list)
#   return(variables_list)
# }
# 
# variables <- prepare_variables(variables_data)
# 
# 
# stname <- "DEBRISOFT"


data_aging_fun <- function(session, input, output, log.dat, com.dat, variables, all_data) {
  
  allowed_delay_in_entry_to_ecrf <- variables$AllowedDelay$allowed_delay_in_visit_entry_to_ecrf
  
  print(allowed_delay_in_entry_to_ecrf)
  
  
  ### variables used in the first 2 tables:
  variables_12 <- variables$VisitNames %>%
    filter(!is.na(ecrfDescription))
  
  ### variables used in the 3rd table:
  variables_3 <- variables$VisitNames %>%
    filter(!is.na(Schedule))
  
  
  # # Function to rename duplicated column names with _2, _3, etc.
  # rename_duplicates <- function(names) {
  #   # Remove the ...<number> suffix from column names
  #   base_names <- str_remove(names, "\\...[0-9]+$")
  #   
  #   # Create a table of how often each base name appears
  #   name_count <- ave(seq_along(base_names), base_names, FUN = seq_along)
  #   
  #   # Keep the first occurrence as is, then append _2, _3, etc.
  #   new_names <- ifelse(name_count == 1, 
  #                       base_names, 
  #                       paste0(base_names, "_", name_count))
  #   
  #   return(new_names)
  # }
  # 
  # # Renaming duplicated column names in com.dat
  # colnames(com.dat) <- rename_duplicates(colnames(com.dat))
  
  
  


print("start1")

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

# log.dat <- read.xlsx("Dane/Research Manager 2024-01-15 11_49_15_at.xlsx")
# com.dat <- read.xlsx("Dane/CRFOverview_20240115_1237.xlsx")

# data_preparation_fun <- function() {
colnames(log.dat) <- c("update.date",
                       "action.list",
                       "participant",
                       "old.value",
                       "new.value",
                       "user.name",
                       "comments")

log.dat$update.date <- as.Date(log.dat$update.date, "%d-%m-%Y")

subsets_list <- list()

#Podzielenie danych na wizyty
subsets_list <- lapply(1:length(variables_12$ecrfDescription), function(i) {
  log.dat[grepl(variables_12$ecrfDescription[i], log.dat$action.list, fixed = TRUE), ]
})

print(subsets_list)

visits.fun <- function(data.set) {
  
  parse_date_multi_locale <- function(x, formats, locales) {
    # Keep original locale so we can restore it
    orig_locale <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", orig_locale))
    
    # Try each locale in sequence
    for (loc in c(orig_locale, locales)) {
      try(Sys.setlocale("LC_TIME", loc), silent = TRUE)
      parsed <- suppressWarnings(as.Date(as.character(x), tryFormats = formats))
      if (any(!is.na(parsed))) {
        return(parsed)
      }
    }
    return(rep(NA, length(x))) # If nothing worked
  }
  
  # 1. Ensure new.value is Date without nuking good data
  if (!inherits(data.set$new.value, "Date")) {
    data.set$new.value <- parse_date_multi_locale(
      x = data.set$new.value,
      formats = c(
        "%d-%b-%Y",  # 22-Jul-2025 or 22-lip-2025
        "%d-%m-%Y",  # 22-07-2025
        "%Y-%m-%d",  # 2025-07-22
        "%d/%m/%Y"   # 22/07/2025
      ),
      locales = c("pl_PL.UTF-8", "de_DE.UTF-8", "fr_FR.UTF-8") # Add more if needed
    )
  }
  
  # 2. Handle duplicated participants
  data.set.d <- data.set[duplicated(data.set$participant), ]
  
  data.set.m <- data.set %>%
    filter(participant %in% data.set.d$participant) %>%
    group_by(participant) %>%
    mutate(update.date = min(update.date, na.rm = TRUE)) %>%
    ungroup()
  
  data.set.m <- data.set.m[!duplicated(data.set.m$participant), ]
  data.set <- data.set[!(data.set$participant %in% data.set.m$participant), ]
  
  # 3. Combine back
  data.set <- bind_rows(data.set, data.set.m) %>%
    arrange(update.date)
  
  # 4. Calculate date_diff only when both dates exist
  data.set$date_diff <- ifelse(
    !is.na(data.set$update.date) & !is.na(data.set$new.value),
    as.numeric(difftime(data.set$update.date, data.set$new.value, units = "days")),
    NA_real_
  )
  
  return(data.set)
}

subsets_list[[1]] <- subsets_list[[1]] %>%
  arrange(update.date) %>%
  arrange(participant) %>%
  distinct(participant, .keep_all = TRUE)
subsets_list[[1]] <- visits.fun(subsets_list[[1]])

# Apply the transformations to each element of the subsets list
subsets_list <- lapply(subsets_list, function(subset) {
  subset <- subset %>%
    arrange(update.date) %>%
    arrange(participant) %>%
    distinct(participant, .keep_all = TRUE)
  
  # Apply the visits.fun function
  subset <- visits.fun(subset)
  
  return(subset)
})

print(subsets_list)
# }

data_v_open <- do.call(rbind, subsets_list)

data_v_open$visit <- factor(data_v_open$action.list, levels = variables_12$ecrfDescription, labels = variables_12$VisitName)

var_label(data_v_open$date_diff) <- "Difference between the date of the visit and its entry to eCRF [days]" 

data_v_open_listing <- data_v_open[,c("participant","user.name","visit","new.value","update.date","date_diff")]
colnames(data_v_open_listing) <- c("Record ID","User Name", "Visit","Visit Date","Enter Date", "Difference between the date of the visit and its entry to eCRF [days]")

####### LISITNG
data_v_open_listing <- arrange(data_v_open_listing,`Record ID`)
data_v_open_listing <- arrange(data_v_open_listing,`Visit Date`)
data_v_open_listing <- arrange(data_v_open_listing,`Visit`)
listing_1 <- flextable(data_v_open_listing)
listing_1 <- fontsize(listing_1, i = NULL, j = 1:6, size = 7, part = "body")

print("przed problemem")

delay_limit <- variables$AllowedDelay$allowed_delay_in_visit_entry_to_ecrf[1]
listing_1 <- bg(
  listing_1,
  i = which(data_v_open_listing[[6]] > delay_limit),
  j = 6,
  bg = "orange"
)

print("po problemie")
listing_1<- fontsize(listing_1, i = NULL, j = 1:6, size = 8, part = "header")
listing_1 <- theme_box(listing_1)
listing_1<- width(listing_1, j=c(1:6), width = c(1,1,1,1,1,3))

#### Tabela
tab_1 <- tbl_summary(data = data_v_open,
                     include = c('date_diff'),
                     by = 'visit',
                     type = date_diff ~ "continuous2",
                     statistic = all_continuous() ~ c("{N_nonmiss}",
                                                      "{mean} ({sd})",
                                                      "{median} [{p25}, {p75}]", 
                                                      "{min}, {max}"
                     ),
                     missing = "no",
                     digits = list(all_continuous() ~ c(0,1,1,1,1,1,1,1,1)
                                   , all_categorical()~c(0,1)))
# tab_1 <- modify_header(tab_1, label = "****", stat_1 = "**Informed Consent**",stat_2 = "**Visit 1**",stat_3 = "**Visit 2**",stat_4 = "**Visit 3**",stat_5 = "**Premature Termination**",stat_6 = "**PI Authentication**")
# Create a named vector for the modify_header arguments
header_labels <- c("label" = "****")

# Add the visit names as arguments for stat_1, stat_2, etc.
for (i in seq_along(variables_12$VisitName)) {
  header_labels[paste0("stat_", i)] <- paste0("**", variables_12$VisitName[i], "**")
}

# Use do.call to apply modify_header with the constructed arguments
tab_1 <- do.call(modify_header, c(list(tab_1), header_labels))

tab_1 <- bold_labels(tab_1)
tab_1 <- modify_footnote(tab_1, all_stat_cols() ~ NA)
tab_1 <- as_flex_table(tab_1)

tab_1 <- flextable::font(tab_1, fontname = "Open Sans", part = "all")
tab_1  <- bold(tab_1 , bold = TRUE, part = "header")
tab_1 <- fontsize(tab_1, size = 7, part = "body")
tab_1 <- fontsize(tab_1, size = 8, part = "header")
tab_1 <- autofit(tab_1)
# tab_1 <- width(tab_1, j=c(1:7), width = c(1.5,0.9,0.9,0.9,0.9,0.9,1.2))


#Wizyta - complete czy nie?

print("tab1")


##################################################################
#Wizyta - complete czy nie?

visits_on_schedule_list <- variables_3[!is.na(variables_3$Schedule),]
visits_included_list <- variables_3[!is.na(variables_3$ChapterName),]

# colnames(com.dat) <- gsub("-", ".", colnames(com.dat))
# colnames(com.dat) <- gsub("–", ".", colnames(com.dat))

### which status is complete?
completed <- c("Monitored", "Locked", "Signed", "Complete")

com.dat <- com.dat %>%
  select(-contains("Reference"), -c("Enrollment date", "Locked")) %>%
  rename(PatientNr = "participant number")
com.dat_ch <- com.dat_ch %>%
  select(-c("Enrollment date", "Locked")) %>%
  rename(PatientNr = "participant number")



chapters_names_list <- colnames(com.dat_ch)[!colnames(com.dat_ch) %in% c("participant number", "Site")]
visits_included_names_list <- visits_included_list$ChapterName





###

# Initialize an empty list to store results
results <- list()


# Loop over each pair of columns in com.dat_ch, including the last column
for (i in 3:(ncol(com.dat_ch))) {
  # Determine the range of columns to select
  if (i < ncol(com.dat_ch)) {
    visit_list <- (which(grepl(colnames(com.dat_ch)[i], colnames(com.dat)))+1):(which(grepl(colnames(com.dat_ch)[i+1], colnames(com.dat)))-1)
  } else {
    # For the last column, select until the end of com.dat
    visit_list <- (which(grepl(colnames(com.dat_ch)[i], colnames(com.dat)))+1):ncol(com.dat)
  }
  
  visit_subchapters <- com.dat %>%
    select(PatientNr, Site, all_of(visit_list)) %>%
    distinct(PatientNr, Site, .keep_all = TRUE) %>%
    pivot_longer(-c(PatientNr, Site), names_to = "Subchapter", values_to = "Status") %>%
    group_by(PatientNr, Site) %>%
    mutate(lowest_status = ifelse(all(na.omit(Status) == "Empty"), "Empty",
                                  ifelse(any(na.omit(Status) == "Incomplete"), "Incomplete",
                                         ifelse(any(na.omit(Status) == "Complete"), "Complete",
                                                ifelse(any(na.omit(Status) == "Signed"), "Signed",
                                                       ifelse(any(na.omit(Status) == "Monitored"), "Monitored",
                                                              ifelse(any(na.omit(Status) == "Locked"), "Locked", ifelse(any(na.omit(Status) == "Locked"), "Locked", "NA")))))))) %>%
    distinct(PatientNr, Site, .keep_all = TRUE) %>%
    select(PatientNr, Site, lowest_status) %>%
    rename_with(~ colnames(com.dat_ch)[i], lowest_status)
  
  results[[i-2]] <- visit_subchapters
  

}


# Combine all results into a single data frame
final_result <- Reduce(function(x, y) merge(x, y, by = c("PatientNr", "Site"), all = TRUE), results)

final_result <- final_result %>%
  select(PatientNr, Site, all_of(visits_included_names_list))


day0_date <- all_data %>%
  select(PatientNr, Site, variables$AllowedDelay$day0_variable) %>%
  rename("zeroDate" = variables$AllowedDelay$day0_variable) %>%
  filter(!is.na(zeroDate))




com.dates <- merge(final_result, day0_date, by = c("PatientNr", "Site"), all = TRUE)
com.dates <- com.dates %>%
  mutate(zeroDate = as.Date(zeroDate, format = "%Y-%m-%d"))



# Filter variables_3 to include only rows where Schedule is not NA and not 0
valid_visits <- variables_3 %>%
  filter(!is.na(Schedule) & Schedule != 0)



# Loop over each row in valid_visits
for (i in seq_len(nrow(valid_visits))) {
  chapter_name <- valid_visits$ChapterName[i]
  schedule_days <- valid_visits$Schedule[i]
  
  # Create a new column name based on ChapterName
  time_col_name <- paste0(chapter_name, ".time")
  status_col_name <- chapter_name
  
  
  # Use mutate to create the new time column in com.dates
  com.dates <- com.dates %>%
    mutate(!!time_col_name := ifelse(is.na(zeroDate), 0, as.numeric(Sys.Date() - zeroDate - schedule_days)))
  
  # Update the status column based on the conditions
  com.dates <- com.dates %>%
    mutate(!!status_col_name := ifelse(
      (get(status_col_name) %in% c("Incomplete", "Empty")) & (get(time_col_name) > schedule_days),
      paste(get(time_col_name), "days"),
      get(status_col_name)
    ))

}



com.dates <- com.dates %>%
  select(PatientNr, Site, all_of(visits_included_names_list))



tab_2 <- data.frame(com.dates)
labels_list <- setNames(gsub("\\.", " ", colnames(tab_2)), colnames(tab_2))
print(paste("Number of rows in tab_2:", nrow(tab_2)))
#Stworzenie matrycy kolorów dla tabeli statusu wizyt
colormatrix <- ifelse(tab_2[, -(1:2)] =="Complete", "#D2D0BA", ifelse(tab_2[, -(1:2)] =="Locked", "#B6BE9C", ifelse(tab_2[, -(1:2)] =="Signed", "#7B9E87", ifelse(tab_2[, -(1:2)] =="Monitored", "#2B74A1", ifelse(tab_2[, -(1:2)] =="Incomplete", "#BF5F67", ifelse(tab_2[, -(1:2)] =="Empty", "white", "light grey"))))))
#Stworzenie flextable, która za kolory tła bierze kolory ze stworzonej powyżej matrycy
tab_2 <- tab_2 %>% flextable() %>% bg(j = 3:ncol(tab_2), bg=colormatrix)
tab_2 <- tab_2 %>%
  set_header_labels(values = labels_list)
#reszta parametrów
# tab_2 <- set_header_labels(tab_2, Participant.ID = "Participant ID", Site = "Site", Visit.0= "Visit 0", Visit.1 = "Visit 1" ,Visit.2 =  "Visit 2",Visit.3 =  "Visit 3", End.of.study= "EOS")

tab_2 <- flextable::font(tab_2, fontname = "Open Sans", part = "all")
tab_2  <- bold(tab_2 , bold = TRUE, part = "header")
tab_2 <- fontsize(tab_2, size = 8, part = "body")
tab_2 <- fontsize(tab_2, size = 9, part = "header")
# tab_2 <- width(tab_2, j=c(1:ncol), width = c(1,0.5,1,1,1,1,1))

print("tab2")

########### DRUKOWANIE
my_doc <- read_docx(path = "CDL_ roboczy papier firmowy.docx")

my_doc <- body_add_par(my_doc, "Data Aging - Entered", style = "heading 1")
my_doc <- body_add_par(my_doc, "Summary - Visit Entered", style = "heading 2")
my_doc <- body_add_flextable(my_doc, tab_1)
my_doc <- body_add_par(my_doc, " ")
my_doc <- body_add_par(my_doc, "Listing - Visit Entered", style = "heading 2")
my_doc <- body_add_flextable(my_doc, listing_1)
my_doc <- body_add_par(my_doc, " ")
my_doc <- body_add_break(my_doc)
my_doc <- body_add_par(my_doc, "Visit completion", style = "heading 2")
my_doc <- body_add_par(my_doc, "*The number of days shows for how long the visit should have been entered into the eCRF", style = "Subtitle")
my_doc <- body_add_flextable(my_doc, tab_2)

return(my_doc)
}




