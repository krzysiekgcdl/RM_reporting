
# Helper to clean text: removes \n, \r, \t, double spaces, trims ends
clean_text <- function(x) {
  if (is.character(x)) {
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
  }
  x
}

# Clean a whole data frame: values and column names
clean_dataframe <- function(df) {
  df <- df %>%
    mutate(across(where(is.character), clean_text))
  names(df) <- clean_text(names(df))
  df
}

connect_to_sharepoint <- function() {
  gr <- AzureGraph::create_graph_login(
    tenant=Sys.getenv("SP_TENANT"),
    app=Sys.getenv("SP_APP"),
    password=Sys.getenv("SP_PASS")
  )
  site <- gr$get_sharepoint_site("https://cleandatalabs.sharepoint.com/sites/Dysk")
  drv <<- site$get_drive("Dyski")
}

load_temp_scripts <- function() {
  scripts <- c("f_raport_querow_ver2_2.R", "generate_dm_report_v3.0.R", "generalised_reporting_data_aging_v2.0.R")
  for (script in scripts) {
    temp_file <- tempfile(fileext = ".R")
    drv$download_file(paste0(main_scripts_dir, "Scripts/", script), dest = temp_file)
    source(temp_file)
  }
}

load_studies_list <- function(tab, refresh=FALSE) {
  if (is.null(STUDIES_LIST) || refresh) {
    STUDIES_LIST <<- read_from_DB(STUDIES_DB_NAME)
  }
  res <- data.frame(Study_Name = STUDIES_LIST[STUDIES_LIST[[tab]] == TRUE, c("Study")])
  return(res)
}