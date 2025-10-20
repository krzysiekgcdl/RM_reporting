

dataAgingUI <- function(id) {
  ns <- NS(id)
  div(id=id,
      tagList(
        tabsetPanel(
          tabPanel("Upload",
                   fluidRow(
                     box(
                       width = 4, height = "200px",
                       solidHeader = TRUE, status = "primary",
                       title = "Select the study name",
                       selectInput(ns("study_name_aging"), label = "Select or enter a study name:", choices = c("DEBRISOFT"), selected = "DEBRISOFT")
                     ),
                     box(
                       width = 4, height = "200px",
                       solidHeader = TRUE, status = "primary",
                       title = "Upload All Data file",
                       fileInput(ns("all_data_for_aging"), label = "Upload 'All Data' Excel file", accept = c(".xlsx"))
                     ),
                     box(
                       width = 4, height = "200px",
                       solidHeader = TRUE, status = "primary",
                       title = "Upload Audit Log",
                       fileInput(ns("audit_log"), label = "Upload 'Audit Log' Excel file", accept = c(".xlsx"))
                     ),
                     box(
                       width = 4, height = "200px",
                       solidHeader = TRUE, status = "primary",
                       title = "Upload Overview File",
                       fileInput(ns("overview_file"), label = "Upload 'Overview (with subchapters)' Excel file", accept = c(".xlsx"))
                     ),
                     conditionalPanel(
                       condition = "output.filesUploaded_aging",
                       box(
                         width = 4, height = "200px",
                         solidHeader = TRUE, status = "primary",
                         title = "Report Download",
                         p(strong("Click the button below to download the report.")),
                         downloadButton(ns("download_auditlog"), label = "Download Report")
                       ),
                       ns=NS(id)
                     )
                   ),
                   fluidRow(
                     conditionalPanel(
                       condition = "output.filesUploaded_aging",
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Data Preview",
                         dataTableOutput(ns("read_audit_log"))
                       ),
                       ns=NS(id)
                     )
                   )
          )
        )
      )
  )
}

dataAgingServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      

      ###################################### DATA AGING ###########################
      
      # Reactive values for file inputs
      all_data_for_aging <- reactiveVal(NULL)   
      audit_log <- reactiveVal(NULL)             # Stores the audit log data
      overview_file <- reactiveVal(NULL)         # Stores the overview file data
      study_name_aging <- reactiveVal(NULL)      # Stores the selected study name
      variables_list_aging <- reactiveVal(NULL)  # List of variables loaded
      
      # # Reactive value for studies list
      # studies_list_da <- reactiveVal()
      # 
      # # Load studies list from CSV file
      # observe({
      #   studies <- read.csv("studies_dm.csv", header = TRUE, stringsAsFactors = FALSE)
      #   studies_list_da(studies)
      # })
      
      # Update choices for study name selectInput
      observe({
        study_choices <- load_studies_list("DM")
        #study_choices <- c(unique(studies_list_dm()$Study_Name))
        updateSelectInput(session, "study_name_aging", choices = study_choices, selected = study_choices[1])
      })
      
      # Update Study Name
      observeEvent(input$study_name_aging, {
        req(input$study_name_aging)
        study_name_aging(input$study_name_aging)
        showNotification(paste("Study selected:", input$study_name_aging), type = "message")
      })
      
      # Function to Read Excel File from SharePoint for Chapters
      read_sharepoint_xlsx_file_aging <- function(file_name) {
        temp_file <- tempfile(fileext = ".xlsx")
        drv$download_file(paste0(STUDY_DA_CHAPTERS_PATH, file_name), dest = temp_file)
        read_excel(temp_file)
      }
      
      # Load Data from Chapters
      observeEvent(study_name_aging(), {
        req(study_name_aging())
        template_name <- paste0(study_name_aging(), ".xlsx")
        tryCatch({
          com.dat_ch <<- read_sharepoint_xlsx_file_aging(template_name)
          showNotification("Chapter data loaded successfully!", type = "message")
        }, error = function(e) {
          showNotification(paste("Error loading chapters:", e$message), type = "error")
        })
      })
      
      # Load Variables Data from Template
      observeEvent(study_name_aging(), {
        req(study_name_aging())
        template_name <- paste0(study_name_aging(), ".xlsx")
        
        tryCatch({
          variables_list_aging(read_sharepoint_file("DataAging_Templates", template_name))
          showNotification("Data Aging Variables data loaded successfully!", type = "message")
          #print(variables_list_aging())
        }, error = function(e) {
          showNotification(paste("Error loading data aging variables data:", e$message), type = "error")
        })
      })
      
      # Check if Files are Uploaded (Audit Log and Overview)
      output$filesUploaded_aging <- reactive({
        !is.null(audit_log()) && !is.null(overview_file()) && !is.null(all_data_for_aging())
      })
      outputOptions(output, "filesUploaded_aging", suspendWhenHidden = FALSE)
      
      # Read All Data File (NO CLEANING)
      observeEvent(input$all_data_for_aging, {
        req(input$all_data_for_aging)
        tryCatch({
          all_data_for_aging(read_excel(input$all_data_for_aging$datapath))
          showNotification("All data for aging loaded successfully!", type = "message")
          output$read_audit_log <- renderDataTable(all_data_for_aging())
        }, error = function(e) {
          showNotification(paste("Error reading all data for aging:", e$message), type = "error")
        })
      })
      
      # Read Audit Log File (CLEANING APPLIED HERE)
      observeEvent(input$audit_log, {
        req(input$audit_log)
        tryCatch({
          audit_log(read_excel(input$audit_log$datapath) %>% clean_dataframe())
          showNotification("Audit log loaded successfully!", type = "message")
          output$read_audit_log <- renderDataTable(audit_log())
        }, error = function(e) {
          showNotification(paste("Error reading audit log:", e$message), type = "error")
        })
      })
      
      # Read Overview File (NO CLEANING)
      observeEvent(input$overview_file, {
        req(input$overview_file)
        tryCatch({
          overview_file(read_excel(input$overview_file$datapath))
          showNotification("Overview file loaded successfully!", type = "message")
        }, error = function(e) {
          showNotification(paste("Error reading overview file:", e$message), type = "error")
        })
      })
      
      
      # Download Handler for the Final Report
      output$download_auditlog <- downloadHandler(
        filename = function() {
          paste("Data_Aging_Report_", study_name_aging(), "_", Sys.Date(), ".docx", sep = "")
        },
        content = function(file) {
          showModal(modalDialog("Please wait. The report is being generated...", footer = NULL))
          on.exit(removeModal())
          tryCatch({
            # Generate the report using the provided function
            doc <- data_aging_fun(session, input, output, audit_log(), overview_file(), variables_list_aging(), all_data_for_aging())
            print(doc, target = file)
            showNotification("Report generated successfully!", type = "message")
          }, error = function(e) {
            showNotification(paste("Error generating report:", e$message), type = "error")
          })
        }
      )
      
    }
  )
}


