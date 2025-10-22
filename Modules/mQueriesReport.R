

queriesReportUI <- function(id) {
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
                       selectInput(ns("study_name"), label = "Select or enter a study name:", choices = c())
                     ),
                     box(
                       width = 4, height = "200px",
                       solidHeader = TRUE, status = "primary",
                       title = "Upload File",
                       fileInput(ns("source_file"), label = "Upload query export from Research Manager in .xlsx format. The filename should start with 'QueriesExport_YYYYMMDD'.")
                     ),
                     conditionalPanel(
                       condition = "output.fileUploaded",
                       box(
                         width = 4, height = "200px",
                         solidHeader = TRUE, status = "primary",
                         title = "Report Download",
                         p(strong("Click the button below to download the report.")),
                         downloadButton(ns("downloadWord"), label = "Download")
                       ),
                       ns=NS(id)
                     )
                   ),
                   fluidRow(
                     conditionalPanel(
                       condition = "output.fileUploaded",
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Data Preview",
                         dataTableOutput(ns("read_data"))
                       ),
                       ns=NS(id)
                     )
                   )
          ),
          tabPanel("Studies List",
                   fluidRow(
                     box(
                       width = 12,
                       solidHeader = TRUE,
                       title = "Studies List",
                       DT::dataTableOutput(ns("studies_table"))
                     )
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       solidHeader = TRUE,
                       title = "Add New Study",
                       textInput(ns("new_study"), label = "New Study Name:"),
                       actionButton(ns("add_study"), "Add Study")
                     ),
                     box(
                       width = 12,
                       solidHeader = TRUE,
                       title = "Remove Study",
                       actionButton(ns("remove_study"), "Remove Selected Study")
                     )
                   )
          )
        )
      )
  )
}

queriesReportServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$fileUploaded <- reactive({
        req(!is.null(input$source_file))
        if(! grepl("\\.xlsx$", input$source_file$name) ) {
          showNotification("Please upload a file in .xlsx format", type = "warning")
          return(NULL)
        }
        pattern <- "^QueriesExport_[0-9]{8}"
        if ( ! grepl(pattern, input$source_file$name)){
          showNotification("Please upload a Research Manager queries report. The filename should start with the format: 'QueriesExport_YYYYMMDD'", type = "warning")
          return(NULL)
        }
        !is.null(input$source_file)
      })
      
      
      # Define UI logic for download button and data preview
      outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
      
      # Reactive value for studies list
      studies_list <- reactiveVal(NULL)
      
      # Load studies list from CSV file
      observe({
        #studies <- read.csv("studies_queries.csv", header = TRUE, stringsAsFactors = FALSE)
        if (is.null(studies_list())) {
          studies <- load_studies_list("Queries")
          studies_list(studies)
        }
        # studies <- load_studies_list("Queries")
        # studies_list(studies)
        # study_choices <- c(unique(studies_list()$Study_Name))
        updateSelectInput(session, "study_name", choices = studies_list()$Study_Name, selected = studies_list()[1,c("Study_Name")])
      })
      
      # Render studies table
      output$studies_table <- renderDT({
        req(studies_list())
        datatable(
          studies_list(), rownames = FALSE, selection = 'multiple', escape = FALSE,
          options = list(
            columnDefs = list(list(className = 'dt-center', targets = '_all')),
            dom = 'Bfrtip',
            buttons = list(
              'copy', 'csv',
              list(
                extend = 'collection',
                buttons = c('excel', 'pdf'),
                text = 'Download',
                filename = "studies_table"
              )
            )
          )
        )
      })
      
      # Remove button action
      observeEvent(input$remove_study, {
        req(input$studies_table_rows_selected)
        studies <- studies_list()
        delete_from_database(STUDIES_DB_NAME, "Study", studies[input$studies_table_rows_selected, c("Study_Name")])
        #studies <- studies[-input$studies_table_rows_selected, , drop = FALSE]
        studies_list(load_studies_list("Queries", TRUE))
      })
      
      # Add study button action
      observeEvent(input$add_study, {
        new_study_name <- input$new_study
        if (nchar(new_study_name) > 0) {
          new_study <- data.frame(
            Study = as.character(new_study_name),
            Queries = 1
          )
          write_audit_log(username = session$userData$USERNAME,
                          action = "add_study",
                          details = paste0("Added ", new_study_name, " to list of queries studies.")
          )
          studies <- add_db_data_return(STUDIES_DB_NAME, new_study)
          studies_list(load_studies_list("Queries", TRUE))
          updateTextInput(session, "new_study", value = "")
          showNotification(paste("Added new study:", new_study_name), type = "message")
        } else {
          showNotification("Could not add study. Name must be unique and at least one character long.", type = "error")
        }
      })
      
      ### Writing in study name
      observeEvent(input$study_name, {
        study_name <<- input$study_name
      })
      
      ### Reading a source file
      observeEvent(input$source_file, {
        req(input$source_file)
        req(input$study_name)
        
        if(! grepl("\\.xlsx$", input$source_file$name) ) {
          return(NULL)
        }
        
        pattern <- "^QueriesExport_[0-9]{8}"
        if ( ! grepl(pattern, input$source_file$name)){
          
          return(NULL)
        }
        
        query_data <<- read.xlsx(input$source_file$datapath)
        query_data_name <<- basename(input$source_file$name)
        output$read_data <- renderDataTable(query_data)
      })
      
      ### A button to download a final report
      output$downloadWord <- downloadHandler(
        filename = function() {
          paste("Query_Report_", study_name, "_", Sys.Date(), ".docx", sep = "")
        },
        content = function(file) {
          showModal(modalDialog("Please wait. The report is being generated...", footer=NULL))
          on.exit(removeModal())
          data_to_be_reported <- query_data
          doc <- raport_querow(session, input, output, data_to_be_reported)
          print(doc, target = file)
        }
      )

      
    }
  )
}


