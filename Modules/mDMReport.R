

DMReportUI <- function(id) {
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
                       selectInput(ns("study_name_dm"), label = "Select or enter a study name:", choices = c())
                     ),
                     box(
                       width = 4, height = "200px",
                       solidHeader = TRUE, status = "primary",
                       title = "Upload File",
                       fileInput(ns("source_file_all_data"), label = "Upload all data export from Research Manager in .xlsx format. The filename should start with 'export_YYYYMMDD'.")
                     ),
                     conditionalPanel(
                       condition = "output.fileUploaded_dm",
                       box(
                         width = 4, height = "200px",
                         solidHeader = TRUE, status = "primary",
                         title = "Report Download",
                         p(strong("Click the button below to download the report.")),
                         downloadButton(ns("downloadWord_dm"), label = "Download")
                       ),
                       ns=NS(id)
                     )
                   ),
                   fluidRow(
                     conditionalPanel(
                       condition = "output.fileUploaded_dm",
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Data Preview",
                         dataTableOutput(ns("read_data_dm"))
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
                       DT::dataTableOutput(ns("studies_table_dm"))
                     )
                   ),
                   fluidRow(
                     box(
                       width = 12,
                       solidHeader = TRUE,
                       title = "Add New Study",
                       textInput(ns("new_study_dm"), label = "New Study Name:"),
                       actionButton(ns("add_study_dm"), "Add Study")
                     ),
                     box(
                       fileInput(ns("variables_template"), "Upload new template in .xlsx format", accept = c(".xlsx")),
                       textInput(ns("variables_name_new"), "Enter New Filename (study name)"),
                       actionButton(ns("save"), "Save File"),
                       verbatimTextOutput(ns("status_new_tempate"))
                     ),
                     fluidRow(
                       tableOutput(ns("preview"))
                     ),
                     box(
                       width = 12,
                       solidHeader = TRUE,
                       title = "Remove Study",
                       actionButton(ns("remove_study_dm"), "Remove Selected Study")
                     )
                   )
          )
        )
      )
  )
}

DMReportServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      ######### DM REPORT
      
      variables <<- NULL
      all_data <<- NULL
      # Reactive value for studies list
      studies_list_dm <- reactiveVal(NULL)
      
      
      output$fileUploaded_dm <- reactive({
        req(!is.null(input$source_file_all_data))
        if(! grepl("\\.xlsx$", input$source_file_all_data$name) ) {
          showNotification("Please upload a file in .xlsx format", type = "warning")
          return(NULL)
        }
        pattern <- "^export-[0-9]{8}"
        if ( ! grepl(pattern, input$source_file_all_data$name)){
          showNotification("Please upload a Research Manager queries report. The filename should start with the format: 'QueriesExport_YYYYMMDD'", type = "warning")
          return(NULL)
        }
        !is.null(input$source_file_all_data)
      })
      
      # Define UI logic for download button and data preview
      outputOptions(output, "fileUploaded_dm", suspendWhenHidden = FALSE)
      
      # Load studies list from CSV file and update choices
      observe({
        #studies <- read.csv("studies_dm.csv", header = TRUE, stringsAsFactors = FALSE)
        if (is.null(studies_list_dm())) {
          studies <- load_studies_list("DM")
          studies_list_dm(studies)
        }
        
        # study_choices <- c(unique(studies_list_dm()$Study_Name))
        updateSelectInput(session, "study_name_dm", choices = studies_list_dm()$Study_Name, selected = studies_list_dm()[1,c("Study_Name")])
      })
      
      # Render studies table
      output$studies_table_dm <- renderDT({
        req(studies_list_dm())
        datatable(
          studies_list_dm(), rownames = FALSE, selection = 'multiple', escape = FALSE,
          options = list(
            columnDefs = list(list(className = 'dt-center', targets = '_all')),
            dom = 'Bfrtip',
            buttons = list(
              'copy', 'csv',
              list(
                extend = 'collection',
                buttons = c('excel', 'pdf'),
                text = 'Download',
                filename = "studies_table_dm"
              )
            )
          )
        )
      })
      
      # Remove button action
      observeEvent(input$remove_study_dm, {
        req(input$studies_table_dm_rows_selected)
        studies <- studies_list_dm()
        delete_from_database(STUDIES_DB_NAME, "Study", studies[input$studies_table_dm_rows_selected, c("Study_Name")])
        studies <- studies[-input$studies_table_dm_rows_selected, , drop = FALSE]
        studies_list_dm(studies)
      })
      
      # Add study button action
      observeEvent(input$add_study_dm, {
        new_study_name_dm <- input$new_study_dm
        if (nchar(new_study_name_dm) > 0) {
          new_study <- data.frame(
            Study = as.character(new_study_name_dm),
            DM = 1
          )
          studies <- add_db_data_return(STUDIES_DB_NAME, new_study)
          studies_list_dm(studies$Studies)
          updateTextInput(session, "new_study_dm", value = "")
        }
      })
      
      
      #### ad new study template
      
      uploaded_data <- reactive({
        req(input$variables_template)
        read_excel(input$variables_template$datapath)
      })
      
      output$preview <- renderTable({
        req(uploaded_data())
        head(uploaded_data(), 10)  # Show first 10 rows
      })
      
      observeEvent(input$save, {
        req(input$variables_template, input$variables_name_new)

        # Write the file
        tryCatch({
          write_sharepoint_file(uploaded_data(), input$variables_name_new)
          output$status_new_tempate <- renderText(paste("File saved successfully as", input$variables_name_new))
        }, error = function(e) {
          output$status_new_tempate <- renderText("Error saving file. Please check the filename and try again.")
        })
      })
      
      
      ### Writing in study name
      observeEvent(input$study_name_dm, {
        study_name_dm <<- input$study_name_dm
      })
      
      observeEvent(input$study_name_dm, {
        req(input$study_name_dm)
        template_name <- paste0(input$study_name_dm, ".xlsx")
        variables <<- read_sharepoint_file("Templates",template_name)
      })
      
      
      ### Reading a source file
      observeEvent(input$source_file_all_data, {
        req(input$source_file_all_data)
        req(input$study_name_dm)
        
        if(! grepl("\\.xlsx$", input$source_file_all_data$name) ) {
          return(NULL)
        }
        
        pattern <- "^export-[0-9]{8}"
        if ( ! grepl(pattern, input$source_file_all_data$name)){
          
          return(NULL)
        }
        
        all_data <<- read_excel(input$source_file_all_data$datapath)
        all_data_name <<- basename(input$source_file_all_data$name)
        output$read_data_dm <- renderDataTable(all_data[1:5, 1:5])
      })
      
      observeEvent(input$downloadWord_dm, {
        withProgress(message = "Processing...", value = 0, {
          for (i in 1:10) {
            Sys.sleep(0.5) # Simulate a time-consuming task
            incProgress(1/10) # Increment the progress
          }
        })
      })
      
      ## A button to download a final report
      output$downloadWord_dm <- downloadHandler(
        filename = function() {
          print(study_name_dm)
          paste("DM_Report_", study_name_dm, "_", Sys.Date(), ".docx", sep = "")
        },
        content = function(file) {
          print("rozpocznij drukowanie")
          showModal(modalDialog("Please wait. The report is being generated...", footer=NULL))
          # Create a new Progress object
          progress <- shiny::Progress$new()
          progress$set(message = "Generating report...", value = 0)
          on.exit(progress$close(), add = TRUE)
          
          excel_temp <- tempfile(fileext = ".xlsx")
          drv$download_file(paste0(main_scripts_dir, "Scripts/ScriptVersions.xlsx"), dest = excel_temp)
          script_versions <<- read_excel(excel_temp)
          
          data_to_be_reported <- all_data
          doc <- generate_dm_report(session, input, output, data_to_be_reported, progress)
          print(doc, target = file)
          
          removeModal()
        }
      )
      
    }
  )
}


