

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
                       selectInput(ns("study_name_dm"), label = "Select or enter a study name:", choices = c("DEBRISOFT"), selected = "DEBRISOFT")
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
                       )
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
                       )
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
      
      write_sharepoint_file <- function(data, file_name) {
        temp_file <- tempfile(fileext = ".xlsx")
        write_xlsx(data, temp_file)
        drv$upload_file(temp_file, paste0(STUDY, file_name))
      }
      
      
      
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
      
      # Reactive value for studies list
      studies_list_dm <<- reactiveVal()
      
      # Load studies list from CSV file and update choices
      observe({
        studies <- read.csv("studies_dm.csv", header = TRUE, stringsAsFactors = FALSE)
        studies_list_dm(studies)
        study_choices <- c(unique(studies_list_dm()$Study_Name))
        updateSelectInput(session, "study_name_dm", choices = study_choices, selected = study_choices[1])
      })
      
      # Render studies table
      output$studies_table_dm <- renderDT({
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
        studies <- studies[-input$studies_table_dm_rows_selected, , drop = FALSE]
        write.csv(studies, "studies_dm.csv", row.names = FALSE)
        studies_list_dm(studies)
      })
      
      # Add study button action
      observeEvent(input$add_study_dm, {
        new_study_name_dm <<- input$new_study_dm
        if (nchar(new_study_name_dm) > 0) {
          new_study_dm <- data.frame(Study_Name = new_study_name_dm, stringsAsFactors = FALSE)
          studies <- rbind(studies_list_dm(), new_study_dm)
          write.csv(studies, "studies_dm.csv", row.names = FALSE)
          studies_list_dm(studies)
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
        req(input$variables_template)
        req(input$variables_name_new)
        
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
      
      
      read_sharepoint_file <- function(variables_data_name) {
        temp_file <- tempfile(fileext = ".xlsx")
        drv$download_file(paste0(STUDY, variables_data_name), dest = temp_file)
        # return(readxl::read_xlsx(temp_file))
        report_description <- read_excel(temp_file, sheet = "RaportDescription")
        # print(report_description)
        sheets <- as.character(report_description$items_names)
        # sheets <- c("Recruitment" ,"Eligibility", "EOS"     ,    "AE"     ,     "SAE"  , "Descriptions")
        #print(sheets)
        variables_list <- lapply(sheets, function(sheet) {
          read_excel(temp_file, sheet = sheet)
          # filter(!is.na(ExportTag))  # Filter out rows with missing ExportTag
        })
        
        #print(variables_list)
        # Name the elements of the list based on sheet names
        names(variables_list) <- sheets
        #print(variables_list)
        return(variables_list)
      }
      
      
      
      observeEvent(input$study_name_dm, {
        req(input$study_name_dm)
        template_name <- paste0(input$study_name_dm, ".xlsx")
        variables <<- read_sharepoint_file(template_name)
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


