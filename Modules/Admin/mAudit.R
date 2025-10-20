

auditUI <- function(id) {
  ns <- NS(id)
  div(id=id,
      tagList(
        box(title = "Audit Log", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
            fluidRow(
              column(9, p("The table can be filtered using the options at the top of each column. Click on a row to see the full details below. If you want to update the table, click the refresh button.")),
              column(3, actionButton(ns("refresh_auditlogs_table"), "Refresh audit tables", icon = icon("refresh")))
            ),
            br(),
            DTOutput(ns("auditlogs_table")) %>% withSpinner(color=PLOT_SPINNER_COLOR)
        ),
        uiOutput(ns("preview_auditlog_panel"))
      )
  )
}

auditServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      get_display_table <- function(raw_table) {
        return(raw_table[, c("timestamp", "usertype", "version", "user", "action", "details")])
      }
      
      current_auditlog_table <- reactiveVal(NULL)
      
      # Render auditlogs table
      output$auditlogs_table <- renderDT({
        if (is.null(current_auditlog_table())) {current_auditlog_table(read_audit_log())}
        datatable(get_display_table(current_auditlog_table()) %>% replace(is.na(.), ""), 
                  selection = "single", 
                  filter = "top",
                  extensions = 'Buttons',
                  options = list(
                    pageLength = 10, 
                    lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
                    order = list(list(1, 'desc')), #Order newest first
                    scrollX = TRUE, 
                    dom = 'Blfrtip', 
                    buttons = list(
                      list(
                        extend = "csv",
                        text = "Download CSV",
                        filename = AUDIT_LOG_DOWNLOAD_FILENAME
                      ),
                      list(
                        extend = "excel",
                        text = "Download Excel",
                        filename = AUDIT_LOG_DOWNLOAD_FILENAME
                      ),
                      list(
                        extend = "copy",
                        text = "Copy",
                        filename = AUDIT_LOG_DOWNLOAD_FILENAME
                      ),
                      list(
                        extend = "print",
                        text = "Print",
                        filename = AUDIT_LOG_DOWNLOAD_FILENAME
                      )
                    ),
                    columnDefs = list(
                      list(
                        targets = 6,
                        render = JS(
                          "function(data, type, row, meta) {",
                          "return type === 'display' && data.length > 50 ?",
                          "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                          "}")))
                  ),
                  editable = FALSE)
      })
      
      observeEvent(input$refresh_auditlogs_table, {
        
        show_modal_spinner(
          spin = "double-bounce",
          color = MODAL_SPINNER_COLOR,
          text = "Please wait, refreshing audit log data...",
          session = shiny::getDefaultReactiveDomain()
        )
        
        auditlogs <- read_audit_log()
        current_auditlog_table(auditlogs)
        output$auditlogs_table <- renderDT({
          datatable(get_display_table(auditlogs) %>% replace(is.na(.), ""), 
                    selection = "single", 
                    filter = "top",
                    extensions = 'Buttons',
                    options = list(
                      pageLength = 10, 
                      lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
                      order = list(list(1, 'desc')), #Order newest first
                      scrollX = TRUE, 
                      dom = 'Blfrtip', 
                      buttons = list(
                        list(
                          extend = "csv",
                          text = "Download CSV",
                          filename = AUDIT_LOG_DOWNLOAD_FILENAME
                        ),
                        list(
                          extend = "excel",
                          text = "Download Excel",
                          filename = AUDIT_LOG_DOWNLOAD_FILENAME
                        ),
                        list(
                          extend = "copy",
                          text = "Copy",
                          filename = AUDIT_LOG_DOWNLOAD_FILENAME
                        ),
                        list(
                          extend = "print",
                          text = "Print",
                          filename = AUDIT_LOG_DOWNLOAD_FILENAME
                        )
                      ),
                      columnDefs = list(
                        list(
                          targets = 6,
                          render = JS(
                            "function(data, type, row, meta) {",
                            "return type === 'display' && data.length > 50 ?",
                            "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                            "}")))
                    ),
                    editable = FALSE)
        })
        
        remove_modal_spinner()
      })
      
      # UI for editing auditlog
      output$preview_auditlog_panel <- renderUI({
        selected <- input$auditlogs_table_rows_selected
        if (length(selected) == 0) return(NULL)
        
        # print(paste("Selected AuditLog: ", selected))
        # print(paste("Current dims: ", paste(dim(current_auditlog_table()), collapse = "x" )))
        auditlogs_df <- current_auditlog_table()
        auditlog_data <- auditlogs_df[selected,]
        auditlog_data$timestamp <- unlist(auditlog_data$timestamp)
        auditlog_data$timestamp2 <- auditlog_data$timestamp
        auditlog_data$user <- unlist(auditlog_data$user)
        auditlog_data$action <- unlist(auditlog_data$action)
        auditlog_data$details <- unlist(auditlog_data$details)
        
        attr(auditlog_data$timestamp2, "tzone") <- Sys.timezone()
        
        # print("auditlog_data")
        # print(auditlog_data)
        
        box(title = "Preview Audit Log", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
          fluidRow(
            column(4, 
                   div(
                     tags$label(paste0("Timestamp (",APP_TIMEZONE,")"), class = "form-label"),
                     tags$input(id = ns("preview_timestamp"), type = "text", 
                                class = "form-control", value = (auditlog_data$timestamp), readonly = NA) # + lubridate::hours(2)
                   )),
            column(4, 
                   div(
                     tags$label("User", class = "form-label"),
                     tags$input(id = ns("preview_user"), type = "text", 
                                class = "form-control", value = auditlog_data$user, readonly = NA)
                   ))),
          br(),
          fluidRow(
            column(4,
                   div(
                     tags$label(paste0("Timestamp (",Sys.timezone(),")"), class = "form-label"),
                     tags$input(id = ns("preview_timestamp"), type = "text", 
                                class = "form-control", value = (auditlog_data$timestamp2), readonly = NA)
                   )),
            column(4,
                   div(
                     tags$label("Action", class = "form-label"),
                     tags$input(id = ns("preview_action"), type = "text", 
                                class = "form-control", value = auditlog_data$action, readonly = NA)
                   ))
          ),
          br(),
          fluidRow(
            column(8, 
                   div(
                     tags$label("Details", class = "form-label"),
                     tags$textarea(id = ns("preview_details"), rows = 20,
                                   class = "form-control", readonly = NA, auditlog_data$details)
                   ))
          )
        )
      })
    }
  )
}


