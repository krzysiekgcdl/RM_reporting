

rolesUI <- function(id) {
  ns <- NS(id)
  div(id = id,
    tagList(
      box(title = "User Roles", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
          fluidRow(
            column(9, p("The table can be filtered using the options at the top of each column. Click on a row to edit it below. If you want to update the table, click the refresh button. If you want to add a new column, click the add role button.")),
            column(3, actionButton(ns("refresh_roles_table"), "Refresh table", icon = icon("refresh")))
          ),
          actionButton(ns("add_role_btn"), "Add role", icon = icon("plus")),
          br(), br(),
          DTOutput(ns("roles_table")) %>% withSpinner(color=PLOT_SPINNER_COLOR)
      ),
      uiOutput(ns("preview_roles_panel"))
    )
  )
}

rolesServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      ns <- session$ns
      
      current_role_table <- reactiveVal(NULL)
      
      observe({
        current_role_table(user_roles())
      })
      
      get_display_table <- function(raw_table) {
        cols <- sapply(raw_table, is.numeric)
        raw_table[,cols] <- lapply(raw_table[,cols], as.logical)
        return(raw_table)
      }
      
      # Render user table
      output$roles_table <- renderDT({
        datatable(get_display_table(current_role_table()), 
                  selection = list(mode = "single", target = "column", selectable = c(-0:-3)), 
                  options = list(pageLength = 20, scrollX = TRUE),
                  editable = FALSE) %>%
          formatStyle(columns = c(2:(max(2,ncol(get_display_table(current_role_table()))))), backgroundColor = styleEqual(c(F,T), c("white", "lightgreen")))
      })
      
      observeEvent(input$refresh_roles_table, {
        show_modal_spinner(
          spin = "double-bounce",
          color = MODAL_SPINNER_COLOR,
          text = "Please wait, refreshing roles...",
          session = shiny::getDefaultReactiveDomain()
        )
        roles_from_DB <- read_roles()
        current_role_table(roles_from_DB)
        user_roles(roles_from_DB)
        remove_modal_spinner()
      })
      
      # UI for editing role
      output$preview_roles_panel <- renderUI({
        selected <- input$roles_table_columns_selected
        if (length(selected) == 0) return(NULL)

        roles_df <- current_role_table()
        if (!(selected %in% colnames(roles_df))) {return(NULL)}
        role_data <- roles_df[,c(1, 2, selected)]
        
        #https://github.com/rstudio/DT/issues/93#issuecomment-111001538
        # create a character vector of shiny inputs
        make_checkboxes <- function(rowlength, id, values, FUN) {
          inputs <- character(rowlength)
          for (i in seq_len(rowlength)) {
            inputs[i] = sprintf(
              '<input id="%s" type="checkbox" class="shiny-input-checkbox" %s/>',
              session$ns(paste0(id, i)), ifelse(values[i] == 0, "", "checked")) #as.character(FUN(paste0(id, i), label = NULL, value = values[i]))
          }
          inputs
        }
        
        # obtain the values of inputs
        shinyValue = function(id, len) {
          unlist(lapply(seq_len(len), function(i) {
            value = input[[paste0(id, i)]]
            if (is.null(value)) NA else value
          }))
        }
        
        res = data.frame(
          a = role_data[,1],
          b = role_data[,2],
          c = make_checkboxes(nrow(role_data), 'v2_', role_data[,3], checkboxInput),
          stringsAsFactors = FALSE
        )
        colnames(res) <- colnames(role_data)
        

        box(title = "Edit Roles", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
            DT::renderDataTable(res, server = FALSE, escape = FALSE,  selection = 'none', filter = "none",
                                options = list(
                                  dom = 't', searching = FALSE, ordering = FALSE, paging = FALSE,
                                  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                                )
            ),
            br(),
            div(
              actionButton(ns("delete_role"), "Delete role", icon = icon("trash"), class = "btn btn-danger"),
              actionButton(ns("save_role_changes"), "Save changes", icon = icon("save"), style="float:right;margin-right:1em;")
            )
        )
      })

      
      # 🔥 Delete User (Remove Record Permanently)
      observeEvent(input$delete_role, {
        selected <- input$roles_table_columns_selected
        if (length(selected) == 0) return()
        roles_df <- current_role_table()
        selected_role <- colnames(roles_df)[selected]

        showModal(modalDialog(
          title = "Confirm Deletion",
          p(paste0("Are you sure you want to delete the role '",selected_role,"'? This action cannot be undone.")),
          footer = tagList(
            modalButton("Cancel", icon = icon("times")),
            actionButton(session$ns("confirm_delete"), "Delete", icon = icon("trash"), class = "btn btn-danger")
          )
        ))
      })
      
      observeEvent(input$confirm_delete, {
        selected <- input$roles_table_columns_selected
        if (length(selected) == 0) return()
        
        roles_df <- current_role_table()
        selected_role <- colnames(roles_df)[selected]
        
        updated_roles <- roles_df[, colnames(roles_df)[colnames(roles_df) != selected_role]] 
        delete_role_from_database(selected_role)
        current_role_table(updated_roles)
        user_roles(updated_roles)
        write_audit_log(
          username = session$userData$USERNAME,
          action = "delete_role",
          details = paste0("Role (", selected_role, ") deleted from system.")
        )
        removeModal()
      })
      
      # Handle role edits
      observeEvent(input$save_role_changes, {
        selected <- input$roles_table_columns_selected
        #print(paste0("Selected role: ", selected))
        
        if (length(selected) == 0) return()
        
        show_modal_spinner(
          spin = "double-bounce",
          color = MODAL_SPINNER_COLOR,
          text = "Please wait, updating role data...",
          session = shiny::getDefaultReactiveDomain()
        )
        
        roles_df <- current_role_table()
        selected_role <- colnames(roles_df)[selected]
        
        # obtain the values of inputs
        shinyValue = function(id, len) {
          unlist(lapply(seq_len(len), function(i) {
            value = input[[paste0(id, i)]]
            if (is.null(value)) NA else value
          }))
        }
        
        roles_df[[selected_role]] <- as.numeric(shinyValue('v2_', nrow(roles_df)))

        update_role_database(roles_df[,c("SidebarMenu", "SidebarSubmenu", selected_role)], selected_role)
        current_role_table(roles_df)
        user_roles(roles_df)
        
        write_audit_log(
          username = session$userData$USERNAME,
          action = "edit_role",
          details = paste0("Role (", selected_role, ") has been updated. New data: ",
                           paste(roles_df[,"SidebarMenu"], "-", roles_df[,"SidebarSubmenu"], ":", roles_df[,selected_role], collapse = ", "))
        )
        remove_modal_spinner()
        showNotification("The user has been successfully edited.", type = "message")
        
      })
      
      observeEvent(input$add_role_btn, {
        showModal(modalDialog(
          title = "Add a new role",
          p("Use the input below to name your new role. Note that this cannot be edited later, so if you'd like to change it you need to delete it and remake it."),
          textInput(session$ns("new_role_name"), "New role"),
          uiOutput(session$ns("role_hint")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_new_role"), "Confirm")
          )
        ))
      })
      
      observeEvent(input$confirm_new_role, {
        if (input$new_role_name %in% colnames(current_role_table())) {
          output$role_hint <- renderUI({HTML(as.character(p("Role is not unique. Please select a different role name.", style = "color:red;")))})
          role_ok <- FALSE
        } else {
          output$role_hint <- renderUI({HTML(as.character(p("Role name OK.", style = "color:green;")))})
          role_ok <- TRUE
        }
        
        if (!(role_ok)) {
          showNotification("Information missing or incorrect. Role could not be added.", type = "warning")
          return()
        }
        
        show_modal_spinner( #Putting this here otherwise it flashes too fast during the pre-checks
          spin = "double-bounce",
          color = MODAL_SPINNER_COLOR,
          text = "Please wait, adding role...",
          session = shiny::getDefaultReactiveDomain()
        )
        
        temp_role <- input$new_role_name
        updated_roles <- write_roles(temp_role) #Add new role to database
        current_role_table(updated_roles)
        user_roles(updated_roles)
        output$role_hint <- renderUI({HTML(as.character(p("Role name must be unique")))})

        updateTextInput(session, "new_role_name", value = "")

        write_audit_log(
          username = session$userData$USERNAME,
          action = "add_role",
          details = paste("Added role:", temp_role)
        )
          
        remove_modal_spinner()
        showNotification("Role has been added to the system.")
        
      })
      
      
    }
  )
}