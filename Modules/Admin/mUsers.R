usersUI <- function(id) {
  ns <- NS(id)
  div(
    id = id,
    tagList(
      tabsetPanel(
        tabPanel("Add Users",
          br(),
          box(title = "Add New Users", width = 12, solidHeader = T, status = "primary",
              p("Use the following form to add new users to the application. Once the user has been added, a randomly generated password will be sent to the given email address along with their username for the user to log in."),
              uiOutput(ns("newuser_success")),
              textInput(ns("user"), "Username", width = "33%"),
              uiOutput(ns("username_hint")),
              hr(),
              textInput(ns("email"), "Email Address", width = "33%"),
              uiOutput(ns("email_hint")),
              hr(),
              fluidRow(
                column(4, selectInput(ns("role"), "User Role", choices = c()))#,
                #column(4, HTML("<br>"), checkboxInput(ns("admin"), "Is Administrator"))
              ),
              p("Select a role for the user."),
              hr(),
              fluidRow(
                column(4, dateInput(ns("expire"), "Expiry Date", value = "2030-01-01")),
                column(4, checkboxInput(ns("first_login"), "Show password change request on login", value = T),
                       checkboxInput(ns("skip_mfa"), "Skip sending a code on login (MFA)", value = F))
              ),
              p("If you wish to have the user unable to access the app after a certain date, change their expiry date. Otherwise, it is recommended to keep these settings as default."),
              br(),
              actionButton(ns("add_user_btn"), "Add user", icon = icon("user-plus"), style = "width:100%;")
          )
        ),
        tabPanel("Edit Users",
          br(),
          box(title = "Current Users", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
              fluidRow(
                column(9, p("Select a user from the list to edit their details. If you don't see a recently added user, click the refresh button.")),
                column(3, actionButton(ns("refresh_users_table"), "Refresh user table", icon = icon("refresh")))
              ),
              hr(),
              DTOutput(ns("users_table")),
          ),
          uiOutput(ns("edit_user_panel")) %>% withSpinner(color=PLOT_SPINNER_COLOR)
        )
      )
    )
  )
}

usersServer <- function(id, starting_users_df) {
  moduleServer(
    id,
    
    function(input, output, session) {
      ns <- session$ns
      
      current_user_table <- reactiveVal(NULL)
      
      observe({
        current_user_table(starting_users_df)
        req(user_roles())
        updateSelectInput(session, "role", choices=colnames(user_roles())[3:ncol(user_roles())])
      })
      
      output$username_hint <- renderUI({
        HTML(as.character(p("Username must be unique.")))
      })
      output$email_hint <- renderUI({
        HTML(as.character(p("Email must contain the @ symbol.")))
      })
      

      #==== ADD USERS ====
      
      observeEvent(input$add_user_btn, {
        if (input$user %in% unique(current_user_table()$user)) {
          output$username_hint <- renderUI({HTML(as.character(p("Username is not unique. Please select a different username.", style = "color:red;")))})
          user_ok <- FALSE
        } else {
          output$username_hint <- renderUI({HTML(as.character(p("Username OK.", style = "color:green;")))})
          user_ok <- TRUE
        }
        if(grepl(EMAIL_CHECK_REGEX, input$email)) {
          output$email_hint <- renderUI({HTML(as.character(p("Email OK.", style = "color:green;")))})
          email_ok <- TRUE
        } else {
          output$email_hint <- renderUI({HTML(as.character(p("Invalid email. Please use a different email.", style = "color:red;")))})
          email_ok <- FALSE
        }
        
        if (!(user_ok & email_ok)) {
          showNotification("Information missing or incorrect. User could not be added.", type = "warning")
          return()
        }
        
        show_modal_spinner( #Putting this here otherwise it flashes too fast during the pre-checks
          spin = "double-bounce",
          color = MODAL_SPINNER_COLOR,
          text = "Please wait, adding user...",
          session = shiny::getDefaultReactiveDomain()
        )
        
        randomized_pass <- generate_password(15) #Make it one character shorter than the intended minimum password
        
        new_user <- data.frame(
          user = as.character(input$user),
          password = scrypt::hashPassword(randomized_pass),
          email = as.character(input$email),
          start = as.character(format(Sys.time(), "%Y-%m-%d", tz = APP_TIMEZONE)),
          expire = input$expire,
          admin = 0,
          role = as.character(input$role),
          first_login = as.numeric(input$first_login),
          skip_mfa = as.numeric(input$skip_mfa),
          is_hashed_password = TRUE
        )

        update_modal_spinner(text = "Please wait, sending email...", session = shiny::getDefaultReactiveDomain())
        
        email_success <- FALSE
        tryCatch(
          expr = {
            email <- envelope( 
              to = input$email, 
              from = "shinyapps@cleandatalabs.com", 
              subject = "CleanDataLabs - Login Details", 
              html = email_login(as.character(input$user),randomized_pass)
            )
            smtp(email)
            email_success <- TRUE
          },
          error = function(e) {
            print(paste("Error in email:", e))
            showNotification("Email failed to send.", type = "warning")
          }
        )
        
        update_modal_spinner(text = "Please wait, updating database...", session = shiny::getDefaultReactiveDomain())
        
        if (email_success) {
          updated_credentials <- add_db_data_return(USER_DB_NAME, new_user) #Add new user to database
          current_user_table(updated_credentials)
          temp_username <- input$user
          output$newuser_success <- renderUI({HTML(as.character(p(paste0("New user '",temp_username,"' has been successfully added."), style = "color:green;")))})
          output$username_hint <- renderUI({HTML(as.character(p("Username must be unique.")))})
          output$email_hint <- renderUI({HTML(as.character(p("Email must contain the @ symbol.")))})

          updateTextInput(session, "user", value = "")
          updateTextInput(session, "email", value = "")
          
          write_audit_log(
            username = session$userData$USERNAME,
            action = "add_user",
            details = paste("Added user:", new_user$user, " with details:",
                            paste(names(new_user), new_user, collapse = ", "))
          )
          
          remove_modal_spinner()
          showNotification("User has been added to the system.")
        } else {
          remove_modal_spinner()
          showNotification("User could not be added.", type = "error")
        }

      })
      
      #==== EDIT USERS ====

      get_display_table <- function(raw_table) {
        #raw_table$admin <- as.logical(raw_table$admin)
        raw_table$first_login <- as.logical(raw_table$first_login)
        raw_table$skip_mfa <- as.logical(raw_table$skip_mfa)
        return(raw_table[, c("user", "email", "role", "first_login", "skip_mfa", "expire")])
      }
      
      # Render user table
      output$users_table <- renderDT({
        datatable(get_display_table(current_user_table()), 
                  selection = "single", 
                  options = list(pageLength = 10, scrollX = TRUE),
                  editable = FALSE) %>%
          formatStyle(columns = "expire", backgroundColor = styleInterval(c(Sys.Date()), c("pink", "white"))) %>%
          formatStyle(columns = c("first_login", "skip_mfa"), backgroundColor = styleEqual(c(F,T), c("white", "lightgreen")))
      })
      
      observeEvent(input$refresh_users_table, {
        show_modal_spinner(
          spin = "double-bounce",
          color = MODAL_SPINNER_COLOR,
          text = "Please wait, refreshing users...",
          session = shiny::getDefaultReactiveDomain()
        )
        users_from_DB <- read_users_from_DB()
        current_user_table(users_from_DB)
        remove_modal_spinner()
      })
      
      # UI for editing user
      output$edit_user_panel <- renderUI({
        selected <- input$users_table_rows_selected
        if (length(selected) == 0) return(NULL)
        
        #print(paste("Selected user: ", selected))
        #print(paste("Current dims: ", paste(dim(current_user_table()), collapse = "x" )))
        users_df <- current_user_table()
        user_data <- users_df[selected,]
        user_data$email <- unlist(user_data$email)
        user_data$start <- unlist(user_data$start)
        user_data$expire <- unlist(user_data$expire)
        #user_data$admin <- unlist(user_data$admin)
        user_data$role <- unlist(user_data$role)
        user_data$first_login <- unlist(user_data$first_login)
        user_data$skip_mfa <- unlist(user_data$skip_mfa)
        user_data$expire <- unlist(user_data$expire)
        
        user_data <- user_data %>% mutate_if(is.character, 
                                             function(x) ifelse(x == "TRUE", 1, ifelse(x == "FALSE", 0, x)))

        output$email_edit_hint <- renderUI(NULL)
        box(title = "Edit Users", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
          fluidRow(
            column(4, 
              tags$label("Username", class = "form-label"),
              tags$input(id = ns("edit_user"), type = "text", 
                        class = "form-control", value = user_data$user, readonly = "readonly")
            )
          ),
          hr(),
          fluidRow(
            column(4, textInput(ns("edit_email"), "Email Address", value = user_data$email),
                      uiOutput(ns("email_edit_hint")),)
          ),
          hr(),
          fluidRow(
            column(4, selectInput(ns("edit_role"), "User Role", choices = colnames(user_roles())[3:ncol(user_roles())], selected = user_data$role))#,
            #column(4, HTML("<br>"), checkboxInput(ns("edit_admin"), "Is Administrator", value = user_data$admin))
          ),
          hr(),
          fluidRow(
            column(4, dateInput(ns("edit_expire"), "Expiry Date", value = user_data$expire)),
            column(4, checkboxInput(ns("edit_first_login"), "Show password change request on login", value = user_data$first_login),
                      checkboxInput(ns("edit_skip_mfa"), "Skip sending a code on login (MFA)", value = user_data$skip_mfa))
          ),
          div(#class = "d-flex justify-content-center mt-4",
              actionButton(ns("save_user_changes"), "Save changes", icon = icon("save")),
              actionButton(ns("delete_user"), "Delete user", class = "btn btn-danger", style="float:right;margin-right:20px;"),
              actionButton(ns("deactivate_user"), "Deactivate user", class = "btn btn-warning", style="float:right;margin-right:1em;")
          )
        )
      })
      
      observeEvent(input$deactivate_user, {
        selected <- input$users_table_rows_selected
        if (length(selected) == 0) return()
        
        users_df <- current_user_table()
        selected_user <- users_df[selected, "user"]

        #Compute yesterday_date and store as yyyy-mm-dd
        yesterday_date <- as.character(Sys.Date() - 1)
        users_df[users_df$user == selected_user, "expire"] <- yesterday_date

        update_user_database(users_df[selected, ], selected_user)
        current_user_table(users_df)
        write_audit_log(
          username = session$userData$USERNAME,
          action = "deactivate_user",
          details = paste0("User (",selected_user,") deactivated. Expiration date: ", yesterday_date)
        )
        showNotification("The user has been successfully deactivated.", type = "warning")
      })
      
      # 🔥 Delete User (Remove Record Permanently)
      observeEvent(input$delete_user, {
        selected <- input$users_table_rows_selected
        if (length(selected) == 0) return()
        users_df <- current_user_table()
        selected_user <- users_df[selected, "user"]
        
        showModal(modalDialog(
          title = "Confirm Deletion",
          "Are you sure you want to delete the user '",selected_user,"'? This action cannot be undone.",
          footer = tagList(
            modalButton("Cancel", icon = icon("times")),
            actionButton(ns("confirm_delete"), "Delete", icon = icon("trash"), class = "btn btn-danger")
          )
        ))
      })
      
      observeEvent(input$confirm_delete, {
        selected <- input$users_table_rows_selected
        if (length(selected) == 0) return()

        users_df <- current_user_table()
        
        selected_user <- users_df[selected, "user"]
        updated_credentials <- users_df[users_df$user != selected_user, ]
        delete_user_from_database(selected_user)
        current_user_table(updated_credentials)
        write_audit_log(
          username = session$userData$USERNAME,
          action = "delete_user",
          details = paste0("User (", selected_user, ") deleted from system.")
        )
        removeModal()
      })
      
      # Handle user edits
      observeEvent(input$save_user_changes, {
        selected <- input$users_table_rows_selected
        #print(paste0("Selected user: ", selected))
        
        if (length(selected) == 0) return()
        
        if(grepl(EMAIL_CHECK_REGEX, input$edit_email)) {
          output$email_edit_hint <- renderUI({HTML(as.character(p("Email OK.", style = "color:green;")))})
        } else {
          output$email_edit_hint <- renderUI({HTML(as.character(p("Invalid email. Please use a different email.", style = "color:red;")))})
          return()
        }
        
        show_modal_spinner(
          spin = "double-bounce",
          color = MODAL_SPINNER_COLOR,
          text = "Please wait, updating user data...",
          session = shiny::getDefaultReactiveDomain()
        )
        
        users_df <- current_user_table()
        selected_user <- users_df[selected, "user"]

        updated_user <- data.frame(
          user = as.character(input$edit_user),
          #password = hashed_password,
          email = as.character(input$edit_email),
          #start = as.character(existing_credentials$start[existing_credentials$user == selected_user]),
          expire = input$edit_expire,
          admin = 0,
          first_login = as.numeric(input$edit_first_login),
          skip_mfa = as.numeric(input$edit_skip_mfa),
          role = as.character(input$edit_role)
        )

        users_df <- rows_update(users_df, updated_user, by = "user")
        
        update_user_database(updated_user, selected_user)
        current_user_table(users_df)
        
        write_audit_log(
          username = session$userData$USERNAME,
          action = "edit_user",
          details = paste0("User (", selected_user, ") has been updated. New data: ",
                           paste(names(updated_user), updated_user, collapse = ", "))
        )
        remove_modal_spinner()
        showNotification("The user has been successfully edited.", type = "message")
        
      })

    }
  )
}