
homeUI <- function(id) {
  ns <- NS(id)
  div(id=id,
      tagList(
        h1(paste0("Welcome to the ", APP_TITLE, " app!")),
        p("Explanation for using the app should go here. It's a work in progress.")
      )
  )
}

homeServer <- function(id, first_login) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        req(first_login)  # Ensure user is authenticated

        # THE FOLLOWING MODAL IS ONLY NECESSARY IF IT TAKES SEVERAL SECONDS TO LOAD SERVERS/OTHER APP DATA. OTHERWISE IT FLASHES
        # show_modal_spinner(
        #   spin = "double-bounce",
        #   color = MODAL_SPINNER_COLOR,
        #   text = "Please wait, gathering data...",
        #   session = shiny::getDefaultReactiveDomain()
        # )
        # 
        # remove_modal_spinner()
        
        
        if (as.logical(first_login)) {
          # Show a modal or navigate to some "change password" tab
          showModal(modalDialog(
            title = paste0("Welcome to ",APP_TITLE,"!"),
            p("You are currently using the default password, for security reasons please update your password below."),
            passwordInput(ns("new_password_1"), "New password"),
            passwordInput(ns("new_password_2"), "Confirm new password"),
            p("A password of at least 16 characters is required. It is recommended to use a passphrase, and a password manager."),
            uiOutput(ns("password_hint")),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_new_password"), "Confirm")
            )
          ))
        }

      })

      observeEvent(input$confirm_new_password, {
        if (is.null(input$new_password_1) || is.na(input$new_password_1) || nchar(input$new_password_1) < 16) {
          output$password_hint <- renderUI({HTML(as.character(p("Password not long enough. Minimum length is 16 characters.", style = "color:red;")))})
          return()
        } else if (input$new_password_1 != input$new_password_2) {
          output$password_hint <- renderUI({HTML(as.character(p("Passwords do not match.", style = "color:red;")))})
          return()
        } else {
          output$password_hint <- renderUI({HTML(as.character(p("Password OK.", style = "color:green;")))})
        }
        
        show_modal_spinner(
          spin = "double-bounce",
          color = MODAL_SPINNER_COLOR,
          text = "Updating your password...",
          session = shiny::getDefaultReactiveDomain()
        )
        
        user_row <- data.frame("user" = c(session$userData$USERNAME), "first_login" = c(0))#credentials_data$USERS[credentials_data$USERS$user == input$forgotpass_emailbutton, ]
        user_row$password <- scrypt::hashPassword(input$new_password_1)
        update_result <- update_password(user_row, user_row$user, refresh=FALSE)
        
        removeModal()
        remove_modal_spinner()
        if (update_result) {
          showModal(modalDialog(
            title = "Password Changed",
            "Password changed successfully!",
            footer = NULL,
            easyClose = TRUE
          ))
        } else {
          showModal(modalDialog(
            title = "Password Update Failed",
            "There was a problem updating your password. Please continue to use your previous password.",
            footer = NULL,
            easyClose = TRUE
          ))
        }
      })
      
    }
  )
}


