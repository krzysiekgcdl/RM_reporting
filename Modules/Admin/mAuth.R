
authUI <- function(id) {
  ns <- NS(id)
  div(id=id,
      tagList(
        tabItems(
             tabItem(tabName = "authtab", authSubUI("authtab")),
             tabItem(tabName = "home", homeUI("home")),
             tabItem(tabName = "users", usersUI("users")),
             tabItem(tabName = "roles", rolesUI("roles")),
             tabItem(tabName = "audit", auditUI("audit")),
             tabItem(tabName = "queries_report", queriesReportUI("queries_report")),
             tabItem(tabName = "dm_report", DMReportUI("dm_report")),
             tabItem(tabName = "data_aging", dataAgingUI("data_aging"))
      )
    )
  )
}

# authServer <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       
#     }
#   )
# }


authSubUI <- function(id) {
  ns <- NS(id)
  div(id=id,
      tagList(
        fluidPage(
          div(#style = "width:50%; margin:auto; text-align:center; margin-top:20%;",
            uiOutput(ns("headertext")), 
            textInput(ns("usercode"), "Input the code"),
            actionButton(ns("verifycode"), "Verify code")
          )
        )
      )
  )
}

authSubServer <- function(id, auth_out) {
  moduleServer(
    id,
    function(input, output, session) {

      output$headertext <- renderUI(h1(paste("Input the code sent to", auth_out$email)))
      
      observeEvent(input$verifycode, {
        if (!is.null(input$usercode) && input$usercode == session$userData$MFA_CODE) {
          responsetext <- "Valid code entered."
          responsetype <- "message"
          session$userData$LOGIN(TRUE)
        } else {
          responsetext <- "Invalid code entered"
          responsetype <- "error"
        }
        showNotification(
          responsetext,
          type = responsetype,
          duration = 2
        )
      })
    }
  )
}


