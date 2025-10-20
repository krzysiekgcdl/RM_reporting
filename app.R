
# Load required libraries
library(shiny)
library(shinydashboard)
library(readxl)
library(writexl)
library(dplyr)
library(officer)
library(DT)
library(openxlsx)
library(gtsummary)
library(flextable)
library(labelled)
library(ggplot2)
#library(ggpubr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(RColorBrewer)
library(textutils)
library(Microsoft365R)
#--MFA--
library(otp)
library(emayili) 
library(shinymanager)
library(DBI) #must have for DB
library(RMariaDB) #for MySQL and MariaDB
library(scrypt)
library(shinyjs)
library(shinybusy)
library(shinycssloaders)

readRenviron(".Renviron")
source("global.R")
source("utils.R")
source("Modules/mQueriesReport.R")
source("Modules/mDMReport.R")
source("Modules/mDataAging.R")
source("authentication/password_utils.R")
source("authentication/database_utils.R")
source("authentication/ui_utils.R")
source("Modules/Admin/mAuth.R")
source("Modules/Admin/mHome.R")
source("Modules/Admin/mUsers.R")
source("Modules/Admin/mRoles.R")
source("Modules/Admin/mAudit.R")

# Read my function

# source("Scripts/f_raport_querow_ver2_2.R")
# source("Scripts/generate_dm_report_v2.0.R")
# source("Scripts/generalised_reporting_data_aging_v2.0.R")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Reporting Portal"),
  dashboardSidebar(
    sidebarMenu(sidebarMenuOutput("sidebarMenu")
      # menuItem("Queries Report", tabName = "queries_report", icon = icon("list")),
      # menuItem("DM Report", tabName = "dm_report", icon = icon("list")),
      # menuItem("Data Aging", tabName = "data_aging", icon = icon("clock"))
    )
  ),
  dashboardBody(
    authUI("authtab"),
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))) #Stops a visual glitch with the tab backgrounds
    # tabItems(
    #   # Queries Report Section
    #   tabItem(tabName = "queries_report", queriesReportUI("queries_report")),
    #   # DM Report Section
    #   tabItem(tabName = "dm_report", DMReportUI("dm_report")),
    #   # Data Aging Section
    #   tabItem(tabName = "data_aging", dataAgingUI("data_aging"))
    #)
  )
)

ui <- secure_app(
  ui = ui,
  enable_admin = TRUE,
  language = "en",
  tags_top = tags$div(tags$h2("Test Auth"), tags$a(onclick="forgotpass()", "Forgot Password"),tags$div(id="forgotpassdiv")),
  head_auth = tags$head(tags$script(src = "head_enter.js"))
)


# Define server logic
server <- function(input, output, session) {
  
  #=== MFA ===
  credentials_data <- reactiveValues(USERS = read_users_from_DB())
  user_roles <<- reactiveVal(read_roles())
  auth_out <<- secure_server(
    check_credentials = my_check_credentials(credentials_data$USERS)
  )
  
  session$userData$MFA_CODE <- NULL
  session$userData$LOGIN <- reactiveVal(FALSE)
  session$userData$USERNAME <- NULL #equivalent to auth_out$user
  values <<- reactiveValues()
  values$FINISHED <- NULL
  
  observeEvent(auth_out$user, {
    if (!auth_out$skip_mfa) {
      session$userData$MFA_CODE <<- sprintf("%05d", sample(10000:99999, 1))
      email <- envelope( 
        to = auth_out$email, 
        from = "shinyapps@cleandatalabs.com", 
        subject = "CleanDataLabs - Your One-Time Password", 
        html = email_MFA(session$userData$MFA_CODE)
      )
      smtp(email) 
    }
  })
  
  #==== FORGOTTEN PASSWORD ====
  
  observeEvent(input$forgotpass_updatepass, {
    if (is.null(input$forgotpass_updatepass) || is.na(input$forgotpass_updatepass) || nchar(input$forgotpass_updatepass) < 16) {
      showNotification("Password not long enough. Minimum length is 16 characters.", type = "error", duration = 2)
      return()
    } else {
      showNotification("Password OK.", type = "message", duration = 2)
    }
    user_row <- credentials_data$USERS[credentials_data$USERS$user == input$forgotpass_emailbutton, ]
    user_row$password <- scrypt::hashPassword(input$forgotpass_updatepass)
    update_result <- update_password(user_row, user_row$user, refresh = TRUE)
    if(update_result) {
      session$reload()
    }
  })
  
  observeEvent(input$forgotpass_emailbutton, {
    email_address <- credentials_data$USERS[credentials_data$USERS$user == input$forgotpass_emailbutton, c("email")]
    if (length(email_address) == 0) {
      showNotification(
        "User doesn't exist.",
        type = "error",
        duration = 2
      )
    } else {
      email <- envelope(
        to = email_address,
        from = "shinyapps@cleandatalabs.com",
        subject = "CleanDataLabs - Forgotten Password Code",
        html = email_forgotpass(input$forgot_pw)
      )
      smtp(email)
      showNotification(
        "Email sent.",
        type = "message",
        duration = 2
      )
    }
  })
  
  #=== RUN SERVERS ===
  
  observeEvent(session$userData$LOGIN(), {
    req(auth_out$user, auth_out$role)
    if(auth_out$skip_mfa || session$userData$LOGIN()) {
      show_modal_spinner(
        spin = "double-bounce",
        color = "#112446",
        text = "User authenticated. Loading data, Please wait...",
        session = shiny::getDefaultReactiveDomain()
      )
    }
  })
  observe({
    req(values$FINISHED)
    remove_modal_spinner()
  })
  
  updateSidebarMenuContent <- function(output, items) {
    output$sidebarMenu <- renderMenu({
      sidebarMenu(id = "sidebarMenu", .list = items)
    })
  }
  
  updateSidebarMenuContent(output, list(
    sidebar_auth()
  ))
  
  observeEvent(session$userData$LOGIN(), {
    req(auth_out$user, auth_out$role)
    
    if (auth_out$skip_mfa || session$userData$LOGIN()) { #If user has successfully gone through 2FA or they can skip MFA
      session$userData$USERNAME <<- auth_out$user
      write_audit_log(
        username = session$userData$USERNAME,
        action = "successful_login"
      )
      load_temp_scripts()
      connect_to_sharepoint()
      homeServer("home", auth_out$first_login)
      queriesReportServer("queries_report")
      DMReportServer("dm_report")
      dataAgingServer("data_aging")
      if (auth_out$role == "Admin" || auth_out$admin == 1) { #SPECIFIC APPS MAY NEED DIFFERENT REQUIREMENTS HERE
        usersServer("users", credentials_data$USERS)
        rolesServer("roles")
        auditServer("audit")
        sidebar_to_update <- sidebar_admin()
      } else {
        sidebar_to_update <- sidebar_home()
      }
      updateSidebarMenuContent(output, sidebar_to_update)
      values$FINISHED <- "done" #Used for the spinner at the start
    } else { #If the user still needs the second factor
      authSubServer("authtab", auth_out)
    }
  })
  
  query_data <- NULL
  study_name <- reactiveVal(NULL)
  # # Reactive value for studies list
  # studies_list_dm <- reactiveVal()

}


# Run the application 
shinyApp(ui = ui, server = server)
