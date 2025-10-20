
sidebar_admin <- function() {
  list(
    menuItem("Home", startExpanded = TRUE, icon = icon("home"),
            menuSubItem("About", tabName = "home", selected = TRUE)),
    menuItem("Queries Report", tabName = "queries_report", icon = icon("list")),
    menuItem("DM Report", tabName = "dm_report", icon = icon("list")),
    menuItem("Data Aging", tabName = "data_aging", icon = icon("clock")),
    menuItem("Admin", icon = icon("hdd", lib = "glyphicon"),
             menuSubItem("Users", tabName = "users"),
             menuSubItem("Roles", tabName = "roles"),
             menuSubItem("Audit Log", tabName = "audit"))
  )
}

sidebar_home <-function() {
  list(
    menuItem("Home", startExpanded = TRUE, icon = icon("home"),
      menuSubItem("About", tabName = "home", selected = TRUE)),
    menuItem("Queries Report", tabName = "queries_report", icon = icon("list")),
    menuItem("DM Report", tabName = "dm_report", icon = icon("list")),
    menuItem("Data Aging", tabName = "data_aging", icon = icon("clock"))
  )
}

sidebar_auth <-function() {
  list(
    menuItem("Authentication", tabName = "authtab", selected=TRUE, icon = icon("lock"))
  )
}


email_MFA <- function(otp) {
  html_body <- glue::glue("
        <div style='font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto;'>
          <div style='background: linear-gradient(135deg, #418535 0%, #2d5a25 100%); color: white; padding: 20px; text-align: center;'>
            <h2>CleanDataLabs</h2>
            <h3>Secure Secret Access</h3>
          </div>
          <div style='padding: 30px; background: #f8f9fa;'>
            <h4>Your One-Time Password</h4>
            <p>You have requested access to a secret message. Use the following one-time password to access your information:</p>
            <div style='background: white; border: 2px solid #418535; border-radius: 8px; padding: 20px; text-align: center; margin: 20px 0;'>
              <h2 style='color: #418535; font-size: 32px; letter-spacing: 5px;'>{otp}</h2>
            </div>
            <p><strong>Important:</strong></p>
            <ul>
              <li>This password is valid until a site refresh</li>
              <li>If you didn't request this, please ignore this email</li>
            </ul>
            <p style='margin-top: 30px; color: #666; font-size: 12px;'>
              This is an automated message from CleanDataLabs secure sharing system.
            </p>
          </div>
        </div>
      ")
  return(html_body)
}

email_forgotpass <- function(forgotpass) {
  html_body <- glue::glue("
        <div style='font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto;'>
          <div style='background: linear-gradient(135deg, #418535 0%, #2d5a25 100%); color: white; padding: 20px; text-align: center;'>
            <h2>CleanDataLabs</h2>
            <h3>Secure Secret Access</h3>
          </div>
          <div style='padding: 30px; background: #f8f9fa;'>
            <h4>Your Forgotten Password Code</h4>
            <p>You have requested access to a secret message. Use the following one-time code to access your information:</p>
            <div style='background: white; border: 2px solid #418535; border-radius: 8px; padding: 20px; text-align: center; margin: 20px 0;'>
              <h2 style='color: #418535; font-size: 32px; letter-spacing: 5px;'>{forgotpass}</h2>
            </div>
            <p><strong>Important:</strong></p>
            <ul>
              <li>This code is valid until a site refresh</li>
              <li>If you didn't request this, please ignore this email</li>
            </ul>
            <p style='margin-top: 30px; color: #666; font-size: 12px;'>
              This is an automated message from CleanDataLabs secure sharing system.
            </p>
          </div>
        </div>
      ")
  return(html_body)
}

email_login <- function(username, password) {
  html_body <- glue::glue("
        <div style='font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto;'>
          <div style='background: linear-gradient(135deg, #418535 0%, #2d5a25 100%); color: white; padding: 20px; text-align: center;'>
            <h2>CleanDataLabs</h2>
            <h3>Secure Secret Access</h3>
          </div>
          <div style='padding: 30px; background: #f8f9fa;'>
            <h4>Your Login Details</h4>
            <p>You have requested access to {APP_TITLE}. Use the following details to log in to the app:</p>
            <div style='background: white; border: 2px solid #418535; border-radius: 8px; padding: 10px; text-align: center; margin: 20px 0;'>
              <p>Username:</p>
              <h2 style='color: #418535; font-size: 32px;'>{username}</h2>
            </div>
            <div style='background: white; border: 2px solid #418535; border-radius: 8px; padding: 10px; text-align: center; margin: 20px 0;'>
              <p>Password:</p>
              <h2 style='color: #418535; font-size: 32px;'>{password}</h2>
            </div>
            <p><strong>Important:</strong></p>
            <ul>
              <li>It is recommended to change this password after logging in for the first time. A menu will pop up giving you the option to change it after successfully logging in the first time.</li>
              <li>If you didn't request this, please ignore this email</li>
            </ul>
            <p style='margin-top: 30px; color: #666; font-size: 12px;'>
              This is an automated message from CleanDataLabs secure sharing system.
            </p>
          </div>
        </div>
      ")
  return(html_body)
}
