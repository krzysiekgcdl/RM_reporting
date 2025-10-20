
main_scripts_dir <- "445_SHINY_REPOS/DM_Reports/"

STUDY <- "445_SHINY_REPOS/DM_Reports/Templates/"

# Define Paths for Templates and Chapters
STUDY_DA_PATH <- "445_SHINY_REPOS/DM_Reports/DataAging_Templates/"
STUDY_DA_CHAPTERS_PATH <- "445_SHINY_REPOS/DM_Reports/DataAging_Chapters/"


# ==== MFA ====
APP_TITLE <- "RM Reporting"
APP_VERSION <- "TEST"

USER_DB_NAME <- "PASSWORD-DB" #Stop admins in one app modifying users in a different app
AUDIT_DB_NAME <- "AUDIT-DB"
ROLES_DB_NAME <- "ROLES-DB"

APP_TIMEZONE <- "Europe/Warsaw"
AUDIT_LOG_DOWNLOAD_FILENAME <- "RM_Reporting_AuditLog"
#USER_ROLE_OPTIONS <- c("Role1", "Role2", "Admin")

MODAL_SPINNER_COLOR <- "#112446"
PLOT_SPINNER_COLOR <- "#0dc5c1"
EMAIL_CHECK_REGEX <- "^[[:alnum:].-_]+@[[:alnum:].-]+$"

smtp <- server(
  host = Sys.getenv("EMAIL_HOST"),
  port = Sys.getenv("EMAIL_PORT"),
  username = Sys.getenv("EMAIL_USER"),
  password = Sys.getenv("EMAIL_PASS"),
  use_ssl = TRUE)