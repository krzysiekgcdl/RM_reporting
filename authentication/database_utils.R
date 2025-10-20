
read_from_DB <- function(db_name){
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT")),
    mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
      #print("POOL CLOSE DB!", type = "message")
    }, error = function(e) {
      #showNotification(paste("Error when pool closing:", e$message), type = "error")
      print(paste("Error when pool closing:", e$message))
    })
  })
  
  df <- NULL
  tryCatch({
    df <- dbReadTable(database_connect, db_name)
  }, error = function(e) {
    showNotification(paste("Error_DB:", e$message), type = "error")
    print(paste("Error_DB:", e$message))
  })
  return(df)
}


#Deletes row based on row key (user)
delete_from_database <- function(db_name, keyname, key = NULL){
  
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  #WRITE TO MYSQL
  if(is.null(key)) {
    print("key is null")
    showNotification(paste0("Failed to delete ",key,", ",keyname," is empty."), type = "error")
  } else {
    sql <- paste0("DELETE FROM `",db_name,"` WHERE `",keyname,"` = ?key")
    query <- sqlInterpolate(database_connect, sql, key=key)
    
    tryCatch({
      dbExecute(database_connect, query)
      save_audit_log(database_connect, db_name, "DELETED ROW", paste("Deleted ", keyname, key), "app")
      showNotification("Row has been deleted.", type = "error")
    }, error = function(e) {
      showNotification(paste("Error_DB:", e$message), type = "error")
      print(paste("Error_DB:", e$message))
    })
  }
  
}



read_users_from_DB <- function(){
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT")),
    mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
      #print("POOL CLOSE DB!", type = "message")
    }, error = function(e) {
      #showNotification(paste("Error when pool closing:", e$message), type = "error")
      print(paste("Error when pool closing:", e$message))
    })
  })
  
  df <- NULL
  tryCatch({
    df <- dbReadTable(database_connect, USER_DB_NAME)
  }, error = function(e) {
    showNotification(paste("Error_DB:", e$message), type = "error")
    print(paste("Error_DB:", e$message))
  })
  return(df)
}

#Update password for a user and refresh app
update_password <- function(new_password_row, username = NULL, refresh = TRUE){

  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  #WRITE TO MYSQL
  if(is.null(username)) {
    print("username is null")
    showNotification("Failed to update password, username is empty.", type = "error")
  } else {
    key <- new_password_row %>% select(user)
    values <- new_password_row %>% select(-user)
    fields <- names(values)
    updates <- sapply(fields, function(fld) {
      sprintf("`%s` = ?%s", fld, fld) #for setting up sqlInterpolateList
    })
    keyupdates <- sprintf("`%s` = ?%s", colnames(key)[1], colnames(key)[1])
    sql <- sprintf(
      "UPDATE `%s` SET %s WHERE %s",
      USER_DB_NAME, paste(updates, collapse = ", "), keyupdates)

    tryCatch({
      query <- sqlInterpolateList(database_connect, sql, vars = as.list(new_password_row)) #vars=list("user"=c("test"), "first_login"=c(1)))
      dbExecute(database_connect, query)
      save_audit_log(database_connect, USER_DB_NAME, "UPDATE PASSWORD", paste("Updated password for user",username), "app")
      if (refresh) {
        showNotification(
          "Updated password. Refreshing app in 3 seconds.",
          type = "message",
          duration = 3
        )
        Sys.sleep(3)
      }
      return(TRUE)
    }, error = function(e) {
      showNotification(paste("Error_DB:", e$message), type = "error")
      print(paste("Error_DB:", e$message))
      return(FALSE)
    })
  }

}


#Updates database based on row key (user)
update_user_database <- function(new_row, username = NULL){ 
  
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  #WRITE TO MYSQL
  if(is.null(username)) {
    print("username is null")
    showNotification("Failed to update user, username is empty.", type = "error")
  } else {
    key <- new_row %>% select(user)
    values <- new_row %>% select(-user)
    fields <- names(values)
    updates <- sapply(fields, function(fld) {
      sprintf("`%s` = ?%s", fld, fld) #for setting up sqlInterpolateList
    })
    audit_updates <- sapply(fields, function(fld) {
      sprintf("`%s` = '%s'", fld, new_row[[fld]])
    })
    keyupdates <- sprintf("`%s` = ?%s", colnames(key)[1], colnames(key)[1])
    sql <- sprintf(
      "UPDATE `%s` SET %s WHERE %s",
      USER_DB_NAME, paste(updates, collapse = ", "), keyupdates)
    
    tryCatch({
      query <- sqlInterpolateList(database_connect, sql, vars = as.list(new_row)) #vars=list("user"=c("test"), "first_login"=c(1)))
      dbExecute(database_connect, query)
      save_audit_log(database_connect, USER_DB_NAME, "UPDATE USER", paste("Updated the following feeds for user",username, ":", paste(audit_updates, collapse = ", ")), "app")
      showNotification("Table updated successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error_DB:", e$message), type = "error")
      print(paste("Error_DB:", e$message))
    })
  }
  
}


#Deletes row based on row key (user)
delete_user_from_database <- function(username = NULL){
  
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  #WRITE TO MYSQL
  if(is.null(username)) {
    print("username is null")
    showNotification("Failed to delete user, username is empty.", type = "error")
  } else {
    sql <- paste0("DELETE FROM `",USER_DB_NAME,"` WHERE `user` = ?user")
    query <- sqlInterpolate(database_connect, sql, user=username)

    tryCatch({
      dbExecute(database_connect, query)
      save_audit_log(database_connect, USER_DB_NAME, "DELETE USER", paste("Deleted user",username), "app")
      showNotification("User has been deleted.", type = "error")
    }, error = function(e) {
      showNotification(paste("Error_DB:", e$message), type = "error")
      print(paste("Error_DB:", e$message))
    })
  }
  
}



# Function to read roles
read_roles <- function() {
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  query <- sprintf("SELECT * FROM `%s`", ROLES_DB_NAME)
  
  tryCatch({
    log_entries <- dbGetQuery(database_connect, query)
    return(log_entries)
  }, error = function(e) {
    showNotification(paste("Error reading roles:", e$message), type = "error")
    print(paste("Error reading roles:", e$message))
    return(NULL)
  })
}

write_roles <- function(new_role){
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  tryCatch({
    #ALTER TABLE `ROLES-DB` ADD `test` BOOLEAN NOT NULL AFTER `ADMIN`; 
    sql <- paste0("ALTER TABLE `",ROLES_DB_NAME,"` ADD `",new_role,"` TINYINT(1) NOT NULL DEFAULT '0';")
    # query <- sqlInterpolate(database_connect, sql, 
    #                         rolename = new_role
    # )
    # print(query)
    dbExecute(database_connect, sql)
    save_audit_log(database_connect, ROLES_DB_NAME, "ADD ROLE", paste0("Added role '",new_role,"'."), "app")
  }, error = function(e) {
    showNotification(paste("Error_DB:", e$message), type = "error")
    print(paste("Error_DB:", e$message))
  })
  return(read_roles())
}


#Updates database based on row key (user)
update_role_database <- function(new_db, rolename = NULL){  #needs to include sidebar columns for ID
  
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  if(is.null(rolename)) {
    print("rolename is null")
    showNotification("Failed to update role, role name is empty.", type = "error")
  } else {
    updates <- c()
    varstable <- data.frame("temp"=c("temp"))
    for(j in 1:nrow(new_db)){
      tvar <- data.frame("a"=c(new_db[j,1]), "b"=c(new_db[j,2]), "c"=c(new_db[j,3]))
      colnames(tvar) <- paste0(colnames(new_db),j)
      varstable <- cbind(varstable, tvar)
      update <- sprintf("`%s` = ?%s", colnames(new_db)[3], paste0(colnames(new_db)[3],j)) #for setting up sqlInterpolateList
      updates <- c(updates, update)
    }
    keyupdates <- sapply(c(1:nrow(new_db)), function(i) {
      sprintf("`%s`.`%s` = ?%s AND `%s`.`%s` = ?%s", 
              ROLES_DB_NAME, colnames(new_db)[1], paste0(colnames(new_db)[1],i),
              ROLES_DB_NAME, colnames(new_db)[2], paste0(colnames(new_db)[2],i))
    })
    
    sql_list <- sprintf(
      "UPDATE `%s` SET %s WHERE %s",
      ROLES_DB_NAME, updates, keyupdates)
    
    sql <- paste0(paste(sql_list, collapse=";@"), ";")
    
    tryCatch({
      query <- sqlInterpolateList(database_connect, sql, vars = unlist(varstable %>% select(-1)))#vars = as.list(new_row)) #vars=list("user"=c("test"), "first_login"=c(1)))
      for (x in stringr::str_split(query, "@")[[1]]) {
        dbExecute(database_connect, x) #turns out you can only do one query at a time...
      }
      save_audit_log(database_connect, ROLES_DB_NAME, "UPDATE ROLE", paste("Updated the following for role",rolename, ":", paste(new_db[,3], collapse=", ")), "app")
      showNotification("Table updated successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error_DB:", e$message), type = "error")
      print(paste("Error_DB:", e$message))
    })
  }
  
}

#Deletes row based on row key (user)
delete_role_from_database <- function(role_name = NULL){
  
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  #WRITE TO MYSQL
  if(is.null(role_name)) {
    print("role is null")
    showNotification("Failed to delete role, role name is empty.", type = "error")
  } else {
    #ALTER TABLE table_name DROP COLUMN column_name; 
    sql <- paste0("ALTER TABLE `",ROLES_DB_NAME,"` DROP COLUMN `",role_name,"`;")
    #query <- sqlInterpolate(database_connect, sql, user=username)
    
    tryCatch({
      dbExecute(database_connect, sql)
      save_audit_log(database_connect, ROLES_DB_NAME, "DELETE ROLE", paste("Deleted role",role_name), "app")
      showNotification("Role has been deleted.", type = "error")
    }, error = function(e) {
      showNotification(paste("Error_DB:", e$message), type = "error")
      print(paste("Error_DB:", e$message))
    })
  }
  
}



#Check database connection
check_db_connection <- function(conn){
  print("Checking database connection...")
  
  try_again <- FALSE
  # Check if the connection is valid
  tryCatch({
    RMariaDB::dbGetQuery(conn, "SELECT 1")
  }, error = function(e) {
    message("Try reconnect...", e$message)
    
    conn <<- RMariaDB::dbConnect(
      drv = RMariaDB::MariaDB(),
      dbname = Sys.getenv("DB_NAME"),
      host = Sys.getenv("DB_HOST"),
      username = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASS"),
      port = as.integer(Sys.getenv("DB_PORT"))
      ,mysql = TRUE
    )
    tryCatch({
      RMariaDB::dbGetQuery(conn, "SELECT 1")
    }
    , error = function(e) {
      message("Reconnection failed:", e$message)
      #showNotification(paste("Error when reconnecting to DB:", e$message), type = "error")
      try_again = TRUE
      return(NULL)
    })
    #return (conn)
  })
  
  
  count_iterations <- 0
  while(try_again){
    try_again = FALSE
    count_iterations <- count_iterations + 1
    if(count_iterations > 6){
      print("Unable to connect to the database. Please try again later.")
      #showNotification("Nie można połączyć się z bazą danych. Proszę spróbować ponownie później.", type = "error")
      return(NULL)
    }
    Sys.sleep(10)
    # Check if the connection is valid
    tryCatch({
      RMariaDB::dbGetQuery(conn, "SELECT 1")
    }, error = function(e) {
      message("Try reconnect...", e$message)
      
      conn <<- RMariaDB::dbConnect(
        drv = RMariaDB::MariaDB(),
        dbname = Sys.getenv("DB_NAME"),
        host = Sys.getenv("DB_HOST"),
        username = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASS"),
        port = as.integer(Sys.getenv("DB_PORT"))
        ,mysql = TRUE
      )
      tryCatch({
        RMariaDB::dbGetQuery(conn, "SELECT 1")
      }
      , error = function(e) {
        message("Reconnection failed:", e$message)
        #showNotification(paste("Error when reconnecting to DB:", e$message), type = "error")
        try_again = TRUE
        return(NULL)
      })
      #return (conn)
    })
    
  }
  
  return (conn)
}

#Universal function to update data in database and return the table at the end
add_db_data_return <- function(name_db, new_df){
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  tryCatch({
    query <- rework_to_query(database_connect, name_db, new_df)
    dbExecute(database_connect, query)
    save_audit_log(database_connect, name_db, "ADD DATA", paste(names(new_df), new_df, collapse = ", "), "app")
  }, error = function(e) {
    showNotification(paste("Error_DB:", e$message), type = "error")
    print(paste("Error_DB:", e$message))
  })

  return(dbReadTable(database_connect, name_db))
}


rework_to_query <- function(conn, db_name, new_df) {

  fields <- names(new_df)
  j <- c(1:length(fields))
  vals <- c()
  vars <- list()
  
  for (i in 1:nrow(new_df)) { #For each row in the table
    vals <- c(vals, paste0("(",paste("?",fields,i,j,collapse=", ", sep=""),")")) #Format each row into (?col11, ?col21),(?col12, ?col22)
    for (n in j) {
      vars[[paste0(fields[n],i,n)]] <- new_df[i,n] #Format the list for sqlInterpolateList
    }
  }
  
  sql <- sprintf(
    "INSERT INTO `%s` (%s) VALUES %s;",
    db_name,
    paste(fields, collapse = ", "),
    paste(vals, collapse = ",")
  )
  
  query <- sqlInterpolateList(conn, sql, vars = vars)
  return(query)
}


#==== AUDIT LOG FUNCTIONS ====

write_audit_log <- function(username, action, details = NULL, usertype = "usr") {
  # Function to save an audit log entry
  # Database connection object make insite function
  # user: User who performed the action
  # action: Action performed (e.g., "update", "delete")
  # details: Additional details about the action (optional)
  
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  save_audit_log(database_connect, username, action, details, usertype)
}

#For audits of already-connected functions/app functions
save_audit_log <- function(con, username, action, details = NULL, usertype = "app") {
  # Function to save an audit log entry
  # con: Database connection object
  # user: User who performed the action
  # action: Action performed (e.g., "update", "delete")
  # details: Additional details about the action (optional)
  
  t   <- Sys.time()          
  cest <- as.POSIXct(t, tz = APP_TIMEZONE)
  
  tryCatch({
    sql <- paste0("INSERT INTO `",AUDIT_DB_NAME,"` (timestamp, usertype, version, user, action, details) VALUES (?timestamp, ?usertype, ?version, ?user, ?action, ?details);")
    query <- sqlInterpolate(con, sql, 
                            timestamp = format(cest, "%Y-%m-%d %H:%M:%S %Z"), 
                            usertype = usertype,
                            version = APP_VERSION,
                            user = username,
                            action = action,
                            details = ifelse(is.null(details), NA, details)
                            )
    dbExecute(con, query)
  }, error = function(e) {
    showNotification(paste("Error saving audit log:", e$message), type = "error")
    print(paste("Error saving audit log:", e$message))
  })
}

# Function to read audit log entries
read_audit_log <- function() {
  # MAKE MYSQL CONNECTION
  database_connect <- RMariaDB::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    port = as.integer(Sys.getenv("DB_PORT"))
    ,mysql = TRUE
  )
  # CLOSE MYSQL CONNECTION AFTER CLOSE FUNC
  on.exit( {
    tryCatch({
      dbDisconnect(database_connect)
    }, error = function(e) {
      print(paste("Error when DB closing:", e$message))
    })
  })
  
  query <- sprintf("SELECT * FROM `%s`", AUDIT_DB_NAME)
  
  tryCatch({
    log_entries <- dbGetQuery(database_connect, query)
    return(log_entries)
  }, error = function(e) {
    showNotification(paste("Error reading audit log:", e$message), type = "error")
    print(paste("Error reading audit log:", e$message))
    return(NULL)
  })
}

#source: https://github.com/r-dbi/DBI/issues/193
sqlInterpolateList <- function(conn, sql, vars=list(), list_vars=list()) {
  if (length(list_vars) > 0) {
    for (name in names(list_vars)) {
      sql <- sub(paste0("\\?", name), paste("?", name, "_list_var", 1:length(list_vars[[name]]), sep="", collapse=" , "), sql)
    }
    list_vars <- lapply(list_vars, function(sublist) {
      names(sublist) <- paste0("list_var", 1:length(sublist))
      sublist
    }) %>% unlist()
    # unlist gives names as "outer.inner" but DBI doesn't like names with periods
    names(list_vars) <- sub("\\.", "_", names(list_vars))
    vars <- c(vars, list_vars)
  }
  DBI::sqlInterpolate(conn, sql, .dots=vars)
}
