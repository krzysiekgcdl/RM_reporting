
generate_password <- function(LENGTH){
  set.seed(NULL)
  punct <- c("!",  "#", "$", "%", "&", "(", ")", "*",  "+", "-", "@")
  nums <- c(0:9)
  chars <- c(letters, LETTERS, punct, nums)
  p <- c(rep(0.0105, 52), rep(0.0102, 11), rep(0.02, 10))
  pword <- paste0(sample(chars, LENGTH, TRUE, prob = p), collapse = "")
  return(pword)
}

my_check_credentials <- function(users_df) {
  # Based on: https://datastorm-open.github.io/shinymanager/reference/check_credentials.html
  
  function(user, password) {
    # Find matching user row
    row <- users_df[users_df$user == user, ]
    if (nrow(row) != 1) {
      return(NULL)  # no such user
    }

    hash_pwd <- trimws(row$password)

    if (scrypt::verifyPassword(hash_pwd, password)) {
      # Return a list of user info for shinymanager
      is_expired <- row$expire < Sys.Date()
      if (is_expired) {
        list(expired = TRUE, user_info = row)
      } else {
        list(result = TRUE, expired = FALSE, user_info = row)
      }
    # } else if (row$must_change & password == row$password) {
    #list( result = TRUE, expired = FALSE, authorized = TRUE,
    #      user_info = row )
    } else {
      return(NULL)
    }
  }
}


