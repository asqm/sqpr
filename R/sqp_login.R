sqp_login <- function(username = NULL, password = NULL) {
  username <- sqp_username(username)
  password <- sqp_pw(password)

  token <- httr::GET(sqp_env$hostname,
                     httr::accept_json(),
                     httr::authenticate(username, password),
                     path = sqp_env$auth)

  catch_error(token)

  sqp_env$token <- httr::content(token, as = "parsed")$access_token
  invisible(TRUE)
}

sqp_username <- function(x) {
  tmp <- if (is.null(x)) Sys.getenv("SQP_USER", '') else x
  if (tmp == "") {
    getOption("SQP_USER", stop("Your username is either NULL or not available as an option/environment variable. See ?sqp_login", call. = FALSE))
  } else {
    tmp
  }
}

sqp_pw <- function(x) {
  tmp <- if (is.null(x)) Sys.getenv("SQP_PW", '') else x
  if (tmp == "") {
    getOption("SQP_PW", stop("Your password is either NULL or not available as an option/environment variable. See ?sqp_login", call. = FALSE))
  } else {
    tmp
  }
}

check_login <- function() {
  if (is.null(sqp_env$token)) stop("You need to be logged in to query from the SQP API. See ?sqp_login")
}

sqp_env <- new.env()
sqp_env$hostname <- "http://ec2-52-14-50-91.us-east-2.compute.amazonaws.com:8080"
sqp_env$auth <- "api/auth"
sqp_env$study <- "/api/v1/studies"
sqp_env$questions <- "/questions"
sqp_env$ques_props <- "/completions"


sqp_env$study_variables <- c("id", "name")
sqp_env$question_variables <- c("id", "study_id", "short_name", "country_iso", "language_iso")



# sqp_login()
# sqp_login("hey")
# sqp_login("hey", "ho")
# sqp_env$token <- NULL
# Sys.setenv("SQP_USER" = "hey")
# Sys.setenv("SQP_PW" = "ho")
# sqp_login()
# Sys.setenv("SQP_USER" = "")
# Sys.setenv("SQP_PW" = "")
# sqp_login()
# options(SQP_USER = "hey",
#         SQP_PW = "ho")
# sqp_login()