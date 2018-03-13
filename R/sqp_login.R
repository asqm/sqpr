#' Login to the SQP API
#'
#' Login to the SQP API directly from R. Make sure that your username and password
#' are previously registered at \url{http://sqp.upf.edu/accounts/login/?next=/loadui/}
#'
#' @param username your user name as a string
#' @param password your password as a string
#'
#' @details Currently \code{sqp_login} offers three possible ways to log in. The first
#' is by placing your username and password as environment variables with names
#' \code{SQP_USER} and \code{SQP_PW}. The second is placing your username and password as variables in
#'  \code{options()} with the same names. And finally, the third, and most insecure way,
#' is by providing your username and password as arguments to \code{sqp_login}.
#' We discourage users to use the third option because it might pose security
#' threats if the code is shared. See the examples below for practical explanations.
#'
#' Note that if the username or password is incorrect, the function will throw an error.
#' Make sure you can log in at \url{http://sqp.upf.edu/accounts/login/?next=/loadui/}
#' first.
#'
#' @return an invisible \code{TRUE}. But it will raise an error if the account doesn't
#' match any SQP accounts.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Let's suppose your username is 'mary' and your password is 'secret'.
#' # Set your environmental variables with Sys.setenv()
#'
#' Sys.setenv(SQP_USER = 'mary')
#' Sys.setenv(SQP_PW = 'secret')
#'
#' # You can run the previous two lines and then delete them.
#' # While that R session is open, sqp_login will search for these two
#' # variables even if you delete the previous lines. For example..
#'
#' sqp_login()
#'
#' # will login successfully. Users interested in setting these values
#' # as permament environment variables are referred to
#' https://csgillespie.github.io/efficientR/r-startup.html#renviron
#'
#' # Similarly, you can set the values as options
#'
#' options(
#'  SQP_USER = 'mary',
#'  SQP_PW = 'secret'
#'  )
#'
#' sqp_login()
#'
#' # Finally, if you never plan to share your code (and even then I suggest the
#' # other options), you can always use the account as arguments to sqp_login.
#'
#' sqp_login(username = 'mary', password = 'secret')
#'
#' }
#'
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
