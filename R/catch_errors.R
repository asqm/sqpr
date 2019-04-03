check_study <- function(id_study) {
  call_request <- auth_GET(paste0(sqp_env$study, "/", id_study, "/"))
  res <- httr::content(call_request)
  has_error <- "status" %in% names(res)
  has_length <- length(res) != 0

  if (has_error && has_length && res$status == 27) {
    stop("Study ", id_study, " was not found. Check get_studies()", call. = FALSE)
  }
  invisible(TRUE)
}

custom_stop <- function(message) {
  stop(message, call. = FALSE)
}

catch_error <- function(call_request) {
  tryCatch(httr::stop_for_status(call_request),
           http_401 = function(c) custom_stop("Failed to login with that username/password. Check that account is registered at http://sqp.upf.edu/"),
           http_404 = function(c) custom_stop("URL not found"),
           http_500 = function(c) custom_stop("Something might be wrong with the SQP API or your username/password are not valid")
  )
}
