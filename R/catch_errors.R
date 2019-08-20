check_study <- function(id_study, all_studies, original_study) {

  length_id <- length(id_study)
  if (length_id == 0) {
    stop("Study ",
         paste0("'", original_study, "'"),
         " was not found. Check get_studies()",
         call. = FALSE
         )
  }

  study <- all_studies$name[match(id_study, all_studies$id)]
  if (length_id > 1) {
    stop("Multiple studies matched. Narrow down to one: ",
         paste(study, collapse = ", "),
         call. = FALSE
         )
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
