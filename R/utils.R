# Check sqp data and throw erros if it's not in correct format.
# If it is in the correct format attach 'sqp' class if it doesn't
# have it.

# Why? Because using any of the sqp_ funs with tidyverse verbs
# (or any other function whatsoever), drops the 'sqp' class. These
# functions will check whether the data is in right format and assign
# the class accordingly. It basically makes sure the data is good for
# later processing.
sqp_reconstruct <- function(sqp_data, variables_check = sqp_env$sqp_columns) {

  # If sqp_data is not in the correct format, throw an error
  check_sqp_data(sqp_data, variables_check)

  # If it has a correct format, then simply add the sqp class if
  # it doesn't have it
  if (!inherits(sqp_data, "sqp")) class(sqp_data) <- c(class(sqp_data), "sqp")
  sqp_data
}

check_sqp_data <- function(sqp_data, available_vars) {
  # Check sqp_env$sqp_columns variables exists

  metrics_available <- all(available_vars %in% names(sqp_data))

  if (!metrics_available) {
    stop("Variables ", paste0(available_vars, collapse = ", "),
         " must be available in `sqp_data`",
         call. = FALSE)
  }

  purrr::walk(sqp_data[available_vars], col_checker)
  if (!is.character(sqp_data[[1]])) {
    stop("First column in `sqp_data` must contain the question names as strings")
  }
}

col_checker <- function(x) {
  if (all(is.na(x))) return(TRUE)

  is_numeric <- is.numeric(x)
  is_perc <- all(x >= 0 & x <= 1, na.rm = TRUE)
  if (!is_numeric | !is_perc) {
    stop(paste0(sqp_env$sqp_columns, collapse = ", "),
         " must be numeric columns with values between/including 0 and 1 in `sqp_data`",
         call. = FALSE)
  }
  invisible(TRUE)
}

columns_present <- function(corr_data, sqp_data, var_names) {
  sum_corr <- corr_data[[1]] %in% var_names
  sum_sqp <- sqp_data[[1]] %in% var_names

  vars_corr <- var_names %in% corr_data[[1]]
  vars_sqp <- var_names %in% sqp_data[[1]]

  if (sum(sum_corr) != length(var_names)) {
    stop("At least one variable not present in `x`: ",
         paste0(var_names[!vars_corr], collapse = ", "),
         call. = FALSE)
  }

  if ((sum(sum_sqp) != length(var_names))) {
    stop("At least one variable not present in `sqp_data`: ",
         paste0(var_names[!vars_sqp], collapse = ", "),
         call. = FALSE)
  }

}


# These functions are aimed at making requests to the SQP API.

# This function allows to retrieve the same requests if
# no parameters have changed. I have to keep it outside the functions
# because if defined inside a fun, then it is deleted after
# the env of the fun is deleted.
memo_GET <- memoise::memoise(httr::GET)

# Make a general request with the login information
auth_GET <- function(path, ...) {
  check_login()

  auth <- httr::add_headers('Authorization' = paste("Bearer", sqp_env$token))

  res <-
    memo_GET(url = sqp_env$hostname,
             path = path,
             config = auth,
             ...)
  res
}

# Wrapper of the previous fun to raise
# any errors early and clearly
safe_GET <- function(path, ...) {
  res <- auth_GET(path, ...)
  catch_error(res)
  res
}

# Wrapper to grab the data from the requests
# If estimates is TRUE returns a list, otherwise
# a tibble
object_request <- function(path, estimates = FALSE) {
  requested <- safe_GET(path)
  get_content <- httr::content(requested, as = 'text')

  if (estimates) {
    json_data <- jsonlite::fromJSON(get_content, flatten = TRUE)$data
    return(json_data)
  } else {
    json_data <- jsonlite::fromJSON(get_content)$data
  }

  final_df <- tibble::as_tibble(json_data)
  final_df
}


sqp_env <- new.env()
sqp_env$hostname <- "http://ec2-52-14-50-91.us-east-2.compute.amazonaws.com:8080"
sqp_env$auth <- "/api/auth"
sqp_env$study <- "/api/v1/studies/"
sqp_env$questions <- "/api/v1/questions/"
sqp_env$q_estimates <- "/completions/"

# Note that the ORDER of these variables matters
# because I subset by index in the code. If changed, then the
# indexes need to change as well.
sqp_env$study_variables <- c("id", "name")
sqp_env$question_variables <- c("id", "study_id", "short_name", "country_iso", "language_iso")

sqp_env$short_estimate_variables <- paste0("prediction.", c("reliability", "validity", "quality"))

sqp_env$all_estimate_variables <- c("question", "id", "question_id", "created", "routing_id", "authorized",
                                    "complete", "error", "errorMessage", "reliability", "validity",
                                    "quality", "reliabilityCoefficient", "validityCoefficient", "methodEffectCoefficient",
                                    "qualityCoefficient", "reliabilityCoefficientInterquartileRange",
                                    "validityCoefficientInterquartileRange", "qualityCoefficientInterquartileRange",
                                    "reliabilityCoefficientStdError", "validityCoefficientStdError",
                                    "qualityCoefficientStdError")
# Variables to pick from the sqp remote data
# and with which to create sqp tables
sqp_env$sqp_columns <- c("quality", "reliability", "validity")
