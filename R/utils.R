# Check sqp data and throw erros if it's not in correct format.
# If it is in the correct format attach 'sqp' class if it doesn't
# have it.

# Why? Because using any of the sqp_ funs with tidyverse verbs
# (or any other function whatsoever), drops the 'sqp' class. These
# functions will check whether the data is in right format and assign
# the class accordingly. It basically makes sure the data is good for
# later processing.
sqp_reconstruct <- function(sqp_data) {

  # If sqp_data is not in the correct format, throw an error
  check_sqp_data(sqp_data)

  # If it has a correct format, then simply add the sqp class if
  # it doesn't have it
  if (!inherits(sqp_data, "sqp")) class(sqp_data) <- c(class(sqp_data), "sqp")
  sqp_data
}

check_sqp_data <- function(sqp_data, available_vars = top_env$sqp_columns) {
  # Check top_env$sqp_columns variables exists

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
  is_numeric <- is.numeric(x)
  is_perc <- all(x >= 0 & x <= 1, na.rm = TRUE)
  if (!is_numeric | !is_perc) {
    stop(paste0(top_env$sqp_columns, collapse = ", "),
         " must be numeric columns with values between/including 0 and 1 in `sqp_data`",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Variables to pick from the sqp remote data
# and with which to create sqp tables
top_env <- new.env(parent = emptyenv())
top_env$sqp_columns <- c("quality", "reliability", "validity")
