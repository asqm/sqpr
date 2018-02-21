# Check sqp data and throw erros if it's not in correct format.
# If it is in the correct format attach 'sqp' class if it doesn't
# have it.

# Why? Because using any of the sqp_ funs with tidyverse verbs
# (or any other function whatsoever), drops the 'sqp' class. These
# functions will check whether the data is in right format and assign
# the class accordingly. It basically makes sure the data is good for
# later processing.
sqp_reconstruct <- function(sqp_data, variables_check = top_env$sqp_columns) {

  # If sqp_data is not in the correct format, throw an error
  check_sqp_data(sqp_data, variables_check)

  # If it has a correct format, then simply add the sqp class if
  # it doesn't have it
  if (!inherits(sqp_data, "sqp")) class(sqp_data) <- c(class(sqp_data), "sqp")
  sqp_data
}

check_sqp_data <- function(sqp_data, available_vars) {
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
  if (all(is.na(x))) return(TRUE)

  is_numeric <- is.numeric(x)
  is_perc <- all(x >= 0 & x <= 1, na.rm = TRUE)
  if (!is_numeric | !is_perc) {
    stop(paste0(top_env$sqp_columns, collapse = ", "),
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

# Variables to pick from the sqp remote data
# and with which to create sqp tables
top_env <- new.env(parent = emptyenv())
top_env$sqp_columns <- c("quality", "reliability", "validity")
