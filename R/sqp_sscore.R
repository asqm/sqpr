## Jorge's function to calculate the quality of sum scores####

# Only select the columns with variable names
# and the quality prediction
# VERY IMPORTANT!
# the function accepts an sqp data with only two columns
# the first one with the variabl names (column 2) and the second
# one with the quality predictions (column 12)

#' Title
#'
#' @param sqp_data
#' @param df
#' @param new_name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sqp_sscore <- function(sqp_data, df, new_name, ...) {

  # Turn all variables into a list and delete the 'list'
  # from the new character vector
  vars_names <- as.character(substitute(list(...)))[-1]
  summary_name <- as.character(substitute(new_name))

  # Check all variables present in df
  vars_not_matched <- !vars_names %in% names(df)
  if (any(vars_not_matched)) {
    stop("One or more variables are not present in df: ",
         paste0(vars_names[vars_not_matched], collapse = ", "),
         call. = FALSE)
  }

  # Check all variables present in sqp_data
  vars_not_matched <- !vars_names %in% sqp_data[[1]]
  if (any(vars_not_matched)) {
    stop("One or more variables are not present in sqp_data: ",
         paste0(vars_names[vars_not_matched], collapse = ", "),
         call. = FALSE)
  }

  the_vars <- df[vars_names]

  # Check all variables are numeric and there are at least two columns in the df data
  if (!all(purrr::map_lgl(the_vars, is.numeric))) stop("All variables must be numeric")
  if (ncol(the_vars) < 2) stop("df must have at least two columns")

  # Check SQP data has correct class and formats
  check_sqp_data(sqp_data)

  # Select the rows with only the selected variales
  # for the sumscore
  rows_to_pick <- sqp_data[[1]] %in% vars_names
  sqp_scores <- sqp_data[rows_to_pick, 2, drop = TRUE]

  new_estimate <-
    columns_sqp("quality", estimate_sscore(sqp_scores, the_vars, vars_names))

  additional_rows <- generic_sqp(summary_name, new_estimate)

  # Bind the unselected questions with the new sumscore
  combined_matrix <- dplyr::bind_rows(sqp_data[!rows_to_pick, ], additional_rows)

  combined_matrix
}

# This is not supposed to be used in isolation.
# Rather with measurement quality as a wrapper
# because it checks all of the arguments are in
# the correct format, etc..
estimate_sscore <- function(sqp_data, df, vars_names) {

  # Calculate the sum score and it's variance
  var_sumscore <- stats::var(rowSums(df, na.rm = TRUE), na.rm = TRUE)

  # Calculat variable of each of the selected variables
  var_othervars <- purrr::map_dbl(df, stats::var, na.rm = TRUE)

  # Each sqp score is subtracted a 1 and multiplied
  # by it's corresponding var_* variables
  adj_sqp <- (1 - sqp_data) *  var_othervars

  # This new vector is summed and divided by the variance of the sumscore
  # and 1 is subtracted from the final result
  final_result <- 1 - sum(adj_sqp) / var_sumscore

  final_result
}


# Check the SQP data
check_sqp_data <- function(sqp_data) {
  stopifnot(is.data.frame(sqp_data))

  first_character <- is.character(sqp_data[[1]])
  all_numeric <- all(purrr::map_lgl(sqp_data[-1], is.numeric))

  # Check all variables are numeric in the sqp data
  if(!first_character | !all_numeric) {
    stop("The first column must be a character vector containing the question names and all other columns must be numeric")
  }
}
