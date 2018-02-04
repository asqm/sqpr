#' Calculate sum score of selected variables
#'
#' \code{sqp_sscore} takes a data frame with quality estimates from
#' \code{sqp_collect} and estimates a sum score for selected variables
#' in \code{...}.
#'
#' @param sqp_data a data frame given by \code{sqp_collect} containing
#' quality estimates from the variables specified in \code{...}.
#' @param df a data frame which contains data for the variables specified
#' in \code{...}.
#' @param new_name a bare unquoted name or a string specifying the name
#' of the new sum score.
#' @param ... bare unquoted names or separate strings specifying
#' the variable names from which to estimate the sum score. They all
#' must be present in \code{sqp_data} and \code{df}. At minimum, it must
#' be two or more variable names.
#'
#'
#' @return a \code{\link[tibble]{tibble}} similar to \code{sqp_data} but
#' with a new row containing the sum score with the name specified in
#' \code{new_name}. The result excludes the variables specified in
#' \code{...} and only shows the new sum score. In future releases
#' there might be an option to keep both the variables in \code{...}
#' and the sum score.
#'
#' @export
#'
#' @examples
#'
#' # Prepare data
#' library(tibble)
#' sqp_df <-
#' tibble(question = paste0("V", 1:5),
#'        quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'        reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
#'        validity = c(NA, NA, 0.6, 0.7, 0.8))
#'
#'
#' sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))
#'
#' sample_data <-
#'  as_tibble(
#'  setNames(
#'   replicate(5, rbinom(1000, 5, 0.6), simplify = FALSE),
#'  paste0("V", 1:5))
#'  )
#' #
#'
#' sqp_sscore(
#' sqp_data = sqp_df,
#' df = sample_data,
#' new_name = new_sumscore,
#' V3, V4
#' )
#'
#' sqp_sscore(
#' sqp_data = sqp_df,
#' df = sample_data,
#' new_name = new_sumscore,
#' "V1", "V2", "V3"
#' )
#'
#' sqp_sscore(
#' sqp_data = sqp_df,
#' df = sample_data,
#' new_name = new_sumscore,
#' V1, random_var
#' )
#'
sqp_sscore <- function(sqp_data, df, new_name, ...) {

  if (!inherits(sqp_data, "sqp")) {
    stop("`sqp_data` must be collected using sqp_collect()",
         call. = FALSE)
  }

  # Turn all variables into a list and delete the 'list'
  # from the new character vector
  vars_names <- as.character(substitute(list(...)))[-1]
  summary_name <- as.character(substitute(new_name))

  # Check all variables present in df
  vars_not_matched <- !vars_names %in% names(df)
  if (any(vars_not_matched)) {
    stop("One or more variables are not present in `df`: ",
         paste0(vars_names[vars_not_matched], collapse = ", "),
         call. = FALSE)
  }

  # Check all variables present in sqp_data
  vars_not_matched <- !vars_names %in% sqp_data[[1]]
  if (any(vars_not_matched)) {
    stop("One or more variables are not present in `sqp_data`: ",
         paste0(vars_names[vars_not_matched], collapse = ", "),
         call. = FALSE)
  }

  the_vars <- df[vars_names]

  # Check all variables are numeric and there are at least two columns in the df data
  if (!all(purrr::map_lgl(the_vars, is.numeric))) {
    stop(paste0(vars_names, collapse = ", "), " must be numeric variables in `df`")
  }

  if (ncol(the_vars) < 2) stop("`df` must have at least two columns")

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

  structure(combined_matrix, class = c(class(combined_matrix), "sqp"))
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
    stop("`sqp_data` must be collected using sqp_collect()",
         call. = FALSE)
  }
}

