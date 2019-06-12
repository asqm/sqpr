#' Calculate quality of sum score of selected variables
#'
#' \code{sqp_sscore} takes a data frame with quality estimates from
#' \code{get_estimates} and estimates the quality of a sum score for
#'  selected variables in \code{...}.
#'
#' @param sqp_data a data frame given by \code{get_estimates} containing
#' quality estimates from the variables specified in \code{...}.
#' @param df a data frame which contains data for the variables specified
#' in \code{...}.
#' @param new_name a bare unquoted name or a string specifying the name
#' of the new sum score.
#' @param ... bare unquoted names or separate strings specifying
#' the variable names from which to estimate quality of their sum score.
#' They all must be present in \code{sqp_data} and \code{df}. At minimum,
#' it must be two or more variable names.
#'
#' @param wt a non-NA numeric vector of the same length as the variables
#' specified in \code{...}. This will be used as weights in calculating the
#' sum scores of all variable. Be default, all variables are given the same
#' weight.
#'
#' @param drop a logical stating whether to drop the questions that compose
#' the sum score (specified in \code{...}) If \code{FALSE} it retains the original questions
#' and the composite score.
#'
#' @return a \code{\link[tibble]{tibble}} similar to \code{sqp_data} but
#' with a new row containing the quality of a sum score with the name
#' specified in \code{new_name}.
#'
#' @export
#'
#' @examples
#'
#' # Toy data
#'
#' library(tibble)
#' sqp_df <-
#' tibble(question = paste0("V", 1:5),
#'        quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'        reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
#'        validity = c(0.8, 0.1, 0.6, 0.7, 0.8))
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
#'
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
#' "V1", "V2"
#' )
#'
#'
sqp_sscore <- function(sqp_data, df, new_name, ..., wt = NULL, drop = TRUE) {

  # Check SQP data has correct class and formats
  sqp_data <- sqp_reconstruct(sqp_data)

  # Turn all variables into a list and delete the 'list'
  # from the new character vector
  vars_names <- unique(as.character(substitute(list(...)))[-1])
  summary_name <- unique(as.character(substitute(new_name)))

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
  if (!all(vapply(the_vars, is.numeric, FUN.VALUE = logical(1)))) {
    stop(paste0(vars_names, collapse = ", "), " must be numeric variables in `df`")
  }

  if (ncol(the_vars) < 2) stop("`df` must have at least two columns")

  # Select the rows with only the selected variales
  # for the sumscore
  rows_to_pick <- sqp_data[[1]] %in% vars_names
  sqp_scores <- sqp_data[rows_to_pick, sqp_env$sqp_columns]

  if (anyNA(sqp_scores)) {
    stop("`sqp_data` must have non-missing values at variable/s: ",
         paste0(sqp_env$sqp_columns, collapse = ", "))
  }

  new_estimate <-
    columns_sqp("quality", estimate_sscore(sqp_scores, the_vars, wt = wt))

  additional_rows <- generic_sqp(summary_name, new_estimate)

  # Bind the unselected questions with the new sumscore

  if (!drop) {
    rows_to_pick <- rep(TRUE, length(rows_to_pick))
  } else {
    rows_to_pick <- !rows_to_pick
  }
  combined_matrix <- dplyr::bind_rows(sqp_data[rows_to_pick, ], additional_rows)
  correct_order <- c("question", sqp_env$sqp_columns)
  new_order <- combined_matrix[c(correct_order, setdiff(names(combined_matrix), correct_order))]

  final_df <- sqp_reconstruct(new_order)
  final_df
}

# This is not supposed to be used in isolation.
# Rather with measurement quality as a wrapper
# because it checks all of the arguments are in
# the correct format, etc..
estimate_sscore <- function(sqp_data, the_data, wt) {

  if (is.null(wt)) wt <- rep(1L, length(the_data))

  is_numeric <- is.numeric(wt)
  is_na <- anyNA(wt)
  correct_length <- length(wt) == ncol(the_data)

  if (!is_numeric | is_na | !correct_length) {
    stop("`wt` must be a non-NA numeric vector with the same length as the number of variables")
  }

  reliability <- grep("^r", sqp_env$sqp_columns, value = TRUE)
  validity <- grep("^v", sqp_env$sqp_columns, value = TRUE)
  quality <- grep("^q", sqp_env$sqp_columns, value = TRUE)

  qy <- sqrt(sqp_data[[quality]])

  # by squaring this you actually get the reliability
  # coefficient.
  ry <- sqrt(sqp_data[[reliability]])
  vy <- sqrt(sqp_data[[validity]])

  # Method effect
  method_e <- sqrt(1 - vy^2)

  std_data <- vapply(the_data, stats::sd, na.rm = TRUE, FUN.VALUE = numeric(1))
  std_sscore <- sd(rowSums(the_data, na.rm = TRUE), na.rm = TRUE)

  # Calculate weight / std dev of the sumscore multiplied by
  # the quality squared.
  var_quality_adj <- ((wt / std_sscore) * qy)^2
  sum_var_qual_adj <- sum(var_quality_adj)
  
  # Here you create
  # all combinations
  comb <- utils::combn(seq_along(the_data), 2, simplify = FALSE)

  # This the multiplication of all variable combinations
  # using ri * mi * mj * rj * si * sj
  # It's better not to use this in isolation but call
  # estimate_sscore as a whole.
  cov_e <- cov_both(comb, ry, method_e)
  est_matrix <- cov(the_data, use = 'complete.obs')
  cov_coefs <- vapply(comb,
                      function(.x) est_matrix[.x[1], .x[2]],
                      FUN.VALUE = numeric(1))

  observed_matrix <- cov_coefs - cov_e

  
  # you need to calculate the product of a combination
  # of the weights by the std of the sscore and observed cor/cov
  # estimate.
  intm <- combn_multiplication(comb, wt, observed_matrix, std_sscore)
  twice_sum <- sum(intm) * 2
  final_sum <- sum_var_qual_adj + twice_sum
  final_sum
}

qcoef_observed <- function(quality, std_data) {
  qcoef_formula <- function(i) (1 - quality[i]) * std_data[i]^2
  vapply(seq_along(quality), qcoef_formula, FUN.VALUE = numeric(1))
}

combn_multiplication <- function(comb, wt, matrix_est, std_sscore) {

  final_mult <- vapply(seq_along(comb), function(i) {
    # both_combn is the combination of variables like 1:2, 2:3 and c(3, 1)
    # below I grab both ends
    separ_first <- comb[[i]][1]
    separ_second <- comb[[i]][2]

    # and the multiply the weights with the std of the sscore
    # so for example (wt[1] / std_sscore) * (wt[2] / std_sscore)
    # for example (wt[1] / std_sscore) * (wt[3] / std_sscore)
    wt_adj <- (wt[separ_first] / std_sscore) * (wt[separ_second] / std_sscore)

    # And then multiply these weight adjustments with the cor/cov estimates
    # and multiply it by the square of the std of the sscore
    matrix_est[i] * wt_adj * std_sscore^2

  }, FUN.VALUE = numeric(1))

  final_mult
}

# For an explanation of this see combn_multiplication
cov_both <- function(combinations, r_coef, method_e) {

  # This formula is not complicated. It's simply the product of
  # the standard deviation of the data, the r_coef and the
  # method effect between all combination of questions.
  cov_formula <- function(one, two, r_coef, method_e) {
    (r_coef[one] * method_e[one]) * (r_coef[two] * method_e[two])
  }

  # Here I apply the formula to all combinations. combinations
  # must be a list where each slot is of length 2 with a pair
  # combination. The whole list must contain all combinations
  result <- vapply(combinations, function(index) {
    index_one <- index[1]
    index_two <- index[2]
    result <- cov_formula(index_one, index_two, r_coef, method_e)
    result
  }, FUN.VALUE = numeric(1))

  result
}
