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
#' @param wt a non-NA numeric vector of the same length as the variables
#' specified in \code{...}. This will be used as weights in calculating the
#' sum scores of all variable. Be default, all variables are given the same weight.
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
#' "V1", "V2", "V3"
#' )
#'
#'
sqp_sscore <- function(sqp_data, df, new_name, ..., wt = NULL) {

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
  if (!all(purrr::map_lgl(the_vars, is.numeric))) {
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
  combined_matrix <- dplyr::bind_rows(sqp_data[!rows_to_pick, ], additional_rows)
  correct_order <- c("question", sqp_env$sqp_columns)
  new_order <- combined_matrix[c(correct_order, setdiff(names(combined_matrix), correct_order))]

  structure(new_order, class = c(class(new_order), "sqp"))
}

# This is not supposed to be used in isolation.
# Rather with measurement quality as a wrapper
# because it checks all of the arguments are in
# the correct format, etc..
estimate_sscore <- function(sqp_data, the_data, wt) {

  if (is.null(wt)) wt <- rep(1, length(the_data))

  is_numeric <- is.numeric(wt)
  is_na <- anyNA(wt)
  correct_length <- length(wt) == ncol(the_data)

  if (!is_numeric | is_na | !correct_length) {
    stop("`wt` must be a non-NA numeric vector with the same length as the number of variables")
  }

  # 1 is validity
  # 2 is reliability
  # 3 is validity

  qy2 <- sqp_data[[sqp_env$sqp_columns[1]]]

  # By squaring this you actually get the reliability
  # coefficient.
  ry <- sqrt(sqp_data[[sqp_env$sqp_columns[2]]])
  vy <- sqrt(sqp_data[[sqp_env$sqp_columns[3]]])

  # Method effect
  method_e <- sqrt(1 - vy^2)

  std_data <- purrr::map_dbl(the_data, stats::sd, na.rm = TRUE)

  # This is the 'quality coefficient obtained by SQP
  # for the observed variable i. (1-qi2)var(yi)
  q_coef <- qcoef_observed(qy2, std_data)

  # Here you create
  # all combinations
  comb <- utils::combn(seq_along(the_data), 2, simplify = FALSE)

  # This the multiplication of all variable combinations
  # using ri * mi * mj * rj * si * sj
  # It's better not to use this in isolation but call
  # estimate_sscore as a whole.
  cov_e <- cov_both(comb, ry, method_e)

  weights_by_qcoef <- sum(wt^2 * q_coef)

  # you need to calculate the product of a combination
  # of the weights by the covariance of errors.
  intm <- sum(combn_multiplication(comb, wt, cov_e)) * 2

  var_ecs <- weights_by_qcoef + intm
  var_composite <- stats::var(rowSums(the_data, na.rm = TRUE))

  1 - (var_ecs / var_composite)
}

qcoef_observed <- function(quality, std_data) {
  purrr::map2_dbl(quality, std_data, ~ (1 - .x) * .y^2)
}

combn_multiplication <- function(comb, wt, cov_e) {
  # This might seem confusing but it's actually not that hard.
  intm <- purrr::map2_dbl(comb, seq_along(comb), function(both_combn, the_seq) {
    # both_combn is the combination of variables like 1:2, 2:3 and c(3, 1)
    # below I grab both ends
    separ_first <- both_combn[1]
    separ_second <- both_combn[2]

    # and the multiply the weigghts with the cov_e
    # so for example wt[1] * wt[2] * cov_e[1]
    # so for example wt[1] * wt[3] * cov_e[3]
    purrr::map2_dbl(separ_first, separ_second, ~ wt[.x] * wt[.y] * cov_e[the_seq])
  })
}

# For an explanation of this see combn_multiplication
cov_both <- function(combinations, r_coef, method_e) {

  # This formula is not complicated. It's simply the product of
  # the standard deviation of the data, the r_coef and the
  # method effect between all combination of questions.
  cov_formula <- function(one, two, r_coef, method_e) {
    (r_coef[one] * method_e[one]) *
    (r_coef[two] * method_e[two])
  }

  # Here I apply the formula to all combinations. combinations
  # must be a list where each slot is of length 2 with a pair
  # combination. The whole list must contain all combinations
  result <- purrr::map_dbl(combinations, function(index) {
    index_one <- index[1]
    index_two <- index[2]
    result <- purrr::map2_dbl(index_one, index_two, cov_formula,
                              r_coef, method_e)
    result
  })
  result
}
