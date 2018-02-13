#' Adjust coefficients for Common Method Variance (CMV)
#'
#' \code{sqp_cmv} takes a correlation tibble from \code{\link{sqp_correlate}}
#' and adjusts the coefficients of the variables specified
#' in  \code{...} with the reliability and validity coefficients given
#' by \code{\link{sqp_collect}}. All variables specified in \code{...} must
#' be present in both \code{x} and \code{sqp_data}.
#'
#' @param x a correlation matrix or a correlation \code{tibble}
#'  given by \code{\link{sqp_correlate}}
#' @param sqp_data a \code{data frame} or \code{tibble} given by \code{\link{sqp_collect}}
#' @param ... two or more variables present in both \code{x} and \code{sqp_data}. Can
#' be both in bare unquoted names or as character strings.
#'
#' @return the same matrix supplied in \code{x} but as a tibble with
#' the correlation coefficients of the variables supplied in \code{...}
#' adjusted for their common method variance.
#' @export
#'
#' @seealso \code{\link{sqp_correlate}}, \code{\link{sqp_collect}}
#'
#' @examples
#'
#' # Note to Jorge: Must be changed to a valid correlation matrix
#' # that has same name in the SQP data base
#' # Also change ALL of this in thte tests doc
#' set.seed(2131)
#' library(tibble)
#'
#' corr_tibble <- sqp_correlate(matrix(rnorm(100, sd = 50), nrow = 20), rnorm(5))
#'
#' # Note to Jorge: Change this to a sqp df when sqp_collect() works.
#' sqp_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(NA, NA, 0.6, 0.7, 0.8))
#'
#' sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))
#'
#' # Show that when y is not from sqp class, there's an error
#'
#' # Original correlation matrix with new diagonal
#' corr_tibble
#'
#' # Coefficient of correlation changes
#' # when adjusting for common method variance
#' sqp_cmv(corr_tibble, sqp_df, V4, V5)
#'
#' # The V5*V4 from both the upper/lower triangles
#' # correlation matrix changed from -0.137 to -0.282
#'
sqp_cmv <- function(x, sqp_data, ...) {
  cmv_vars <- as.character(substitute(list(...)))[-1]

  if (length(cmv_vars) < 2) {
    stop("You need to supply at least two variables to calculate the common method variance",
         call. = FALSE)
  }

  sqp_data <- sqp_reconstruct(sqp_data)

  if (is.matrix(x)) x <- tibble::as_tibble(x, rownames = "rowname")

  sum_corr <- x[[1]] %in% cmv_vars
  sum_sqp <- sqp_data[[1]] %in% cmv_vars

  vars_corr <- cmv_vars %in% x[[1]]
  vars_sqp <- cmv_vars %in% sqp_data[[1]]

  if (sum(sum_corr) != length(cmv_vars)) {
    stop("At least one variable not present in `x`: ",
         paste0(cmv_vars[!vars_corr], collapse = ", "),
         call. = FALSE)
  }

  if ((sum(sum_sqp) != length(cmv_vars))) {
    stop("At least one variable not present in `sqp_data`: ",
         paste0(cmv_vars[!vars_sqp], collapse = ", "),
         call. = FALSE)
  }

  # If estimate_cmv ends up being exportable, then this needs
  # to be moved inside estimate_cmv and find a way to pass
  # cmv_vars to estimate_cmv
  if (anyNA(sqp_data[sum_sqp, c("reliability", "validity")])) {
    stop("`sqp_data` must have non-missing values at variable/s: ",
         paste0(cmv_vars, collapse = ", "))
  }

  cmv <- estimate_cmv(sqp_data[sum_sqp, ])

  corrected_corr <- tibble::as_tibble(corr2cmv(x, cmv, cmv_vars))

  corrected_corr
}

# Workhorse of the function. It simply estimates
# the common method variance of the variables.

estimate_cmv <- function(sqp_data) {
  first_part <- sqrt(sqp_data[["reliability"]])
  second_part <- sqrt(1 - sqp_data[["validity"]])

  cmv <- purrr::reduce(c(first_part, second_part), `*`)
  cmv
}

# This function is the one doing the replacing in
# the correlation matrix. It adjusts only the
# lower.tri of the matrix.
corr2cmv <- function(x, cmv, cmv_vars) {
  # Here I sort because if not I would be
  # getting the index of the upper triangle
  # and we want to work with the lower.tri
  x_row_low <- sort(match(cmv_vars, x[[1]]))
  x_col_low <- sort(match(cmv_vars, names(x)))
  x_row_up <- match(cmv_vars, x[[1]])
  x_col_up <- match(cmv_vars, names(x))


  x <- as.data.frame(x)

  # Because we only want to adjust the triangle
  # below the diagonal, we ignore the upper triangle.
  # The upper triangle will be eliminated in future call.
  p <- x[x_row_low, x_col_low] # subset only the select variables
  p[lower.tri(p)] <- p[lower.tri(p)] - cmv # adjust the lower.tri
  x[x_row_low, x_col_low] <- p # replace in the original data.frame

  p <- x[x_row_up, x_col_up] # subset only the select variables
  p[upper.tri(p)] <- p[upper.tri(p)] - cmv # adjust the upper.tri
  x[x_row_up, x_col_up] <- p # replace in the original data.frame

  x
}

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

check_sqp_data <- function(sqp_data) {
  # Check top_env$sqp_columns variables exists

  metrics_available <- all(top_env$sqp_columns %in% names(sqp_data))

  if (!metrics_available) {
    stop("Variables ", paste0(top_env$sqp_columns, collapse = ", "),
         " must be available in `sqp_data`",
         call. = FALSE)
  }

  purrr::walk(sqp_data[top_env$sqp_columns], col_checker)
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


