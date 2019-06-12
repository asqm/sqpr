#' Adjust a covariance matrix for Common Method Variance (CMV)
#'
#' \code{sqp_cmv_cov} accepts a covariance matrix, a covariance data frame or a covariance
#' tibble from \code{\link{sqp_covariance}}
#' and adjusts the coefficients of the variables specified
#' in  \code{...} with the reliability and validity coefficients given
#' by \code{\link{get_estimates}}. All variables specified in \code{...} must
#' be present in both \code{x} and \code{sqp_data}. Optionally, you can supply
#' the cmv coefficient in the argument \code{cmv}.
#'
#' @param x a covariance matrix, a covariance data frame or a covariance
#'  \code{tibble} given by \code{\link{sqp_covariance}}.
#' @param sqp_data a data frame or tibble of class \code{sqp} given by
#' \code{\link{get_estimates}}.
#' @param ... two or more variables present in both \code{x} and \code{sqp_data}. Can
#' be both in bare unquoted names or as character strings.
#'
#' @param original_data A data frame containing the original data for variables specified
#' in \code{...}. These are the original columns of the variables from the covariance
#' matrix supplied in \code{x}.
#'
#' @param cmv an optional numeric vector of length 1 which contains the
#' CMV coefficient of the variables specified in \code{...}.
#' This argument is left available if the user has reasons to input their own CMV.
#' By default, it is set to NULL and it is calculated internally.
#'
#' @return the same matrix supplied in \code{x} but as a tibble with
#' the covariance coefficients of the variables supplied in \code{...}
#' adjusted for their common method variance.
#'
#' If \code{x}  had row names, they're moved as a column called
#' 'rowname'. If it doesnt have row names or a column named 'rowname' a new
#' column is created with the column names to mimic a covariance matrix.
#'
#' @export
#'
#' @seealso \code{\link{sqp_covariance}} for calculating covariance matrices,
#' \code{\link{get_estimates}} for obtaining the SQP estimates directly into R
#' and \code{\link{estimate_cmv}} for calculating the CMV manually.
#'
#' @examples
#'
#' set.seed(2131)
#' library(tibble)
#'
#' original_df <- as.data.frame(matrix(rnorm(100, sd = 50), nrow = 20))
#' cov_tibble <- sqp_covariance(original_df)
#'
#' # Toy dataset
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
#' # Original covariance matrix with new diagonal
#' cov_tibble
#'
#' # Coefficient of covariance changes
#' # when adjusting for common method variance
#' sqp_cmv_cov(cov_tibble, sqp_df, V4, V5, original_data = original_df)
#'
#' # The V5*V4 from both the upper/lower triangles
#' # covariance matrix changed from -181 to -634
#'
sqp_cmv_cov <- function(x, sqp_data, ..., original_data, cmv = NULL) {
  cmv_vars <- unique(as.character(substitute(list(...)))[-1])

  if (!(is.data.frame(x) | is.matrix(x))) {
    stop("`x` must be a covariance data frame or matrix")
  }

  if (length(cmv_vars) < 2) {
    stop("You need to supply at least two variables to calculate the Common Method Variance",
         call. = FALSE)
  }

  sqp_data <- sqp_reconstruct(sqp_data, c("reliability", "validity"))

  x <- matrix2tibble(x)

  # Check if all supplied variables are present in both
  # dfs
  columns_present(x, sqp_data, cmv_vars)

  selected_rows <- sqp_data[[1]] %in% cmv_vars
  if (is.null(cmv)) cmv <- estimate_cmv(sqp_data[selected_rows, ])

  if (!all(cmv_vars %in% names(original_data))) {
    stop("Variables ",
         paste(cmv_vars[!cmv_vars %in% names(original_data)], collapse = ", "),
         " are not preset in `original_data`")
  }

  # We multiply by the SD of the data because all SQP coefficients are standardized.
  # This way, we unstandardize them to get an unstandardized cmv
  cmv <- prod(cmv, vapply(original_data[cmv_vars], stats::sd, na.rm = TRUE, FUN.VALUE = numeric(1)))

  corrected_cov <- tibble::as_tibble(replace_matrix_cmv(x, cmv, cmv_vars),
                                     .name_repair = 'minimal')
  corrected_cov
}
