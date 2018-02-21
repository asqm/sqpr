#' Adjust a correlation matrix for Common Method Variance (CMV)
#'
#' \code{sqp_cmv} accepts a correlation matrix or a correlation
#' tibble from \code{\link{sqp_correlate}}
#' and adjusts the coefficients of the variables specified
#' in  \code{...} with the reliability and validity coefficients given
#' by \code{\link{sqp_collect}}. All variables specified in \code{...} must
#' be present in both \code{x} and \code{sqp_data}. Optionally, you can supply
#' the cmv coefficient in the argument \code{cmv}.
#'
#' @param x a correlation matrix, a correlation data frame or a correlation
#'  \code{tibble} given by \code{\link{sqp_correlate}}. If any of the previous
#'  have row names, they're moved as a column called 'rowname'. If they don't
#'  have row names or a column named 'rowname' a new column is created with
#'  the column names to mimic a correlation matrix.
#' @param sqp_data a data frame or tibble of class \code{sqp} given by \code{sqp_collect}.
#' @param ... two or more variables present in both \code{x} and \code{sqp_data}. Can
#' be both in bare unquoted names or as character strings.
#' @param cmv an optional numeric vector of length 1 which contains the
#' CMV coefficient of the variables specified in \code{...}. It is strongly
#' suggested that this coefficient is estimated via \code{\link{estimate_cmv}}.
#' By default, it is set to NULL and it is calculated internally.
#'
#' @return the same matrix supplied in \code{x} but as a tibble with
#' the correlation coefficients of the variables supplied in \code{...}
#' adjusted for their common method variance.
#' @export
#'
#' @seealso \code{\link{sqp_correlate}} and \code{\link{sqp_collect}} for the correct
#' format of the data and \code{\link{estimate_cmv}} for calculating the CMV manually.
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
sqp_cmv <- function(x, sqp_data, ..., cmv = NULL) {
  cmv_vars <- unique(as.character(substitute(list(...)))[-1])

  if (!(is.data.frame(x) | is.matrix(x))) {
    stop("`x` must be a correlation data frame or matrix")
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

  corrected_corr <- tibble::as_tibble(corr2cmv(x, cmv, cmv_vars))

  corrected_corr
}


#' Estimate the Common Method Variance (CMV) coefficient of a set of variables
#'
#'
#' @param sqp_data a data frame or tibble of class \code{sqp} given by \code{\link{sqp_collect}}
#'  which contains the desired variables from which to estimate the CMV.
#'
#' @return a numeric vector of length one with the estimated coefficient
#' @export
#'
#' @seealso \code{\link{sqp_cmv}} for automatically adjusting a correlation
#' matrix for the CMV and \code{\link{sqp_collect}} for obtaining SQP data.
#'
#' @examples
#' library(tibble)
#'
#' sqp_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(0.5, 0.1, 0.6, 0.7, 0.8))
#'
#' estimate_cmv(sqp_df)
#'
#' \dontrun{
#'
#' sqp_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(NA, 0.1, 0.6, 0.7, 0.8))
#'
#' estimate_cmv(sqp_df)
#' # Error in estimate_cmv(sqp_df) :
#' # `sqp_data` must have non-missing values at columns reliability and validity for all variables
#' }
#'
#'
estimate_cmv <- function(sqp_data) {
  sqp_cols <- c("reliability", "validity")
  sqp_reconstruct(sqp_data, sqp_cols)

  if (anyNA(sqp_data[sqp_cols])) {
    stop("`sqp_data` must have non-missing values at columns reliability and validity for all variables")
  }

  first_part <- sqrt(sqp_data[[sqp_cols[1]]])
  second_part <- sqrt(1 - sqp_data[[sqp_cols[2]]])

  cmv <- prod(c(first_part, second_part))
  cmv
}

# This function is the one doing the replacement of the upper
# and lower of the correlation matrix.
corr2cmv <- function(x, cmv, cmv_vars) {
  x_row_low <- sort(match(cmv_vars, x[[1]]))
  x_col_low <- sort(match(cmv_vars, names(x)))

  x <- as.data.frame(x)

  p <- x[x_row_low, x_col_low] # subset only the select variables
  p[lower.tri(p)] <- p[lower.tri(p)] - cmv # adjust the lower.tri
  p[upper.tri(p)] <- p[upper.tri(p)] - cmv # adjust the upper.tri
  x[x_row_low, x_col_low] <- p # replace in the original data.frame

  x
}


matrix2tibble <- function(x) {
  has_rowname_col <- "rowname" %in% names(x)

  # It has a column rowname and is a tibble
  # then it's porbbaly from sqp_correlate
  if (tibble::has_name(x, "rowname") && tibble::is_tibble(x)) {
    return(x)
  } else if (tibble::has_rownames(x) & !has_rowname_col) {
    # If it has rownames and doesn't have a row name column
    # turn into tibble with row name column
    return(tibble::as_tibble(x, rownames = "rowname"))
  }

  x <- tibble::as_tibble(x)

  if (!tibble::has_rownames(x) & !has_rowname_col) {
    x <- tibble::add_column(x,
                            rowname = names(x),
                            .before = 1)
  }
  x
}
