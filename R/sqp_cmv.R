#' Adjust coefficients for Common Method Variance (CMV)
#'
#' \code{sqp_cmv} takes a correlation tibble from \code{\link{sqp_correlate}}
#' and adjusts the coefficients of the variables specified
#' in  \code{...} with the reliability and validity coefficients given
#' by \code{\link{sqp_collect}}. All variables specified in \code{...} must
#' be present in both \code{x} and \code{y}.
#'
#' @param x a correlation \code{tibble} given by \code{\link{sqp_correlate}}
#' @param y a data frame or tibble given by \code{\link{sqp_collect}}
#' @param ... two or more variables present in both \code{x} and \code{y}. Can
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
#' # The V5*V4 cell from the lower triangle of the
#' # correlation matrix changed from -0.137 to -0.282
#'
sqp_cmv <- function(x, y, ...) {
  cmv_vars <- as.character(substitute(list(...)))[-1]

  if (length(cmv_vars) < 2) {
    stop("You need to supply at least two variables to calculate the common method variance",
         call. = FALSE)
  }

  if (!inherits(y, "sqp")) {
    stop("`y` must be of class 'sqp'. Use collect_sqp() to collect the sqp data.", call. = FALSE)
  }

  if (is.matrix(x)) x <- tibble::as_tibble(x, rownames = "rowname")

  sum_corr <- x[[1]] %in% cmv_vars
  sum_sqp <- y[[1]] %in% cmv_vars

  vars_corr <- cmv_vars %in% x[[1]]
  vars_sqp <- cmv_vars %in% y[[1]]

  if (sum(sum_corr) != length(cmv_vars)) {
    stop("At least one variable not present in `x`: ",
         paste0(cmv_vars[!vars_corr], collapse = ", "),
         call. = FALSE)
  }

  if ((sum(sum_sqp) != length(cmv_vars))) {
    stop("At least one variable not present in `y`: ",
         paste0(cmv_vars[!vars_sqp], collapse = ", "),
         call. = FALSE)
  }

  # If estimate_cmv ends up being exportable, then this needs
  # to be moved inside estimate_cmv and find a way to pass
  # cmv_vars to estimate_cmv
  if (anyNA(y[sum_sqp, c("reliability", "validity")])) {
    stop("`y` must have non-missing values at variable/s: ",
         paste0(cmv_vars, collapse = ", "))
  }

  cmv <- estimate_cmv(y[sum_sqp, ])

  corrected_corr <- tibble::as_tibble(corr2cmv(x, cmv, cmv_vars))

  corrected_corr
}

# Workhorse of the function. It simply estimates
# the common method variance of the variables.

estimate_cmv <- function(y) {
  first_part <- sqrt(y[["reliability"]])
  second_part <- sqrt(1 - y[["validity"]])

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
  x_row <- sort(match(cmv_vars, x[[1]]))
  x_col <- sort(match(cmv_vars, names(x)))

  x <- as.data.frame(x)

  # Because we only want to adjust the triangle
  # below the diagonal, we ignore the upper triangle.
  # The upper triangle will be eliminated in future call.
  p <- x[x_row, x_col] # subset only the select variables
  p[lower.tri(p)] <- p[lower.tri(p)] - cmv # adjust the lower.tri
  x[x_row, x_col] <- p # replace in the original data.frame
  x
}

