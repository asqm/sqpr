#' Calculate a correlation matrix with an adjusted diagonal
#'
#' \code{sqp_correlate} calculates a correlation matrix through
#' \code{\link[stats]{cor}} and replaces the diagonal with the supplied
#' numeric vector. It's a wrapper around \code{\link[stats]{cor}} with slight tweaks.
#'
#' @param x a matrix or data frame with numeric columns.
#' @param diag_adj a numeric vector with length equal to the number of columns of \code{x}
#' @param use an optional character string giving a method for computing covariances
#'  in the presence of missing values. This must be (an abbreviation of) one of
#'  the strings "everything", "all.obs", "complete.obs", "na.or.complete", or
#'  "pairwise.complete.obs".
#' @param method a character string indicating which correlation coefficient
#'  (or covariance) is to be computed. One of "pearson" (default), "kendall",
#'   or "spearman": can be abbreviated.
#'
#' @return a correlation \code{tibble} with variable names as a column and
#' the diagonal replaced by \code{diag_adj}
#'
#' @seealso \code{\link[stats]{cor}} for the workhorse behind the function and for
#' details on the \code{use} and \code{method} arguments.
#'
#' @export
#'
#' @examples
#'
#' # New diagonal
#' new_diagonal <- rnorm(ncol(mtcars))
#'
#' sqp_correlate(mtcars, new_diagonal)
#'
#' sqp_correlate(mtcars, new_diagonal, method = "kendall")
#'
sqp_correlate <- function(x, diag_adj, use = "complete.obs", method = "pearson") {
  if (!is.numeric(diag_adj)) stop("`diag_adj` must be numeric")
  corr_matrix <- stats::cor(x = x, use = use, method = method)
  diag(corr_matrix) <- diag_adj

  # If has no rows/column names, create them
  names_matrix <- dimnames(corr_matrix)
  if (!is.list(names_matrix) & length(names_matrix) != 3) {
    both_names <- paste0("V", seq_len(ncol(corr_matrix)))
    dimnames(corr_matrix) <- list(both_names, both_names)
  }

  formatted_matrix <- tibble::rownames_to_column(as.data.frame(corr_matrix))

  dplyr::as_tibble(formatted_matrix)
}
