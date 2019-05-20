#' Calculate a correlation matrix with an adjusted diagonal
#'
#' \code{sqp_correlate} calculates a correlation matrix through
#' \code{\link[stats]{cor}} and multiplies the diagonal with the supplied
#' numeric vector. It's a wrapper around \code{\link[stats]{cor}} with slight tweaks.
#'
#' @param x a matrix or data frame with numeric columns.
#'
#' @param diag_adj a numeric vector with length equal to the number of columns of \code{x} to be multiplied by the diagonal.
#' Alternatively, it can be of length 1 which will be repeated through the whole diagonal.
#' By default it multiplies by 1, giving the same diagonal.
#'
#' @param use an optional character string giving a method for computing covariances
#'  in the presence of missing values. This must be (an abbreviation of) one of
#'  the strings "everything", "all.obs", "complete.obs", "na.or.complete", or
#'  "pairwise.complete.obs".
#' @param method a character string indicating which correlation coefficient
#'  (or covariance) is to be computed. One of "pearson" (default), "kendall",
#'   or "spearman": can be abbreviated.
#'
#' @return a correlation \code{tibble} with variable names as a column and
#' the diagonal multiplied by \code{diag_adj}
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
#' sqp_correlate(mtcars)
#'
#' sqp_correlate(mtcars, new_diagonal)
#'
#' sqp_correlate(mtcars, new_diagonal, method = "kendall")
#'
sqp_correlate <- function(x, diag_adj = 1, use = "complete.obs", method = "pearson") {
  cor_cov_matrix(stats::cor,
                 x = x,
                 diag_adj = diag_adj,
                 use = use,
                 method = method)
}

#' Calculate a covariance matrix with an adjusted diagonal
#'
#' \code{sqp_covariance} calculates a covariance matrix through
#' \code{\link[stats]{cov}} and multiplies the diagonal with the supplied
#' numeric vector. It's a wrapper around \code{\link[stats]{cov}} with slight tweaks.
#'
#' @inheritParams sqp_correlate
#'
#' @return a covariance \code{tibble} with variable names as a column and
#' the diagonal multiplied by \code{diag_adj}
#'
#' @seealso \code{\link[stats]{cov}} for the workhorse behind the function and for
#' details on the \code{use} and \code{method} arguments.
#'
#' @export
#'
#' @examples
#'
#' # New diagonal
#' new_diagonal <- rnorm(ncol(mtcars))
#'
#' sqp_covariance(mtcars)
#'
#' sqp_covariance(mtcars, new_diagonal)
#'
#' sqp_covariance(mtcars, new_diagonal, method = "kendall")
#'
sqp_covariance <- function(x, diag_adj = 1, use = "complete.obs", method = "pearson") {
  cor_cov_matrix(stats::cov,
                 x = x,
                 diag_adj = diag_adj,
                 use = use,
                 method = method)
}


cor_cov_matrix <- function(fun, x, diag_adj, use, method) {
  if (!is.numeric(diag_adj)) stop("`diag_adj` must be numeric")
  obj_matrix <- fun(x = x, use = use, method = method)
  diag_adj <- if (length(diag_adj) == 1) rep(diag_aj) * ncol(obj_matrix) else diag_adj
  if (length(diag_adj) != ncol(obj_matrix)) stop("`diag_adj` must be the same length as the number of columns in `x`")

  diag(obj_matrix) <- diag_adj * diag(obj_matrix)

  # If has no rows/column names, create them
  names_matrix <- dimnames(obj_matrix)
  if (!is.list(names_matrix) & length(names_matrix) != 3) {
    both_names <- paste0("V", seq_len(ncol(obj_matrix)))
    dimnames(obj_matrix) <- list(both_names, both_names)
  }

  formatted_matrix <- tibble::rownames_to_column(as.data.frame(obj_matrix))

  dplyr::as_tibble(formatted_matrix)
}
