#' Title
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sqp_cmv <- function(x, y, ...) {
  cmv_vars <- as.character(substitute(list(...)))[-1]

  if (length(cmv_vars) == 1) {
    stop("You need to supply at least two variables to calculate the common method variance",
         call. = FALSE)
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

  cmv <- estimate_cmv(y, sum_sqp)

  corrected_corr <- tibble::as_tibble(corr2cmv(x, cmv, cmv_vars))

  corrected_corr
}


estimate_cmv <- function(y, vars_sqp) {

  first_part <- sqrt(y[vars_sqp, "reliability", drop = TRUE])
  second_part <- sqrt(1 - y[vars_sqp, "validity", drop = TRUE])

  cmv <- purrr::reduce(c(first_part, second_part), `*`)
  cmv
}

# @unexported
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

