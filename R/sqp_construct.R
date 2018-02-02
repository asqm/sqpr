#' Title
#'
#' @param question_name
#' @param metrics
#'
#' @return
#' @export
#'
#' @examples
construct_sqp <- function(question_name, metrics) {
  question <- as.character(substitute(question_name))
  construct_sqp_(question, metrics)
}


#' Title
#'
#' @param question_name
#' @param metrics
#'
#' @return
#' @export
#'
#' @examples
construct_sqp_ <- function(question_name, metrics) {

  question <- question_name

  if (length(question) > 1) stop("question_name must have only one question")

  is_list <- is.list(metrics)
  named <- !is.null(names(metrics))
  numeric <- is.numeric(unlist(metrics))

  if (!named | !numeric | !is_list) {
    stop("metrics must be a named numeric list",
         call. = FALSE)
  }

  if (length(names(metrics)) != length(unlist(metrics))) {
    stop("metrics must contain only one element per name",
         call. = FALSE)
  }

  sqp_metrics <- columns_sqp(names(metrics), unlist(metrics))

  generic_sqp(question, sqp_metrics)
}

# unexported
# Specify columns that should be in the SQP data and
# replacements
# returns a named list with the replacements
# added
columns_sqp <- function(columns_to_fill, replacement) {

  if (!all(columns_to_fill %in% top_env$sqp_columns)) {
    stop("One or more of the specified metrics don't match the SQP column names",
         call. = FALSE)
  }

  # sqp_columns is a global variable defining
  # the columns that SQP needs to have
  num_cols <- length(top_env$sqp_columns)
  empty_cols <- purrr::set_names(purrr::rerun(num_cols, NA), top_env$sqp_columns)

  # iterate through each column/replacement and fill
  # out the empty list
  for (some_cols in seq_along(columns_to_fill)) {
    chosen_col <- columns_to_fill[some_cols]
    empty_cols[[chosen_col]] <- replacement[some_cols]
  }

  filled_cols <- empty_cols
  filled_cols
}

# unexported
# Create a tibble with the question name and
# the sqp matrics. Returns the tibble
generic_sqp <- function(question_name, sqp_metrics) {
  stopifnot(!is.null(names(sqp_metrics)), is.list(sqp_metrics))

  dplyr::as_tibble(c(question = question_name, sqp_metrics))
}

# Variables to pick from the sqp remote data
# and with which to create sqp tables
top_env <- new.env(parent = emptyenv())
top_env$sqp_columns <- c("quality", "reliability", "validity")

