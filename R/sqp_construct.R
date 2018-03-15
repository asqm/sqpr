#' Construct an SQP tibble manually
#'
#' \code{sqp_construct} is designed to to create SQP tibbles
#' by manually inserting new metrics such as quality or validity.
#'
#' @param question_name a character string that will be used as the question name
#' @param metrics a list containing new SQP metrics. Currently it only
#' supports quality, reliability and validity. Can also specify one of the metrics
#' and the remaining are set to NA by default
#'
#' @return a \code{\link[tibble]{tibble}} of one row with the supplied metrics. It also has
#' class \code{sqp} for further manipulations within the \code{sqpr} package.
#'
#' @details \code{sqp_construct_} is useful if you're interested in programming
#' with \code{sqpr} rather than using it interactively. If you want to use
#' \code{sqp_construct} inside a function, use the equivalent \code{sqp_construct_}
#' which uses standard evaluation.
#'
#' @export
#'
#' @examples
#'
#' sqp_construct(new_question, list(quality = 0.3))
#'
#' sqp_construct(new_question, list(quality = 0.3, validity = 0.2))
#'
#' # Note that specifying a column which is not availabe in SQP data
#' # will throw an error
#'
#' # sqp_construct(new_question, list(random_col = 0.3, validity = 0.2))
#' # Error: One or more of the specified metrics don't match the SQP column names
#'
#' # Currently only quality, reliability and validity are allowed.
#'
sqp_construct <- function(question_name, metrics) {
  question <- as.character(substitute(question_name))
  sqp_construct_(question, metrics)
}

#' @rdname sqp_construct
#' @export
sqp_construct_ <- function(question_name, metrics) {

  question <- question_name

  if (length(question) > 1) stop("`question_name` must have only one question",
                                 call. = FALSE)

  is_list <- is.list(metrics)
  named <- !is.null(names(metrics))
  numeric <- is.numeric(unlist(metrics))

  if (!named | !numeric | !is_list) {
    stop("`metrics` must be a named numeric list",
         call. = FALSE)
  }

  if (length(names(metrics)) != length(unlist(metrics))) {
    stop("`metrics` must contain only one element per name",
         call. = FALSE)
  }

  sqp_metrics <- columns_sqp(names(metrics), unlist(metrics))

  generic_sqp(question, sqp_metrics)
}

# Specify columns that should be in the SQP data and
# replacements
# returns a named list with the replacements
# added
columns_sqp <- function(columns_to_fill, replacement) {

  if (!all(columns_to_fill %in% sqp_env$sqp_columns)) {
    stop("One or more of the specified `metrics` don't match the SQP column names",
         call. = FALSE)
  }

  # sqp_columns is a global variable defining
  # the columns that SQP needs to have
  num_cols <- length(sqp_env$sqp_columns)
  empty_cols <- purrr::set_names(purrr::rerun(num_cols, NA_real_),
                                 sqp_env$sqp_columns)

  # iterate through each column/replacement and fill
  # out the empty list
  for (some_cols in seq_along(columns_to_fill)) {
    chosen_col <- columns_to_fill[some_cols]
    empty_cols[[chosen_col]] <- replacement[some_cols]
  }

  filled_cols <- empty_cols
  filled_cols
}

# Create a tibble with the question name and
# the sqp matrics. Returns the tibble
generic_sqp <- function(question_name, sqp_metrics) {
  stopifnot(!is.null(names(sqp_metrics)), is.list(sqp_metrics))

  sqp_data <- dplyr::as_tibble(c(question = question_name, sqp_metrics))
  structure(sqp_data, class = c(class(sqp_data), "sqp"))
}
