# add that you can search for many variables with
# a character vector

# test complete ids and empty ids
# Add tests for one and 2 or more ids
# add tests for one empty and one complete id

#' Extract variable estimates from the SQP prediction algorithm
#'
#' @param id a numeric vector containing the id(s) of variable(s) of interest. Can
#' be one or more id's.
#' @param all_columns a logical stating whether to extract all available
#' columns from the SQP database. See the details section for a list of all possible variables.
#'
#' @details SQP predictions can have both 'authorized' predictions, which are
#' performed by the SQP software and 'crowd-sourced' predictions which are
#' added to the database by other users. By default, \code{get_estimates}
#' always returns the 'authorized' prediction when it is available. When
#' it is not, it returns the first non-authorized prediction, and so on.
#' If neither 'authorized' nor 'crowd-sourced' predictions are available it returns
#' an error.
#'
#' \code{get_estimates} returns a four column \code{\link[tibble]{tibble}} with
#' the question name and the estimates for \code{quality}, \code{reliability and
#' \code{validity}. However, if \code{all_columns} is set to \code{TRUE} the returned
#' \code{\link[tibble]{tibble}} contains new columns. Below you can find the description
#' of all columns:
#'
#'
#' @return \code{get_estimates} returns \code{\link[tibble]{tibble}} with the predictions.
#' The number of columns depends on the \code{all_columns} argument.
#' \code{get_question_name} returns a character vector with the question name(s).
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Log in with sqp_login first. See ?sqp_login
#'
#' get_estimates(c(1, 2, 86))
#'
#' get_estimates(c(1, 2, 86), all_columns = TRUE)
#'
#' # Explore variable names
#'
#' get_question_name(1)
#'
#' get_question_name(1:10)
#'
#' }
#'
get_estimates <- function(id, all_columns = FALSE) {
  stopifnot(is.numeric(id), length(id) >= 1)
  collapsed_id <- paste0(id, collapse = ",")

  q_name <- get_question_name(collapsed_id)

  url_id <- paste0(sqp_env$questions, collapsed_id, sqp_env$q_estimates)
  raw_data <- object_request(url_id, estimates = TRUE)

  list_data <- purrr::pmap(list(raw_data, q_name, id),
                           make_estimate_df,
                           all_columns = all_columns)

  final_df <- tibble::as_tibble(do.call(rbind, list_data))

  final_df <- structure(final_df, class = c(class(final_df), "sqp"))
  final_df
}

#' @rdname get_estimates
#' @export
get_question_name <- function(id) {
  stopifnot(is.numeric(id), length(id) >= 1)
  almost_q_name <-
    httr::content(
      safe_GET(paste0(sqp_env$questions, id)), as = "text"
    )

  q_name <- tolower(jsonlite::fromJSON(almost_q_name)$short_name)
}

make_estimate_df <- function(raw_data, var_name, id, all_columns = FALSE) {

  # If empty estimates..
  if (all(c(1, 1) == dim(raw_data))) {
    sqp_data <-
      sqp_construct_(var_name,
                     metrics = list(quality = NA_integer_), # random metric
                     all_columns)
    # only for all columns, bc otherwise
    # you the 4 column layout of sqp of
    # short columns is lost
    if (all_columns) sqp_data$question_id <- id

    return(sqp_data)
  }

  valid_rows <- !is.na(raw_data$authorized)

  if (!any(valid_rows)) stop("No valid predictions for", " `", q_name,"`")

  raw_data <- raw_data[valid_rows, ]

  # If two authorized predictions
  # are added, always returns the first one
  # in order
  row_to_pick <- ifelse(any(raw_data$authorized),
                        which(raw_data$authorized), 1)

  cols_to_pick <- if (all_columns) names(raw_data) else sqp_env$short_estimate_variables
  final_df <- raw_data[row_to_pick, cols_to_pick]

  final_df <- purrr::set_names(final_df, ~ gsub("prediction.", "", .x))

  final_df <- tibble::add_column(final_df, question = var_name, .before = 1)
  final_df
}
