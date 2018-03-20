#' Manually bind new rows to an SQP data frame
#'
#' @param sqp_data a \code{data frame} or \code{tibble} given by \code{\link{get_estimates}}
#' @param question a character string that will be used as the question name
#' @param metrics a list containing new SQP metrics. Currently it only
#' supports 'quality', 'reliability' and 'validity'. Can also specify one of the metrics
#' and the remaining are set to NA by default
#'
#' @return \code{sqp_data} with a new row that contains the new question name
#' and the metrics as columns. Metrics must match existing names from SQP data
#' such as 'quality', 'reliability' and 'validity'.
#'
#' @export
#'
#' @seealso \code{\link{sqp_construct}} for the underlying workhorse.
#'
#' @examples
#'
#' library(tibble)
#' sqp_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(NA, NA, 0.6, 0.7, 0.8))
#'
#' sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))
#'
#' sqp_bind_metrics(sqp_df, new_question, list(quality = 0.7))
#'
#' sqp_bind_metrics(sqp_df, new_question, list(quality = 0.7, reliability = 0.2))
#'
#' # Specifying a wrong metric name results in error
#'
#' # sqp_bind_metrics(sqp_df, new_question, list(wrong_metric = 0.7))
#' # Error: One or more of the specified `metrics` don't match the SQP column names
#'
#' # Currently only quality, reliability and validity are allowed.
#'
sqp_bind_metrics <- function(sqp_data, question, metrics) {

  sqp_reconstruct(sqp_data)

  question_name <- as.character(substitute(question))

  binded_sqp <- dplyr::bind_rows(sqp_data, sqp_construct_(question_name, metrics))
  binded_sqp
}
