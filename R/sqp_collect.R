#' Collect data from the Survey Quality Prediction database.
#'
#' @param ... variable names to collect from the SQP database
#'
#' @return a tibble with the selected variables as rows and SQP metrics
#' as columns
#'
#' @export
#'
#' @examples
#'
#' 5 + 5
#'
sqp_collect <- function(...) {
  # selected_vars <- as.character(substitute(list(...))[-1])
  #
  # sqp_data <-
  #   suppressWarnings(
  #     suppressMessages(
  #       readr::read_csv2("SQPexport_20171230_2255.csv")
  #     ))
  #
  #   # sqp_data_fin <-
  #   #   dplyr::transmute(
  #   #   sqp_data,
  #   #   question = tolower(`Question name`),
  #   #   quality = as.numeric(`Quality (q2)`),
  #   #   reliability = as.numeric(`Reliability (r2)`),
  #   #   validity = as.numeric(`Validity (v2)`)
  #   # )
  #
  # sqp_data_fin
}
