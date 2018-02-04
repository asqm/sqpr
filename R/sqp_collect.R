#' Collect data from the Survey Quality Prediction database.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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


# Variables to pick from the sqp remote data
# and with which to create sqp tables
top_env <- new.env(parent = emptyenv())
top_env$sqp_columns <- c("quality", "reliability", "validity")
