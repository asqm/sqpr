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

library(httr)

## Log in to SQP
# Check error when user puts wrong name/password

sqp_login("hey", "ho")

sqp_GET <- function(path, ...) {
  res <-
    httr::GET(url = sqp_env$hostname,
              path = path,
              httr::add_headers('Authorization' = paste("Bearer",
                                                                 sqp_env$token)),
              ...)
  stop_for_status(res)
  res
}


## Extract questions by study
myenv$study <- "/api/v1/studies/1111111"

sqp_get_studies <- function(query = NULL) {
  res <- sqp_GET(sqp_env$study, query = list(user_id = 1708,
                                             query))
  tibble::as_tibble(jsonlite::fromJSON(content(res, as = "text"))$data)
}

sqp_GET(sqp_env$study, query = list(user_id = 1708))
sqp_GET(sqp_env$questions)
sqp_GET(sqp_env$ques_props)
