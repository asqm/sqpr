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

myenv <- new.env()
myenv$hostname <- "http://ec2-52-14-50-91.us-east-2.compute.amazonaws.com:8080"
myenv$auth <- "api/auth"


## Log in to SQP
# Check error when user puts wrong name/password

sqp_login <- function(user, pass) {
  resp <-
    safe_GET(
      url = myenv$hostname,
      config = authenticate(user, pass),
      path = myenv$auth
  )
  
  error_notjson(resp)
  content(resp, as = "parsed")
}

safe_GET <- function(url_handle, config, path) {
  stop_for_status(
    GET(url = url_handle,
        config = authenticate("hey", "ho"),
        path = path),
    "connect to the SQP API. Do you have internet connection?"
  )
}

error_notjson <- function(x) {
  if (http_type(x) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
}

token <- sqp_login("hey", "ho")$access_token


## Extract questions by study
myenv$study <- "/api/v1/studies"
myenv$questions <- "/api/v1/studies/1111111"

(url <-
    modify_url(myenv$hostname, path = myenv$study,
               query = list(user_id = 1708)))

p <-
  GET(url = url,
    # config = authenticate("hey", "ho"),
    # config = config(httpauth = 1, userpwd = "hey:pw"),
    add_headers('Authorization' = paste("Bearer", token)))

http_status(p)

myenv$user <- "/api/v1/users/6b99f7bb-4e7f-4c40-9a12-8472d7dff3c6"

