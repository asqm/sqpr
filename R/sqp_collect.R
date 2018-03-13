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
#
# ## Log in to SQP
# # Check error when user puts wrong name/password
#
# sqp_login("hey", "ho")

auth_GET <- function(path, ...) {
  check_login()

  auth <- httr::add_headers('Authorization' = paste("Bearer", sqp_env$token))

  res <-
    httr::GET(url = sqp_env$hostname,
              path = path,
              config = auth,
              ...)
  res
}

safe_GET <- function(path, ...) {
  res <- sqp_GET(path, ...)
  catch_error(res)
  res
}

object_request <- function(path) {
  requested <- safe_GET(path)
  get_content <- httr::content(requested, as = 'text')
  final_df <- tibble::as_tibble(jsonlite::fromJSON(get_content)$data)
  final_df
}

# Get all studies
get_studies <- function() {
  final_df <- object_request(sqp_env$study)
  final_df[sqp_env$study_variables]
}

# Find studies by name
find_studies <- function(question_name) {
  studies <- get_studies()
  studies_names <- studies[[sqp_env$study_variables[2]]]
  sel_rows <- grepl(question_name, studies_names, ignore.case = TRUE)
  studies[sel_rows, ]
}

# get questions by study
get_questions <- function(id_study) {
  check_study(id_study)
  q_studies_path <- paste0(sqp_env$study, "/", id_study, sqp_env$questions)
  final_df <- object_request(q_studies_path)
  final_df[sqp_env$question_variables]
}

# find questions by name in study
find_questions <- function(id_study, question_name) {
  questions_df <- get_questions(id_study)
  all_questions <- questions_df[[sqp_env$question_variables[3]]]
  sel_rows <- grepl(question_name, all_questions, ignore.case = TRUE)
  questions_df[sel_rows, ]
}

sqp_login("hey", "ho")
sqp_login("oriol.marti@gmail.com", "omarti0920")

get_studies()
id_study <- find_studies('Australia')$id[2]

ess <- get_questions(1)
ess <- find_questions(1, "tvtot")
