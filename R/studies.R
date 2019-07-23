#' Extract available studies from the SQP 3.0 API
#'
#' \code{find_studies} allows you to search for studies by names while
#' \code{get_studies} will return all available studies in the SQP 3.0 database.
#'
#' @param study a string with the name of the study. Upper and lower cases are
#' ignored and regular expressions are supported.
#'
#' @details The user should almost always start by using \code{find_studies}
#' which searches for a study based on it's name. If nothing useful comes up
#' then use \code{get_studies} which will return all available studies.
#'
#' The returned \code{\link[tibble]{tibble}} will contain
#' the id and name of the study/studies. The user should take note of
#' the id of the desired study to checkout which questions are available
#' using \code{\link{get_questions}} and \code{\link{find_questions}}.
#'
#' @return A two column \code{\link[tibble]{tibble}} with the id and name
#' of the study.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Set your login information here. See ?sqp_login
#' sqp_login()
#'
#' find_studies("ess")
#' find_studies("australia")
#'
#' # or get_studies() for all studies
#'
#' get_studies()
#'
#'}
find_studies <- function(study) {
  stopifnot(is.character(study))
  studies <- get_studies()
  studies_names <- studies[[sqp_env$study_variables[2]]]
  sel_rows <- grepl(study, studies_names, ignore.case = TRUE)
  studies[sel_rows, ]
}

#' @rdname find_studies
#' @export
get_studies <- function() {
  final_df <- object_request(sqp_env$study)
  final_df[sqp_env$study_variables]
}
