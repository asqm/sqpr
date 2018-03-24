#' Extract question information from specific studies
#'
#' Using the id of a particular study, extract information on a given
#' question.
#'
#' @param id_study a numeric value that specifies the desired study. Find with
#' \code{\link{find_studies}} or \code{\link{get_studies}}.
#' @param question_name A vector of strings specifying the variable names.
#' A search is performed for similar names from all variables in the study.
#' Upper or lower case is ignored and regular expressions are supported.
#' @param all_columns \code{TRUE} or \code{FALSE} stating whether to return all
#' columns. By default it returns the `id` of the question, the `study_id`, the
#' `short_name` of the question and the `country_iso` and `language_iso` of the
#' question. If set to \code{TRUE}, it returns additional columns with the
#' specific wording of the question, answer options, etc..
#'
#' @return a \code{\link[tibble]{tibble}}
#' @export
#'
#' @examples
#'\dontrun{
#' # Set login information here. See ?sqp_login
#' sqp_login()
#'
#' # 1 is ESS round 1
#' find_questions(1, "tvtot")
#'
#' # If ran again, even with a different variable, it loads
#' # the data from memory, making it very fast
#'
#' find_questions(1, "tvpol")
#'
#'
#' # If that doesn't fit, then use get_questions and search for the question
#' # manually through filtering:
#'
#' library(dplyr)
#' get_questions(1) %>%
#'    filter(country_iso == "ES",
#'           language_iso == "spa",
#'           grepl("Tot", .$short_name))
#'
#' # Can also search for several variables in one call
#'
#' find_questions(1, c("tvpol", "tvtot"))
#'
#' # Or use regular expressions. For example, to find all variables
#' # that begin with tv
#'
#' find_questions(1, "^tv")
#'}
#'
find_questions <- function(id_study, question_name, all_columns = FALSE) {
  stopifnot(!missing(id_study), !missing(question_name))

  questions_df <- get_questions(id_study, all_columns = all_columns)

  all_questions <- questions_df[[sqp_env$question_variables[3]]]

  # You can search for many variables
  question_name <- paste0(question_name, collapse = "|")

  sel_rows <- grepl(question_name, all_questions, ignore.case = TRUE)

  questions_df[sel_rows, ]
}

#' @rdname find_questions
get_questions <- function(id_study, all_columns = FALSE) {
  stopifnot(is.numeric(id_study), length(id_study) != 0)

  check_study(id_study)

  q_studies_path <- paste0(sqp_env$study, id_study, "/questions")

  final_df <- object_request(q_studies_path)

  if (all_columns) return(final_df)

  final_df[sqp_env$question_variables]
}
