#' Extract question information from specific studies
#'
#' Use find_question to search for information on a given question
#' in a study and get_questions to return all the question in a study.
#'
#' @param study a character specifying the \strong{name} of the desired study.
#' Find with \code{\link{find_studies}} or \code{\link{get_studies}}.
#' 
#' @param question_name A vector of strings specifying the variable names.
#' A search is performed for similar names from all variables in the study.
#' Upper or lower case is ignored and regular expressions are supported.
#' 
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
#' # 1 is the study ESS round 1
#' find_questions("ESS round 1", "tvtot")
#'
#' # If that doesn't fit, then use get_questions and search for the question
#' # manually through filtering:
#'
#' all_questions <- get_questions("ESS round 1")
#' cnt_filt <- all_questions$country_iso == "ES"
#' lang_filt <- all_questions$language_iso == "spa"
#' var_filt <- grepl("TvTot", all_questions$short_name)
#'
#' # The desired "TvTot" variable from Spain, in Spanish
#' all_questions[cnt_filt & lang_filt & var_filt, ]
#' 
#' # Can also search for several variables in one call
#'
#' find_questions("ESS round 1", c("tvpol", "tvtot"))
#'
#' # Or use regular expressions. For example, to find all variables
#' # that begin with tv
#'
#' find_questions(1, "^tv")
#'}
#'
find_questions <- function(study, question_name, all_columns = FALSE) {
  stopifnot(!missing(study),
            !missing(question_name),
            is.character(question_name),
            length(question_name) >= 1)

  questions_df <- get_questions(study, all_columns = all_columns)

  all_questions <- questions_df[[sqp_env$question_variables[3]]]

  # You can search for many variables
  question_name <- paste0(question_name, collapse = "|")

  sel_rows <- grepl(question_name, all_questions, ignore.case = TRUE)

  questions_df[sel_rows, ]
}

#' @rdname find_questions
#' @export
get_questions <- function(study, all_columns = FALSE) {
  stopifnot(is.character(study), length(study) == 1)

  all_studies <- get_studies()
  find_study <-
    grepl(paste0("^", study, "$"), all_studies$name, ignore.case = TRUE)

  id_study <- all_studies[find_study, "id", drop = TRUE]
  check_study(id_study, all_studies, study)
  q_studies_path <- paste0(sqp_env$study, id_study, "/questions")
  final_df <- object_request(q_studies_path)
  if (all_columns) return(final_df)
  final_df[sqp_env$question_variables]
}
