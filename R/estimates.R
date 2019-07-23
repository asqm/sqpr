#' Extract variable estimates from the SQP 3.0
#' prediction algorithm
#'
#' @param id a numeric vector containing the id(s) of variable(s) of interest. Can
#' be one or more id's.
#' @param all_columns a logical stating whether to extract all available
#' columns from the SQP 3.0 database. See the details section for a list of all
#' possible variables.
#' @param authorized \code{TRUE} to return \strong{only} the authorized prediction or
#' \code{FALSE} to return all available predictions. If set to \code{FALSE} a
#' a warning is issued reminding the user to pick one prediction for each variable
#' based on the \code{user_id} and \code{user_username} columns.
#'
#' @details
#' SQP predictions can be both 'authorized' predictions, which are
#' performed by the SQP 3.0 software, and 'crowd-sourced' predictions which are
#' added to the database by other users. By default, \code{get_estimates}
#' always returns the 'authorized' prediction when it is available. When
#' it is not, it returns the first non-authorized prediction, and so on.
#' If the user wants to choose a specific prediction, then
#' \code{authorized = FALSE} will return all available predictions for each question.
#' 
#' \code{get_estimates} returns a four column \code{\link[tibble]{tibble}} with
#' the question name and the estimates for \code{quality}, \code{reliability} and
#' \code{validity}. However, if \code{all_columns} is set to \code{TRUE} the returned
#' \code{\link[tibble]{tibble}} contains new columns. Below you can find the description
#' of all columns:
#'
#' \itemize{
#' \item question: the literal name of the question in the questionnaire of the study
#' \item question_id: the API internal ID of the question
#' \item id: this is the coding ID, that is, the coding of the authorized prediction
#' \item created: Date of the API request
#' \item routing_id: Version of the coding scheme applied to get that prediction.
#' \item authorized: Whether it is an 'authorized' prediction or not. See the details section
#' \item complete: Whether all fields of the coding are complete
#' \item user_id: The id of the user that crowd-sourced the prediction
#' \item user_username: The account name of the user that crowd-sourced the prediction
#' \item error: Whether there was an error in making the prediction. For an example,
#'  see \url{http://sqp.upf.edu/loadui/#questionPrediction/12552/42383}
#' \item errorMessage: The error message, if there was an error
#' \item reliability: The strenght between the true score factor and the observed
#'  variable or 1 - proportion random error in the observed variance. Computed as
#'  the squared of the reliability coefficient
#' \item validity: The strength between the latent concept factor and the
#'  true score factor or 1 - proportion method error variance in the true
#'  score variance. Computed as the square of the validity coefficient
#' \item quality: The strength between the latent concept factor and the
#'  observed variable or 1 - proportion of random and method error variance
#'  in the latent concept's variance. Computed as the product of reliability
#'   and validity.
#' \item reliabilityCoefficient: The effect between the true score factor and
#'  the observed variable
#' \item validityCoefficient: The effect between the latent concept factor and
#'  the true score factor
#' \item methodEffectCoefficient: The effect between the method factor and the
#'  true score factor
#' \item qualityCoefficient: It is computed as the square root of the quality
#' \item reliabilityCoefficientInterquartileRange: Interquartile range for the reliability coefficient
#' \item validityCoefficientInterquartileRange: Interquartile range for the validity coefficient
#' \item qualityCoefficientInterquartileRange: Interquartile range for the quality coefficient
#' \item reliabilityCoefficientStdError: Predicted standard error of the reliability coefficient
#' \item validityCoefficientStdError: Predicted standard error of the validity coefficient
#' \item qualityCoefficientStdError: Predicted standard error of the quality coefficient
#' }
#'
#'
#' @seealso \code{\link{sqp_login}} for logging in to the SQP 3.0 API through R and
#' \code{\link{find_questions}} and \code{\link{find_studies}} for locating
#' the variables of interest to use in \code{\link{get_estimates}}.
#'
#' @return \code{\link{get_estimates}} returns a \code{\link[tibble]{tibble}} with the predictions.
#' If \code{id} is of length 0, \code{\link{get_estimates}} returns an empty data frame.
#' In both situations, the number of columns depends on the \code{all_columns} argument.
#' \code{\link{get_question_name}} returns a character vector with the question name(s) unless
#' id is of length 0 where it returns an empty character.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Log in with sqp_login first. See ?sqp_login
#' sqp_login()
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
get_estimates <- function(id, all_columns = FALSE, authorized = TRUE) {
  stopifnot(is.numeric(id))

  if (length(id) < 1) {
    empty_df <-
      sqp_construct("empty", list(quality = NA_real_),
                    all_columns = all_columns)[0, ]
    return(empty_df)
  }
    
  if (length(id) > 100) {
    stop("The SQP API only accepts 100 requests per call and `id` has length greater than 100")
  }

  collapsed_id <- paste0(id, collapse = ",")
  url_id <- paste0(sqp_env$questions, collapsed_id, sqp_env$q_estimates)

  q_name <- get_question_name(id)
  raw_data <- object_request(url_id, estimates = TRUE)

  list_data <- Map(
    function(x, y, z) make_estimate_df(x, y, z, all_columns = all_columns, authorized = authorized),
    raw_data,
    q_name,
    id
  )

  if (!authorized) message("Authorized was set to FALSE, remember to pick only one single prediction for all questions")

  final_df <- tibble::as_tibble(do.call(rbind, list_data))

  final_df <- sqp_reconstruct(final_df)
  final_df
}

#' @rdname get_estimates
#' @export
get_question_name <- function(id) {
  stopifnot(is.numeric(id))

  if (length(id) < 1) return(character())

  collapsed_id <- paste0(id, collapse = ",")
  almost_q_name <-
    httr::content(
      safe_GET(paste0(sqp_env$questions, collapsed_id)), as = "text"
    )

  q_name <- tolower(jsonlite::fromJSON(almost_q_name)$short_name)
  q_name
}

make_estimate_df <- function(raw_data, var_name, id, all_columns = FALSE, authorized = TRUE) {

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

  if (!any(valid_rows)) stop("No valid predictions for", " `", var_name,"`")

  raw_data <- raw_data[valid_rows, ]

  # If two authorized predictions
  # are added, always returns the first one
  # in order

  if (authorized) {
    row_to_pick <- ifelse(any(raw_data$authorized),
                          which(raw_data$authorized), 1)
  } else {
    row_to_pick <- seq_len(nrow(raw_data))
  }

  if (all_columns) {
    # To preserve consistency I place the sqp vars in front
    # and grab the unique columns to delete the same variables
    # that are in the middle.
    cols_to_pick <-
      unique(c(sqp_env$short_estimate_variables, names(raw_data)))
  } else {
    cols_to_pick <- sqp_env$short_estimate_variables
  }

  final_df <- raw_data[row_to_pick, cols_to_pick]

  names(final_df) <- gsub("prediction.", "", names(final_df))

  final_df <- tibble::add_column(final_df, question = var_name, .before = 1)
  final_df
}
