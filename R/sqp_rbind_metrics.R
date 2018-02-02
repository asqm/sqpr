# Binds a df to a fully constructed sqp data with
# new values

#' Title
#'
#' @param df
#' @param question
#' @param metrics must be a named list containing only one numeric element per name.
#'
#' @return
#' @export
#'
#' @examples
sqp_bind_metrics <- function(df, question, metrics) {
  stopifnot(is.data.frame(df))

  question_name <- as.character(substitute(question))

  binded_sqp <- dplyr::bind_rows(df, construct_sqp_(question_name, metrics))
  binded_sqp
}
