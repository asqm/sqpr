# Binds a df to a fully constructed sqp data with
# new values

#' Title
#'
#' @param df
#' @param question
#' @param metrics
#'
#' @return
#' @export
#'
#' @examples
bind_metrics <- function(df, question, metrics) {
  stopifnot(is.data.frame(df))

  question_name <- as.character(substitute(question))

  binded_sqp <- dplyr::bind_rows(df, construct_sqp_(question_name, metrics))
  binded_sqp
}
