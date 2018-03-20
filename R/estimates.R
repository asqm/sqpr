# # Always picks the authorized version of an estimate
# # otherwise it picks the first unauthorized prediction.
#
# library(magrittr)
# library(tidyverse)
#
# sqp_login("cimentadaj@gmail.com", "Lolasouno2")
# find_studies('')
#
# # add that you can search for many variables with
# # a character vector
#
# # test complete ids and empty ids
# # Add tests for one and 2 or more ids
# # add tests for one empty and one complete id
#
# id <-
#   find_questions(1, "ppltrst") %>%
#   filter(country_iso == "ES",
#          language_iso == "spa") %$%
#   id
#
# id <- 1
# id <- 4
#
# get_estimates <- function(id, all_columns = FALSE) {
#   stopifnot(is.numeric(id))
#   id <- paste0(id, collapse = ",")
#
#   q_name <- grab_question_name(id)
#
#   url_id <- paste0(sqp_env$questions, id, sqp_env$q_estimates)
#   raw_data <- object_request(url_id, estimates = TRUE)
#
#   list_data <- Map(make_estimate_df, raw_data, q_name, all_columns = all_columns)
#
#   final_df <- tibble::as_tibble(do.call(rbind, list_data))
#
#   final_df <- structure(final_df, class = c(class(final_df), "sqp"))
#   final_df
# }
#
# grab_question_name <- function(id) {
#   almost_q_name <-
#     httr::content(
#       safe_GET(paste0(sqp_env$questions, id)), as = "text"
#     )
#
#   q_name <- tolower(jsonlite::fromJSON(almost_q_name)$short_name)
# }
#
# make_estimate_df <- function(raw_data, var_name, all_columns = FALSE) {
#   valid_rows <- !is.na(raw_data$authorized)
#
#   if (!any(valid_rows)) stop("No valid predictions for", " `", q_name,"`")
#
#   raw_data <- raw_data[valid_rows, ]
#
#   # If two authorized predictions
#   # are added, always returns the first one
#   # in order
#   row_to_pick <- ifelse(any(raw_data$authorized),
#                         which(raw_data$authorized), 1)
#
#   cols_to_pick <- if (all_columns) names(raw_data) else sqp_env$short_estimate_variables
#   final_df <- raw_data[row_to_pick, cols_to_pick]
#
#   final_df <- purrr::set_names(final_df, ~ gsub("prediction.", "", .x))
#
#   final_df <- tibble::add_column(final_df, question = var_name, .before = 1)
#   final_df
# }
#
# id <- find_studies("ess round")$id
# get_estimates(60131)
# sqp_construct()
# # Things to do:
# # Whenever a variable has no estimates, it currently
# # drops the field from the JSON. I asked Oriol
# # to provide a Null whenever there's no information.
# # When you have that information, include in
# # `make_estimate_df` a function that check if it's null
# # if it is, then create an empty data frame with all
# # variable in sqp_env$all_estimate variables set to NULL
# # except the id (fill it w/ id)
#
# # Check that oriol sets the maximum page to 1000 so that
# # you can search for studies interactively.
#
# # Document and test the get_estimates function
