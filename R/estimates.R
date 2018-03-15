library(magrittr)
library(tidyverse)

sqp_login("oriol.marti@gmail.com", "omarti0920")
find_studies('')

# add that you can search for many variables with
# a character vector

# test complete ids and empty ids

id <-
  find_questions(1, "tvtot") %>%
  filter(country_iso == "ES",
         language_iso == "spa") %$%
  id

id <- 1

url_id <- paste0(sqp_env$questions,
                 id,
                 sqp_env$q_estimates)

requested <- safe_GET(url_id)
get_content <- httr::content(requested, as = 'text')

json_data <- jsonlite::fromJSON(get_content, flatten = TRUE)$data[[1]]

valid <- !is.na(json_data$authorized)

# replace this question with the short name of the question when
# available!
if (!any(valid)) stop("No valid predictions for this question")

json_data <- json_data[valid, ]

# If two authorized predictions
# are added, always returns the first one
# in order
row_to_pick <-
  ifelse(any(json_data$authorized),
       which(json_data$authorized),
       1)

r_data <- tibble::as_tibble(json_data)[row_to_pick, sqp_env$estimate_variables]
final_df <- purrr::set_names(r_data, sqp_env$sqp_columns)

final_df <- structure(final_df, class = c(class(final_df), "sqp"))
final_df

