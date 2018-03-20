context("test-sqp_reconstruct.R")


test_that("sqp_reconstruct checks data format", {
  variables <- paste0(sqp_env$sqp_columns, collapse = ", ")

  sqp_df <-
    tibble(question = paste0("V", 1:3),
           not_indf = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 0.6))

  expect_error(
    sqp_reconstruct(sqp_df),
    paste0("Variables ",  variables," must be available in `sqp_data`")
  )

  sqp_df <-
    tibble(question = 1:3,
           quality = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 1.2))

  expect_error(
    sqp_reconstruct(sqp_df),
    paste0(variables,
           " must be numeric columns with values between/including 0 and 1 in `sqp_data`"
           )
  )

  sqp_df <-
    tibble(question = 1:3,
           quality = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 0.2))

  expect_error(
    sqp_reconstruct(sqp_df),
    "First column in `sqp_data` must contain the question names as strings"
  )
})

test_that("sqp_reconstruct assigns sqp class if everything is fine", {
  sqp_df <-
    tibble(question = paste0("V", 1:3),
           quality = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 0.6))

  expect_s3_class(sqp_reconstruct(sqp_df), "sqp")

})
