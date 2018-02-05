context("test-sqp_cmv.R")


set.seed(2131)
suppressWarnings(library(tibble))

corr_tibble <-
  sqp_correlate(matrix(rnorm(100, sd = 50), nrow = 20),
                rnorm(5))

# Note to Jorge: Change this to a sqp df when sqp_collect() works.

# test missing:
# When y is not from sqp_collect(), sqp_cmv must throw an error
# Show that when y is not from sqp class, there's an error

sqp_df <-
 tibble(question = paste0("V", 1:5),
 quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
 reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
 validity = c(NA, NA, 0.6, 0.7, 0.8))

sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))


test_that("sqp_cmv returns correct output", {
  cmv_tib <- sqp_cmv(corr_tibble, sqp_df, V4, V5)
  expect_is(cmv_tib, "data.frame")

  # First column is the row names
  expect_is(cmv_tib[[1]], "character")

  # All other columns are numeric
  expect_true(all(vapply(cmv_tib[-1], is.numeric, FUN.VALUE = logical(1))))

  # All row names have a at least one letter, which means
  # that the row names were not extracted raw if `x`
  # was a matrix
  expect_true(all(grepl("[[:alpha:]]{1,}", cmv_tib[[1]])))

  # df is symmetric when excluding the rowname variables
  expect_equal(nrow(cmv_tib), ncol(cmv_tib) - 1)

  # Also handles character strings as variables
  expect_identical(cmv_tib,
                   sqp_cmv(corr_tibble, sqp_df, "V4", "V5"))
})

test_that("sqp_cmv throws specific errors", {
  expect_error(sqp_cmv(corr_tibble, sqp_df),
               "You need to supply at least two variables to calculate the common method variance")

  expect_error(sqp_cmv(corr_tibble, sqp_df, V2),
               "You need to supply at least two variables to calculate the common method variance")

  expect_error(sqp_cmv(corr_tibble, sqp_df, V2, V3),
               "`sqp_data` must have non-missing values at variable/s: V2, V3")

  expect_error(sqp_cmv(corr_tibble, sqp_df, hey, other),
               "At least one variable not present in `x`: hey, other")
})

test_that("sqp_sscore adds sqp class to valid sqp_data", {
  tmp <- sqp_df
  class(tmp) <- c("tbl_df", "tbl", "data.frame")

  noclass <- sqp_cmv(
    corr_tibble,
    sqp_data = tmp,
    V4, V5
  )

  valid_class <- sqp_cmv(
    corr_tibble,
    sqp_data = sqp_df,
    V4, V5
  )
  expect_identical(valid_class, noclass)
})

