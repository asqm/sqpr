context("test-seq_correlate.R")


test_that("sqp_correlate returns correct df", {
  # New diagonal
  new_diagonal <- rnorm(ncol(mtcars))
  corr_tibble <- sqp_correlate(mtcars, new_diagonal)

  expect_is(corr_tibble, "tbl_df")
  expect_is(corr_tibble[[1]], "character")

  # If input is a matrix it returns the row/column
  # names
  ex_matrix <- matrix(rnorm(100, sd = 50), nrow = 20)
  corr_matrix <- sqp_correlate(ex_matrix, rnorm(ncol(ex_matrix)))

  expect_is(corr_matrix[[1]], "character")

  # The rownames have at least 1 character in the names
  # This supposes that it's not the raw number but a
  # pre-processed column names
  expect_true(all(grepl("[[:alpha:]]{1,}", corr_matrix[[1]])))
})

test_that("sqp_correlate returns correct df", {
  expect_error(sqp_correlate(mtcars, 2:3),
               "replacement diagonal has wrong length")

  expect_error(sqp_correlate(1:5, 2:3),
               "supply both 'x' and 'y' or a matrix-like 'x'")

  expect_error(sqp_correlate(mtcars, "wrong_diag"),
               "`diag_adj` must be numeric")
})
