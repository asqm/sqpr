context("test-seq_correlate.R")

test_matrix_operations <- function(fun, fun_str) {
  test_that(paste0(fun_str, " returns correct df"), {
    # New diagonal
    new_diagonal <- rnorm(ncol(mtcars))
    corr_tibble <- fun(mtcars, new_diagonal)

    expect_is(corr_tibble, "tbl_df")
    expect_is(corr_tibble[[1]], "character")

    # If input is a matrix it returns the row/column
    # names
    ex_matrix <- matrix(rnorm(100, sd = 50), nrow = 20)
    corr_matrix <- fun(ex_matrix, rnorm(ncol(ex_matrix)))

    expect_is(corr_matrix[[1]], "character")

    # The rownames have at least 1 character in the names
    # This supposes that it's not the raw number but a
    # pre-processed column names
    expect_true(all(grepl("[[:alpha:]]{1,}", corr_matrix[[1]])))
  })

  test_that("sqp_correlate returns correct df", {
    expect_error(fun(mtcars, 2:4),
                 "`diag_adj` must be the same length as the number of columns in `x`")

    expect_error(fun(1:5, 2:3),
                 "supply both 'x' and 'y' or a matrix-like 'x'")

    expect_error(fun(mtcars, "wrong_diag"),
                 "`diag_adj` must be numeric")
  })
}

test_matrix_operations(sqp_correlate, "sqp_correlate")
test_matrix_operations(sqp_covariance, "sqp_covariance")
