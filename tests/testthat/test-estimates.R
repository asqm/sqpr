context("test-estimates.R")

row_n <- 10

test_that("get_estimates returns correct output", {
  sqp_login()
  set.seed(1231)
  sqp_data <- get_estimates(sample(1:1000, row_n))

  expect_is(sqp_data, "sqp")
  expect_length(sqp_data, 4)
  expect_true(nrow(sqp_data) == row_n)
  expect_true(all(sqp_env$sqp_columns %in% names(sqp_data)))
})

test_that("get_estimates returns all columns correctly", {
  sqp_login()
  set.seed(1231)
  sqp_data <- get_estimates(sample(1:1000, row_n), all_columns = TRUE)

  expect_length(sqp_data, length(sqp_env$all_estimate_variables))
  expect_is(sqp_data, "sqp")
  expect_true(nrow(sqp_data) == row_n)
  expect_true(all(sqp_env$all_estimate_variables %in% names(sqp_data)))
})


# Here I chose one variable that was empty at the time.
# If this raises an error, then just find another empty variables
test_that("get_estimates converts empty estimates into missing tibble", {
  sqp_data <- get_estimates(86, all_columns = TRUE)

  expect_length(sqp_data, length(sqp_env$all_estimate_variables))
  expect_is(sqp_data, "sqp")
  expect_true(nrow(sqp_data) == 1)
  expect_true(all(sqp_env$all_estimate_variables %in% names(sqp_data)))

  # Right now two variables are being filled out. If that increases
  # then the number just has to increase
  expect_equivalent(sum(is.na(sqp_data)), length(sqp_env$all_estimate_variables) - 2)
})


test_that("get_question_name and get_estimates stop at length 0 numerics", {
  expect_error(get_question_name(numeric(0)),
               "length(.+) >= 1 is not TRUE")
  expect_error(get_estimates(numeric(0)),
               "length(.+) >= 1 is not TRUE")
})
