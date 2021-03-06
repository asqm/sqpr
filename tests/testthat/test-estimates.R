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

test_that("get_estimates returns unauthorized predictions", {
  sqp_login()
  set.seed(1231)
  sqp_data <- get_estimates(4, all_columns = TRUE, authorized = FALSE)

  expect_length(sqp_data, length(sqp_env$all_estimate_variables))
  expect_is(sqp_data, "sqp")
  expect_true(nrow(sqp_data) > 1)
  expect_true(all(sqp_env$all_estimate_variables %in% names(sqp_data)))
})


test_that("get_estimates fails when authorized is FALSE and all_columns TRUE", {
  sqp_login()
  set.seed(1231)

  expect_error(
    get_estimates(4, all_columns = FALSE, authorized = FALSE),
    regexp = "If authorized is set to `FALSE`, argument `all_columns` must be set to `TRUE` to identify which variables are authorized/non-authorized"
  )
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

test_that("get_estimates doesn't accept more than 100 id's", {
  expect_error(get_estimates(1:101), regexp= "The SQP API only accepts 100 requests per call and `id` has length greater than 100")  
})


test_that("get_question_name and get_estimates only accept numeric id's", {
  expect_error(get_question_name(character()), "is.numeric\\(id\\) is not TRUE")
  expect_error(get_estimates(character()), "is.numeric\\(id\\) is not TRUE")  
})

test_that("get_question_name and get_estimates return empty objects when id is of length 0", {

  short_cols <- sqp_construct("empty", list(quality = NA_real_))[0, ]
  long_cols <- sqp_construct("empty", list(quality = NA_real_), all_columns = TRUE)[0, ]
  
  expect_identical(get_estimates(id = numeric()), short_cols)
  expect_identical(get_estimates(id = numeric(), all_columns = TRUE), long_cols)
  expect_identical(get_question_name(numeric()), character())
  
})
