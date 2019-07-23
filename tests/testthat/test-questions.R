context("test-questions.R")

test_that("get_questions returns correct df", {
  sqp_login()
  correct_df <- get_questions(12)

  expect_is(correct_df, "tbl_df")
  expect_length(correct_df, 5)
  expect_false(nrow(correct_df) == 0)
})

test_that("get_questions handles unexistent questions", {
  sqp_login()
  expect_error(get_questions(numeric()),
               "length\\(.*\\) != 0 is not TRUE")


  expect_error(get_questions(9^9e00),
               "Study [0-9]+ was not found. Check get_studies()")

  expect_error(get_questions("ess"),
               regexp = ".* is not TRUE")
})

test_that("find_questions accepts regular expressions", {
  sqp_login()
  regexp <- find_questions(12, "a$")

  expect_is(regexp, "tbl_df")
  expect_length(regexp, length(sqp_env$question_variables))
  expect_false(nrow(regexp) == 0)
})

test_that("find_questions handles empty strings", {
  sqp_login()
  regexp <- find_questions(12, "")

  expect_is(regexp, "tbl_df")
  expect_length(regexp, length(sqp_env$question_variables))
  expect_false(nrow(regexp) == 0)
})

test_that("find_questions returns extra columns", {
  sqp_login()
  expect_gt(ncol(find_questions(12, "", all_columns = TRUE)),
            length(sqp_env$question_variables))
})
