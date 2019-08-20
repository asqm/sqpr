context("test-questions.R")

test_that("get_questions returns correct df", {
  sqp_login()
  correct_df <- get_questions("ESS round 5")

  expect_is(correct_df, "tbl_df")
  expect_length(correct_df, 5)
  expect_false(nrow(correct_df) == 0)
})

test_that("get_questions handles unexistent questions", {
  sqp_login()
  expect_error(get_questions(numeric()),
               "is.character(study) is not TRUE",
               fixed = TRUE)


  expect_error(get_questions("whatever"),
               "Study 'whatever' was not found. Check get_studies()")

  expect_error(get_questions(character()),
               regexp = "length(study) == 1 is not TRUE",
               fixed = TRUE)

  expect_error(get_questions("ESS round 2|ESS round 4"),
               "Multiple studies matched. Narrow down to one: ESS Round 2, ESS Round 4", #nolintr
               fixed = TRUE)

})

test_that("find_questions accepts regular expressions", {
  sqp_login()
  regexp <- find_questions("GLES Nachwahlstudie", "a$")

  expect_is(regexp, "tbl_df")
  expect_length(regexp, length(sqp_env$question_variables))
  expect_false(nrow(regexp) == 0)
})

test_that("find_questions handles empty strings", {
  sqp_login()
  regexp <- find_questions("GLES Nachwahlstudie", "")

  expect_is(regexp, "tbl_df")
  expect_length(regexp, length(sqp_env$question_variables))
  expect_false(nrow(regexp) == 0)
})

test_that("find_questions returns extra columns", {
  sqp_login()
  expect_gt(ncol(find_questions("GLES Nachwahlstudie", "", all_columns = TRUE)),
            length(sqp_env$question_variables))
})
