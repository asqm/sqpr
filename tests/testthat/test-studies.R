context("test-studies.R")

test_that("get_studies returns a correct data frame", {
  sqp_login()
  all_studies <- get_studies()
  expect_is(all_studies, "tbl_df")
  expect_equal(ncol(all_studies), length(sqp_env$study_variables))
})

test_that("find_studies returns correct output", {
  sqp_login()
  expect_identical(find_studies(""), get_studies())
  expect_error(find_studies(character()))
  expect_error(find_studies(1))
})
