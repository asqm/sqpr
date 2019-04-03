context("test-sqp_login.R")

test_that("login fails with random account", {
  expect_error(sqp_login("hey", "ho"),
               "^Something might be wrong with the SQP API or your username/password are not valid")
})

sqp_env$token <- NULL

test_that("login succeeds with environment variables", {
  expect_silent(sqp_login())
  expect_false(sqp_env$token == "")
})

sqp_env$token <- NULL

test_that("login succeeds with variables as", {
  user <- Sys.getenv("SQP_USER")
  pw <- Sys.getenv("SQP_PW")

  Sys.unsetenv("SQP_USER")
  Sys.unsetenv("SQP_PW")

  expect_silent(sqp_login(user, pw))
  expect_false(sqp_env$token == "")

  Sys.setenv("SQP_USER" = user)
  Sys.setenv("SQP_PW" = pw)
})
