context("test-sqp_login.R")

test_that("login fails with random account", {
  expect_error(sqp_login("cimentadaj", "ho"),
               "^Failed to login with that username/password")
})

sqp_env$token <- NULL

test_that("login succeeds with environment variables", {
  expect_silent(sqp_login())
  expect_false(sqp_env$token == "")
})

sqp_env$token <- NULL

user <- Sys.getenv("SQP_USER")
pw <- Sys.getenv("SQP_PW")

test_that("login succeeds with variables as", {
  options(SQP_USER = user, SQP_PW = pw)
  expect_silent(sqp_login())
  expect_false(sqp_env$token == "")
})
