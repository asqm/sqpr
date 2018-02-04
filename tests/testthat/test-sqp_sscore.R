context("test-sqp_sscore.R")

set.seed(231321)
library(tibble)
sqp_df <-
  tibble(question = paste0("V", 1:5),
         quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
         reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
         validity = c(NA, NA, 0.6, 0.7, 0.8))


sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))

sample_data <-
  as_tibble(
    setNames(
      replicate(5, c(rbinom(1000, 5, 0.6), NA), simplify = FALSE),
      paste0("V", 1:5))
  )

test_that("sqp_sscore returns correct output", {
  result <-
    sqp_sscore(
      sqp_data = sqp_df,
      df = sample_data,
      new_name = new_sumscore,
      V3, V4
    )

  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sqp")

  expect_equal(nrow(result), 4)
  expect_equal(nrow(result), ncol(result))
  expect_is(result[[1]], "character")

  # Same result:
  expect_equal(round(result[4, 2, drop = TRUE], 3), 0.563)
})

sqp_df <-
  tibble(question = paste0("V", 1:5),
         quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
         reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
         validity = c(NA, NA, 0.6, 0.7, 0.8),
         random_var = NA_real_)


sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))

sample_data <-
  as_tibble(
    setNames(
      replicate(6, c(rbinom(1000, 5, 0.6), NA), simplify = FALSE),
      paste0("V", 1:6))
  )

test_that("sqp_sscore checks for arguments", {

  expect_error(
    sqp_sscore(
      sqp_data = mtcars,
      df = sample_data,
      new_name = new_sumscore,
      V1, V2),
    "`sqp_data` must be collected using sqp_collect()"
  )

  expect_error(
    sqp_sscore(
      sqp_data = sqp_df,
      df = sample_data,
      new_name = new_sumscore),
    "`df` must have at least two columns"
  )

  tmp <- sample_data
  tmp$V5 <- as.character(tmp$V5)

  expect_error(
    sqp_sscore(
      sqp_data = sqp_df,
      df = tmp,
      new_name = new_sumscore,
      V1, V5),
    "V1, V5 must be numeric variables in `df`"
  )

  tmp_sqp <- sqp_df
  tmp_sqp$quality <- as.character(tmp_sqp$quality)

  expect_error(
    sqp_sscore(
      sqp_data = tmp_sqp,
      df = sample_data,
      new_name = new_sumscore,
      V1, V5),
    "`sqp_data` must be collected using sqp_collect()"
  )
})

test_that("sqp_sscore checks variables are in both dfs", {
  expect_error(
    sqp_sscore(
      sqp_data = sqp_df,
      df = sample_data,
      new_name = new_sumscore,
      V1, random_var),
    "One or more variables are not present in `df`: random_var"
  )

  expect_error(
    sqp_sscore(
      sqp_data = sqp_df,
      df = sample_data,
      new_name = new_sumscore,
      V1, V6),
    "One or more variables are not present in `sqp_data`: V6"
  )
})
