context("test-sqp_construct.R")

test_that("sqp_construct gives correct output", {

  new_sqp <-
    construct_sqp(new_question,
                  list(quality = 0.3, validity = 0.2))

  expect_equal(nrow(new_sqp), 1)
  expect_equal(ncol(new_sqp), 4)
  expect_equal(sum(is.na(new_sqp)), 1)

  expect_s3_class(new_sqp, "data.frame")
  expect_s3_class(new_sqp, "sqp")

  expect_true(all(vapply(new_sqp[-1], is.numeric, FUN.VALUE = logical(1))))
})


test_that("sqp_construct handles arguments in correct format", {

  expect_error(
    construct_sqp(
      c("one_question", "twoquestions"),
      list(quality = 0.3, validity = 0.2)),
      "`question_name` must have only one question"
               )

  expect_error(
    construct_sqp(one_question, list(quality = 0.3, hey = 0.2)),
    "One or more of the specified `metrics` don't match the SQP column names"
    )

  expect_error(
    construct_sqp(one_question, c(hey = 1)),
    "`metrics` must be a named numeric list"
  )

  expect_error(
    construct_sqp(one_question, list(hey = c(1, 2))),
    "`metrics` must contain only one element per name"
  )
})
