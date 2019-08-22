## test_that("multiplication works", {

check_df <- function(sqp_data, nc = 4, nr = 1) {
  expect_is(sqp_data, "sqp")
  expect_length(sqp_data, nc)
  expect_true(nrow(sqp_data) == nr)
  expect_true(all(sqp_env$sqp_columns %in% names(sqp_data)))
}

test_that("get_sqp returns correct output", {
  sqp_login()
  set.seed(1231)

  sqp_data <-
    get_sqp(
      "ESS round 4",
      "tvtot",
      "es",
      "spa"
    )

  check_df(sqp_data)
})

test_that("get_sqp returns all columns correctly", {
  sqp_login()
  set.seed(1231)

  sqp_data <-
    get_sqp(
      "ESS round 4",
      "tvtot",
      "es",
      "spa",
      all_columns = TRUE
    )

  check_df(sqp_data, nc = 23)
})

test_that("get_sqp returns empty df when wrong question", {
  sqp_login()
  set.seed(1231)

  sqp_data <-
    get_sqp(
      "ESS round 4",
      "random",
      "es",
      "spa"
    )

  check_df(sqp_data, nr = 0)

  sqp_data <-
    get_sqp(
      "ESS round 4",
      "random",
      "es",
      "spa",
      all_columns = TRUE
    )

  check_df(sqp_data, nc = 23, nr = 0)
})

test_that("get_sqp throws error with wrong study", {
  sqp_login()

  expect_error(
    get_sqp(
      c("random_study"),
      "tvtot",
      "es",
      "spa"
    ),
    "Study 'random_study' was not found. Check get_studies()",
    fixed = TRUE
  )

  # For multiple studies
  expect_error(
    get_sqp(
      c("ESS round 4", "ESS round 2"),
      "tvtot",
      "es",
      "spa"
    ),
    "length(study) == 1 is not TRUE",
    fixed = TRUE
  )

  expect_error(
    get_sqp(
      character(),
      "tvtot",
      "es",
      "spa"
    ),
    "length(study) == 1 is not TRUE",
    fixed = TRUE
  )

})

test_that("get_sqp throws error with wrong question", {
  sqp_login()

  expect_error(
    get_sqp(
      "ESS Round 4",
      numeric(),
      "es",
      "spa"
    ),
    "is.character(question_name) is not TRUE",
    fixed = TRUE
  )

expect_error(
    get_sqp(
      "ESS Round 4",
      character(),
      "es",
      "spa"
    ),
    "length(question_name) >= 1 is not TRUE",
    fixed = TRUE
  )

})


test_that("get_sqp throws error with wrong country/lang", {
  sqp_login()

  expect_error(
    get_sqp(
      "ESS Round 4",
      "tvtot",
      numeric(),
      "spa"
    ),
    "is.character(country) is not TRUE",
    fixed = TRUE
  )

  expect_error(
    get_sqp(
      "ESS Round 4",
      "tvtot",
      c("spa", "ger"),
      "spa"
    ),
    "length(country) == 1 is not TRUE",
    fixed = TRUE
  )

  expect_error(
    get_sqp(
      "ESS Round 4",
      "tvtot",
      "es",
      numeric()
    ),
    "is.character(lang) is not TRUE",
    fixed = TRUE
  )

  
  expect_error(
    get_sqp(
      "ESS Round 4",
      "tvtot",
      "es",
      c("spa", "ita")
    ),
    "length(lang) == 1 is not TRUE",
    fixed = TRUE
  )

})
