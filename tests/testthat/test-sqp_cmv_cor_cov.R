context("test-sqp_cmv_cor.R")

set.seed(2131)
suppressWarnings(library(tibble))

original_df <- as.data.frame(matrix(rnorm(100, sd = 50), nrow = 20))
corr_tibble <- sqp_correlate(original_df, rnorm(5))

# test missing:
# When y is not from sqp_collect(), sqp_cmv_cor must throw an error
# Show that when y is not from sqp class, there's an error

sqp_df <-
  tibble(question = paste0("V", 1:5),
         quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
         reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
         validity = c(NA, NA, 0.6, 0.7, 0.8)
         )

sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))

correct_format <- function(p) {
  expect_is(p, "data.frame")

  expect_equal(names(p)[[1]], "rowname")

  # First column is the row names
  expect_is(p[[1]], "character")

  # All other columns are numeric
  expect_true(all(vapply(p[-1], is.numeric, FUN.VALUE = logical(1))))

  # All row names have a at least one letter, which means
  # that the row names were not extracted raw if `x`
  # was a matrix
  expect_true(all(grepl("[[:alpha:]]{1,}", p[[1]])))

  # df is symmetric when excluding the rowname variables
  expect_equal(nrow(p), ncol(p) - 1)
  invisible(TRUE)
}

test_cor_cov <- function(fun, fun_str) {

  test_that(paste0(fun_str, " returns correct output"), {
    cmv_tib <- fun(x = corr_tibble,
                   sqp_data = sqp_df,
                   V4, V5)

    correct_format(cmv_tib)

    # Also handles character strings as variables
    expect_identical(cmv_tib, fun(corr_tibble, sqp_df, "V4", "V5"))
  })

  test_that(paste0(fun_str, "`x` argument works fine with matrix"), {
    random_vec <- rnorm(10, sd = 50)

    matr_nothing <- matrix(random_vec, 5, 5)

    matr_row <- matrix(random_vec, 5, 5,
                       dimnames = list(paste0("V", seq_len(5))))

    matr_col <- matrix(random_vec, 5, 5,
                       dimnames = list(NULL, paste0("V", seq_len(5))))

    matr_both <- matrix(random_vec, 5, 5,
                        dimnames = list(paste0("V", seq_len(5)),
                                        paste0("V", seq_len(5))))


    # Matrix no row or col names
    cmv_matr <- fun(matr_nothing, sqp_df, V4, V5)
    correct_format(cmv_matr)

    # Matrix row names
    cmv_matr <- fun(matr_row, sqp_df, V4, V5)
    correct_format(cmv_matr)

    # Matrix col names
    cmv_matr <- fun(matr_col, sqp_df, V4, V5)
    correct_format(cmv_matr)

    # Matrix row and col names
    cmv_matr <- fun(matr_both, sqp_df, V4, V5)
    correct_format(cmv_matr)
  })

  test_that(paste0(fun_str, "`x` argument works fine with data frame"), {
    random_vec <- rnorm(10, sd = 50)

    df_no_rows <- as.data.frame(matrix(random_vec, 5, 5))

    # df no row names
    cmv_matr <- fun(df_no_rows, sqp_df, V4, V5)
    correct_format(cmv_matr)

    rownames(df_no_rows) <- paste0("V", seq_len(5))
    df_rows <- df_no_rows

    # df row names
    cmv_matr <- fun(df_rows, sqp_df, V4, V5)
    correct_format(cmv_matr)
  })

  test_that(paste0(fun_str, " works with cmv argument"), {
    sqp_df <-
      tibble(question = paste0("V", 1:5),
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
             reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
             validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

    filtered_df <- subset(sqp_df, question %in% c("V4", "V5"))

    cmv_aut <- fun(corr_tibble, sqp_df, V4, V5)
    cmv_manual <- fun(corr_tibble, sqp_df, V4, V5, cmv = estimate_cmv(filtered_df))

    expect_equal(cmv_aut, cmv_manual)
  })

  test_that(paste0(fun_str, " uses only unique variable names"), {
    cmv_tib <- fun(corr_tibble, sqp_df, V4, V5, V5)
    expect_is(cmv_tib, "data.frame")

    # First column is the row names
    expect_is(cmv_tib[[1]], "character")

    # All other columns are numeric
    expect_true(all(vapply(cmv_tib[-1], is.numeric, FUN.VALUE = logical(1))))

    # df is symmetric when excluding the rowname variables
    expect_equal(nrow(cmv_tib), ncol(cmv_tib) - 1)
  })

  test_that(paste0(fun_str, " replaces upper and lower triangle"), {
    up_equal <- function(x) {
      tp <- x[-1]
      all(sort(tp[lower.tri(tp)]) == sort(tp[upper.tri(tp)]))
    }

    # Two variables
    cmv_tib <- as.data.frame(fun(corr_tibble, sqp_df, V4, V5))
    expect_true(up_equal(cmv_tib))

    # Three variables
    cmv_tib <- as.data.frame(fun(corr_tibble, sqp_df, V3, V4, V5))
    expect_true(up_equal(cmv_tib))
  })

  test_that(paste0(fun_str, " adds sqp class to valid sqp_data"), {
    tmp <- sqp_df
    class(tmp) <- c("tbl_df", "tbl", "data.frame")

    noclass <- fun(
      corr_tibble,
      sqp_data = tmp,
      V4, V5
    )

    valid_class <- fun(
      corr_tibble,
      sqp_data = sqp_df,
      V4, V5
    )
    expect_identical(valid_class, noclass)
  })
}

test_cor_cov(sqp_cmv_cor, "sqp_cmv_cov")


# Given that original_data is not necessary in sqp_cmv_corr, I create
# sqp_cmv_cov with the original data argument prefilled so that all
# of the tests run this function and the sqp_cmv_cor and sqp_cmv_cov tests
# can be reused.

partial_cov <- function(x, sqp_data, ..., cmv = NULL) {
  sqp_cmv_cov(x = x, sqp_data = sqp_data, ... = ..., original_data = original_df, cmv = cmv)
}

test_cor_cov(partial_cov, "sqp_cmv_cov")


test_input_errors <- function(fun, fun_str) {
  type <- if (grepl("cor", fun_str)) "correlation" else "covariance"

  test_that(paste0(fun_str, " throws specific errors"), {
    expect_error(fun(list(), sqp_df),
                 paste0("`x` must be a ", type ," data frame or matrix"))

    expect_error(fun(corr_tibble, sqp_df),
                 "You need to supply at least two variables to calculate the Common Method Variance")

    expect_error(fun(corr_tibble, sqp_df, V2),
                 "You need to supply at least two variables to calculate the Common Method Variance")

    expect_error(fun(corr_tibble, sqp_df, V2, V3),
                 "`sqp_data` must have non-missing values at columns reliability and validity for all variables")

    expect_error(fun(corr_tibble, sqp_df, hey, other),
                 "At least one variable not present in `x`: hey, other")
  })
}

test_input_errors(sqp_cmv_cor, "sqp_cmv_cor")
test_input_errors(partial_cov, "sqp_cmv_cov")


library(essurvey)
selected_vars <- c("polintr", "ppltrst", "trstplt")
ess_email <- Sys.getenv("ess_email")
ess7es <- import_country("Spain", 7, ess_email)[c(selected_vars, "pspwght", "pweight")]

ess7es <- ess7es[complete.cases(ess7es), ]
ess7es3var <- ess7es[selected_vars]

test_that("sqp_cmv_cor returns correct calculation",  {
  ## Apply weighted correlation with pspwght
  # wt_cor_cv <- cov.wt(ess7es3var, wt = ess7es$pspwght, cor = TRUE)
  # original_corr_weighted <- wt_cor_cv$cor
  original_corr <- cor(ess7es3var)

  ## Using sqpr
  sqp_login()
  study_id <- find_studies("ESS Round 7")$id

  question_ids <- find_questions(study_id, selected_vars)
  question_ids <- question_ids[with(question_ids, country_iso == "ES" & language_iso == "spa"), "id", drop = TRUE]

  sqp_df <- get_estimates(question_ids)
  sqp_df <- sqp_df[order(sqp_df$question), ]
  diag(original_corr) <- sqp_df$quality
  # diag(original_corr_weighted) <- sqp_df$quality

  tst_cmv <- sqp_cmv_cor(original_corr, sqp_df, ppltrst, trstplt)
  tmp_corrected_cor <- tibble::as_tibble(cov2cor(as.matrix(tst_cmv[, selected_vars])))
  tmp_corrected_cor <- tibble::add_column(tmp_corrected_cor, rowname = tst_cmv$rowname, .before = 1)
  tmp_corrected_cor <- as.data.frame(tmp_corrected_cor)

  correct_df <- data.frame(stringsAsFactors=FALSE,
                           rowname = c("polintr", "ppltrst", "trstplt"),
                           polintr = c(1, -0.307183576911697, -0.246956866582566),
                           ppltrst = c(-0.307183576911697, 1, 0.195267934255757),
                           trstplt = c(-0.246956866582566, 0.195267934255757, 1)
                           )

  expect_equivalent(tmp_corrected_cor, correct_df)
})
