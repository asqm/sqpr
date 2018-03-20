## ---- echo = FALSE, warning=FALSE, message=FALSE-------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE
)

## ---- echo = FALSE-------------------------------------------------------
library(sqpr)

user <- Sys.getenv("SQP_USER")
pw <- Sys.getenv("SQP_PW")

sqp_login(user, pw)

## ---- eval = FALSE-------------------------------------------------------
#  library(sqpr)

## ---- eval = FALSE-------------------------------------------------------
#  Sys.setenv(SQP_USER = 'This is where your email goes')
#  Sys.setenv(SQP_PW = 'This is where your password goes')

## ---- eval = FALSE-------------------------------------------------------
#  sqp_login()

## ---- eval = FALSE-------------------------------------------------------
#  options(
#    SQP_USER = 'This is where your email goes'),
#    SQP_PW = 'This is where your password goes'
#  )
#  sqp_login()

## ---- eval = FALSE-------------------------------------------------------
#  sqp_login(
#    'This is where your email goes',
#    'This is where your password goes'
#  )

## ------------------------------------------------------------------------
find_studies("ess")

## ------------------------------------------------------------------------
study_id <- find_studies("ESS Round 4")$id

## ------------------------------------------------------------------------
find_questions(study_id, "tv")

## ------------------------------------------------------------------------
tv_qs <- find_questions(study_id, "^tv")
tv_qs

## ------------------------------------------------------------------------
sp_tv <- tv_qs[tv_qs$language_iso == "spa", ]
sp_tv

## ------------------------------------------------------------------------
get_estimates(sp_tv$id)

## ------------------------------------------------------------------------
get_estimates(sp_tv$id, all_columns = TRUE)

