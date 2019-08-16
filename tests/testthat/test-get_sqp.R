## test_that("multiplication works", {

##   # Should return empty data frame
##   get_sqp(
##     c("random_study"),
##     "tvtot",
##     "es",
##     "spa"
##   )

##   # Should generate error about only one study being possible to search
##   get_sqp(
##     c("ESS Round 4", "random_study"),
##     "tvtot",
##     "es",
##     "spa"
##   )

##   # Should generate error about study being empty
##   get_sqp(
##     character(),
##     "tvtot",
##     "es",
##     "spa"
##   )

##   # This should throw an error
##   # Check for character and length and the error should be informative
##   # for higher level functions so use explicit name of question name
##   ## rather than question id
##   get_sqp(
##     "ESS Round 4",
##     character(),
##     "es",
##     "spa"
##   )

##   # Should throw an error
##   get_sqp(
##     "ESS Round 4",
##     "tvtot",
##     "es"
##     character(),
##     )

##   # Should throw an error
##   # Why? Because if we get several country/languages (which is possible),
##   # the result of get_estimates won't allow to identify which estimates belong
##   ## to which country/lang
##   get_sqp(
##     "ESS round 4",
##     "tvtot",
##     "es",
##     c("spa", "cat"),
##     )

##   # Should it throw an error suggesting the available languages?
##   get_sqp(
##     "ESS Round 4",
##     "tvtot",
##     "es"
##     "es"
##   )

## })
