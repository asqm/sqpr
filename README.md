
sqpr
====

The `sqpr` package allows you to calculate several estimations of the quality of your survey questions and also adjust your estimations for measurement error. Moreover, it gives easy access to the API of the Survey Quality Prediction [website](http://sqp.upf.edu/), a data base that contains over 40,000 predictions on the quality of questions.

Installation
------------

`sqpr` is not currently on CRAN but you can install the developing version of from Github with:

``` r
# install.packages("devtools")
devtools::install_github("recsm/sqpr")
```

Example
-------

### Registration and logging in

Register in the [SQP website](http://sqp.upf.edu/accounts/register/) and confirm your registration through your email.

First, load the package in R and provide your registered credentials.

``` r
library(sqpr)
Sys.setenv(SQP_USER = 'This is where your email goes')
Sys.setenv(SQP_PW = 'This is where your password goes')
```

Effectively login with `sqpr_login`.

``` r
sqp_login()
```

For details on the login process see the `Accessing the SQP API vignette` from the package.

Once you've ran `sqp_login()` once, you're all set to work with the SQP API! No need to run it again unless you close the R session.

Exploring the SQP API
---------------------

You can query all the questions in a specific study to check whether a specific question has quality predictions. Use `find_studies` to locate the `id` of a certain study

``` r
ess_four <- find_studies("ESS Round 4")
ess_four
#> # A tibble: 1 x 2
#>      id name       
#>   <int> <chr>      
#> 1     4 ESS Round 4
```

Ok, so we have our `study_id`. Which questions are in that study? `find_questions` will do the work for you.

``` r
q_ess <- find_questions(ess_four$id, "tv")
```

That might take a while because it's downloading all of the data to your computer. However, if you run `find_questions` (or any other API related function) once, then any repetition should be instant as it loads everything from your computer rather than downloading it again.

Let's query further down to get questions for a specific question.

``` r
sp_tv <- q_ess[q_ess$language_iso == "spa", ]
sp_tv
#> # A tibble: 3 x 5
#>      id study_id short_name country_iso language_iso
#>   <int>    <int> <chr>      <chr>       <chr>       
#> 1  7999        4 TvTot      ES          spa         
#> 2 27699        4 TvPol      ES          spa         
#> 3 27638        4 PrtVtxx    ES          spa
```

The hard part is done now. Once we have the `id` of your questions of interest, we supply it to `get_estimates` and it will bring the quality predictions for those questions.

``` r
predictions <- get_estimates(sp_tv$id)
predictions
#> # A tibble: 3 x 4
#>   question reliability validity quality
#>   <chr>          <dbl>    <dbl>   <dbl>
#> 1 tvtot          0.713    0.926   0.660
#> 2 tvpol         NA       NA      NA    
#> 3 prtvtxx       NA       NA      NA
```

`get_estimates` will return all question names as lower case for increasing the chances of compatibility with the name in the questionnair of the study.

`sqpr` does much more than querying data from the SQP API, such as calculating common method variance of several questions and adjusting correlations for measurement error, among other things. The two vignettes of the package elaborate much more on how to query data and use it in your analysis.
