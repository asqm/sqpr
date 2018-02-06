library(tidyverse)

# Standard deviation of data
std_data <- c(2.591, 2.236, 2.167)

## Variance composite score
var_cs <- 38.7139


## Quality estimates
# quality <-
#   suppressMessages(suppressWarnings(read_csv2("SQPexport_20171230_2255.csv"))) %>%
#   select(question = `Question name`,
#          ends_with("2)"),
#          matches("\\scoefficient\\s(.*)"), -contains("Method effect")) %>%
#   set_names(~ str_sub(.x, start = -3) %>% str_replace_all("[:punct:]", "")) %>%
#   rename(reliability = r2,
#          validity = v2,
#          quality = q2,
#          r_coef = r,
#          v_coef = v,
#          q_coef = q) %>%
#   mutate(rowname = tolower(ion)) %>%
#   select(rowname, everything(), -ion)

quality <-
  tibble(quality = c(0.770, 0.805, 0.767),
         reliability = rep(NA, 3),
         validity = rep(NA, 3),
         q_coef = rep(NA, 3),
         r_coef = c(0.903, 0.909, 0.924),
         v_coef = c(0.972, 0.987, 0.948))

# Reliability and validity coefficient
r_coef <- quality$r_coef
v_coef <- quality$v_coef
# Quality
qr2 <- quality$quality
# Method effect
method_e <- sqrt(1 - v_coef^2)

## Weights
wt <- c(0.333, 0.333, 0.333)

variance_error <- function(quality, std_data) {
  purrr::map2_dbl(quality, std_data, ~ (1 - .x) * .y^2)
}

cov_both <- function(y) {
  result <- map_dbl(y, ~ std_data[.x] * r_coef[.x] * method_e[.x])
  prod(result)
}


# variance of errors
var_e <- variance_error(qr2, std_data)
#  Σ wk2 var(ek)
wk2 <- round(sum(wt^2 * var_e), 3)


# covariance of error
comb <- list(1:2, 2:3, c(1, 3))
cov_e <- map_dbl(comb, cov_both)

combn_two <- list(1:2, 2:3, c(1, 3))
intm <- map2_dbl(combn_two, seq_along(combn_two), function(both_combn, the_seq) {
  separ_first <- both_combn[1]
  separ_second <- both_combn[2]
  map2_dbl(separ_first, separ_second, ~ wt[.x] * wt[.y] * cov_e[the_seq])
})

wkwk <- sum(intm) * 2

var_ecs <- wk2 + wkwk

round(1 - (var_ecs / var_cs), 3)
