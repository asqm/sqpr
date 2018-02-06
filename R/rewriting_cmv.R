library(purrr)

std_data <- c(2.59131, 2.235977, 2.166643)
r_coef <- c(0.903, 0.909, 0.924)
v_coef <- c(0.972, 0.987, 0.948)
method_e <- sqrt(1 - v_coef^2) # method effect
qr2 <- c(0.77, 0.805, 0.767)
wt <- c(0.33, 0.33, 0.33)

variance_error <- function(quality_r2, std_data) {
  # This might be better vectorized
  (1 - quality_r2) * std_data^2
}

cov_both <- function(y) {
  result <- map_dbl(y, ~ std_data[.x] * r_coef[.x] * method_e[.x])
  prod(result)
}

# variance of errors
var_e <- map2_dbl(qr2, std_data, variance_error)

# covariance of error
comb <- list(1:2, 2:3, c(1, 3))
cov_e <- map_dbl(comb, cov_both)

#  Σ wk2 var(ek)
sum(round(wt^2 * var_e, 3))
