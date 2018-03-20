## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ------------------------------------------------------------------------
library("lavaan")
library("tidyverse")
library("ess")
library("sqpr")

## ---- echo = FALSE-------------------------------------------------------
your_email <- Sys.getenv("your_email")

## ---- results = 'hide'---------------------------------------------------
ess7es<-
  ess_country("Spain", 7, your_email) %>%
  recode_missings() 

# Create composite scores
ess7es <-
  ess7es %>%
  mutate(poltrst = trstprl + trstplt + trstprt,
         serv = stfedu + stfhlth,
         systmrsp = psppsgv + psppipl + ptcpplt)

selected_vars <- c("trstprl", "trstplt", "trstprt",
                   "stfedu", "stfhlth", "psppsgv",
                   "psppipl", "ptcpplt", "ppltrst",
                   "polintr", "stflife", "stfeco",
                   "agea","eisced")

composite_scores <- c("poltrst", "serv", "systmrsp")

all_vars <- c(composite_scores, selected_vars) # for using later

## ------------------------------------------------------------------------
ess7es

## ---- eval = FALSE-------------------------------------------------------
#  Sys.setenv(SQP_USER = 'your_username')
#  Sys.setenv(SQP_PW = 'your_pw')

## ------------------------------------------------------------------------
sqp_login()
study_id <- find_studies("ESS Round 7")$id

questions <-
  study_id %>%
  find_questions(selected_vars[1:12]) %>%
  filter(country_iso == "ES", language_iso == "spa")

## ------------------------------------------------------------------------
all(tolower(questions$short_name) %in% selected_vars[1:12])

## ------------------------------------------------------------------------
sqp_data <-
  get_estimates(questions$id) %>%
  arrange(question)

## ------------------------------------------------------------------------
sqp_data

## ------------------------------------------------------------------------
sqp_sscore(sqp_data = sqp_data,
           df = ess7es,
           new_name = poltrst,
           trstprl, trstplt, trstprt)

## ------------------------------------------------------------------------
Quality <-
  sqp_data %>%
  sqp_sscore(df = ess7es, new_name = poltrst, trstprl, trstplt, trstprt) %>%
  sqp_sscore(df = ess7es, new_name = serv, stfedu, stfhlth) %>%
  sqp_sscore(df = ess7es, new_name = systmrsp, psppsgv, psppipl, ptcpplt) 

## ------------------------------------------------------------------------
Quality

## ------------------------------------------------------------------------
Quality <- 
  Quality %>%
  sqp_bind_metrics(agea, list(quality = 1)) %>%
  sqp_bind_metrics(eisced, list(quality = 0.93))

Quality

## ------------------------------------------------------------------------
variables_order <- c("poltrst",
                     "serv",
                     "systmrsp",
                     "ppltrst",
                     "polintr",
                     "stflife",
                     "stfeco",
                     "agea",
                     "eisced")

Quality <- Quality[match(variables_order, Quality$question), ]

## ------------------------------------------------------------------------
ess7escorr <- ess7es %>% select(variables_order)

## ------------------------------------------------------------------------
# Exploratory correlation matrix (in order of the columns in data frame):
original_corr_2 <- cor(ess7escorr, use = "complete.obs", method = "pearson")

original_corr_2

## ------------------------------------------------------------------------
corr_q2 <-
  sqp_correlate(x = ess7escorr,
                diag_adj = Quality$quality,
                use = "complete.obs",
                method = "pearson")

corr_q2

## ------------------------------------------------------------------------
#subtract the cmv from the observed correlation
corr_q2_cmv <-
  sqp_cmv(x = corr_q2,
          sqp_data = Quality,
          stfeco, stflife)

corr_q2_cmv

## ------------------------------------------------------------------------
cmv <-
  Quality %>%
  filter(question %in% c("stflife", "stfeco")) %>%
  sqpr:::estimate_cmv()

0.27 - cmv

## ------------------------------------------------------------------------
corrected_corr <- corr_q2_cmv %>% select(-rowname) %>% as.matrix() %>% cov2cor()

corrected_corr

## ------------------------------------------------------------------------
model<- "poltrst ~ ppltrst + stflife + polintr + stfeco + serv + systmrsp + agea + eisced"

# Model based on original correlation matrix
fit <-
  sem(model,
      sample.cov=original_corr_2,
      sample.nobs= 1624) 

# Model based on corrected correlation matrix 
fit.corrected <-
  sem(model,
      sample.cov=corrected_corr,
      sample.nobs= 1624) 

## ---- fig.width = 7, fig.with = 9----------------------------------------
coef_table <-
  list(fit, fit.corrected) %>%
  map(parameterestimates) %>%
  map(~ filter(.x, lhs == "poltrst")) %>%
  map(~ select(.x, rhs, est, ci.lower, ci.upper)) %>%
  bind_rows() %>%
  mutate(model = rep(c("original", "corrected"), each = 9))

coef_table %>%
  ggplot(aes(rhs, est, colour = model)) +
  geom_linerange(aes(ymin = ci.lower, ymax = ci.upper), position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(x = "Predictors", y = "Estimated coefficients") +
  theme_bw()

## ------------------------------------------------------------------------
# Relative increase (they don't only go up!):
coef(fit.corrected) / coef(fit)

## ------------------------------------------------------------------------
R2_uncorr <- inspect(fit, 'r2')
R2 <- inspect(fit.corrected, 'r2')

# Change of R2:
R2 - R2_uncorr

