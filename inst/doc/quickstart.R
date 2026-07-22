## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----glimpse------------------------------------------------------------------
str(sample_survey[, c("pw", "region", "sex", "unknown_elig", "responded")])

## ----totals-------------------------------------------------------------------
m_region <- as.data.frame(table(region = population$region))
m_sex    <- as.data.frame(table(sex    = population$sex))
m_region

## ----recipe-------------------------------------------------------------------
fit <- weighting_spec(sample_survey, base_weights = pw) |>
  step_unknown_eligibility(unknown = unknown_elig, by = "region") |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "raking",
                 totals = list(m_region, m_sex), count = "Freq") |>
  prep()

## ----weights------------------------------------------------------------------
w <- collect_weights(fit)
summary(w$.weight)

## ----summary------------------------------------------------------------------
summary(fit)

