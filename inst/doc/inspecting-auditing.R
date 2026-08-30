## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(weightflow)

fit <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "propensity",
                   engine = "logit", formula = ~ region + sex + age) |>
  step_calibrate(method = "raking",
                 margins = list(region = c(table(population$region)),
                                sex    = c(table(population$sex)))) |>
  prep()

## ----print--------------------------------------------------------------------
fit

## ----alerts-------------------------------------------------------------------
has_alerts(fit)
weighting_alerts(fit)

if (has_alerts(fit)) {
  message("Review needed before dissemination:")
  for (a in weighting_alerts(fit)) message("  - ", a)
}

## ----step-detail--------------------------------------------------------------
det <- collect_step_detail(fit, step = "nonresponse_1")
head(det)

## ----propensities-------------------------------------------------------------
props <- collect_propensities(fit)
summary(props$.propensity)

## ----domains------------------------------------------------------------------
domain_summary(fit, by = "region")

