## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----poststrat----------------------------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "poststratify",
                 margins = list(region = c(table(population$region)))) |>
  prep()
wf$steps[[2]]$diagnostics

## ----poststrat-multi----------------------------------------------------------
totals_cross <- colSums(model.matrix(~ region * sex, population))
wf2 <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "linear", formula = ~ region * sex,
                 totals = totals_cross) |>
  prep()
wf2$steps[[2]]$diagnostics

## ----raking-------------------------------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "raking",
                 margins = list(region = c(table(population$region)),
                                sex    = c(table(population$sex)))) |>
  prep()
wf$steps[[2]]$diagnostics

## ----linear-------------------------------------------------------------------
totals <- colSums(model.matrix(~ region + sex + age, population))
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "linear", formula = ~ region + sex + age,
                 totals = totals) |>
  prep()
wf$steps[[2]]$diagnostics

## ----bounded------------------------------------------------------------------
totals_rs <- colSums(model.matrix(~ region + sex, population))
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "linear", formula = ~ region + sex, totals = totals_rs,
                 bounds = c(0.83, 1.2)) |>
  prep()
range(wf$steps[[2]]$diagnostics$achieved / wf$steps[[2]]$diagnostics$target)

## ----equal-cluster------------------------------------------------------------
totals_rs <- colSums(model.matrix(~ region + sex, population))
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "linear", formula = ~ region + sex, totals = totals_rs,
                 cluster = "household_id", equal_within_cluster = TRUE) |>
  prep()
# every member of a household shares one weight
tapply(wf$final_weight, sample_survey$household_id, function(x) diff(range(x))) |>
  max()

