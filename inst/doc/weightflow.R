## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----data---------------------------------------------------------------------
dat <- sample_one
dat$age_grp <- cut(dat$age, c(0, 30, 45, 60, Inf),
                   labels = c("18-30", "31-45", "46-60", "60+"))

## ----recipe-------------------------------------------------------------------
fitted <- weighting_spec(dat, base_weights = pw) |>
  step_unknown_eligibility(unknown = unknown_elig, by = "region",
                           cluster = "household_id") |>
  step_drop_ineligible(ineligible = ineligible) |>
  step_nonresponse(respondent = hh_responded, method = "weighting_class",
                   by = "region", cluster = "household_id") |>
  step_select_within(prob = p_within) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = c("region", "sex", "age_grp")) |>
  step_calibrate(method = "raking",
                 margins = list(region = c(table(population$region)),
                                sex    = c(table(population$sex)))) |>
  prep()

## ----trim---------------------------------------------------------------------
trimmed <- weighting_spec(dat, base_weights = pw) |>
  step_unknown_eligibility(unknown = unknown_elig, by = "region",
                           cluster = "household_id") |>
  step_drop_ineligible(ineligible = ineligible) |>
  step_nonresponse(respondent = hh_responded, method = "weighting_class",
                   by = "region", cluster = "household_id") |>
  step_select_within(prob = p_within) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = c("region", "sex", "age_grp")) |>
  step_calibrate(method = "raking",
                 margins = list(region = c(table(population$region)),
                                sex    = c(table(population$sex)))) |>
  step_trim_weights() |>
  prep()

## ----summary------------------------------------------------------------------
summary(fitted)

## ----deff-compare-------------------------------------------------------------
c(no_trim = design_effect(fitted$final_weight)$deff,
  trimmed = design_effect(trimmed$final_weight)$deff)

