## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)
has_ranger <- requireNamespace("ranger", quietly = TRUE)

## ----recipe, eval = has_ranger------------------------------------------------
spec <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_model_calibration(
    x_formula  = ~ region + sex,                       # consistency constraints
    models     = list(income = y_model(income ~ age + sex + region,
                                       engine = "forest")),
    population = population)
fitted <- prep(spec)

## ----check, eval = has_ranger-------------------------------------------------
fitted$steps[[2]]$diagnostics

## ----estimate, eval = has_ranger----------------------------------------------
est_total  <- sum(fitted$final_weight * sample_survey$income, na.rm = TRUE)
true_total <- sum(population$income)
c(estimated = est_total, population = true_total,
  rel_error = (est_total - true_total) / true_total)

## ----equal-cluster------------------------------------------------------------
fit_hh <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class", by = "region") |>
  step_model_calibration(
    x_formula  = ~ sex + region,
    models     = list(income = y_model(income ~ age + sex, engine = "glm")),
    population  = population,
    cluster = "household_id", equal_within_cluster = TRUE) |>
  prep()

# the weight is constant within each household (checked over active members)
w <- fit_hh$final_weight
max(tapply(w[w > 0], sample_survey$household_id[w > 0],
           function(x) diff(range(x))))

