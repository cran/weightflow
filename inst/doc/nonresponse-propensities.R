## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)
has_rpart  <- requireNamespace("rpart",  quietly = TRUE)
has_ranger <- requireNamespace("ranger", quietly = TRUE)

## ----wc-----------------------------------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  prep()
summary(wf)

## ----wc-check-----------------------------------------------------------------
before <- tapply(sample_survey$pw,  sample_survey$region, sum)
after  <- tapply(wf$final_weight,   sample_survey$region, sum)
round(cbind(before, after, diff = after - before), 6)

## ----prop-logit, warning = FALSE----------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "propensity",
                   formula = ~ region + sex + age, engine = "logit",
                   num_classes = 5) |>
  prep()
summary(wf)

## ----prop-tree, eval = has_rpart----------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "propensity",
                   formula = ~ region + sex + age, engine = "tree",
                   num_classes = 5) |>
  prep()
design_effect(wf$final_weight)$deff

## ----prop-forest, eval = has_ranger-------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "propensity",
                   formula = ~ region + sex + age, engine = "forest",
                   num_classes = 5) |>
  prep()
design_effect(wf$final_weight)$deff

## ----crossfit, eval = has_ranger----------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "propensity",
                   formula = ~ region + sex + age, engine = "forest",
                   num_classes = 5, crossfit = 5, crossfit_seed = 1) |>
  prep()
design_effect(wf$final_weight)$deff

## ----weight-model, warning = FALSE--------------------------------------------
f <- function(wm) weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "propensity",
                   formula = ~ region + sex + age, engine = "logit",
                   num_classes = NULL, weight_model = wm) |>
  prep()
c(weighted   = design_effect(f(TRUE)$final_weight)$deff,
  unweighted = design_effect(f(FALSE)$final_weight)$deff)

## ----cluster------------------------------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region", cluster = "household_id") |>
  prep()
design_effect(wf$final_weight)$deff

## ----nr-calib-----------------------------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "calibration",
                   formula = ~ region + sex) |>
  prep()
summary(wf)

## ----nr-calib-check-----------------------------------------------------------
X      <- model.matrix(~ region + sex, sample_survey)
resp   <- sample_survey$responded == 1
before <- colSums(sample_survey$pw * X)               # respondents + nonrespondents
after  <- colSums(wf$final_weight[resp] * X[resp, ])  # respondents, calibrated
round(rbind(before, after, diff = after - before), 4)

## ----nr-calib-integ-----------------------------------------------------------
wf <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "calibration",
                   formula = ~ region, cluster = "household_id",
                   equal_within_cluster = TRUE) |>
  prep()
design_effect(wf$final_weight)$deff

