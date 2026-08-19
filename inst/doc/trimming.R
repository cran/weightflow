## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----base---------------------------------------------------------------------
base <- weighting_spec(sample_survey, base_weights = pw) |>
  step_unknown_eligibility(unknown = unknown_elig, by = "region") |>
  step_nonresponse(respondent = responded, method = "propensity",
                   engine = "logit", formula = ~ region + sex + age,
                   num_classes = NULL) |>
  step_calibrate(method = "raking",
                 margins = list(region = c(table(population$region)),
                                sex    = c(table(population$sex))))

fit0 <- prep(base)
design_effect(fit0$final_weight)[c("deff", "n_eff")]

## ----auto---------------------------------------------------------------------
tukey  <- base |> step_trim_weights(method = "tukey")  |> prep()
potter <- base |> step_trim_weights(method = "potter") |> prep()

k <- length(tukey$steps)               # the trim is the last step
rbind(tukey  = tukey$steps[[k]]$diagnostics[, c("method", "upper", "n_capped")],
      potter = potter$steps[[k]]$diagnostics[, c("method", "upper", "n_capped")])

## ----redistribute-------------------------------------------------------------
uniform <- base |>
  step_trim_weights(upper = 2500, redistribute = "uniform") |>
  prep()
design_effect(uniform$final_weight)[["deff"]]

## ----trim-median--------------------------------------------------------------
by_median <- base |>
  step_trim(max_ratio = 4, reference = "median", redistribute = FALSE) |>
  prep()
range(by_median$final_weight[by_median$final_weight > 0])

## ----trim-by------------------------------------------------------------------
by_region <- base |>
  step_trim(max_ratio = 4, reference = "median", by = "region",
            redistribute = FALSE) |>
  prep()
range(by_region$final_weight[by_region$final_weight > 0])

## ----trimcal------------------------------------------------------------------
w   <- collect_weights(fit0, drop_zero = FALSE)$.weight
pos <- w[w > 0]
lo  <- as.numeric(quantile(pos, 0.05)); up <- as.numeric(quantile(pos, 0.95))

trimmed <- base |>
  step_trim_calibrated(~ region + sex, lower = lo, upper = up) |>
  prep()

## ----trimcal-check------------------------------------------------------------
X <- model.matrix(~ region + sex, sample_survey)
round(colSums((trimmed$final_weight - w) * X), 6)
range(trimmed$final_weight[trimmed$final_weight > 0])

## ----trimcal-by---------------------------------------------------------------
lo_by <- tapply(w, sample_survey$region, function(x) as.numeric(quantile(x[x > 0], 0.05)))
up_by <- tapply(w, sample_survey$region, function(x) as.numeric(quantile(x[x > 0], 0.95)))
lo_by                                   # a vector named by the levels of `by`

trimmed_by <- base |>
  step_trim_calibrated(~ region + sex, lower = lo_by, upper = up_by, by = "region") |>
  prep()
tapply(trimmed_by$final_weight[trimmed_by$final_weight > 0],
       sample_survey$region[trimmed_by$final_weight > 0], range)

## ----trimcal-cross, eval = FALSE----------------------------------------------
# d <- sample_survey
# d$reg_sex <- interaction(d$region, d$sex, sep = "_", drop = TRUE)
# 
# # build the recipe on `d`, then trim with bounds named "North_F", "North_M", ...
# weighting_spec(d, base_weights = pw) |>
#   # ... eligibility, nonresponse, calibration ...
#   step_trim_calibrated(~ region + sex, lower = lo_cell, upper = up_cell,
#                        by = "reg_sex")

