## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----base---------------------------------------------------------------------
base <- weighting_spec(sample_one, base_weights = pw) |>
  step_unknown_eligibility(unknown = unknown_elig, by = "region",
                           cluster = "household_id") |>
  step_drop_ineligible(ineligible = ineligible) |>
  step_nonresponse(respondent = hh_responded, method = "weighting_class",
                   by = "region", cluster = "household_id") |>
  step_select_within(prob = p_within)

## ----boost, eval = requireNamespace("xgboost", quietly = TRUE)----------------
fit_boost <- base |>
  step_nonresponse(respondent = responded, method = "propensity",
                   formula = ~ region + sex + age, engine = "boost",
                   num_classes = 5) |>
  prep()

## ----crossfit, eval = requireNamespace("xgboost", quietly = TRUE)-------------
fit_cf <- base |>
  step_nonresponse(respondent = responded, method = "propensity",
                   formula = ~ region + sex + age, engine = "boost",
                   num_classes = 5, crossfit = 5, crossfit_seed = 1) |>
  prep()

## ----crossfit-deff, eval = requireNamespace("xgboost", quietly = TRUE)--------
c(in_sample = design_effect(collect_weights(fit_boost)$.weight)$deff,
  crossfit  = design_effect(collect_weights(fit_cf)$.weight)$deff)

## ----modelcal, eval = requireNamespace("xgboost", quietly = TRUE)-------------
fit_mc <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_model_calibration(
    x_formula  = ~ region + sex,
    models     = list(income = y_model(income ~ age + sex + region,
                                       engine = "boost")),
    population = population, crossfit = 5, crossfit_seed = 1) |>
  prep()

fit_mc$steps[[2]]$diagnostics

## ----ridge--------------------------------------------------------------------
pop_totals <- c("(Intercept)" = nrow(population),
                regionSouth = sum(population$region == "South"),
                regionEast  = sum(population$region == "East"),
                regionWest  = sum(population$region == "West"),
                sexM        = sum(population$sex == "M"))

fit_ridge <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "linear", formula = ~ region + sex,
                 totals = pop_totals, penalty = 1) |>
  prep()

fit_ridge$steps[[2]]$diagnostics

## ----potter-------------------------------------------------------------------
trimmed_tukey <- base |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = c("region", "sex")) |>
  step_trim_weights(method = "tukey") |>
  prep()

trimmed_potter <- base |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = c("region", "sex")) |>
  step_trim_weights(method = "potter") |>
  prep()

trimmed_tukey$steps[[6]]$diagnostics[, c("method", "upper", "n_capped")]
trimmed_potter$steps[[6]]$diagnostics[, c("method", "upper", "n_capped")]

## ----trimcal------------------------------------------------------------------
recipe <- base |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = c("region", "sex")) |>
  step_calibrate(method = "raking",
                 margins = list(region = c(table(population$region))))

pre <- prep(recipe)
w   <- collect_weights(pre, drop_zero = FALSE)$.weight
w   <- w[w > 0]
lo  <- as.numeric(quantile(w, 0.05)); up <- as.numeric(quantile(w, 0.95))

trimmed <- recipe |>
  step_trim_calibrated(~ region, lower = lo, upper = up) |>
  prep()

## ----trimcal-check------------------------------------------------------------
Xr     <- model.matrix(~ region, sample_one)
w_cal  <- collect_weights(pre, drop_zero = FALSE)$.weight
w_trim <- trimmed$final_weight
round(rbind(after_calibration = colSums(w_cal  * Xr),
            after_trim        = colSums(w_trim * Xr),
            difference        = colSums((w_trim - w_cal) * Xr)), 6)
range(w_trim[w_trim > 0])

