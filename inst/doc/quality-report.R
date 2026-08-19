## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)
has_rpart <- requireNamespace("rpart", quietly = TRUE)

## ----fit, eval = has_rpart, warning = FALSE, message = FALSE------------------
totals_r <- colSums(model.matrix(~ region, population))

spec <- weighting_spec(sample_survey, base_weights = pw) |>
  step_unknown_eligibility(unknown = unknown_elig, by = "region",
                           cluster = "household_id") |>
  step_nonresponse(respondent = responded, method = "propensity",
                   engine = "tree", formula = ~ region + sex + age,
                   crossfit = 5, crossfit_seed = 1, num_classes = NULL,
                   cluster = "household_id") |>
  step_calibrate(method = "linear", formula = ~ region, totals = totals_r,
                 cluster = "household_id", equal_within_cluster = TRUE)

# feasible trim interval from the calibrated weights
w  <- collect_weights(prep(spec), drop_zero = FALSE)$.weight
w  <- w[w > 0]
lo <- as.numeric(quantile(w, 0.02)); up <- as.numeric(quantile(w, 0.98))

recipe <- spec |>
  step_trim_calibrated(~ region, lower = lo, upper = up,
                       cluster = "household_id", equal_within_cluster = TRUE) |>
  step_round(method = "preserve_total")

fit  <- prep(recipe)
boot <- bootstrap_weights(recipe, replicates = 15, strata = "region",
                          psu = "psu", seed = 1, progress = FALSE)

path <- report_weighting(
  fit, replicates = boot, domains = ~ region, open = FALSE,
  metadata = list(
    survey           = "Living Conditions Survey (illustrative)",
    reference_period = "2024",
    geography        = "National",
    producer         = "National Statistical Office",
    frame            = "Master sample of dwellings",
    totals_source    = "Population projections (2023)",
    totals_date      = "2024-06-30"))

## ----embed, echo = FALSE, results = "asis", eval = has_rpart------------------
h <- paste(readLines(path, warn = FALSE), collapse = "\n")
h <- gsub("&", "&amp;", h, fixed = TRUE)
h <- gsub('"', "&quot;", h, fixed = TRUE)
cat(sprintf(
  '<iframe srcdoc="%s" width="100%%" height="720" style="border:1px solid #ddd;border-radius:8px"></iframe>',
  h))

