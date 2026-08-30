## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----recipe-------------------------------------------------------------------
rec <- weighting_spec(sample_survey, base_weights = pw) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "raking", id = "calib_main",
                 margins = list(region = c(table(population$region)),
                                sex    = c(table(population$sex))))
fit <- prep(rec)
summary(fit)

## ----gate---------------------------------------------------------------------
if (has_alerts(fit)) {
  # in CI: stop() here so the pipeline fails and the run is not published
  weighting_alerts(fit)
} else {
  "no quality incidents"
}

## ----assert, eval = FALSE-----------------------------------------------------
# rec |> step_assert(max_deff = 2.5, min_n_eff = 500)

## ----seed---------------------------------------------------------------------
boot <- bootstrap_weights(fit, replicates = 100, strata = "region",
                          psu = "psu", seed = 20260601, progress = FALSE)
boot_mean(boot, "income")

## ----report, eval = FALSE-----------------------------------------------------
# report_weighting(
#   fit, replicates = boot, file = "weights_2026.html", lang = "en",
#   metadata = list(
#     survey           = "Continuous Household Survey",
#     reference_period = "2026",
#     producer         = "National Statistical Office",
#     frame            = "Population and housing census 2023",
#     totals_source    = "Population projections 2026",
#     version          = "1.0"))

## ----disseminate, eval = FALSE------------------------------------------------
# pub <- collect_replicate_weights(boot)          # point + replicate weights
# # survey / srvyr read them directly:
# des <- as_svrepdesign(boot)

