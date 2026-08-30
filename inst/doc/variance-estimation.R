## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)
has_survey <- requireNamespace("survey", quietly = TRUE)
has_srvyr  <- requireNamespace("srvyr", quietly = TRUE) &&
              requireNamespace("dplyr", quietly = TRUE)

## ----recipe, warning = FALSE--------------------------------------------------
dat <- sample_one
dat$age_grp <- cut(dat$age, c(0, 30, 45, 60, Inf),
                   labels = c("18-30", "31-45", "46-60", "60+"))
dat$f <- 0.15                    # illustrative first-stage sampling fraction (used later)

spec <- weighting_spec(dat, base_weights = pw) |>
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
                                sex    = c(table(population$sex))))

boot <- bootstrap_weights(spec, replicates = 200, strata = "region",
                          psu = "psu", seed = 2024, progress = FALSE)
boot

## ----estimates----------------------------------------------------------------
boot_mean(boot,  "income")     # mean income
boot_total(boot, "employed")   # total employed
boot_mean(boot,  "employed")   # employment rate

## ----custom-------------------------------------------------------------------
bootstrap_estimate(boot, function(w, d) {
  ok <- !is.na(d$income) & w > 0
  stats::median(rep(d$income[ok], times = round(w[ok])))   # weighted median (approx.)
})

## ----survey, eval = has_survey------------------------------------------------
fitted <- prep(spec)
des <- as_svydesign(fitted, ids = "psu", strata = "region")
survey::svymean(~income, des, na.rm = TRUE)

## ----svrep, eval = has_survey-------------------------------------------------
rep_des <- as_svrepdesign(boot)
survey::svymean(~income, rep_des, na.rm = TRUE)

## ----srvyr, eval = has_srvyr--------------------------------------------------
df <- collect_replicate_weights(boot)
d_rep <- srvyr::as_survey_rep(df, weights = .weight,
                              repweights = dplyr::starts_with("rep_"),
                              type = "bootstrap", combined.weights = TRUE,
                              scale = 1 / attr(df, "R"), rscales = 1, mse = TRUE)
srvyr::summarise(d_rep, mean_income = srvyr::survey_mean(income, na.rm = TRUE))

## ----jackknife, warning = FALSE-----------------------------------------------
jk <- jackknife_weights(spec, strata = "region", psu = "psu", progress = FALSE)
jk

jack_mean(jk,  "income")     # mean income, with the JKn variance
jack_total(jk, "employed")   # total employed

## ----fpc, warning = FALSE-----------------------------------------------------
boot0 <- bootstrap_weights(spec, replicates = 200, strata = "region",
                           psu = "psu", seed = 2024, progress = FALSE)
bootf <- bootstrap_weights(spec, replicates = 200, strata = "region",
                           psu = "psu", fpc = "f", seed = 2024, progress = FALSE)
c(no_fpc = boot_total(boot0, "employed")$se,
  fpc    = boot_total(bootf, "employed")$se)   # the correction lowers the SE

## ----ci-----------------------------------------------------------------------
emp <- function(w, d) sum(w * d$employed, na.rm = TRUE)
bootstrap_estimate(boot, emp)                          # normal (default)
bootstrap_estimate(boot, emp, ci_type = "t")           # t: wider, uses df
bootstrap_estimate(boot, emp, ci_type = "percentile")  # empirical quantiles

