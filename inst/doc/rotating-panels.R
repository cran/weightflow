## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)
set.seed(20260910)

## ----design-------------------------------------------------------------------
pd <- panel_design(panel_ine, unit = c("household_id", "person_no"), wave = "wave",
                   rotation_group = "rotation_group", pattern = "6")
pd

## ----profiles-----------------------------------------------------------------
prof <- function(p) round(weightflow:::.wf_pattern_overlap(p)$profile[1:5], 3)
rbind(`6        (Canada LFS, ECH Uruguay)` = prof("6"),
      `4(0)1    (ECLAC ch. XVI example)`   = prof("4(0)1"),
      `2-(2)-2  (Chile ENE, Italy)`        = prof("2-(2)-2"),
      `4-8-4    (US CPS)`                  = prof("4-8-4"),
      `1(2)5    (PNAD Continua)`           = prof("1(2)5"))

## ----change-------------------------------------------------------------------
w1 <- subset(panel_ine, wave == 1 & disposition == "R")
w2 <- subset(panel_ine, wave == 2 & disposition == "R")
rec <- function(d) weighting_spec(d, base_weights = pw) |>
  step_nonresponse(respondent = disposition == "R", by = "sex")

wb <- wave_bootstrap(list(T1 = rec(w1), T2 = rec(w2)), replicates = 100,
                     strata = "stratum", psu = "psu", seed = 1, progress = FALSE)
change_mean(wb, "unemployed")

## ----contrast, eval = FALSE---------------------------------------------------
# panel_estimate(wb, mean_of("unemployed"), contrast = c(-1, 1))   # the net change
# panel_estimate(wb, mean_of("unemployed"))                        # the average

## ----chain, eval = FALSE------------------------------------------------------
# # period 1: nothing to coordinate with yet
# s1 <- wave_step(rec(w1), estimands = EST, replicates = 500,
#                 strata = "stratum", psu = "psu", period = "2026-01", seed = 1)
# saveRDS(wave_carry(s1), "carry/2026-01.rds")
# 
# # period 2, weeks later, in a fresh session
# prev <- readRDS("carry/2026-01.rds")
# s2 <- wave_step(rec(w2), previous = prev, estimands = EST, replicates = 500,
#                 strata = "stratum", psu = "psu", period = "2026-02", seed = 2)
# s2$weights   # the cross-sectional weights the office publishes
# s2$change    # the net change against 2026-01, with rho and deff_change
# s2$strata    # the coordination diagnostic, stratum by stratum
# saveRDS(wave_carry(s2), "carry/2026-02.rds")

## ----wavecontrast, eval = FALSE-----------------------------------------------
# tr <- lapply(c("2026-01", "2026-02", "2026-03"),
#              \(m) readRDS(sprintf("carry/%s.rds", m)))
# 
# wave_contrast(tr, "unemployment_rate")                          # rolling quarter
# wave_contrast(tr, "unemployment_rate", contrast = c(-1, 0, 1))  # T3 - T1

## ----flows--------------------------------------------------------------------
wide <- panel_merge(list(T1 = subset(panel_ine, wave == 1),
                         T2 = subset(panel_ine, wave == 2)),
                    by = c("household_id", "person_no"), require = "all")

lw <- weighting_spec(wide, base_weights = pw_T1) |>
  step_drop_ineligible(disposition_T2 == "OS", reason = "left the target population") |>
  step_attrition(respondent = disposition_T2 == "R", method = "propensity",
                 formula = ~ age_T1 + sex_T1) |>
  prep()

transition_matrix(lw, from = "lf_status_T1", to = "lf_status_T2", format = "row")

