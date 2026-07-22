## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----recode, eval = FALSE-----------------------------------------------------
# dat$unknown_elig <- as.integer(dat$disposition %in%
#                                  c("noncontact", "undelivered", "not_worked"))
# dat$ineligible   <- as.integer(dat$disposition %in%
#                                  c("out_of_scope", "vacant", "business"))
# dat$responded    <- as.integer(dat$disposition == "complete")

## ----dispositions-------------------------------------------------------------
dat <- sample_one

# the whole field disposition in a single column, matching the tree above
table(disposition = dat$disposition)

## ----two-styles---------------------------------------------------------------
# the indicator column and the equivalent condition on `disposition` agree
identical(dat$ineligible == 1L, dat$disposition == "ineligible")

# so these two calls are interchangeable:
#   step_drop_ineligible(ineligible = ineligible)
#   step_drop_ineligible(ineligible = disposition == "ineligible")

## ----pipeline-----------------------------------------------------------------
dat$age_grp <- cut(dat$age, c(0, 30, 45, 60, Inf),
                   labels = c("18-30", "31-45", "46-60", "60+"))

fitted <- weighting_spec(dat, base_weights = pw) |>
  # 1. unresolved cases: redistribute their weight within region (no roster,
  #    so at the household level via cluster)
  step_unknown_eligibility(unknown = disposition == "unknown eligibility",
                           by = "region", cluster = "household_id") |>
  # 2. out-of-scope cases: remove after they absorbed the unknown share
  step_drop_ineligible(ineligible = disposition == "ineligible") |>
  # 3. household nonresponse: reached households vs not, within region
  #    (whole-household outcome, so at the household level via cluster)
  step_nonresponse(respondent = disposition != "household nonresponse",
                   method = "weighting_class", by = "region",
                   cluster = "household_id") |>
  # 4. within-household selection of one person
  step_select_within(prob = p_within) |>
  # 5. person nonresponse, within demographic cells
  step_nonresponse(respondent = disposition == "eligible respondent",
                   method = "weighting_class", by = c("region", "sex", "age_grp")) |>
  prep()

fitted

## ----summary------------------------------------------------------------------
summary(fitted)

## ----pipeline-indicators------------------------------------------------------
fitted2 <- weighting_spec(dat, base_weights = pw) |>
  step_unknown_eligibility(unknown = unknown_elig, by = "region",
                           cluster = "household_id") |>
  step_drop_ineligible(ineligible = ineligible) |>
  step_nonresponse(respondent = hh_responded, method = "weighting_class",
                   by = "region", cluster = "household_id") |>
  step_select_within(prob = p_within) |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = c("region", "sex", "age_grp")) |>
  prep()

# same weights as the disposition-based version
all.equal(fitted$final_weight, fitted2$final_weight)

