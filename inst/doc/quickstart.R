## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)
# Figures are kept as standalone .svg files and embedded as data URIs. An inline
# <svg> element does not survive an HTML re-parse (libxml2 lowercases attribute
# names, so `viewBox` becomes `viewbox` and the figure collapses to zero height);
# a data URI is one attribute value and is copied verbatim.
wf_fig <- function(file, alt = "")
  sprintf('<img src="%s" alt="%s" style="width:100%%;max-width:680px;" />',
          knitr::image_uri(file.path("figures", file)), alt)

## ----qs-scattered, echo = FALSE, results = "asis"-----------------------------
cat(wf_fig("qs-scattered.svg", "Weighting spread across scripts and intermediate files"))

## ----qs-map, echo = FALSE, results = "asis"-----------------------------------
cat(wf_fig("qs-map.svg", "From target population to respondents"))

## ----qs-cascade, echo = FALSE, results = "asis"-------------------------------
cat(wf_fig("qs-cascade.svg", "The whole strategy as one recipe object"))

## ----glimpse------------------------------------------------------------------
str(sample_survey[, c("pw", "region", "sex", "unknown_elig", "responded")])

## ----totals-------------------------------------------------------------------
m_region <- as.data.frame(table(region = population$region))
m_sex    <- as.data.frame(table(sex    = population$sex))
m_region

## ----recipe-------------------------------------------------------------------
fit <- weighting_spec(sample_survey, base_weights = pw) |>
  step_unknown_eligibility(unknown = unknown_elig, by = "region") |>
  step_nonresponse(respondent = responded, method = "weighting_class",
                   by = "region") |>
  step_calibrate(method = "raking",
                 totals = list(m_region, m_sex), count = "Freq") |>
  prep()

## ----weights------------------------------------------------------------------
w <- collect_weights(fit)
summary(w$.weight)

## ----summary------------------------------------------------------------------
summary(fit)

