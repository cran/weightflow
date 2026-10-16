## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)
set.seed(20260910)
# Figures are kept as standalone .svg files and embedded as data URIs. An inline
# <svg> element does not survive an HTML re-parse (libxml2 lowercases attribute
# names, so `viewBox` becomes `viewbox` and the figure collapses to zero height);
# a data URI is one attribute value and is copied verbatim.
wf_fig <- function(file, alt = "")
  sprintf('<img src="%s" alt="%s" style="width:100%%;max-width:960px;" />',
          knitr::image_uri(file.path("figures", file)), alt)

## ----pl-eclac, echo = FALSE, results = "asis"---------------------------------
cat(wf_fig("pl-eclac.svg", "The ECLAC chapter XVI sequence and the recipe verb for each step"))

## ----merge--------------------------------------------------------------------
waves <- split(panel_puro, panel_puro$wave)
names(waves) <- paste0("T", 1:4)

wide <- panel_merge(waves, by = c("household_id", "person_no"), require = "all")
c(T1_sample = nrow(waves$T1), linked_all_four = nrow(wide))

## ----shrink-------------------------------------------------------------------
vapply(2:4, function(k)
  nrow(panel_merge(waves[1:k], by = c("household_id", "person_no"), require = "all")),
  integer(1))

## ----disposition--------------------------------------------------------------
table(wave_4 = wide$disposition_T4)

## ----cascade------------------------------------------------------------------
resp <- vapply(1:4, function(k) wide[[paste0("disposition_T", k)]] == "R", logical(nrow(wide)))
wide$responded_always <- rowSums(resp) == 4

lw <- weighting_spec(wide, base_weights = pw_T1) |>
  step_drop_ineligible(disposition_T4 == "OS", reason = "left the target population") |>
  step_unknown_eligibility(disposition_T4 == "UNK", by = "region_T1") |>
  step_attrition(respondent = responded_always, method = "propensity",
                 formula = ~ age_T1 + sex_T1 + region_T1) |>
  prep()

lw

## ----composition--------------------------------------------------------------
grp <- function(a) cut(a, c(-Inf, 29, 49, Inf), labels = c("18-29", "30-49", "50+"))
t1  <- waves$T1
sh  <- function(w, g) round(100 * tapply(w, g, sum) / sum(w), 1)
act <- lw$final_weight > 0

rbind(T1_target  = sh(t1$pw, grp(t1$age)),
      unadjusted = sh(wide$pw_T1[wide$responded_always], grp(wide$age_T1[wide$responded_always])),
      adjusted   = sh(lw$final_weight[act], grp(wide$age_T1[act])))

## ----attrition----------------------------------------------------------------
data.frame(through = paste0("T", 1:4),
           complete = vapply(1:4, function(k) sum(rowSums(resp[, 1:k, drop = FALSE]) == k), 0L))

## ----calibrate----------------------------------------------------------------
t1 <- waves$T1
sex_tab <- data.frame(sex_T1   = names(tapply(t1$pw, t1$sex, sum)),
                      Freq      = as.numeric(tapply(t1$pw, t1$sex, sum)))
reg_tab <- data.frame(region_T1 = names(tapply(t1$pw, t1$region, sum)),
                      Freq      = as.numeric(tapply(t1$pw, t1$region, sum)))

lw <- weighting_spec(wide, base_weights = pw_T1) |>
  step_drop_ineligible(disposition_T4 == "OS", reason = "left the target population") |>
  step_unknown_eligibility(disposition_T4 == "UNK", by = "region_T1") |>
  step_attrition(respondent = responded_always, method = "propensity",
                 formula = ~ age_T1 + sex_T1 + region_T1) |>
  step_calibrate(method = "raking", totals = list(sex_tab, reg_tab), count = "Freq") |>
  prep()

c(calibrated = sum(lw$final_weight), T1_population = sum(t1$pw))

## ----flows--------------------------------------------------------------------
STATES <- c("emp", "unemp", "inact")
transition_matrix(lw, from = "lf_status_T1", to = "lf_status_T4",
                  states = STATES, format = "row")

## ----boot---------------------------------------------------------------------
b <- bootstrap_weights(lw, replicates = 100, strata = "stratum_T1", psu = "psu_T1",
                       seed = 1, progress = FALSE)
boot_transition(b, "lf_status_T1", "lf_status_T4", states = STATES, format = "row")

## ----grossflows---------------------------------------------------------------
boot_flows(b, "lf_status_T1", "lf_status_T4", states = STATES)

