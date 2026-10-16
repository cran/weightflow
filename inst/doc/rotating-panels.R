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

## ----rp-calendar, echo = FALSE, results = "asis"------------------------------
cat(wf_fig("rp-calendar.svg", "Which earlier periods share sample with period t"))

## ----change-------------------------------------------------------------------
w1 <- subset(panel_ine, wave == 1 & disposition == "R")
w2 <- subset(panel_ine, wave == 2 & disposition == "R")
rec <- function(d) weighting_spec(d, base_weights = pw) |>
  step_nonresponse(respondent = disposition == "R", by = "sex")

wb <- wave_bootstrap(list(T1 = rec(w1), T2 = rec(w2)), replicates = 60,
                     strata = "stratum", psu = "psu", seed = 1, progress = FALSE)
change_mean(wb, "unemployed")

## ----rp-cycle, echo = FALSE, results = "asis"---------------------------------
cat(wf_fig("rp-cycle.svg", "The cycle of one period"))

## ----plan---------------------------------------------------------------------
dk   <- subset(panel_ine, wave == 1 & disposition == "R")
plan <- rec(dk) |>
  step_domain(sex) |>                                   # domains of study
  step_estimate(mean(unemployed), label = "unemp")      # the estimand

plan

## ----midchain-----------------------------------------------------------------
d1 <- subset(panel_ine, wave == 1 & disposition == "R")
d2 <- subset(panel_ine, wave == 2 & disposition == "R")
one <- function(d) rec(d) |> step_estimate(mean(unemployed), label = "unemp")
two <- function(d) one(d) |> step_estimate(mean(age), label = "mean_age")   # declared late

s1 <- wave_step(rec(d1), estimands = one(d1), replicates = 20, strata = "stratum",
                psu = "psu", period = "t1", seed = 1, progress = FALSE)
s2 <- try(wave_step(rec(d2), previous = wave_carry(s1), estimands = two(d2),
                    replicates = 20, strata = "stratum", psu = "psu",
                    period = "t2", seed = 2, progress = FALSE), silent = TRUE)

## ----chain--------------------------------------------------------------------
store <- file.path(tempdir(), "carry"); dir.create(store, showWarnings = FALSE)
month <- c("2026-01", "2026-02", "2026-03")

for (k in 1:3) {
  dk   <- subset(panel_ine, wave == k & disposition == "R")
  plan <- rec(dk) |> step_domain(sex) |> step_estimate(mean(unemployed), label = "unemp")
  prev <- lapply(rev(list.files(store, full.names = TRUE)), readRDS)   # what is on disk
  out  <- wave_step(rec(dk), previous = if (length(prev)) prev else NULL,
                    estimands = plan, replicates = 60, strata = "stratum", psu = "psu",
                    period = month[k], seed = k, progress = FALSE)
  saveRDS(wave_carry(out), file.path(store, paste0(month[k], ".rds")))
}

list.files(store)

## ----chainout-----------------------------------------------------------------
out$change[, c("estimand", "from", "to", "estimate", "se", "rho", "deff_change")]

## ----untouched----------------------------------------------------------------
all.equal(out$weights, prep(rec(subset(panel_ine, wave == 3 & disposition == "R")))$final_weight)

## ----strata-------------------------------------------------------------------
out$strata

## ----rp-carry, echo = FALSE, results = "asis"---------------------------------
cat(wf_fig("rp-carry.svg", "What the carry holds, thin and fat"))

## ----carrysize----------------------------------------------------------------
cy <- readRDS(file.path(store, "2026-03.rds"))
names(cy)[!vapply(cy, is.null, logical(1))]

c(carry_KB  = round(file.size(file.path(store, "2026-03.rds")) / 1024, 1),
  period_rows = nrow(subset(panel_ine, wave == 3 & disposition == "R")))

## ----fatcarry-----------------------------------------------------------------
thin <- wave_step(rec(w1), estimands = one(w1), replicates = 60, strata = "stratum",
                  psu = "psu", period = "t1", seed = 1, progress = FALSE)
fat  <- wave_step(rec(w1), estimands = one(w1), replicates = 60, strata = "stratum",
                  psu = "psu", period = "t1", seed = 1, carry = "fat", progress = FALSE)

f <- function(x) { p <- tempfile(); saveRDS(wave_carry(x), p); round(file.size(p) / 1024, 1) }
c(thin_KB = f(thin), fat_KB = f(fat))

## ----contrast-----------------------------------------------------------------
tr <- lapply(file.path(store, paste0(month, ".rds")), readRDS)

wave_contrast(tr, "unemp")[, c("estimand", "estimate", "se", "lo", "hi")]
wave_contrast(tr, "unemp", contrast = c(-1, 0, 1))[, c("estimand", "estimate", "se")]

## ----sigma--------------------------------------------------------------------
S <- vapply(tr, function(p) p$theta[["unemp"]], numeric(60))
colnames(S) <- month
signif(stats::cov(S), 3)

## ----rp-branches, echo = FALSE, results = "asis"------------------------------
cat(wf_fig("rp-branches.svg", "The two branches: how much changed, and who moved"))

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

