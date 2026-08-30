## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----frame--------------------------------------------------------------------
set.seed(1)
NH  <- 4000L; m <- 3L                       # first-phase households, members each
reg <- sample(c("A", "B", "C"), NH, replace = TRUE, prob = c(0.5, 0.3, 0.2))
u   <- rnorm(NH, c(A = 10, B = 16, C = 24)[reg], 4)

frame <- data.frame(
  hh      = rep(seq_len(NH), each = m),
  region  = rep(reg, each = m),
  income  = rep(u, each = m) + rnorm(NH * m, 0, 5),
  w1      = 10                              # first-phase design weight
)

## phase 2: Poisson subsample of households, rate by region
p2_by  <- c(A = 0.25, B = 0.45, C = 0.70)
sel_hh <- runif(NH) < p2_by[reg]
frame$selected <- as.integer(frame$hh %in% which(sel_hh))
frame$p2       <- p2_by[frame$region]

## ----recipe-------------------------------------------------------------------
spec <- weighting_spec(frame, base_weights = w1) |>
  step_subsample(selected = selected, prob = p2, psu = "hh")

fit <- prep(spec)
summary(fit)

## ----boot---------------------------------------------------------------------
boot <- bootstrap_weights(spec, replicates = 200, seed = 1, progress = FALSE)
boot_mean(boot, "income")

## ----cascade, eval = FALSE----------------------------------------------------
# weighting_spec(frame, base_weights = w1) |>
#   step_subsample(selected = selected, prob = p2, psu = "hh") |>
#   step_nonresponse(respondent = responded, method = "weighting_class", by = "region") |>
#   step_calibrate(margins = list(region = region_totals), method = "poststratify")

## ----ref-cal, eval = FALSE----------------------------------------------------
# # `phase1` is the full first-phase sample (with the auxiliary `x` and its weight
# # `w1`); `phase1_reps` are replicate weights for the first-phase design.
# ref <- reference_sample(phase1, weights = "w1", replicates = phase1_reps)
# 
# weighting_spec(sample, base_weights = w1) |>
#   step_subsample(selected = selected, prob = p2, psu = "hh") |>
#   step_calibrate(method = "linear", formula = ~ x, population = ref)

