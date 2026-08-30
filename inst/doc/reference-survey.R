## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(weightflow)

## ----ref----------------------------------------------------------------------
set.seed(1)
N   <- nrow(population)
ref <- population[sample(N, 2000), ]
ref$w <- N / nrow(ref)          # the reference survey's own design weight

fit <- weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "raking", formula = ~ region + sex,
                 population = reference_sample(ref, "w")) |>
  prep()

# the calibration hit the reference's *weighted* region totals:
tapply(collect_weights(fit)$.weight, sample_survey$region, sum)
tapply(ref$w, ref$region, sum)

## ----frame-equivalence--------------------------------------------------------
frame_ref <- population
frame_ref$w <- 1
w_ref   <- (weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "raking", formula = ~ region,
                 population = reference_sample(frame_ref, "w")) |> prep())$final_weight
w_plain <- (weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "raking",
                 margins = list(region = c(table(population$region)))) |> prep())$final_weight
all.equal(w_ref, w_plain)

## ----variance-----------------------------------------------------------------
# replicate weights for the reference survey (its own design)
rep_ref <- bootstrap_weights(weighting_spec(ref, base_weights = w),
                             replicates = 100, strata = "region", psu = "psu",
                             seed = 1, progress = FALSE)$replicates

boot <- weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "raking", formula = ~ region + sex,
                 population = reference_sample(ref, "w", replicates = rep_ref)) |>
  bootstrap_weights(replicates = 100, strata = "region", psu = "psu",
                    seed = 2, progress = FALSE)

boot_total(boot, "income")

