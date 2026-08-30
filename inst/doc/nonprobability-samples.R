## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----frame--------------------------------------------------------------------
set.seed(1)
N   <- nrow(population)
p   <- plogis(-2 + 0.9 * (population$sex == "M") - 0.02 * (population$age - 45))
vol <- population[runif(N) < p, c("region", "sex", "age", "income")]  # volunteers
ref <- population[sample(N, 1000), c("region", "sex", "age")]         # reference
ref$d <- N / 1000                                                     # its design weights

truth <- mean(population$income)
c(truth = truth, naive_volunteer_mean = mean(vol$income))

## ----spec---------------------------------------------------------------------
ref_sample <- reference_sample(ref, weights = "d")

## ----pseudoweight, warning = FALSE, message = FALSE---------------------------
spec <- weighting_spec(vol, base_weights = NULL, nonprob = TRUE) |>
  step_pseudoweight(reference = ref_sample, formula = ~ region + sex + age,
                    engine = "logit")
fit <- prep(spec)

w <- fit$final_weight
c(pseudoweighted_mean = weighted.mean(vol$income, w),
  sum_of_weights = sum(w), N = N)     # the (1-p)/p pseudo-weights sum to N

## ----dr, eval = FALSE---------------------------------------------------------
# weighting_spec(vol, base_weights = NULL, nonprob = TRUE) |>
#   step_pseudoweight(reference = ref_sample, formula = ~ region + sex + age,
#                     engine = "logit") |>
#   step_calibrate(method = "raking", formula = ~ region + sex,
#                  population = ref_sample)

## ----datadefect---------------------------------------------------------------
dd <- data_defect(fit)
dd

## ----boot, warning = FALSE, message = FALSE-----------------------------------
boot <- bootstrap_weights(spec, replicates = 200, seed = 1, progress = FALSE)
boot_mean(boot, "income")

