## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----design-------------------------------------------------------------------
set.seed(11)
mk <- function(psus, shift = 0) {
  do.call(rbind, lapply(psus, function(k) {
    data.frame(stratum = (k - 1L) %/% 100L, psu = k, w = 20,
               y = rnorm(8, 10 + 2 * ((k %% 7) - 3) + shift, 2))
  }))
}
w1 <- mk(unlist(lapply(1:4, function(h) h * 100L + 1:5)))              # PSUs 1-5
w2 <- mk(unlist(lapply(1:4, function(h) h * 100L + c(3:5, 6:7))), 0.5) # 3-7: rotates 2 of 5
w3 <- mk(unlist(lapply(1:4, function(h) h * 100L + c(5:7, 8))), 1)     # 5-8: only 4 left

sp  <- function(d) weighting_spec(d, base_weights = w)
EST <- list(mean_y = function(w, d) weighted.mean(d$y, w))

## ----chain--------------------------------------------------------------------
t1 <- wave_step(sp(w1), estimands = EST, replicates = 300, strata = "stratum",
                psu = "psu", period = "T1", seed = 1, progress = FALSE)
k1 <- wave_carry(t1)

t2 <- wave_step(sp(w2), previous = k1, estimands = EST, replicates = 300,
                strata = "stratum", psu = "psu", period = "T2", seed = 2, progress = FALSE)
t2$strata

## ----caseiii------------------------------------------------------------------
k2 <- wave_carry(t2)
t3 <- wave_step(sp(w3), previous = list(k2, k1), estimands = EST, replicates = 300,
                strata = "stratum", psu = "psu", period = "T3", seed = 3, progress = FALSE)
t3$strata

## ----pairing------------------------------------------------------------------
c(cor_replicates = cor(k1$theta$mean_y, k2$theta$mean_y),
  rho_reported   = t2$change$rho)

## ----shuffle------------------------------------------------------------------
set.seed(4)
cor(k1$theta$mean_y, sample(k2$theta$mean_y))

## ----lags---------------------------------------------------------------------
t3$change[, c("from", "to", "estimate", "se", "rho", "deff_change")]

## ----untouched----------------------------------------------------------------
identical(t2$weights, prep(sp(w2))$final_weight)

