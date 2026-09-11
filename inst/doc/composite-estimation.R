## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----setup-data---------------------------------------------------------------
wv <- lapply(1:3, function(k) {
  d <- subset(panel_ine, wave == k & disposition == "R"); d$sex <- factor(d$sex); d
})
Xtot <- function(d) colSums(d$pw * model.matrix(~ sex, data = d))

## ----seed---------------------------------------------------------------------
seed <- weighting_spec(wv[[1]], base_weights = pw) |>
  step_cre(previous = NULL, status = lf_status, formula = ~ sex,
           totals = Xtot(wv[[1]]), status_ref = "inact") |>
  prep()

wave2 <- weighting_spec(wv[[2]], base_weights = pw) |>
  step_cre(previous = seed, status = lf_status, composite = list(NULL, "sex"),
           id_unit = c("household_id", "person_no"), formula = ~ sex,
           totals = Xtot(wv[[2]]), alpha = 2/3, status_ref = "inact")

## ----compare------------------------------------------------------------------
EST <- list(unemp_rate = function(w, d) weighted.mean(d$unemployed, w, na.rm = TRUE))

run <- function(composite) {
  prev <- NULL; out <- list()
  for (k in 1:3) {
    sp <- weighting_spec(wv[[k]], base_weights = pw)
    sp <- if (composite)
      step_cre(sp, previous = if (k == 1) NULL else out[[k - 1]]$prepped,
               status = lf_status, composite = list(NULL, "sex"),
               id_unit = c("household_id", "person_no"), formula = ~ sex,
               totals = Xtot(wv[[k]]), alpha = 2/3, status_ref = "inact")
    else
      step_calibrate(sp, method = "linear", formula = ~ sex, totals = Xtot(wv[[k]]))
    s <- wave_step(sp, previous = prev, estimands = EST, replicates = 150,
                   strata = "stratum", psu = "psu", period = paste0("T", k),
                   seed = 100 + k, progress = FALSE)
    out[[k]] <- list(step = s, carry = wave_carry(s), prepped = prep(sp))
    prev <- rev(lapply(out, function(z) z$carry))
  }
  out
}

cre   <- run(TRUE)
plain <- run(FALSE)

tab <- function(o, label) {
  ch <- o[[3]]$step$change
  data.frame(recipe = label, from = ch$from, se = round(ch$se, 5),
             rho = round(ch$rho, 3), deff_change = round(ch$deff_change, 3))
}
rbind(tab(cre, "composite (CRE)"), tab(plain, "plain calibration"))

## ----var04--------------------------------------------------------------------
b <- tryCatch(bootstrap_weights(wave2, replicates = 5, strata = "stratum",
                                psu = "psu", progress = FALSE),
              warning = function(w) conditionMessage(w))
b

## ----sizes--------------------------------------------------------------------
c(thin_KB = as.numeric(object.size(plain[[1]]$carry)) / 1024,
  fat_KB  = as.numeric(object.size(cre[[1]]$carry))   / 1024)

## ----audit--------------------------------------------------------------------
c(injected = cre[[2]]$step$n_cre_injected, skipped = cre[[2]]$step$n_cre_skipped)

## ----rotation, eval = FALSE---------------------------------------------------
# step_cre(spec, previous = seed, status = lf_status, composite = list(NULL, "sex"),
#          id_unit = c("household_id", "person_no"), formula = ~ sex,
#          totals = Xtot(wv[[2]]), rotation_group = "rotation_group",
#          status_ref = "inact")

