## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 5, fig.height = 3.4,
                      fig.align = "center", dpi = 120)
library(weightflow)
has_survey  <- requireNamespace("survey",  quietly = TRUE)
has_ggplot  <- requireNamespace("ggplot2", quietly = TRUE)
show_plot   <- has_survey && has_ggplot

## ----data---------------------------------------------------------------------
d   <- sample_survey
pop <- population

# a demographic breakdown gives the calibration more auxiliary variables
brk <- c(0, 30, 45, 60, Inf); lab <- c("18-30", "31-45", "46-60", "60+")
d$age_grp   <- cut(d$age,   brk, labels = lab)
pop$age_grp <- cut(pop$age, brk, labels = lab)

# tidy population margins reused throughout
reg_tab <- as.data.frame(table(region  = pop$region))
sex_tab <- as.data.frame(table(sex     = pop$sex))
age_tab <- as.data.frame(table(age_grp = pop$age_grp))

# model-matrix totals for the region + sex + age-group calibration
totals  <- colSums(model.matrix(~ region + sex + age_grp, pop))

## ----helper-------------------------------------------------------------------
# weightflow brand palette (from the pkgdown site)
wf_primary <- "#3d3580"; wf_violet <- "#7a6ad0"
wf_green   <- "#1d9e75"; wf_amber  <- "#c9822b"; wf_grey <- "#6b7280"

theme_wf <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", colour = wf_primary),
      plot.subtitle = ggplot2::element_text(colour = wf_grey, size = 9),
      axis.title    = ggplot2::element_text(colour = wf_grey),
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank())
}

# Helper: numeric agreement + the OVERLAP of the two g-weight (adjustment factor,
# g = final / base) distributions. Because the two packages agree, the survey and
# weightflow densities land exactly on top of each other.
agree <- numeric(0)                                   # collect max|diff| per method

compare <- function(w_sv, w_wf, label, base = d$pw) {
  diff <- max(abs(w_wf - w_sv))
  agree[[label]] <<- diff                             # store for the final table
  if (has_ggplot) {
    g <- rbind(data.frame(package = "survey",     g = w_sv / base),
               data.frame(package = "weightflow", g = w_wf / base))
    print(ggplot2::ggplot(g, ggplot2::aes(g, fill = package, colour = package)) +
      ggplot2::geom_density(alpha = 0.4, linewidth = 0.5) +
      ggplot2::scale_fill_manual(values = c(survey = wf_amber, weightflow = wf_violet)) +
      ggplot2::scale_colour_manual(values = c(survey = wf_amber, weightflow = wf_violet)) +
      ggplot2::labs(title = label,
                    subtitle = sprintf("g-weights overlap (survey vs weightflow) · max |w_wf - w_sv| = %.1e", diff),
                    x = "g (adjustment factor)", y = "density", fill = NULL, colour = NULL) +
      theme_wf())
  }
  invisible(diff)
}

## ----poststratify, eval = has_survey, fig.alt = "Scatter of weightflow versus survey weights for post-stratification, points on the identity line."----
library(survey)
ps_tab <- as.data.frame(table(region = pop$region, sex = pop$sex))

wf <- weighting_spec(d, base_weights = pw) |>
  step_calibrate(method = "poststratify", totals = ps_tab, count = "Freq") |>
  prep()
w_wf <- wf$final_weight

des    <- svydesign(ids = ~1, weights = ~pw, data = d)
des_ps <- postStratify(des, ~region + sex, ps_tab)
w_sv   <- weights(des_ps)

compare(w_sv, w_wf, "Post-stratification (region x sex)")

## ----raking, eval = has_survey, fig.alt = "Scatter of weightflow versus survey weights for raking, points on the identity line."----
wf <- weighting_spec(d, base_weights = pw) |>
  step_calibrate(method = "raking", totals = list(reg_tab, sex_tab, age_tab),
                 count = "Freq") |>
  prep()
w_wf <- wf$final_weight

des_rk <- rake(des, list(~region, ~sex, ~age_grp), list(reg_tab, sex_tab, age_tab),
               control = list(epsilon = 1e-10, maxit = 100))
w_sv   <- weights(des_rk)

compare(w_sv, w_wf, "Raking (region + sex + age group)")

## ----greg-linear, eval = has_survey, fig.alt = "Scatter of weightflow versus survey weights for linear GREG calibration, points on the identity line."----
wf <- weighting_spec(d, base_weights = pw) |>
  step_calibrate(method = "linear", formula = ~ region + sex + age_grp,
                 totals = totals, calfun = "linear") |>
  prep()
w_wf <- wf$final_weight
w_sv <- weights(calibrate(des, ~ region + sex + age_grp, population = totals,
                          calfun = "linear"))
g_linear <- w_wf / d$pw
compare(w_sv, w_wf, "Distance: linear (GREG)")

## ----greg-raking, eval = has_survey, fig.alt = "Scatter of weightflow versus survey weights for raking-distance calibration, points on the identity line."----
wf <- weighting_spec(d, base_weights = pw) |>
  step_calibrate(method = "linear", formula = ~ region + sex + age_grp,
                 totals = totals, calfun = "raking", maxit = 500, tol = 1e-10) |>
  prep()
w_wf <- wf$final_weight
w_sv <- weights(calibrate(des, ~ region + sex + age_grp, population = totals,
                          calfun = "raking", maxit = 500, epsilon = 1e-10))
g_raking <- w_wf / d$pw
compare(w_sv, w_wf, "Distance: raking (exponential)")

## ----greg-logit, eval = has_survey, fig.alt = "Scatter of weightflow versus survey weights for logit-distance calibration, points on the identity line."----
bnds <- c(0.5, 2)
wf <- weighting_spec(d, base_weights = pw) |>
  step_calibrate(method = "linear", formula = ~ region + sex + age_grp,
                 totals = totals, calfun = "logit", bounds = bnds,
                 maxit = 500, tol = 1e-10) |>
  prep()
w_wf <- wf$final_weight
w_sv <- weights(calibrate(des, ~ region + sex + age_grp, population = totals,
                          calfun = "logit", bounds = bnds, maxit = 500, epsilon = 1e-10))
g_logit <- w_wf / d$pw
compare(w_sv, w_wf, "Distance: logit (bounded)")

## ----gdist, eval = show_plot, fig.width = 6, fig.height = 3.6, fig.alt = "Density curves of the adjustment factors for the linear, raking and logit distances, centred near one."----
gdist <- rbind(
  data.frame(distance = "linear", g = g_linear),
  data.frame(distance = "raking", g = g_raking),
  data.frame(distance = "logit",  g = g_logit))
gdist$distance <- factor(gdist$distance, levels = c("linear", "raking", "logit"))

ggplot2::ggplot(gdist, ggplot2::aes(g, fill = distance)) +
  ggplot2::geom_density(alpha = 0.4, colour = NA) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = wf_grey) +
  ggplot2::scale_fill_manual(values = c(linear = wf_primary, raking = wf_green,
                                        logit = wf_amber)) +
  ggplot2::labs(title = "Distribution of adjustment factors",
                subtitle = "g = final / base, by calibration distance",
                x = "g (adjustment factor)", y = "density", fill = NULL) +
  theme_wf()

## ----integrative, eval = has_survey, fig.alt = "Scatter of weightflow versus survey weights for integrative one-weight-per-household calibration, points on the identity line."----
wf <- weighting_spec(d, base_weights = pw) |>
  step_calibrate(method = "linear", formula = ~ region + sex + age_grp,
                 totals = totals, cluster = "household_id",
                 equal_within_cluster = TRUE) |>
  prep()
w_wf <- wf$final_weight

des_hh  <- svydesign(ids = ~household_id, weights = ~pw, data = d)
des_int <- calibrate(des_hh, ~ region + sex + age_grp, population = totals,
                     calfun = "linear", aggregate.stage = 1)
w_sv    <- weights(des_int)

within_hh <- max(tapply(w_wf, d$household_id, function(z) diff(range(z))))
cat("max within-household weight range (weightflow):", within_hh, "\n")

compare(w_sv, w_wf, "Integrative (one weight per household)")

## ----domain, eval = has_survey, fig.alt = "Scatter of weightflow versus survey weights for domain-partitioned calibration, points on the identity line."----
sex_by_region <- as.data.frame(table(region = pop$region, sex     = pop$sex))
age_by_region <- as.data.frame(table(region = pop$region, age_grp = pop$age_grp))

wf <- weighting_spec(d, base_weights = pw) |>
  step_calibrate(method = "linear", formula = ~ sex + age_grp,
                 totals = list(sex = sex_by_region, age_grp = age_by_region),
                 count = "Freq", by = "region") |>
  prep()
w_wf <- wf$final_weight

w_sv <- numeric(nrow(d))
for (r in levels(d$region)) {
  idx   <- which(d$region == r)
  des_r <- svydesign(ids = ~1, weights = ~pw, data = d[idx, ])
  tot_r <- colSums(model.matrix(~ sex + age_grp, pop[pop$region == r, ]))
  w_sv[idx] <- weights(calibrate(des_r, ~ sex + age_grp, population = tot_r,
                                 calfun = "linear"))
}

compare(w_sv, w_wf, "Domain calibration (by region)")

## ----estimate, eval = has_survey----------------------------------------------
wf <- weighting_spec(d, base_weights = pw) |>
  step_calibrate(method = "raking", totals = list(reg_tab, sex_tab, age_tab),
                 count = "Freq") |>
  prep()
w_wf   <- wf$final_weight
des_rk <- rake(des, list(~region, ~sex, ~age_grp), list(reg_tab, sex_tab, age_tab),
               control = list(epsilon = 1e-10, maxit = 100))

est <- data.frame(
  quantity   = c("mean(age)", "total(age)"),
  weightflow = c(weighted.mean(d$age, w_wf), sum(w_wf * d$age)),
  survey     = c(as.numeric(coef(svymean(~age, des_rk))),
                 as.numeric(coef(svytotal(~age, des_rk)))))
est$difference <- est$weightflow - est$survey
est

## ----summary, eval = has_survey-----------------------------------------------
data.frame(method = names(agree), `max abs weight difference` = unname(agree),
           check.names = FALSE, row.names = NULL)

## ----trimcal-regenesees, eval = FALSE-----------------------------------------
# # weightflow
# w_wf <- muestra |>
#   weighting_spec(base_weights = pw) |>
#   step_calibrate(method = "linear", formula = ~ sex + age, totals = pop_tot) |>
#   step_trim_calibrated(~ sex + age, lower = lo, upper = up, calfun = "linear") |>
#   prep() |> collect_weights(drop_zero = FALSE)
# 
# # ReGenesees
# des   <- ReGenesees::e.svydesign(data = muestra, ids = ~ id, weights = ~ pw)
# templ <- ReGenesees::fill.template(universe = universo,
#            template = ReGenesees::pop.template(des, calmodel = ~ sex + age))
# cal   <- ReGenesees::e.calibrate(des, df.population = templ, calfun = "linear")
# w_rg  <- weights(ReGenesees::trimcal(cal, w.range = c(lo, up)))

