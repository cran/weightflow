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

## ----np-two-files, echo = FALSE, results = "asis"-----------------------------
cat(wf_fig("np-two-files.svg", "The volunteer file and the probability reference"))

## ----frame--------------------------------------------------------------------
set.seed(1)
pop <- population
pop$poor    <- as.integer(pop$income < quantile(pop$income, 0.12))  # ~12%: the rare one
pop$retired <- as.integer(pop$age >= 65)
N <- nrow(pop)

take <- runif(N) < plogis(-1.9 + 0.9 * (pop$sex == "M") - 0.025 * (pop$age - 45))
vol  <- pop[take, ]                                   # the volunteers: no weights at all
ri   <- sample(which(!take), 800)
ref  <- pop[ri, c("region", "sex", "age")]            # the reference: no study variables
ref$d <- N / 800                                      # its design weights

c(volunteers = nrow(vol), reference = nrow(ref))

## ----bias---------------------------------------------------------------------
ind   <- c("income", "employed", "poor", "retired")
truth <- vapply(ind, function(v) mean(pop[[v]]), numeric(1))
pct   <- function(w) round(100 * (vapply(ind, function(v)
           weighted.mean(vol[[v]], w), numeric(1)) / truth - 1), 1)

pct(rep(1, nrow(vol)))        # relative bias (%) of the unweighted volunteer file

## ----refreps------------------------------------------------------------------
rep_ref <- bootstrap_weights(weighting_spec(ref, base_weights = d),
                             replicates = 40, seed = 11,
                             progress = FALSE)$replicates

sr <- reference_sample(ref, weights = "d", replicates = rep_ref)

## ----pseudo, warning = FALSE--------------------------------------------------
pw_only <- weighting_spec(vol, base_weights = NULL, nonprob = TRUE) |>
  step_pseudoweight(reference = sr, formula = ~ region + sex + age,
                    engine = "logit") |>
  prep()

c(sum_of_weights = sum(pw_only$final_weight), N = N)

## ----np-traces, echo = FALSE, results = "asis"--------------------------------
cat(wf_fig("np-traces.svg", "Three ways to turn one fitted score into a weight, and the trace each leaves"))

## ----traces, warning = FALSE--------------------------------------------------
ref1 <- ref; ref1$one <- 1          # the reference stacked at weight one
pw_one <- weighting_spec(vol, base_weights = NULL, nonprob = TRUE) |>
  step_pseudoweight(reference = reference_sample(ref1, weights = "one"),
                    formula = ~ region + sex + age) |>
  prep(warn = FALSE)

round(c(odds           = sum(pw_only$final_weight) / N,
        plain_inverse  = (sum(pw_only$final_weight) + nrow(vol)) / N,
        reference_at_1 = sum(pw_one$final_weight) / N,
        n_vol_over_N   = nrow(vol) / N,
        n_ref_over_N   = nrow(ref) / N), 4)

## ----np-recipe, echo = FALSE, results = "asis"--------------------------------
cat(wf_fig("np-recipe.svg", "The multipurpose doubly robust weight as a single recipe"))

## ----unicall, warning = FALSE-------------------------------------------------
cens <- list(sex    = as.data.frame(table(sex    = pop$sex)),
             region = as.data.frame(table(region = pop$region)))

spec <- weighting_spec(vol, base_weights = NULL, nonprob = TRUE) |>
  step_pseudoweight(reference = sr, formula = ~ region + sex + age,
                    engine = "logit") |>
  step_model_calibration(
    x_formula = ~ sex + region,          # the KNOWN control totals: met exactly
    x_totals  = cens,
    models    = list(                    # one working model per study variable
      income   = y_model(income   ~ age + sex + region, engine = "glm"),
      employed = y_model(employed ~ age + sex + region, engine = "glm")),
    population = sr,
    crossfit   = 3)

fit <- prep(spec)
w   <- fit$final_weight

## ----fourway------------------------------------------------------------------
rbind(unweighted = pct(rep(1, nrow(vol))),
      uni_weight = pct(w))

## ----cons---------------------------------------------------------------------
head(fit$steps[[2]]$diagnostics, 7)

## ----np-ceiling, echo = FALSE, results = "asis"-------------------------------
cat(wf_fig("np-ceiling.svg", "The rank ceiling and the two ways out of it"))

## ----ceiling, warning = FALSE-------------------------------------------------
mk <- function(ys, rhs = "~ age + sex + region")
  setNames(lapply(ys, function(v)
    y_model(as.formula(paste(v, rhs)), engine = "glm")), ys)

dev <- function(models, crossfit = NULL) {
  f <- prep(weighting_spec(vol, base_weights = NULL, nonprob = TRUE) |>
    step_pseudoweight(reference = sr, formula = ~ region + sex + age) |>
    step_model_calibration(x_formula = ~ sex + region, x_totals = cens,
                           models = models, population = sr, crossfit = crossfit),
    warn = FALSE)
  d <- f$steps[[2]]$diagnostics
  max(abs(d$achieved - d$target) / abs(d$target))   # worst constraint, relative
}

signif(c(K1              = dev(mk("income")),
         K2              = dev(mk(c("income", "employed"))),
         K2_crossfit     = dev(mk(c("income", "employed")), crossfit = 3),
         K2_richer_basis = dev(mk(c("income", "employed"),
                                  "~ age + I(age^2) + sex * region"))), 2)

## ----cost, warning = FALSE----------------------------------------------------
ys <- c("income", "employed", "poor", "retired")
cost <- t(vapply(seq_along(ys), function(k) {
  f <- prep(weighting_spec(vol, base_weights = NULL, nonprob = TRUE) |>
    step_pseudoweight(reference = sr, formula = ~ region + sex + age) |>
    step_model_calibration(x_formula = ~ sex + region, x_totals = cens,
                           models = mk(ys[seq_len(k)]), population = sr,
                           crossfit = 3), warn = FALSE)
  wk <- f$final_weight
  c(K = k, deff = design_effect(wk)$deff, max_min = max(wk) / min(wk[wk > 0]),
    negatives = sum(wk < 0))
}, numeric(4)))
round(cost, 2)

## ----refvar, warning = FALSE--------------------------------------------------
sr_fixed <- reference_sample(ref, weights = "d")          # no replicates
spec_fixed <- weighting_spec(vol, base_weights = NULL, nonprob = TRUE) |>
  step_pseudoweight(reference = sr_fixed, formula = ~ region + sex + age) |>
  step_model_calibration(x_formula = ~ sex + region, x_totals = cens,
                         models = mk(c("income", "employed")),
                         population = sr_fixed, crossfit = 3)

b_full  <- bootstrap_weights(spec,       replicates = 20, seed = 2, progress = FALSE)
b_fixed <- bootstrap_weights(spec_fixed, replicates = 20, seed = 2, progress = FALSE)

c(reference_resampled = boot_mean(b_full,  "income")$se,
  reference_fixed     = boot_mean(b_fixed, "income")$se)

## ----disseminate--------------------------------------------------------------
out <- collect_replicate_weights(b_full, scramble = TRUE)
c(rows = nrow(out), columns = ncol(out))
names(out)[1:4]

## ----datadefect---------------------------------------------------------------
data_defect(pw_only)

## ----nonign, warning = FALSE--------------------------------------------------
set.seed(2)
z   <- as.numeric(scale(pop$income))                 # the driver nothing observes
t2  <- runif(N) < plogis(-1.6 + 0.8 * z)
v2  <- pop[t2, ]
r2  <- pop[sample(which(!t2), 800), c("region", "sex", "age")]; r2$d <- N / 800
sr2 <- reference_sample(r2, weights = "d")

f2 <- prep(weighting_spec(v2, base_weights = NULL, nonprob = TRUE) |>
  step_pseudoweight(reference = sr2, formula = ~ sex + age) |>
  step_model_calibration(x_formula = ~ sex,
                         x_totals = list(sex = as.data.frame(table(sex = pop$sex))),
                         models = list(income = y_model(income ~ sex + age)),
                         population = sr2, crossfit = 3), warn = FALSE)

w2 <- f2$final_weight; d2 <- f2$steps[[2]]$diagnostics
c(max_total_deviation = max(abs(d2$achieved - d2$target)),
  negative_weights    = sum(w2 < 0),
  design_effect       = round(design_effect(w2)$deff, 2))

## ----benchmark----------------------------------------------------------------
round(100 * (as.numeric(tapply(w2, v2$region, sum)) /
             as.numeric(table(pop$region)) - 1), 1)

