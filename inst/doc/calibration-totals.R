## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(weightflow)

## ----data---------------------------------------------------------------------
data(population)
data(sample_survey)

## ----ps-tidy------------------------------------------------------------------
region_totals <- as.data.frame(table(region = population$region))
region_totals

ps <- weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "poststratify", totals = region_totals, count = "Freq") |>
  prep()

sum(collect_weights(ps)$.weight)   # sums to the population size N

## ----ps-classic---------------------------------------------------------------
ps_classic <- weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "poststratify",
                 margins = list(region = c(table(population$region)))) |>
  prep()

sum(collect_weights(ps_classic)$.weight)

## ----ps-cross-----------------------------------------------------------------
rs_totals <- as.data.frame(table(region = population$region,
                                 sex    = population$sex))
head(rs_totals)

ps_cross <- weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "poststratify", totals = rs_totals, count = "Freq") |>
  prep()

sum(collect_weights(ps_cross)$.weight)

## ----rake-tidy----------------------------------------------------------------
m_region <- as.data.frame(table(region = population$region))
m_sex    <- as.data.frame(table(sex    = population$sex))

rk <- weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "raking",
                 totals = list(m_region, m_sex), count = "Freq") |>
  prep()

sum(collect_weights(rk)$.weight)

## ----rake-classic-------------------------------------------------------------
rk_classic <- weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "raking",
                 margins = list(region = c(table(population$region)),
                                sex    = c(table(population$sex)))) |>
  prep()

sum(collect_weights(rk_classic)$.weight)

## ----lin-classic--------------------------------------------------------------
# The classic model-matrix vector: intercept = N, and region *without* its
# reference level (the first, "North"), with model.matrix column names.
pop_tot <- c("(Intercept)" = nrow(population),
             regionSouth = sum(population$region == "South"),
             regionEast  = sum(population$region == "East"),
             regionWest  = sum(population$region == "West"),
             sexM        = sum(population$sex == "M"))

lin_classic <- weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "linear", formula = ~ region + sex, totals = pop_tot) |>
  prep()

sum(collect_weights(lin_classic)$.weight)

## ----lin-tidy-----------------------------------------------------------------
lin_tidy <- weighting_spec(sample_survey, base_weights = pw) |>
  step_calibrate(method = "linear", formula = ~ region + sex,
                 totals = list(region = m_region, sex = m_sex),
                 count = "Freq") |>
  prep()

sum(collect_weights(lin_tidy)$.weight)

## ----lin-mixed----------------------------------------------------------------
resp <- subset(sample_survey, responded == 1)

lin_mixed <- weighting_spec(resp, base_weights = pw) |>
  step_calibrate(method = "linear", formula = ~ region + sex + income,
                 totals = list(region = m_region, sex = m_sex,
                               income = sum(population$income)),
                 count = "Freq") |>
  prep()

# the calibrated weights reproduce every target
X <- model.matrix(~ region + sex + income, data = resp)
colSums(collect_weights(lin_mixed)$.weight * X)

## ----dom-raking---------------------------------------------------------------
# benchmarks known BY REGION: a sex margin and an age-group margin per region.
pop  <- transform(population,
  age_grp = cut(age, c(0, 30, 45, 60, Inf), labels = c("18-30","31-45","46-60","60+")))
samp <- transform(sample_survey,
  age_grp = cut(age, c(0, 30, 45, 60, Inf), labels = c("18-30","31-45","46-60","60+")))

sex_by_region <- as.data.frame(table(region = pop$region, sex     = pop$sex))
age_by_region <- as.data.frame(table(region = pop$region, age_grp = pop$age_grp))

dom <- weighting_spec(samp, base_weights = pw) |>
  step_calibrate(method = "raking",
                 totals = list(sex_by_region, age_by_region),
                 count = "Freq", by = "region") |>
  prep()

# within each region the weights reproduce BOTH margins
w <- dom$final_weight
round(xtabs(w ~ region + sex,     data = cbind(samp, w = w)))
round(xtabs(w ~ region + age_grp, data = cbind(samp, w = w)))

## ----dom-linear---------------------------------------------------------------
inc_by_region <- aggregate(income ~ region, population, sum)   # region, income
resp <- subset(sample_survey, responded == 1)

lin_dom <- weighting_spec(resp, base_weights = pw) |>
  step_calibrate(method = "linear", formula = ~ sex + income,
                 totals = list(sex = sex_by_region, income = inc_by_region),
                 count = "Freq", calfun = "raking", by = "region") |>
  prep()

# the income total is reproduced within each region
w   <- lin_dom$final_weight
got <- tapply(w * resp$income, resp$region, sum)
cbind(calibrated = round(got),
      benchmark  = inc_by_region$income[match(names(got), inc_by_region$region)])

## ----mc-xtotals---------------------------------------------------------------
resp <- subset(sample_survey, responded == 1)

mc <- weighting_spec(resp, base_weights = pw) |>
  step_model_calibration(
    x_formula  = ~ region + age,                                # consistency block
    models     = list(income = y_model(income ~ age + sex,      # model block
                                       engine = "glm")),
    population = population,                                     # used for prediction
    x_totals   = list(region = m_region, age = sum(population$age)),
    count      = "Freq") |>
  prep()

# both blocks are reproduced: the X totals and the model prediction total
mc$steps[[1]]$diagnostics

