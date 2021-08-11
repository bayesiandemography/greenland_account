
## Calculate immigration and emigration rates,
## which will eventually be supplied to the model
## as covariates. For the time being, we
## use a single set of rates, the posterior
## medians.

library(dembase)
library(demest)
library(dplyr)

datasets <- readRDS("out/datasets.rds")

options(width = 120) ## to display summary properly

## Get point estimates for demographic rates ----------------------------------


## Births (with birth-specific exposure term)

## Note that birth rates arranged by the age, triangle,
## and *sex* of the parent

births <- datasets$reg_births %>%
    collapseDimension(dimension = "sex")

exposure_bth <- datasets$reg_popn %>%
    subarray(sex == "Female") %>%
    subarray(age %in% dimnames(births)$age) %>%
    exposure(triangle = TRUE)

filename_bth <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ age + time),
                    jump = 0.4),
              y = births,
              exposure = exposure_bth,
              filename = filename_bth,
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 5)
fetchSummary(filename_bth)
rates_bth <- fetch(filename_bth,
                  where = c("model", "likelihood", "rate")) %>%
    collapseIterations(FUN = median) %>%
    as.data.frame() %>%
    rename(rate_bth = value) %>%
    mutate(sex = "Female")


## Deaths (with exposure term)

deaths <- datasets$reg_deaths
exposure <- datasets$reg_popn %>%
    exposure(triangles = TRUE) %>%
    aperm(perm = names(deaths))
exposure[(deaths > 0) & (exposure == 0)] <- 0.5

filename_dth <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ age * sex + time),
                    jump = 0.4),
              y = datasets$reg_deaths,
              exposure = exposure,
              filename = filename_dth,
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 5)
fetchSummary(filename_dth)
rates_dth <- fetch(filename_dth,
                  where = c("model", "likelihood", "rate")) %>%
    collapseIterations(FUN = median) %>%
    as.data.frame() %>%
    rename(rate_dth = value)


## Immigration (no exposure term)

filename_im <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ age * sex + time,
                                useExpose = FALSE),
                    jump = 0.4),
              y = datasets$reg_immigration,
              filename = filename_im,
              nBurnin = 2000,
              nSim = 2000,
              nChain = 4,
              nThin = 20)
fetchSummary(filename_im)
rates_im <- fetch(filename_im,
                  where = c("model", "likelihood", "count")) %>%
    collapseIterations(FUN = median) %>%
    as.data.frame() %>%
    rename(rate_im = count)


## Emigration (with exposure term)

exposure <- datasets$reg_popn %>%
    exposure(triangles = TRUE)

filename_em <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ age * sex + time),
                    jump = 0.4),
              y = datasets$reg_emigration,
              exposure = exposure,
              filename = filename_em,
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 5)
fetchSummary(filename_em)
rates_em <- fetch(filename_em,
                  where = c("model", "likelihood", "rate")) %>%
    collapseIterations(FUN = median) %>%
    as.data.frame() %>%
    rename(rate_em = value)


## Combine and save -----------------------------------------------------------

rates <- rates_dth %>% ## 'rates_dth' has ages and sexes that 'rates_bth' does not
    left_join(rates_bth, by = c("age", "triangle", "sex", "time")) %>% 
    inner_join(rates_im, by = c("age", "triangle", "sex", "time")) %>%
    inner_join(rates_em, by = c("age", "triangle", "sex", "time")) %>%
    tibble() %>%
        mutate(birth_cohort = time - age - (triangle == "Upper"),
           born_before_est_period = birth_cohort <= min(time),
           k = if_else(born_before_est_period,
                       2L * (time - min(time)) + (triangle == "Lower") + 1L,
                       2L * age + (triangle == "Upper") + 1L)) %>%
    select(birth_cohort, sex, k, rate_bth, rate_dth, rate_im, rate_em) %>%
    arrange(k, sex, birth_cohort)


saveRDS(rates,
        file = "out/rates.rds")

