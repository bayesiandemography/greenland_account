
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
## and sex of the parent

births <- datasets$reg_births %>%
    collapseDimension(dimension = "sex")

exposure_bth <- datasets$reg_popn %>%
    subarray(sex == "Female") %>%
    subarray(age %in% dimnames(births)$age) %>%
    exposure(triangle = TRUE)

filename_bth <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ age + region + time),
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
estimateModel(Model(y ~ Poisson(mean ~ age * sex + region + time),
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


## Combined inward (no exposure term)

y <- datasets$reg_immigration + subarray(datasets$reg_internal, direction == "In")

filename_cin <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ age * sex + region + time,
                                useExpose = FALSE),
                    jump = 0.4),
              y = y,
              filename = filename_cin,
              nBurnin = 2000,
              nSim = 2000,
              nChain = 4,
              nThin = 20)
fetchSummary(filename_cin)
rates_cin <- fetch(filename_cin,
                  where = c("model", "likelihood", "count")) %>%
    collapseIterations(FUN = median) %>%
    as.data.frame() %>%
    rename(rate_cin = count)


## Combined outward (with exposure term)

y <- datasets$reg_emigration + subarray(datasets$reg_internal, direction == "Out")

exposure <- datasets$reg_popn %>%
    exposure(triangles = TRUE) %>%
    makeCompatible(y)

exposure[(y > 0) & (exposure == 0)] <- 0.5

filename_cout <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ age * sex + region + time),
                    jump = 0.4),
              y = y,
              exposure = exposure,
              filename = filename_cout,
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 5)
fetchSummary(filename_cout)
rates_cout <- fetch(filename_cout,
                    where = c("model", "likelihood", "rate")) %>%
    collapseIterations(FUN = median) %>%
    as.data.frame() %>%
    rename(rate_cout = value)




## Combine and save -----------------------------------------------------------

rates <- rates_dth %>% ## 'rates_dth' has ages and sexes that 'rates_bth' does not
    left_join(rates_bth, by = c("age", "triangle", "sex", "region", "time")) %>% 
    inner_join(rates_cin, by = c("age", "triangle", "sex", "region", "time")) %>%
    inner_join(rates_cout, by = c("age", "triangle", "sex", "region", "time")) %>%
    tibble() %>%
    mutate(birth_cohort = time - age - (triangle == "Upper"),
           born_before_est_period = birth_cohort <= min(time),
           k = if_else(born_before_est_period,
                       2L * (time - min(time)) + (triangle == "Lower") + 1L,
                       2L * age + (triangle == "Upper") + 1L)) %>%
    select(birth_cohort, sex, region, k, rate_bth, rate_dth, rate_cin, rate_cout) %>%
    arrange(k, sex, birth_cohort, region)


saveRDS(rates,
        file = "out/rates.rds")

