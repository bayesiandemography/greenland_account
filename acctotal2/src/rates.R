
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

exposure <- datasets$reg_popn %>%
    exposure()


## Births (with exposure term)

filename_bth <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ 1)),
              y = datasets$reg_births,
              exposure = exposure,
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
    rename(rate_bth = value)


## Deaths (with exposure term)

filename_dth <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ 1)),
              y = datasets$reg_births,
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
estimateModel(Model(y ~ Poisson(mean ~ 1, useExpose = FALSE)),
              y = datasets$reg_immigration,
              filename = filename_im,
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 5)
fetchSummary(filename_im)
rates_im <- fetch(filename_im,
                  where = c("model", "likelihood", "count")) %>%
    collapseIterations(FUN = median) %>%
    as.data.frame() %>%
    rename(rate_im = count)


## Emigration (with exposure term)

filename_em <- tempfile()
estimateModel(Model(y ~ Poisson(mean ~ 1)),
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

rates <- rates_bth %>%
    inner_join(rates_dth, by = "time") %>%
    inner_join(rates_im, by = "time") %>%
    inner_join(rates_em, by = "time")


saveRDS(rates,
        file = "out/rates.rds")

