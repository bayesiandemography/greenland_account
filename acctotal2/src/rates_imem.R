
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

## Get point estimates for immigration and emigration rates -------------------

exposure <- datasets$reg_popn %>%
    exposure()


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

rates_imem <- inner_join(rates_im, rates_em, by = "time")

saveRDS(rates_imem,
        file = "out/rates_imem.rds")

