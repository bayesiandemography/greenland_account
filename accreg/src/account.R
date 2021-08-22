
library(dembase)
library(dplyr)
library(readr)

datasets <- readRDS("out/datasets.rds")

## The number of ins, by age, sex, and time, does not quite equal th
## number of outs. We need to harmonize them before we can create
## a Pool object.

reg_internal <- datasets$reg_internal
ins <- reg_internal %>%
    subarray(direction == "In")
outs_raw <- reg_internal %>%
    subarray(direction == "Out")
eps <- 0.001
outs_adj <- ins %>%
    collapseDimension(dimension = "region") %>%
    redistribute(weights = outs_raw + eps) ## to cope with zeros
internal <- dbind(Out = outs_adj, In = ins, along = "direction") %>%
    Pool( direction = "direction", between = "region")


## Make account

account <- Movements(population = datasets$reg_popn,
                     births = datasets$reg_births,
                     internal = internal,
                     entries = list(immigration = datasets$reg_immigration),
                     exits = list(deaths = datasets$reg_deaths,
                                  emigration = datasets$reg_emigration))

saveRDS(account,
        file = "out/account.rds")

