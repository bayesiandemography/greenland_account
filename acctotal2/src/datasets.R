

## Create datasets from CSV files. Note that this is copied
## straight from 'acctotal', and the format is a bit awkward
## for the 'pomp' model. Depending on how things develop,
## we may change it.


library(dembase)
library(dplyr)
library(readr)

## Assemble individual datasets -----------------------------------------------

## Registered population 

reg_popn_df  <- read_csv("../data/reg_popn.csv")

reg_popn <- reg_popn_df %>%
    dtabs(count ~ time) %>%
    Counts(dimscales = c(time = "Points"))


## Registered births

reg_births_df  <- read_csv("../data/reg_births.csv")

reg_births <- reg_births_df %>%
    dtabs(count ~ time) %>%
    Counts(dimscales = c(time = "Intervals"))


## Registered deaths

reg_deaths_df  <- read_csv("../data/reg_deaths.csv")

reg_deaths <- reg_deaths_df %>%
    dtabs(count ~ time) %>%
    Counts(dimscales = c(time = "Intervals"))


## Registered immigration

reg_immigration_df  <- read_csv("../data/reg_immigration.csv")

reg_immigration <- reg_immigration_df %>%
    dtabs(count ~ time) %>%
    Counts(dimscales = c(time = "Intervals"))


## Registered emigration

reg_emigration_df  <- read_csv("../data/reg_emigration.csv")

reg_emigration <- reg_emigration_df %>%
    dtabs(count ~ time) %>%
    Counts(dimscales = c(time = "Intervals"))



## Combine into list and save -------------------------------------------------


datasets <- list(reg_popn = reg_popn,
                 reg_births = reg_births,
                 reg_deaths = reg_deaths,
                 reg_immigration = reg_immigration,
                 reg_emigration = reg_emigration)

saveRDS(datasets,
        file = "out/datasets.rds")

