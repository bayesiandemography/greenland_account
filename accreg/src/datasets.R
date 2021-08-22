
library(dembase)
library(dplyr)
library(readr)

## Assemble individual datasets -----------------------------------------------

## Registered population 

reg_popn_df  <- read_csv("../data/reg_popn_region.csv")

reg_popn <- reg_popn_df %>%
    dtabs(count ~ age + sex + region + time) %>%
    Counts(dimscales = c(time = "Points"))


## Registered births

reg_births_df  <- read_csv("../data/reg_births_region.csv")

reg_births <- reg_births_df %>%
    dtabs(count ~ age + triangle + sex + region + time) %>%
    Counts(dimscales = c(time = "Intervals"))


## Registered deaths

reg_deaths_df  <- read_csv("../data/reg_deaths_region.csv")

reg_deaths <- reg_deaths_df %>%
    dtabs(count ~ age + triangle + sex + region + time) %>%
    Counts(dimscales = c(time = "Intervals"))


## Registered immigration

reg_immigration_df  <- read_csv("../data/reg_immigration_region.csv")

reg_immigration <- reg_immigration_df %>%
    dtabs(count ~ age + triangle + sex + region + time) %>%
    Counts(dimscales = c(time = "Intervals"))


## Registered emigration

reg_emigration_df  <- read_csv("../data/reg_emigration_region.csv")

reg_emigration <- reg_emigration_df %>%
    dtabs(count ~ age + triangle + sex + region + time) %>%
    Counts(dimscales = c(time = "Intervals"))


## Registered internal migration

reg_internal_df  <- read_csv("../data/reg_internal_region.csv")

reg_internal <- reg_internal_df %>%
    mutate(direction = factor(direction, levels = c("Out", "In"))) %>%
    dtabs(count ~ age + triangle + sex + region + time + direction) %>%
    Counts(dimscales = c(time = "Intervals"))



## Combine into list and save -------------------------------------------------


datasets <- list(reg_popn = reg_popn,
                 reg_births = reg_births,
                 reg_deaths = reg_deaths,
                 reg_immigration = reg_immigration,
                 reg_emigration = reg_emigration,
                 reg_internal = reg_internal)

saveRDS(datasets,
        file = "out/datasets.rds")

