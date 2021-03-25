
library(tidyverse)
library(demest)
library(pxweb)

## Download and reformat data -------------------------------------------------

data_df <- pxweb_get_data(url = "https://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BEXCALC.PX",
                          query =   list(cohort = "*",
                                         "place of birth" = "T",
                                         gender = c("M", "K"),
                                         "triangles(Lexis)" = "*",
                                         event = "*",
                                         time = "*")) %>%
    select(cohort,
           sex = gender,
           triangle = "triangles(Lexis)",
           event,
           time,
           count = "Population Account") %>%
    mutate(sex = factor(sex,
                        levels = c("Woman", "Man"),
                        labels = c("Female", "Male"))) %>%
    mutate(cohort = as.integer(cohort),
           time = as.integer(time)) %>%
    mutate(age = if_else(event == "Status ultimo", # uses fact that population counts are at end of year
                         time - cohort,
                         time - cohort - (triangle == "Upper"))) %>%
    filter((age >= 0) | (event == "Correction"))

## get births data from a live births table, which,
## unlike the population account table,
## includes the age of the mother
births_df <- pxweb_get_data(url = "https://bank.stat.gl/api/v1/en/Greenland/BE/BE10/BE1001/BE100120/BEXBBL3.PX",
                            query = list("age" = "*",
                                         "district (Mother)" = "00000",
                                         "gender" = "*",
                                         "time" = "*")) %>%
    select(age, sex = gender, time, count = Livebirth) %>%
    mutate(sex = factor(sex, levels = c("Girls", "Boys"), labels = c("Female", "Male")))
           

## Make components of demographic account -------------------------------------

## can't use maximum age from 'data_df', since this goes up to 120
age_max <- 102 ## 
## add extra, unused open age group because accounts need to have open age group
age_levels <- c(seq.int(from = 0, to = age_max), "103+")

time_min <- 1993

population <- data_df %>%
    filter(event == "Status ultimo") %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    dtabs(count ~ age + sex + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Points"))

births <- births_df %>%
    filter(time > time_min) %>%
    dtabs(count ~ age + sex + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

deaths <- data_df %>%
    filter(event == "Death") %>%
    filter(time > time_min) %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

immigration <- data_df %>%
    filter(event == "Immigration") %>%
    filter(time > time_min) %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

emigration <- data_df %>%
    filter(event == "Emigration") %>%
    filter(time > time_min) %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

correction <- data_df %>%
    filter(event == "Correction") %>%
    filter(time > time_min) %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))


## Make demographic account ---------------------------------------------------

## 'Movements' randomly allocates births to Lower and Upper triangles,
## though this makes no difference to the demographic accounting.

set.seed(0)

account_no_correction <- Movements(population = population,
                                   births = births,
                                   entries = list(immigration = immigration),
                                   exits = list(deaths = deaths,
                                                emigration = emigration))
summary(account_no_correction) ## note that 'all cells consistent' is FALSE


account_with_correction <- Movements(population = population,
                                     births = births,
                                     entries = list(immigration = immigration),
                                     exits = list(deaths = deaths,
                                                  emigration = emigration),
                                     net = list(correction = correction))
summary(account_with_correction) ## note that 'all cells consistent' is FALSE

## Apply crude (and very slow!) method to make accounts consistent
account_consistent <- makeConsistent(account_no_correction)
summary(account_consistent)


## Problem with accounting ----------------------------------------------------

population_check <- data_df %>%
    filter(event == "Status ultimo",
           sex == "Female",
           cohort == 1993,
           time %in% 1993:1994)

deaths_check <- data_df %>%
    filter(event == "Death",
           sex == "Female",
           cohort == 1993,
           time == 1994)

immigration_check <- data_df %>%
    filter(event == "Immigration",
           sex == "Female",
           cohort == 1993,
           time == 1994)

emigration_check <- data_df %>%
    filter(event == "Emigration",
           sex == "Female",
           cohort == 1993,
           time == 1994)

correction_check <- data_df %>%
    filter(event == "Correction",
           sex == "Female",
           cohort == 1993,
           time == 1994)


population_diff <- (population_check$count[population_check$time == "1994"]
    - population_check$count[population_check$time == "1993"])

components_diff <- (-sum(deaths_check$count)
    + sum(immigration_check$count)
    - sum(emigration_check$count)
    + sum(correction_check$count))

all.equal(population_diff, components_diff) ## currently differ by 1
    

## Annual sum of corrections

corrections_df <- pxweb_get_data(url = "https://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BEXCALC.PX",
                                 query =   list(cohort = "*",
                                                "place of birth" = "T",
                                                gender = c("M", "K"),
                                                "triangles(Lexis)" = "*",
                                                event = "C",
                                                time = "*")) %>%
    select(cohort,
           triangle = "triangles(Lexis)",
           time,
           count = "Population Account") %>%
    mutate(age = as.integer(time) - as.integer(cohort) - (triangle == "Upper"))

## annual totals
corrections_df %>%
    count(time, wt = count)

## some with age < 0
corrections_df %>%
    filter(age < 0, count > 0)


