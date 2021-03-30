
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
    mutate(cohort = as.integer(as.character(cohort)),
           time = as.integer(as.character(time))) %>%
    mutate(age = if_else(event == "Population (end of year)", # uses fact that population counts are at end of year
                         time - cohort,
                         time - cohort - (triangle == "Upper"))) %>%
    filter(age >= 0) ## excludes 7 corrections where age < 0

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

population <- data_df %>%
    filter(event == "Population (end of year)") %>%
    mutate(age = factor(age, levels = age_levels)) %>%
  dtabs(count ~ age + sex + time) %>%
  Counts(dimscales = c(age = "Intervals", time = "Points"))

births <- births_df %>%
    dtabs(count ~ age + sex + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

deaths <- data_df %>%
    filter(event == "Death") %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

immigration <- data_df %>%
    filter(event == "Immigration") %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

emigration <- data_df %>%
    filter(event == "Emigration") %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

correction <- data_df %>%
    filter(event == "Correction") %>%
    mutate(age = factor(age, levels = age_levels)) %>%
    dtabs(count ~ age + sex + time) %>%
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
    filter(event == "Population (end of year)",
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
    






data_corr <- pxweb_get_data(url = "https://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BEXCALC.PX",
                          query =   list(cohort = "*",
                                         "place of birth" = "T",
                                         gender = "*",
                                         "triangles(Lexis)" = "*",
                                         event = "C",
                                         time = "2020"))
    
