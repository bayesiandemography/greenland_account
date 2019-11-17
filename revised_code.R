
library(pxweb)
library(dplyr)
library(tidyr)
library(dembase)

raw_data_df <- pxweb_get_data(url = "http://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BEXCALC.PX",
                              query =   list(generation = "*",
                                             "place of birth" = "T",
                                             gender = c("1", "2"),
                                             "triangles(lexis)" = "*",
                                             event = "*",
                                             time = "*"))

data_df <- raw_data_df %>%
    select(cohort = generation,
           sex = gender,
           triangle = "triangles(lexis)",
           event,
           time,
           count = "Population Account") %>%
    filter(count > 0L) %>% # restrict to observed combinations
    mutate(sex = factor(sex, 
                        levels = c("Woman", "Man"),
                        labels = c("Female", "Male"))) %>%
    mutate(cohort = as.integer(levels(cohort))[cohort],
           time = as.integer(levels(time))[time]) %>%
    mutate(age = if_else(event == "Status ultimo", # uses fact that population counts are at end of year
                         time - cohort,
                         time - cohort - (triangle == "Upper")))
    
population <- data_df %>%
    filter(event == "Status ultimo") %>%
    dtabs(count ~ age + sex + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Points"))
plot(population)

births <- data_df %>%
    filter(event == "Birth") %>%
    mutate(age = factor(age, 
                        levels = seq(from = min(age), to = max(age)))) %>% # missing some ages
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))
plot(births)
## all births have an "Upper" Lexis triangle

deaths <- data_df %>%
    filter(event == "Death") %>%
    mutate(age = factor(age, 
                        levels = seq(from = min(age), to = max(age)))) %>% # missing some ages
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))
plot(deaths)
## some negative ages

immigration <- data_df %>%
    filter(event == "Immigration") %>%
    mutate(age = factor(age, 
                        levels = seq(from = min(age), to = max(age)))) %>% # missing some ages
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))
plot(immigration)
## some negative ages

emigration <- data_df %>%
    filter(event == "Emigration") %>%
    mutate(age = factor(age, 
                        levels = seq(from = min(age), to = max(age)))) %>% # missing some ages
    dtabs(count ~ age + sex + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))
plot(emigration)
## some negative ages
