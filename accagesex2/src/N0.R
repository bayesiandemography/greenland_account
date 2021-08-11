
## Construct 'data' argument for model -
## registered population, immigration,
## and emigration

library(dplyr)

datasets <- readRDS("out/datasets.rds")

initial_popn <- datasets$reg_popn %>%
    as.data.frame() %>%
    mutate(birth_cohort = time - age,
           born_before_est_period = birth_cohort <= min(time)) %>%
    filter(born_before_est_period,
           time == min(time)) %>%
    select(birth_cohort, sex, count) %>%
    rename(N0 = count)

births <- datasets$reg_births %>%
    as.data.frame() %>%
    count(sex, time, wt = count, name = "N0") %>%
    rename(birth_cohort = time)

N0 <- bind_rows(initial_popn, births) %>%
    tibble()

saveRDS(N0,
        file = "out/N0.rds")
