
## Construct 'data' argument for model -
## registered population, immigration,
## and emigration

library(dplyr)

datasets <- readRDS("out/datasets.rds")

data_popn <- datasets$reg_popn %>%
    as.data.frame() %>%
    tibble() %>%
    rename(YN = count) %>%
    mutate(birth_cohort = time - age,
           born_before_est_period = birth_cohort <= min(time) ,
           k = if_else(born_before_est_period,
                       2L * (time - min(time)),
                       2L * age + 1)) %>%
    select(birth_cohort, sex, k, YN) %>%
    filter(k > 0) ## k == 0 included in N0


data_im <- datasets$reg_immigration %>%
    as.data.frame() %>%
    tibble() %>%
    rename(Yim = count) %>%
    mutate(birth_cohort = time - age - (triangle == "Upper"),
           born_before_est_period = birth_cohort <= min(time),
           k = if_else(born_before_est_period,
                       2L * (time - min(time)) + (triangle == "Lower") + 1L,
                       2L * age + (triangle == "Upper") + 1L)) %>%
    select(birth_cohort, sex, k, Yim)
    

data_em <- datasets$reg_emigration %>%
    as.data.frame() %>%
    tibble() %>%
    rename(Yem = count) %>%
    mutate(birth_cohort = time - age - (triangle == "Upper"),
           born_before_est_period = birth_cohort <= min(time),
           k = if_else(born_before_est_period,
                       2L * (time - min(time)) + (triangle == "Lower") + 1L,
                       2L * age + (triangle == "Upper") + 1L)) %>%
    select(birth_cohort, sex, k, Yem)

data <- data_popn %>%
    full_join(data_im, by = c("birth_cohort", "sex", "k")) %>%
    left_join(data_em, by = c("birth_cohort", "sex", "k")) %>%
    arrange(birth_cohort, sex, k)


saveRDS(data,
        file = "out/data.rds")
