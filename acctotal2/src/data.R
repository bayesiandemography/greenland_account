
## Construct 'data' argument for model -
## registered population, immigration,
## and emigration

library(dplyr)

datasets <- readRDS("out/datasets.rds")

rates <- readRDS("out/rates.rds")

data_popn <- datasets$reg_popn %>%
    as.data.frame() %>%
    slice(-1) %>%
    rename(YN = count)

data_bth <- datasets$reg_births %>%
    as.data.frame() %>%
    rename(Ybth = count)

data_dth <- datasets$reg_deaths %>%
    as.data.frame() %>%
    rename(Ydth = count)

data_im <- datasets$reg_immigration %>%
    as.data.frame() %>%
    rename(Yim = count)

data_em <- datasets$reg_emigration %>%
    as.data.frame() %>%
    rename(Yem = count)

data_rates <- rates %>%
    select(time, Yratebth = rate_bth, Yratedth = rate_dth)

data <- data_popn %>%
    inner_join(data_bth, by = "time") %>%
    inner_join(data_dth, by = "time") %>%
    inner_join(data_im, by = "time") %>%
    inner_join(data_em, by = "time") %>%
    inner_join(data_rates, by = "time")

saveRDS(data,
        file = "out/data.rds")


