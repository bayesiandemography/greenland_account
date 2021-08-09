
## Construct 'data' argument for model -
## registered population, immigration,
## and emigration

library(dplyr)

datasets <- readRDS("out/datasets.rds")

data_popn <- datasets$reg_popn %>%
    as.data.frame() %>%
    slice(-1) %>%
    rename(YN = count)

data_im <- datasets$reg_immigration %>%
    as.data.frame() %>%
    rename(Yim = count)

data_em <- datasets$reg_emigration %>%
    as.data.frame() %>%
    rename(Yem = count)

data <- data_popn %>%
    inner_join(data_im, by = "time") %>%
    inner_join(data_em, by = "time")


saveRDS(data,
        file = "out/data.rds")


