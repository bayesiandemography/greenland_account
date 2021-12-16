
library(demest)
library(dplyr)
library(tidyr)


prob <- c(0.025, 0.5, 0.975)

vals_old <- fetch("../accreg/out/model.est", where = c("account", "population")) %>%
    collapseIterations(prob = prob) %>%
    midpoints(dimension = "age") %>%
    as.data.frame() %>%
    pivot_wider(names_from = quantile, values_from = count) %>%
    rename(lower = "2.5%", median = "50%", upper = "97.5%")

saveRDS(vals_old,
        file = "vals_old.rds")

